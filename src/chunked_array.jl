# Chunked Array Reading API
# Provides ChunkedArray type for iterating over chunks

"""
    ChunkInfo

Metadata for a single chunk in the B-tree.
"""
struct ChunkInfo
    offset::RelOffset
    chunk_size::Int
    filter_mask::Int
    idx::Tuple  # 0-based HDF5 element indices (including element size dimension)
end

"""
    ChunkedArray{T,N}

Lazy wrapper around a chunked dataset that allows iterating over individual chunks
without loading the entire array into memory.

# Fields
- `file::JLDFile` - The JLD2 file containing the dataset
- `dataset_offset::RelOffset` - Offset of the dataset header
- `array_size::NTuple{N,Int}` - Size of the full array
- `chunk_dims::NTuple{N,Int}` - Dimensions of each chunk
- `datatype::H5Datatype` - HDF5 datatype of the array
- `layout::DataLayout` - Data layout information (must be chunked)
- `filters::FilterPipeline` - Compression filters applied to chunks
- `rr::ReadRepresentation{T}` - Read representation for deserialization
- `chunks::Vector{ChunkInfo}` - Parsed chunk metadata from B-tree
"""
struct ChunkedArray{T,N}
    file::JLDFile
    dataset_offset::RelOffset
    array_size::NTuple{N,Int}
    chunk_dims::NTuple{N,Int}
    datatype::H5Datatype
    layout::DataLayout
    filters::FilterPipeline
    rr::ReadRepresentation{T}
    dataspace::ReadDataspace
    chunks::Vector{ChunkInfo}  # Pre-parsed chunk metadata
end

"""
    Chunk{T,N}

Represents a single chunk of data with its metadata.

# Fields
- `data::Array{T,N}` - The actual chunk data
- `indices::CartesianIndex{N}` - Starting index of the chunk in the full array (1-based)
- `chunk_indices::CartesianIndex{N}` - Chunk grid position (1-based)
- `offset::RelOffset` - File offset where the chunk data is stored
- `size::Int` - Size of the chunk data in bytes
"""
struct Chunk{T,N}
    data::Array{T,N}
    indices::CartesianIndex{N}
    chunk_indices::CartesianIndex{N}
    offset::RelOffset
    size::Int
end

"""
    get_chunked_array(f::JLDFile, name::String) -> ChunkedArray

Get a ChunkedArray object for a dataset, allowing iteration over individual chunks.

# Example
```julia
jldopen("file.jld2", "r") do f
    chunked = JLD2.get_chunked_array(f, "my_dataset")

    # Iterate over all chunks
    for chunk in chunked
        println("Chunk at ", chunk.indices, ": ", size(chunk.data))
    end

    # Or access a specific chunk
    chunk = chunked[2, 3]  # Get chunk at grid position (2,3)
end
```
"""
function get_chunked_array(f::JLDFile, name::String)
    # Get the RelOffset for this dataset
    link = lookup_link(f.root_group, name)
    if isnothing(link)
        throw(KeyError(name))
    end

    # Extract offset from link
    offset = if isa(link, HardLink)
        link.target
    else
        throw(ArgumentError("Can only create ChunkedArray from direct datasets, not external links"))
    end

    # Read dataset header to get chunk information
    dataspace = ReadDataspace()
    dt::H5Datatype = PlaceholderH5Datatype()
    layout::DataLayout = DataLayout(0,LcCompact,0,-1)
    filter_pipeline::FilterPipeline = FilterPipeline()

    for msg in HeaderMessageIterator(f, offset)
        if msg.type == HmDataspace
            dataspace = ReadDataspace(f, msg)
        elseif msg.type == HmDatatype
            dt = HmWrap(HmDatatype, msg).dt::H5Datatype
        elseif msg.type == HmDataLayout
            layout = DataLayout(f, msg)
        elseif msg.type == HmFilterPipeline
            filter_pipeline = FilterPipeline(msg)
        end
    end

    if dt isa PlaceholderH5Datatype
        throw(InvalidDataException("No datatype message found"))
    end

    if !ischunked(layout)
        throw(ArgumentError("Dataset is not chunked"))
    end

    # Get read representation
    rr = jltype(f, dt)

    # Extract array dimensions by reading from file
    ndims = Int(dataspace.dimensionality)
    seek(f.io, dataspace.dimensions_offset)
    array_size = Tuple(reverse(ntuple(i -> Int(jlread(f.io, Int64)), Val(ndims))))
    N = ndims

    # Extract chunk dimensions from layout
    # For V1 B-tree (version 3), layout.chunk_dimensions is in HDF5 order (reversed)
    # and already excludes element size
    # We reverse it and take first ndims elements to get Julia order
    chunk_dims = Tuple(reverse(Int.(layout.chunk_dimensions))[1:ndims])

    T = julia_repr(rr)

    # Parse the B-tree to get all chunk metadata
    chunks = if layout.version == 3
        # Use existing function to read all chunks from V1 B-tree
        raw_chunks = read_v1btree_dataset_chunks(f, h5offset(f, layout.data_offset), layout.dimensionality)
        # Convert to ChunkInfo objects
        ChunkInfo[ChunkInfo(c.offset, c.chunk_size, c.filter_mask, c.idx) for c in raw_chunks]
    else
        ChunkInfo[]  # Empty for non-V1-btree layouts
    end

    return ChunkedArray{T,N}(f, offset, array_size, chunk_dims, dt, layout, filter_pipeline, rr, dataspace, chunks)
end

"""
    chunk_dimensions(ca::ChunkedArray) -> NTuple

Get the dimensions of each chunk.
"""
chunk_dimensions(ca::ChunkedArray) = ca.chunk_dims

"""
    num_chunks(ca::ChunkedArray) -> Int

Get the total number of chunks in the array.
"""
function num_chunks(ca::ChunkedArray{T,N}) where {T,N}
    prod(cld.(ca.array_size, ca.chunk_dims))
end

"""
    chunk_grid_size(ca::ChunkedArray) -> NTuple

Get the dimensions of the chunk grid (number of chunks in each dimension).
"""
function chunk_grid_size(ca::ChunkedArray{T,N}) where {T,N}
    Tuple(cld.(ca.array_size, ca.chunk_dims))
end

# Iteration interface
function Base.iterate(ca::ChunkedArray{T,N}) where {T,N}
    grid_size = chunk_grid_size(ca)
    indices = CartesianIndices(grid_size)
    state = iterate(indices)
    state === nothing && return nothing

    chunk_idx, next_state = state
    chunk = _read_chunk(ca, chunk_idx)
    return (chunk, (indices, next_state))
end

function Base.iterate(ca::ChunkedArray{T,N}, state) where {T,N}
    indices, iter_state = state
    next = iterate(indices, iter_state)
    next === nothing && return nothing

    chunk_idx, next_state = next
    chunk = _read_chunk(ca, chunk_idx)
    return (chunk, (indices, next_state))
end

Base.length(ca::ChunkedArray) = num_chunks(ca)
Base.eltype(::Type{ChunkedArray{T,N}}) where {T,N} = Chunk{T,N}

# Indexing interface
function Base.getindex(ca::ChunkedArray{T,N}, I::Vararg{Int,N}) where {T,N}
    # Convert to CartesianIndex and read chunk
    chunk_idx = CartesianIndex(I)

    # Validate indices
    grid_size = chunk_grid_size(ca)
    for (i, (idx, max_idx)) in enumerate(zip(I, grid_size))
        if idx < 1 || idx > max_idx
            throw(BoundsError(ca, I))
        end
    end

    return _read_chunk(ca, chunk_idx)
end

"""
    _read_chunk(ca::ChunkedArray, chunk_idx::CartesianIndex)

Internal function to read a specific chunk from the file.
"""
function _read_chunk(ca::ChunkedArray{T,N}, chunk_idx::CartesianIndex{N}) where {T,N}
    # Calculate starting position in the array (1-based)
    array_indices = CartesianIndex(Tuple((chunk_idx.I .- 1) .* ca.chunk_dims .+ 1))

    # Calculate actual chunk size (may be smaller at edges)
    chunk_end = min.(array_indices.I .+ ca.chunk_dims .- 1, ca.array_size)
    actual_chunk_size = chunk_end .- array_indices.I .+ 1

    # Convert to 0-based HDF5 indices in reversed order with element size dimension
    # HDF5 uses element indices (not chunk counts)
    hdf5_element_indices_0based = reverse(array_indices.I .- 1)
    hdf5_indices_with_elemsize = tuple(hdf5_element_indices_0based..., 0)

    # Find chunk in pre-parsed list
    chunk_info = nothing
    for c in ca.chunks
        if c.idx == hdf5_indices_with_elemsize
            chunk_info = c
            break
        end
    end

    if isnothing(chunk_info)
        throw(InvalidDataException("Chunk not found at grid indices $(chunk_idx.I), " *
                                 "array indices $(array_indices.I), " *
                                 "HDF5 indices $hdf5_indices_with_elemsize"))
    end

    # Read chunk data directly from file
    chunk_data = _read_chunk_data(
        ca.file,
        chunk_info,
        T,
        actual_chunk_size,
        ca.rr,
        ca.filters
    )

    return Chunk{T,N}(
        chunk_data,
        array_indices,
        chunk_idx,
        chunk_info.offset,
        chunk_info.chunk_size
    )
end

"""
    _read_chunk_data(f::JLDFile, chunk_info, T, chunk_dims, rr, filters)

Read and decompress chunk data from file.
"""
function _read_chunk_data(f::JLDFile, chunk_info, ::Type{T}, chunk_dims, rr, filters) where T
    io = f.io
    seek(io, fileoffset(f, chunk_info.offset))

    # Create array to hold chunk data
    ndims = length(chunk_dims)
    vchunk = Array{T, ndims}(undef, chunk_dims...)

    # Read chunk with appropriate filter handling
    read_chunk_with_filters!(vchunk, f, rr, chunk_info.chunk_size, filters, chunk_info.filter_mask)

    return vchunk
end

"""
    read_chunked_array(f::JLDFile, v::Array, dataspace::ReadDataspace,
                      rr::ReadRepresentation, layout::DataLayout,
                      filters::FilterPipeline, header_offset::RelOffset,
                      ndims::Int)

Read a chunked dataset from file into the preallocated array `v`.
Handles V1 B-tree chunked storage with optional compression filters.
"""
function read_chunked_array(f::JLDFile, v::Array{T}, dataspace::ReadDataspace,
                           @nospecialize(rr::ReadRepresentation), layout::DataLayout,
                           filters::FilterPipeline, header_offset::RelOffset,
                           ndims::Int) where T
    io = f.io

    if layout.version == 3
        # version 1 B-tree
        # This version appears to be padding incomplete chunks
        chunks = read_v1btree_dataset_chunks(f, h5offset(f, layout.data_offset), layout.dimensionality)
        # array cartesian indices
        ids = CartesianIndices(v)
        # chunk dimensions in same order as data dims
        chunk_dims = reverse(layout.chunk_dimensions)[1:ndims]
        # chunk indices starting at 1,1
        chunk_ids = CartesianIndices(tuple(chunk_dims...))

        # For V1 B-tree, layout.chunk_dimensions already excludes element size
        for chunk in chunks
            seek(io, fileoffset(f, chunk.offset))

            vchunk = Array{T, Int(ndims)}(undef, chunk_dims...)
            read_chunk_with_filters!(vchunk, f, rr, chunk.chunk_size, filters, chunk.filter_mask)

            cidx = chunk.idx::NTuple{Int(ndims+1), Int}
            chunk_root = CartesianIndex(reverse(cidx[1:end-1]))
            # Index into v
            vidxs = intersect(ids, chunk_ids .+ chunk_root)
            # Index into chunk
            ch_idx = CartesianIndices(size(vidxs))

            @views v[vidxs] .= vchunk[ch_idx]
        end
        track_weakref!(f, header_offset, v)
        return v
    end
    throw(UnsupportedVersionException("Encountered a chunked array ($layout) that is not implemented."))
end

function write_chunked_array(f::JLDFile, name::String, data::AbstractArray, chunk_dims::Vector{Int},
                            filters=nothing, wsession::JLDWriteSession=JLDWriteSession())
    if !f.writable
        throw(ArgumentError("Cannot write to file opened in read-only mode"))
    end

    if length(chunk_dims) != ndims(data)
        throw(ArgumentError("chunk_dims length ($(length(chunk_dims))) must match array dimensions ($(ndims(data)))"))
    end

    if any(chunk_dims .<= 0)
        throw(ArgumentError("All chunk dimensions must be positive"))
    end

    # Validate chunk dimensions don't exceed array dimensions
    data_dims = size(data)
    if any(chunk_dims .> data_dims)
        @warn "Some chunk dimensions exceed array dimensions - chunks will be automatically clipped"
    end

    # Use provided filters or fall back to file defaults
    if isnothing(filters) || (filters isa Tuple && isempty(filters))
        filters = f.compress
    end

    # Get group context
    g = f.root_group
    prewrite(f)
    (g, name) = pathize(g, name, true)

    y = data
    odr = objodr(y)
    dataspace = WriteDataspace(f, y, odr)
    datatype = h5type(f, y)
    io = f.io

    datasz = (odr_sizeof(odr)::Int * numel(dataspace))::Int
    psz = payload_size_without_storage_message(dataspace, datatype)
    psz += CONTINUATION_MSG_SIZE

    local_filters = FilterPipeline(map(filters) do filter
        Filters.set_local(filter, odr, dataspace, ())
    end)

    @assert data isa ArrayMemory
    layout_class = LcChunked
    psz += jlsizeof(Val(HmDataLayout);
        version = 3,
        layout_class,
        dimensions = fill(1, ndims(data)+1),
        # Dummy values for message size computation
        data_address = 0,
    )
    if !isempty(local_filters)
        psz += Filters.pipeline_message_size(local_filters)
    end

    fullsz = jlsizeof(ObjectStart) + size_size(psz) + psz + 4

    header_offset = f.end_of_data
    seek(io, header_offset)
    f.end_of_data = header_offset + fullsz

    cio = begin_checksum_write(io, fullsz - 4)
    jlwrite(cio, ObjectStart(size_flag(psz)))
    write_size(cio, psz)
    write_header_message(cio, Val(HmFillValue); flags=0x09)
    write_header_message(cio, Val(HmDataspace); dataspace.dataspace_type, dimensions=dataspace.size)
    for attr in dataspace.attributes
        write_header_message(cio, f, attr)
    end
    write_header_message(cio, Val(HmDatatype), 1 | (2*isa(datatype, CommittedDatatype)); dt=datatype)

    if !isempty(local_filters)
        Filters.write_filter_pipeline_message(cio, local_filters)
    end

    pos = position(cio)

    btree, total_chunk_size, num_chunks = write_chunked_dataset_with_v1btree(f, data, odr, local_filters, wsession, chunk_dims)

    seek(cio, pos)
    # Prepare DataLayout parameters
    dl_dims = UInt32.([reverse(chunk_dims)..., odr_sizeof(odr)])

    # Write DataLayout message version 3 with V1 B-tree
    write_header_message(cio, Val(HmDataLayout);
        version=3,
        layout_class,
        dimensionality=UInt8(length(chunk_dims)+1 ),
        data_address=btree.root,
        # Reversed dimensions with element size as last dim for V3 format (Int32)
        dimensions=dl_dims
    )

    write_continuation_placeholder(cio)
    jlwrite(f.io, end_checksum(cio))

    g[name] =  h5offset(f, header_offset)

    return btree
end

# Export public API
export ChunkedArray, Chunk, get_chunked_array, chunk_dimensions, num_chunks, chunk_grid_size, write_chunked_array