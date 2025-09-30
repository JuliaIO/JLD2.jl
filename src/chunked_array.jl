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

    # Handle compression
    if iscompressed(filters)
        if chunk_info.filter_mask == 0
            # All filters applied
            read_compressed_array!(vchunk, f, rr, chunk_info.chunk_size, filters)
        else
            # Some filters were skipped
            if length(filters.filters) == 1
                # Single filter was skipped - read uncompressed
                read_array!(vchunk, f, rr)
            else
                # Multiple filters - apply only those not masked
                mask = Bool[chunk_info.filter_mask & 2^(n-1) == 0 for n in eachindex(filters.filters)]
                if any(mask)
                    rf = FilterPipeline(filters.filters[mask])
                    read_compressed_array!(vchunk, f, rr, chunk_info.chunk_size, rf)
                else
                    read_array!(vchunk, f, rr)
                end
            end
        end
    else
        # No compression
        read_array!(vchunk, f, rr)
    end

    return vchunk
end

# Export public API
export ChunkedArray, Chunk, get_chunked_array, chunk_dimensions, num_chunks, chunk_grid_size