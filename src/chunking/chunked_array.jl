"""Metadata for a single chunk: offset, size, filter mask, and HDF5 indices."""
struct ChunkInfo
    offset::RelOffset
    chunk_size::Int
    filter_mask::Int
    idx::CartesianIndex
end

"""Lazy wrapper around a chunked dataset for chunk-wise iteration without loading entire array."""
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
    chunks::Vector{ChunkInfo}
end

"""Single chunk with data, position indices, file offset, and size."""
struct Chunk{T,N}
    data::Array{T,N}
    indices::CartesianIndex{N}
    chunk_indices::CartesianIndex{N}
    offset::RelOffset
    size::Int
end

"""Get ChunkedArray for dataset, allowing chunk-wise iteration."""
function get_chunked_array(f::JLDFile, name::String)
    link = lookup_link(f.root_group, name)
    isnothing(link) && throw(KeyError(name))

    offset = is_hard_link(link) ? link.offset :
        throw(ArgumentError("Can only create ChunkedArray from direct datasets"))

    dataspace = ReadDataspace()
    dt::H5Datatype = PlaceholderH5Datatype()
    layout::DataLayout = DataLayout(0, LcCompact, 0, UNDEFINED_ADDRESS)
    filter_pipeline::FilterPipeline = FilterPipeline()

    for msg in HeaderMessageIterator(f, offset)
        msg.type == HmDataspace && (dataspace = ReadDataspace(f, msg))
        msg.type == HmDatatype && (dt = HmWrap(HmDatatype, msg).dt::H5Datatype)
        msg.type == HmDataLayout && (layout = DataLayout(f, msg))
        msg.type == HmFilterPipeline && (filter_pipeline = FilterPipeline(msg))
    end

    dt isa PlaceholderH5Datatype && throw(InvalidDataException("No datatype message found"))
    ischunked(layout) || throw(ArgumentError("Dataset is not chunked"))

    rr = jltype(f, dt)
    ndims = Int(dataspace.dimensionality)
    seek(f.io, dataspace.dimensions_offset)
    array_size = Tuple(reverse(ntuple(i -> Int(jlread(f.io, Int64)), Val(ndims))))
    chunk_dims = Tuple(reverse(Int.(layout.chunk_dimensions))[1:ndims])
    T = julia_repr(rr)

    chunks = layout.version == 3 ?
        [ChunkInfo(c.offset, c.size, c.filter_mask, c.idx) for c in
         JLD2.BTrees.read_v1btree(f, layout.data_offset; layout.dimensionality)] :
        ChunkInfo[]

    return ChunkedArray{T,ndims}(f, offset, array_size, chunk_dims, dt, layout,
                                  filter_pipeline, rr, dataspace, chunks)
end

chunk_dimensions(ca::ChunkedArray) = ca.chunk_dims
num_chunks(ca::ChunkedArray) = prod(cld.(ca.array_size, ca.chunk_dims))
chunk_grid_size(ca::ChunkedArray) = cld.(ca.array_size, ca.chunk_dims)

function Base.iterate(ca::ChunkedArray{T,N}) where {T,N}
    indices = CartesianIndices(chunk_grid_size(ca))
    state = iterate(indices)
    isnothing(state) && return nothing
    chunk_idx, next_state = state
    chunk_idx = CartesianIndex((chunk_idx.I .- 1) .* ca.chunk_dims .+ 1)
    return (_read_chunk(ca, chunk_idx), (indices, next_state))
end

function Base.iterate(ca::ChunkedArray{T,N}, state) where {T,N}
    indices, iter_state = state
    next = iterate(indices, iter_state)
    isnothing(next) && return nothing
    chunk_idx, next_state = next
    chunk_idx = CartesianIndex((chunk_idx.I .- 1) .* ca.chunk_dims .+ 1)
    return (_read_chunk(ca, chunk_idx), (indices, next_state))
end

Base.length(ca::ChunkedArray) = num_chunks(ca)
Base.eltype(::Type{ChunkedArray{T,N}}) where {T,N} = Chunk{T,N}

function Base.getindex(ca::ChunkedArray{T,N}, I::Vararg{Int,N}) where {T,N}
    chunk_idx = CartesianIndex((I .- 1) .* ca.chunk_dims .+ 1)
    grid_size = chunk_grid_size(ca)
    for (i, (idx, max_idx)) in enumerate(zip(I, grid_size))
        (idx < 1 || idx > max_idx) && throw(BoundsError(ca, I))
    end
    return _read_chunk(ca, chunk_idx)
end

function _read_chunk(ca::ChunkedArray{T,N}, chunk_idx::CartesianIndex{N}) where {T,N}
    array_indices = chunk_idx#CartesianIndex((chunk_idx.I .- 1) .* ca.chunk_dims .+ 1)
    chunk_end = min.(array_indices.I .+ ca.chunk_dims .- 1, ca.array_size)
    actual_chunk_size = chunk_end .- array_indices.I .+ 1

    hdf5_indices_with_elemsize = (reverse(array_indices.I .- 1)..., 0)

    chunk_info = findfirst(c -> c.idx == array_indices, ca.chunks)
    if isnothing(chunk_info)
        @info ca.chunks chunk_idx
        throw(InvalidDataException(
        "Chunk not found at grid $(chunk_idx.I), array $(array_indices.I), HDF5 $hdf5_indices_with_elemsize"))
    end
    seek(ca.file.io, fileoffset(ca.file, ca.chunks[chunk_info].offset))
    vchunk = Array{T, N}(undef, actual_chunk_size...)
    JLD2.read_compressed_array!(vchunk, ca.file, ca.rr, ca.chunks[chunk_info].chunk_size,
                                ca.filters, ca.chunks[chunk_info].filter_mask)

    return Chunk{T,N}(vchunk, array_indices, chunk_idx,
                      ca.chunks[chunk_info].offset, ca.chunks[chunk_info].chunk_size)
end

"""Read chunked dataset into preallocated array, dispatching by layout version and indexing type."""
function read_chunked_array(f::JLDFile, v::Array{T}, dataspace::ReadDataspace,
                           @nospecialize(rr::ReadRepresentation), layout::DataLayout,
                           filters::FilterPipeline, header_offset::RelOffset,
                           ndims::Int, fill_value::T=zero(T)) where T
    if layout.version == 3
        chunks = JLD2.BTrees.read_v1btree(f, layout.data_offset; layout.dimensionality)
        chunk_dims_julia = Tuple(Int.(reverse(layout.chunk_dimensions)[1:ndims]))

        for chunk in chunks
            read_and_assign_chunk!(f, v, chunk.idx, chunk.offset, chunk.size, chunk_dims_julia, rr, filters, 0)
        end
        return v
    elseif layout.version == 4
        return layout.chunk_indexing_type == 2 ? read_implicit_index_chunks(f, v, dataspace, rr, layout, filters, header_offset, ndims, fill_value) :
               layout.chunk_indexing_type == 3 ? read_fixed_array_chunks(f, v, dataspace, rr, layout, filters, header_offset, ndims, fill_value) :
               layout.chunk_indexing_type == 4 ? read_extensible_array_chunks(f, v, dataspace, rr, layout, filters, header_offset, ndims, fill_value) :
               layout.chunk_indexing_type == 5 ? read_v2btree_chunks(f, v, dataspace, rr, layout, filters, header_offset, ndims, fill_value) :
               throw(UnsupportedVersionException("Chunk indexing type $(layout.chunk_indexing_type) not implemented"))
    end
    throw(UnsupportedVersionException("Layout version $(layout.version) not implemented"))
end
