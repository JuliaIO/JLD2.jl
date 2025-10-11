# Chunking Helper Functions
# Shared utilities for chunk indexing and data preparation

"""
Precomputed stride multipliers for converting Julia chunk coordinates to HDF5 linear indices.
"""
struct ChunkLinearIndexer
    down_chunks::Vector{Int}
    ndims::Int
end

function ChunkLinearIndexer(grid_dims::NTuple{N,Int}) where N
    grid_dims_hdf5 = reverse(grid_dims)
    down_chunks = zeros(Int, N)
    acc = 1
    for i in N:-1:1
        down_chunks[i] = acc
        acc *= grid_dims_hdf5[i]
    end
    ChunkLinearIndexer(down_chunks, N)
end

"""Calculate chunk grid dimensions (number of chunks in each dimension)."""
compute_grid_dims(data_size, chunk_dims) = cld.(data_size, chunk_dims)

"""Convert Julia chunk coordinates (1-based) to HDF5 linear index (0-based)."""
function compute_linear_index(indexer::ChunkLinearIndexer, julia_chunk_idx::CartesianIndex)
    hdf5_coords = reverse(Tuple(julia_chunk_idx) .- 1)
    return sum(indexer.down_chunks .* hdf5_coords)
end

"""Convert 0-based linear chunk index (HDF5) to 1-based CartesianIndex (Julia)."""
function linear_index_to_chunk_coords(linear_idx::Int, grid_dims::NTuple{N,Int}) where N
    grid_dims_hdf5 = reverse(grid_dims)
    coords_hdf5 = zeros(Int, N)
    idx = linear_idx
    for i in N:-1:1
        coords_hdf5[i] = idx % grid_dims_hdf5[i]
        idx = div(idx, grid_dims_hdf5[i])
    end
    return CartesianIndex(Tuple(reverse(coords_hdf5) .+ 1))
end

"""
Iterator yielding `(julia_chunk_idx::CartesianIndex{N}, linear_idx::Int)` tuples for all
chunks in a grid. Linear indices are 0-based (HDF5 order).
"""
struct ChunkGrid{N}
    grid_dims::NTuple{N,Int}
    indexer::ChunkLinearIndexer
end

ChunkGrid(array_dims::NTuple{N,Int}, chunk_dims::NTuple{N,Int}) where N =
    ChunkGrid(cld.(array_dims, chunk_dims))

function ChunkGrid(grid_dims::NTuple{N,Int}) where N
    ChunkGrid{N}(grid_dims, ChunkLinearIndexer(grid_dims))
end

function Base.iterate(grid::ChunkGrid{N}) where N
    cart_iter = CartesianIndices(grid.grid_dims)
    result = iterate(cart_iter)
    isnothing(result) && return nothing
    julia_chunk_idx, cart_state = result
    return ((julia_chunk_idx, compute_linear_index(grid.indexer, julia_chunk_idx)),
            (cart_iter, cart_state))
end

function Base.iterate(grid::ChunkGrid{N}, state) where N
    cart_iter, cart_state = state
    result = iterate(cart_iter, cart_state)
    isnothing(result) && return nothing
    julia_chunk_idx, cart_state = result
    return ((julia_chunk_idx, compute_linear_index(grid.indexer, julia_chunk_idx)),
            (cart_iter, cart_state))
end

Base.length(grid::ChunkGrid) = prod(grid.grid_dims)
Base.eltype(::Type{ChunkGrid{N}}) where N = Tuple{CartesianIndex{N}, Int}

"""Compute 1-based starting position for a chunk from its grid index."""
@inline chunk_start_from_index(chunk_idx::CartesianIndex{N}, chunk_dims::NTuple{N,Int}) where N =
    (Tuple(chunk_idx) .- 1) .* chunk_dims .+ 1

"""Normalize and localize filter pipeline for chunk compression."""
function prepare_filter_pipeline(filters, odr, dataspace)
    normalized_filters = Filters.normalize_filters(filters)
    iscompressed(normalized_filters) || return normalized_filters
    FilterPipeline(map(f -> Filters.set_local(f, odr, dataspace, ()), normalized_filters))
end

"""Pad partial edge chunk to full chunk size (HDF5 requirement)."""
function pad_chunk_data(chunk_data_partial::Array{T,N}, target_chunks::NTuple{N,Int},
                        fill_value=zero(T)) where {T,N}
    size(chunk_data_partial) == target_chunks && return chunk_data_partial
    full_chunk = fill(fill_value, target_chunks)
    full_chunk[ntuple(i -> 1:size(chunk_data_partial, i), N)...] = chunk_data_partial
    return full_chunk
end

"""Apply filters and write chunk to file, returning (offset, size)."""
function prepare_and_write_chunk(f::JLDFile, chunk_data::Array, odr, filter_pipeline, wsession)
    chunk_bytes = if iscompressed(filter_pipeline)
        first(Filters.compress(filter_pipeline, chunk_data, odr, f, wsession))
    else
        io_buf = IOBuffer()
        write_data(io_buf, f, chunk_data, odr, datamode(odr), wsession)
        take!(io_buf)
    end

    chunk_pos = f.end_of_data
    seek(f.io, chunk_pos)
    write(f.io, chunk_bytes)
    f.end_of_data = chunk_pos + sizeof(chunk_bytes)
    return (h5offset(f, chunk_pos), sizeof(chunk_bytes))
end

"""Extract chunk region from data array, returning (chunk_data, start_idx, end_idx)."""
function extract_chunk_region(data::Array{T,N}, chunk_grid_idx::CartesianIndex,
                             chunks::NTuple{N,Int}) where {T,N}
    start_idx = (Tuple(chunk_grid_idx) .- 1) .* chunks .+ 1
    end_idx = min.(start_idx .+ chunks .- 1, size(data))
    return (data[map(:, start_idx, end_idx)...], start_idx, end_idx)
end

"""Extract chunk at 1-based starting position with given dimensions."""
function extract_chunk(data::AbstractArray, chunk_indices, chunk_dims)
    ndims(data) == length(chunk_indices) == length(chunk_dims) ||
        throw(ArgumentError("Dimension mismatch"))

    chunk_indices_tuple = Tuple(chunk_indices)
    chunk_dims_tuple = Tuple(chunk_dims)
    chunk = similar(data, chunk_dims_tuple)

    avail_end = min.(size(data), chunk_indices_tuple .+ chunk_dims_tuple .- 1)
    avail_size = avail_end .- chunk_indices_tuple .+ 1

    chunk[map(:, ntuple(i -> 1, ndims(data)), avail_size)...] =
        data[map(:, chunk_indices_tuple, avail_end)...]

    return chunk
end

"""Write all chunks and return (addresses, sizes, indexer) in linear HDF5 order."""
function write_all_chunks_linear(f::JLDFile, data::Array{T,N}, chunks::NTuple{N,Int},
                                odr, filter_pipeline, wsession;
                                pad_chunks=true, fill_value=zero(T)) where {T,N}
    chunk_grid = ChunkGrid(size(data), chunks)
    chunk_offsets = Vector{RelOffset}(undef, length(chunk_grid))
    chunk_sizes = Vector{UInt64}(undef, length(chunk_grid))

    for (julia_chunk_idx, linear_idx) in chunk_grid
        chunk_data_partial = first(extract_chunk_region(data, julia_chunk_idx, chunks))
        chunk_data = pad_chunks ? pad_chunk_data(chunk_data_partial, chunks, fill_value) : chunk_data_partial
        chunk_offset, chunk_size = prepare_and_write_chunk(f, chunk_data, odr, filter_pipeline, wsession)
        chunk_offsets[linear_idx + 1] = chunk_offset
        chunk_sizes[linear_idx + 1] = chunk_size
    end

    return (chunk_offsets, chunk_sizes, chunk_grid.indexer)
end

"""
Write all chunks as records for V2 B-tree. Non-filtered chunks are padded, filtered may be unpadded.
Returns vector of `(address, coords)` or `(address, size, filter_mask, coords)` tuples.
"""
function write_all_chunks_as_records(f::JLDFile, data::Array{T,N}, chunks::NTuple{N,Int},
                                    odr, filter_pipeline, wsession) where {T,N}
    chunk_grid = ChunkGrid(size(data), chunks)
    compressed = iscompressed(filter_pipeline)
    RecordType = compressed ? Tuple{RelOffset, UInt64, UInt32, Vector{UInt64}} :
                             Tuple{RelOffset, Vector{UInt64}}
    chunk_records = Vector{RecordType}(undef, length(chunk_grid))

    for (julia_chunk_idx, linear_idx) in chunk_grid
        chunk_data_partial = first(extract_chunk_region(data, julia_chunk_idx, chunks))
        chunk_data = compressed ? chunk_data_partial : pad_chunk_data(chunk_data_partial, chunks, zero(T))
        chunk_offset, chunk_size = prepare_and_write_chunk(f, chunk_data, odr, filter_pipeline, wsession)
        hdf5_coords = collect(UInt64.(reverse(Tuple(julia_chunk_idx) .- 1)))

        chunk_records[linear_idx + 1] = compressed ?
            (chunk_offset, chunk_size, UInt32(0), hdf5_coords) :
            (chunk_offset, hdf5_coords)
    end

    return chunk_records
end


"""Read chunk with decompression. Filter mask indicates which filters were skipped (bit n set = skip)."""
function read_chunk_with_filters!(vchunk::Array, f::JLDFile, rr, chunk_size::Int,
                                 filters::FilterPipeline, filter_mask)
    filter_mask = UInt32(filter_mask)
    if !Filters.iscompressed(filters)
        JLD2.read_array!(vchunk, f, rr)
    elseif filter_mask == 0
        JLD2.read_compressed_array!(vchunk, f, rr, chunk_size, filters)
    elseif length(filters.filters) == 1
        JLD2.read_array!(vchunk, f, rr)
    else
        mask = [filter_mask & 2^(n-1) == 0 for n in eachindex(filters.filters)]
        if any(mask)
            JLD2.read_compressed_array!(vchunk, f, rr, chunk_size, FilterPipeline(filters.filters[mask]))
        else
            JLD2.read_array!(vchunk, f, rr)
        end
    end
    return vchunk
end

"""Convert 0-based linear chunk index to HDF5 0-based element indices (reversed)."""
function hdf5_element_indices_from_linear(linear_idx::Int, grid_dims::NTuple{N,Int},
                                          chunk_dims::NTuple{N,Int}) where N
    chunk_coords_julia = linear_index_to_chunk_coords(linear_idx, grid_dims)
    return reverse((Tuple(chunk_coords_julia) .- 1) .* chunk_dims)
end

"""Compute 1-based element start position from Julia chunk coordinates."""
chunk_element_start_julia(chunk_coords::CartesianIndex{N}, chunk_dims::NTuple{N,Int}) where N =
    (Tuple(chunk_coords) .- 1) .* chunk_dims .+ 1

"""Assign chunk to output array, handling edge chunks and padding."""
function assign_chunk_to_array!(v::Array{T,N}, vchunk::Array{T,N},
                               chunk_grid_idx::CartesianIndex{N},
                               chunk_dims::NTuple{N,Int}) where {T,N}
    chunk_start = (Tuple(chunk_grid_idx) .- 1) .* chunk_dims .+ 1
    chunk_end = min.(chunk_start .+ chunk_dims .- 1, size(v))
    actual_region_size = chunk_end .- chunk_start .+ 1
    output_ranges = ntuple(i -> chunk_start[i]:chunk_end[i], N)

    if size(vchunk) == chunk_dims
        v[output_ranges...] = vchunk[ntuple(i -> 1:actual_region_size[i], N)...]
    else
        v[output_ranges...] = vchunk
    end
    return nothing
end

"""Read chunk from file and assign to output array, handling edge chunks and compression."""
function read_and_assign_chunk!(f::JLDFile, v::Array{T}, chunk_start::NTuple{N,Int},
                               chunk_address::RelOffset, chunk_size_bytes::Int,
                               chunk_dims_julia::NTuple{N,Int}, rr, filters,
                               filter_mask) where {T,N}
    seek(f.io, fileoffset(f, chunk_address))

    chunk_end = min.(chunk_start .+ chunk_dims_julia .- 1, size(v))
    actual_chunk_size = chunk_end .- chunk_start .+ 1
    is_edge_chunk = actual_chunk_size != chunk_dims_julia

    # HDF5 spec: non-filtered chunks always padded, filtered chunks may be unpadded
    vchunk = if is_edge_chunk && iscompressed(filters) &&
                chunk_size_bytes < prod(chunk_dims_julia) * sizeof(T)
        Array{T, N}(undef, actual_chunk_size...)
    else
        Array{T, N}(undef, chunk_dims_julia...)
    end

    read_chunk_with_filters!(vchunk, f, rr, chunk_size_bytes, filters, filter_mask)

    output_ranges = ntuple(i -> chunk_start[i]:chunk_end[i], N)
    if size(vchunk) == chunk_dims_julia
        v[output_ranges...] = vchunk[ntuple(i -> 1:actual_chunk_size[i], N)...]
    else
        v[output_ranges...] = vchunk
    end
    return nothing
end
