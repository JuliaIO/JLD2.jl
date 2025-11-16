# Chunking Helper Functions
# Shared utilities for chunk indexing and data preparation

struct ChunkIndexIterator{N}
    cidxs::CartesianIndices{N}
    dim_offsets::NTuple{N, Int}
end

ChunkIndexIterator(array_dims::NTuple{N,Int}, chunk_dims::NTuple{N,Int}) where N =
    ChunkIndexIterator(cld.(array_dims, chunk_dims))

function ChunkIndexIterator(grid_dims::NTuple{N,Int}) where N
    ChunkIndexIterator{N}(
        CartesianIndices(grid_dims),
        ntuple(i->prod(grid_dims[1:i-1], init=1), N)
        )
end

function Base.iterate(cit::ChunkIndexIterator)
    result = iterate(cit.cidxs)
    isnothing(result) && return nothing
    cidx, cart_state = result
    linidx = sum(cit.dim_offsets .* (Tuple(cidx).-1))
    return ((cidx, linidx), cart_state)
end

function Base.iterate(cit::ChunkIndexIterator, state)
    result = iterate(cit.cidxs, state)
    isnothing(result) && return nothing
    cidx, cart_state = result
    linidx = sum(cit.dim_offsets .* (Tuple(cidx).-1))
    return ((cidx, linidx), cart_state)
end
Base.length(cit::ChunkIndexIterator) = length(cit.cidxs)
Base.eltype(::Type{ChunkIndexIterator{N}}) where N = Tuple{CartesianIndex{N}, Int}

"""
    chunk_start_from_index(chunk_idx::CartesianIndex{N}, chunk_dims::NTuple{N,Int}) where N

Compute 1-based starting element position for a chunk from its grid index.
For example, chunk (2,3) with chunk_dims (5,10) starts at element (6,21).
"""
@inline chunk_start_from_index(chunk_idx::CartesianIndex{N}, chunk_dims::NTuple{N,Int}) where N =
    (Tuple(chunk_idx) .- 1) .* chunk_dims .+ 1

"""
    linear_index_to_chunk_coords(linear_idx::Int, grid_dims::NTuple{N,Int}) where N

Convert 0-based linear chunk index (HDF5 ordering) to 1-based CartesianIndex (Julia ordering).

HDF5 uses column-major linear indexing with reversed dimensions. This function:
1. Converts linear index to HDF5 coordinates (reversed, 0-based)
2. Reverses back to Julia dimension order
3. Converts to 1-based indexing
"""
function linear_index_to_chunk_coords(linear_idx::Int, grid_dims::NTuple{N,Int}) where N
    grid_dims_hdf5 = reverse(grid_dims)
    coords_hdf5 = ntuple(N) do i
        idx_i = linear_idx
        for j in N:-1:i+1
            idx_i = div(idx_i, grid_dims_hdf5[j])
        end
        idx_i % grid_dims_hdf5[i]
    end
    return CartesianIndex(reverse(coords_hdf5) .+ 1)
end

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
    chunk_bytes, filter_mask = if iscompressed(filter_pipeline)
        Filters.compress(filter_pipeline, chunk_data, odr, f, wsession)
    else
        io_buf = IOBuffer()
        JLD2.write_data(io_buf, f, chunk_data, odr, JLD2.datamode(odr), wsession)
        take!(io_buf), UInt32(0)
    end

    chunk_pos = f.end_of_data
    seek(f.io, chunk_pos)
    write(f.io, chunk_bytes)
    f.end_of_data = chunk_pos + sizeof(chunk_bytes)
    return h5offset(f, chunk_pos), sizeof(chunk_bytes), filter_mask
end

"""
Extract chunk region from data array using grid-based indexing.
"""
function extract_chunk_region(data::Array{T,N}, chunk_grid_idx::CartesianIndex,
                             chunks::NTuple{N,Int}) where {T,N}
    start_idx = (Tuple(chunk_grid_idx) .- 1) .* chunks .+ 1
    end_idx = min.(start_idx .+ chunks .- 1, size(data))
    return (data[map(:, start_idx, end_idx)...], start_idx, end_idx)
end

function write_all_chunks(f::JLDFile,
        data::Array{T,N},
        chunks::NTuple{N,Int}, odr, filter_pipeline, wsession;
        pad_chunks = true) where {T,N}
    chunk_grid = ChunkIndexIterator(size(data), chunks)
    RecordType = @NamedTuple{offset::RelOffset, idx::CartesianIndex{N}, chunk_size::UInt64, filter_mask::UInt32, }
    chunk_metadata = Vector{RecordType}(undef, length(chunk_grid))

    for (julia_chunk_idx, linear_idx) in chunk_grid
        chunk_data = first(extract_chunk_region(data, julia_chunk_idx, chunks))
        pad_chunks && (chunk_data = pad_chunk_data(chunk_data, chunks, zero(T)))
        offset, chunk_size, filter_mask = prepare_and_write_chunk(f, chunk_data, odr, filter_pipeline, wsession)

        chunk_metadata[linear_idx + 1] = (; offset, idx=julia_chunk_idx, chunk_size, filter_mask)
    end

    return chunk_metadata
end

"""Read chunk from file and assign to output array, handling edge chunks and compression."""
function read_and_assign_chunk!(f::JLDFile, v::Array{T}, _chunk_start,
                               chunk_address::RelOffset, chunk_size_bytes,
                               chunk_dims_julia::NTuple{N,Int}, rr, filters,
                               filter_mask,
                               is_padded = true
                               ) where {T,N}
    seek(f.io, fileoffset(f, chunk_address))
    chunk_start = Tuple(_chunk_start)
    chunk_end = min.(chunk_start .+ chunk_dims_julia .- 1, size(v))
    actual_chunk_size = chunk_end .- chunk_start .+ 1

    vchunk = if !is_padded
        Array{T, N}(undef, actual_chunk_size...)
    else
        Array{T, N}(undef, chunk_dims_julia...)
    end

    JLD2.read_compressed_array!(vchunk, f, rr, chunk_size_bytes, filters, filter_mask)

    output_ranges = ntuple(i -> chunk_start[i]:chunk_end[i], N)
    if size(vchunk) == chunk_dims_julia
        v[output_ranges...] = vchunk[ntuple(i -> 1:actual_chunk_size[i], N)...]
    else
        v[output_ranges...] = vchunk
    end
    return nothing
end
