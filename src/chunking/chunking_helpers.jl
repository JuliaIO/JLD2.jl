# Chunking Helper Functions
# Shared utilities for chunk indexing and data preparation

"""
    ChunkLinearIndexer

Precomputed structure for converting Julia chunk coordinates to HDF5 linear indices.

# Fields
- `down_chunks::Vector{Int}` - Stride multipliers for each dimension in HDF5 order
- `ndims::Int` - Number of dimensions

# Usage
```julia
indexer = ChunkLinearIndexer(grid_dims)
linear_idx = compute_linear_index(indexer, julia_chunk_idx)
```
"""
struct ChunkLinearIndexer
    down_chunks::Vector{Int}
    ndims::Int
end

"""
    ChunkLinearIndexer(grid_dims::NTuple{N,Int}) where N

Create indexer from grid dimensions in Julia order.

Precomputes stride multipliers for fast linear index calculation.
This replaces the old `compute_chunk_index` function with a more efficient
approach that reuses the precomputed indexer across multiple lookups.
"""
function ChunkLinearIndexer(grid_dims::NTuple{N,Int}) where N
    grid_dims_hdf5 = reverse(grid_dims)
    ndims_hdf5 = length(grid_dims_hdf5)

    down_chunks = zeros(Int, ndims_hdf5)
    acc = 1
    for i in ndims_hdf5:-1:1
        down_chunks[i] = acc
        acc *= grid_dims_hdf5[i]
    end

    ChunkLinearIndexer(down_chunks, ndims_hdf5)
end

"""
    compute_grid_dims(data_size, chunk_dims) -> NTuple

Calculate the chunk grid dimensions for a given data size and chunk dimensions.

# Returns
Tuple of grid dimensions (number of chunks in each dimension).

# Example
```julia
data_size = (100, 200)
chunk_dims = (10, 25)
grid_dims = compute_grid_dims(data_size, chunk_dims)  # Returns (10, 8)
```
"""
compute_grid_dims(data_size, chunk_dims) = cld.(data_size, chunk_dims)

"""
    compute_linear_index(indexer::ChunkLinearIndexer, julia_chunk_idx::CartesianIndex) -> Int

Convert Julia chunk coordinates (1-based) to HDF5 linear index (0-based).

Returns 0-based linear index suitable for array indexing with +1.
"""
function compute_linear_index(indexer::ChunkLinearIndexer, julia_chunk_idx::CartesianIndex)
    hdf5_coords = reverse(Tuple(julia_chunk_idx) .- 1)  # 0-based, reversed

    linear_idx = 0
    for i in 1:indexer.ndims
        linear_idx += indexer.down_chunks[i] * hdf5_coords[i]
    end

    return linear_idx
end

"""
    prepare_filter_pipeline(filters, odr, dataspace)

Normalize and localize filter pipeline for chunk compression.

# Arguments
- `filters` - User-provided filter specification (Symbol, FilterPipeline, or nothing)
- `odr` - Object data representation
- `dataspace` - Write dataspace

# Returns
`FilterPipeline` or `nothing`
"""
function prepare_filter_pipeline(filters, odr, dataspace)
    normalized_filters = Filters.normalize_filters(filters)
    iscompressed(normalized_filters) || return normalized_filters

    FilterPipeline(map(normalized_filters) do filter
        Filters.set_local(filter, odr, dataspace, ())
    end)
end

"""
    pad_chunk_data(chunk_data_partial::Array{T,N}, target_chunks::NTuple{N,Int},
                   fill_value=zero(T)) where {T,N}

Pad partial chunk to full chunk size.

HDF5 requires all chunks to be the same size, even edge chunks.
"""
function pad_chunk_data(chunk_data_partial::Array{T,N}, target_chunks::NTuple{N,Int},
                        fill_value=zero(T)) where {T,N}
    actual_size = size(chunk_data_partial)
    actual_size == target_chunks && return chunk_data_partial

    full_chunk = fill(fill_value, target_chunks)
    ranges = ntuple(i -> 1:actual_size[i], N)
    full_chunk[ranges...] = chunk_data_partial
    return full_chunk
end

"""
    prepare_and_write_chunk(f::JLDFile, chunk_data::Array, odr, filter_pipeline, wsession)

Apply filters (if any) and write chunk to file.

# Returns
`(chunk_offset, chunk_size)` tuple where offsets are file positions and size is compressed size.
"""
function prepare_and_write_chunk(f::JLDFile, chunk_data::Array, odr, filter_pipeline, wsession)
    # Apply filters if specified
    chunk_bytes = if !iscompressed(filter_pipeline)
        io_buf = IOBuffer()
        write_data(io_buf, f, chunk_data, odr, datamode(odr), wsession)
        take!(io_buf)
    else
        compressed, _ = Filters.compress(filter_pipeline, chunk_data, odr, f, wsession)
        compressed
    end

    # Write chunk to file
    chunk_pos = f.end_of_data
    seek(f.io, chunk_pos)
    write(f.io, chunk_bytes)
    f.end_of_data = chunk_pos + sizeof(chunk_bytes)

    return (h5offset(f, chunk_pos), sizeof(chunk_bytes))
end

"""
    extract_chunk_region(data::Array{T,N}, chunk_grid_idx::CartesianIndex,
                        chunks::NTuple{N,Int}) where {T,N}

Extract a chunk region from data array given a CartesianIndex.

# Returns
Tuple of `(chunk_data_partial, start_idx, end_idx)`
"""
function extract_chunk_region(data::Array{T,N}, chunk_grid_idx::CartesianIndex,
                             chunks::NTuple{N,Int}) where {T,N}
    chunk_coords = Tuple(chunk_grid_idx)
    start_idx = (chunk_coords .- 1) .* chunks .+ 1
    end_idx = min.(start_idx .+ chunks .- 1, size(data))

    chunk_data_partial = data[map(:, start_idx, end_idx)...]
    return (chunk_data_partial, start_idx, end_idx)
end

"""
    extract_chunk(data::AbstractArray, chunk_indices, chunk_dims)

Extract a chunk of data at the given 1-based chunk starting positions.

# Arguments
- `data`: Source array
- `chunk_indices`: 1-based starting indices for the chunk (can be Vector or Tuple)
- `chunk_dims`: Dimensions of the chunk (can be Vector or Tuple)

# Returns
The extracted chunk as a contiguous array, with the requested chunk_dims size.
Edge chunks are properly handled by only copying available data and padding the rest.

# Example
```julia
data = reshape(1:24, 4, 6)
chunk = extract_chunk(data, [1, 1], [2, 3])  # Extract 2×3 chunk from top-left
```
"""
function extract_chunk(data::AbstractArray, chunk_indices, chunk_dims)
    ndims(data) == length(chunk_indices) == length(chunk_dims) ||
        throw(ArgumentError("chunk_indices and chunk_dims dimensionality must match data"))

    # Convert to tuples if needed
    chunk_indices_tuple = Tuple(chunk_indices)
    chunk_dims_tuple = Tuple(chunk_dims)

    # Allocate output chunk
    chunk = similar(data, chunk_dims_tuple)

    # Calculate available range (handle edge chunks)
    avail_end = min.(size(data), chunk_indices_tuple .+ chunk_dims_tuple .- 1)
    avail_ranges = map(:, chunk_indices_tuple, avail_end)
    avail_size = length.(avail_ranges)

    # Copy available data to chunk
    chunk_view = map(:, ntuple(i -> 1, ndims(data)), avail_size)
    chunk[chunk_view...] = data[avail_ranges...]

    return chunk
end

"""
    write_all_chunks_linear(f::JLDFile, data::Array{T,N}, chunks::NTuple{N,Int},
                           odr, filter_pipeline, wsession; pad_chunks=true) where {T,N}

Write all chunks and return addresses/sizes in linear order.

# Arguments
- `pad_chunks`: If true, pad partial chunks to full size (for Fixed Array/Implicit Index)

# Returns
- `(chunk_offsets, chunk_sizes, indexer)` where sizes are nothing if unfiltered
"""
function write_all_chunks_linear(f::JLDFile, data::Array{T,N}, chunks::NTuple{N,Int},
                                odr, filter_pipeline, wsession;
                                pad_chunks=true, fill_value=zero(T)) where {T,N}
    grid_dims = cld.(size(data), chunks)
    n_chunks = prod(grid_dims)

    # Create indexer for linear ordering
    indexer = ChunkLinearIndexer(grid_dims)

    # Allocate arrays for results
    chunk_offsets = Vector{RelOffset}(undef, n_chunks)
    chunk_sizes = Vector{UInt64}(undef, n_chunks)

    # Write chunks in Julia order, store at linear index
    for julia_chunk_idx in CartesianIndices(grid_dims)
        # Extract chunk data
        chunk_data_partial, _, _ = extract_chunk_region(data, julia_chunk_idx, chunks)

        # Pad if needed
        chunk_data = if pad_chunks
            pad_chunk_data(chunk_data_partial, chunks, fill_value)
        else
            chunk_data_partial
        end

        # Write chunk
        chunk_offset, chunk_size = prepare_and_write_chunk(f, chunk_data, odr, filter_pipeline, wsession)

        # Store at linear index
        linear_idx = compute_linear_index(indexer, julia_chunk_idx)
        chunk_offsets[linear_idx + 1] = chunk_offset
        chunk_sizes[linear_idx + 1] = chunk_size
    end

    return (chunk_offsets, chunk_sizes, indexer)
end

"""
    write_all_chunks_as_records(f::JLDFile, data::Array{T,N}, chunks::NTuple{N,Int},
                               odr, filter_pipeline, wsession) where {T,N}

Write all chunks and return records with chunk coordinates for V2 B-tree.

# Returns
- Vector of records: either `(address, coords)` or `(address, size, filter_mask, coords)`
"""
function write_all_chunks_as_records(f::JLDFile, data::Array{T,N}, chunks::NTuple{N,Int},
                                    odr, filter_pipeline, wsession) where {T,N}
    grid_dims = cld.(size(data), chunks)
    n_chunks = prod(grid_dims)
    indexer = ChunkLinearIndexer(grid_dims)

    # Allocate record array with appropriate type
    if !iscompressed(filter_pipeline)
        chunk_records = Vector{Tuple{RelOffset, Vector{UInt64}}}(undef, n_chunks)
    else
        chunk_records = Vector{Tuple{RelOffset, UInt64, UInt32, Vector{UInt64}}}(undef, n_chunks)
    end

    # Write chunks and collect records
    for julia_chunk_idx in CartesianIndices(grid_dims)
        # Extract chunk (unpadded for V2 B-tree)
        chunk_data_partial, _, _ = extract_chunk_region(data, julia_chunk_idx, chunks)

        # Write chunk
        chunk_offset, chunk_size = prepare_and_write_chunk(f, chunk_data_partial, odr, filter_pipeline, wsession)

        # Get HDF5 coordinates (0-based, reversed)
        hdf5_coords = collect(UInt64.(reverse(Tuple(julia_chunk_idx) .- 1)))

        # Store record at linear index
        linear_idx = compute_linear_index(indexer, julia_chunk_idx)
        if !iscompressed(filter_pipeline)
            chunk_records[linear_idx + 1] = (chunk_offset, hdf5_coords)
        else
            chunk_records[linear_idx + 1] = (chunk_offset, chunk_size, UInt32(0), hdf5_coords)
        end
    end

    return chunk_records
end


"""
    read_chunk_with_filters!(vchunk::Array, f::JLDFile, rr, chunk_size::Int,
                            filters::FilterPipeline, filter_mask::UInt32)

Read chunk data from the current position in `f`, applying decompression based on
`filters` and `filter_mask`. The `filter_mask` indicates which filters were skipped
during compression (bit n set = filter n was skipped).
"""
function read_chunk_with_filters!(
    vchunk::Array,
    f::JLDFile,
    rr,
    chunk_size::Int,
    filters::FilterPipeline,
    filter_mask
)
    filter_mask = UInt32(filter_mask)
    if Filters.iscompressed(filters)
        if filter_mask == 0
            JLD2.read_compressed_array!(vchunk, f, rr, chunk_size, filters)
        else
            if length(filters.filters) == 1
                JLD2.read_array!(vchunk, f, rr)
            else
                mask = Bool[filter_mask & 2^(n-1) == 0 for n in eachindex(filters.filters)]
                if any(mask)
                    rf = FilterPipeline(filters.filters[mask])
                    JLD2.read_compressed_array!(vchunk, f, rr, chunk_size, rf)
                else
                    JLD2.read_array!(vchunk, f, rr)
                end
            end
        end
    else
        JLD2.read_array!(vchunk, f, rr)
    end
    return vchunk
end

"""
    julia_chunk_idx_to_hdf5_element_indices(chunk_grid_idx::CartesianIndex{N},
                                           chunk_dims_julia::NTuple{N,Int}) where N

Convert Julia chunk grid index (1-based) to HDF5 element indices (0-based, reversed).

# Arguments
- `chunk_grid_idx`: 1-based chunk position in Julia order (e.g., CartesianIndex(2, 3))
- `chunk_dims_julia`: Chunk dimensions in Julia order (e.g., (10, 20))

# Returns
Tuple of HDF5 element indices with element size dimension appended (e.g., (40, 10, 0))

# Example
```julia
chunk_grid_idx = CartesianIndex(2, 3)  # Second chunk in dim 1, third in dim 2
chunk_dims = (10, 20)
indices = julia_chunk_idx_to_hdf5_element_indices(chunk_grid_idx, chunk_dims)
# Returns: (40, 10, 0) - element positions in HDF5 order + element dimension
```
"""
function julia_chunk_idx_to_hdf5_element_indices(chunk_grid_idx::CartesianIndex{N},
                                                 chunk_dims_julia::NTuple{N,Int}) where N
    # Convert 1-based chunk indices to 0-based element indices in Julia order
    julia_element_indices_0based = (Tuple(chunk_grid_idx) .- 1) .* chunk_dims_julia

    # Reverse to HDF5 order and append element size dimension (always 0)
    hdf5_element_indices_0based = reverse(julia_element_indices_0based)

    return tuple(hdf5_element_indices_0based..., 0)
end

"""
    assign_chunk_to_array!(v::Array{T,N}, vchunk::Array{T,N},
                          chunk_grid_idx::CartesianIndex{N},
                          chunk_dims::NTuple{N,Int}) where {T,N}

Assign chunk data to the appropriate region in the output array, handling edge chunks.

# Arguments
- `v`: Output array to write into
- `vchunk`: Chunk data to assign (may be full chunk size, even for edge chunks)
- `chunk_grid_idx`: 1-based chunk grid position in Julia order
- `chunk_dims`: Chunk dimensions in Julia order

# Details
This function handles:
- Edge chunks that don't fill the full chunk_dims
- Proper indexing for both the output array and chunk data
- Intersection between chunk region and array bounds

The function uses CartesianIndices intersection to elegantly handle edge cases.
"""
function assign_chunk_to_array!(v::Array{T,N}, vchunk::Array{T,N},
                               chunk_grid_idx::CartesianIndex{N},
                               chunk_dims::NTuple{N,Int}) where {T,N}
    # Calculate 0-based offset for this chunk in the array
    chunk_root = CartesianIndex(Tuple((chunk_grid_idx.I .- 1) .* chunk_dims))

    # Create indices for the chunk data (starting at 1)
    chunk_ids = CartesianIndices(tuple(chunk_dims...))

    # Get all valid indices in the output array
    array_ids = CartesianIndices(v)

    # Compute intersection (handles edge chunks automatically)
    vidxs = intersect(array_ids, chunk_ids .+ chunk_root)

    # Calculate corresponding indices in the chunk
    ch_idx = CartesianIndices(size(vidxs))

    # Copy chunk data to output array
    @views v[vidxs] .= vchunk[ch_idx]

    return nothing
end

"""
    read_and_assign_chunk!(f::JLDFile, v::Array{T}, chunk_grid_idx::CartesianIndex,
                          chunk_address::RelOffset, chunk_size_bytes::Int,
                          chunk_dims_julia::NTuple{N,Int}, rr, filters, filter_mask) where {T,N}

Read a chunk from file and assign it to the output array in one operation.

# Arguments
- `f`: JLD2 file
- `v`: Output array
- `chunk_grid_idx`: 1-based chunk grid position in Julia order
- `chunk_address`: File offset of chunk data
- `chunk_size_bytes`: Size of chunk data in bytes (compressed or uncompressed)
- `chunk_dims_julia`: Chunk dimensions in Julia order
- `rr`: Read representation
- `filters`: Filter pipeline for decompression
- `filter_mask`: Filter mask indicating which filters were applied

# Details
Combines chunk reading and array assignment to reduce code duplication.
Automatically handles edge chunks by reading full chunk size and using
intersection-based assignment.
"""
function read_and_assign_chunk!(f::JLDFile, v::Array{T}, chunk_grid_idx::CartesianIndex,
                               chunk_address::RelOffset, chunk_size_bytes::Int,
                               chunk_dims_julia::NTuple{N,Int}, rr, filters,
                               filter_mask) where {T,N}
    # Seek to chunk data
    seek(f.io, fileoffset(f, chunk_address))

    # Create temporary array for this chunk (always full size)
    vchunk = Array{T, N}(undef, chunk_dims_julia...)

    # Read chunk data with filter support
    read_chunk_with_filters!(vchunk, f, rr, chunk_size_bytes, filters, filter_mask)

    # Assign to output array (handles edge chunks)
    assign_chunk_to_array!(v, vchunk, chunk_grid_idx, chunk_dims_julia)

    return nothing
end
