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
    linear_index_to_chunk_coords(linear_idx::Int, grid_dims::NTuple{N,Int}) where N -> CartesianIndex{N}

Convert a 0-based linear chunk index to multi-dimensional chunk coordinates.

# Arguments
- `linear_idx`: 0-based linear index in HDF5 ordering
- `grid_dims`: Number of chunks in each dimension (Julia order)

# Returns
1-based CartesianIndex in Julia order

# Example
```julia
grid_dims = (10, 8)  # 10 chunks in first dim, 8 in second
chunk_idx = linear_index_to_chunk_coords(15, grid_dims)
# Returns: CartesianIndex(6, 2) - meaning 6th chunk in dim 1, 2nd in dim 2
```
"""
function linear_index_to_chunk_coords(linear_idx::Int, grid_dims::NTuple{N,Int}) where N
    # The linear index is in HDF5 order where the slowest (leftmost in HDF5) dimension varies first
    # This corresponds to the LAST dimension in Julia order varying slowest
    # So we need to convert from HDF5 linear to Julia coordinates

    # Convert grid dimensions to HDF5 order (reversed)
    grid_dims_hdf5 = reverse(grid_dims)

    # Convert linear index to multi-dimensional coordinates (HDF5 order, 0-based)
    # In HDF5 order: rightmost (fastest) dimension varies fastest in linear indexing
    coords_hdf5 = zeros(Int, N)
    idx = linear_idx
    for i in N:-1:1  # Process from fastest (rightmost in HDF5) to slowest (leftmost in HDF5)
        coords_hdf5[i] = idx % grid_dims_hdf5[i]
        idx = div(idx, grid_dims_hdf5[i])
    end

    # Reverse to Julia order and convert to 1-based
    coords_julia = reverse(coords_hdf5) .+ 1
    return CartesianIndex(Tuple(coords_julia))
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

# HDF5 Specification Compliance
Per HDF5 Format Specification Appendix C:
- Non-filtered chunks (Client ID = 0) MUST be padded to full chunk dimensions
- Filtered chunks (Client ID = 1) MAY be unpadded (size explicitly stored)

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
        # Extract chunk
        chunk_data_partial, _, _ = extract_chunk_region(data, julia_chunk_idx, chunks)

        # HDF5 spec: Non-filtered chunks MUST be padded, filtered chunks MAY be unpadded
        chunk_data = if !iscompressed(filter_pipeline)
            pad_chunk_data(chunk_data_partial, chunks, zero(T))
        else
            chunk_data_partial  # Filtered chunks can be unpadded (size stored explicitly)
        end

        # Write chunk
        chunk_offset, chunk_size = prepare_and_write_chunk(f, chunk_data, odr, filter_pipeline, wsession)

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
    hdf5_element_indices_from_linear(linear_idx::Int, grid_dims::NTuple{N,Int},
                                      chunk_dims::NTuple{N,Int}) where N

Convert 0-based linear chunk index to HDF5 0-based element indices.

HDF5 always describes chunks by the 0-based index of their root element, not by chunk numbers.
This function computes those indices directly from the linear chunk index.

# Arguments
- `linear_idx`: 0-based linear chunk index in HDF5 ordering
- `grid_dims`: Number of chunks in each dimension (Julia order)
- `chunk_dims`: Chunk dimensions (Julia order)

# Returns
Tuple of 0-based element indices in HDF5 order (reversed from Julia)

# Example
```julia
# For a 30×80 array with 10×20 chunks (3×4 grid):
linear_idx = 0  # First chunk
grid_dims = (3, 4)
chunk_dims = (10, 20)
indices = hdf5_element_indices_from_linear(0, grid_dims, chunk_dims)
# Returns (0, 0) - root element of first chunk in HDF5 order

linear_idx = 5  # Chunk at Julia position (2, 2)
# Returns (20, 10) - root element in HDF5 order
```
"""
function hdf5_element_indices_from_linear(linear_idx::Int, grid_dims::NTuple{N,Int},
                                          chunk_dims::NTuple{N,Int}) where N
    # Convert linear index to Julia chunk coordinates (1-based)
    chunk_coords_julia = linear_index_to_chunk_coords(linear_idx, grid_dims)

    # Convert to 0-based element indices in Julia order
    element_indices_julia = (Tuple(chunk_coords_julia) .- 1) .* chunk_dims

    # Reverse to HDF5 order
    return reverse(element_indices_julia)
end

"""
    chunk_element_start_julia(chunk_coords::CartesianIndex{N}, chunk_dims::NTuple{N,Int}) where N

Compute 1-based element start position from Julia chunk coordinates.

# Arguments
- `chunk_coords`: 1-based chunk coordinates in Julia order
- `chunk_dims`: Chunk dimensions (Julia order)

# Returns
1-based starting element position (Tuple)
"""
function chunk_element_start_julia(chunk_coords::CartesianIndex{N}, chunk_dims::NTuple{N,Int}) where N
    return (Tuple(chunk_coords) .- 1) .* chunk_dims .+ 1
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
    # Calculate starting position for this chunk in the array
    chunk_start = (Tuple(chunk_grid_idx) .- 1) .* chunk_dims .+ 1

    # Calculate ending position (limited by array bounds)
    chunk_end = min.(chunk_start .+ chunk_dims .- 1, size(v))

    # Determine actual region size in the output array
    actual_region_size = chunk_end .- chunk_start .+ 1

    # Create ranges for assignment in output array
    output_ranges = ntuple(i -> chunk_start[i]:chunk_end[i], N)

    # If vchunk is padded (full chunk_dims), extract only the actual data region
    # If vchunk is already the right size (unpadded), use it as-is
    if size(vchunk) == chunk_dims
        # Padded chunk: extract only actual data region
        chunk_ranges = ntuple(i -> 1:actual_region_size[i], N)
        v[output_ranges...] = vchunk[chunk_ranges...]
    else
        # Unpadded chunk: should match actual region size exactly
        v[output_ranges...] = vchunk
    end

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

    # Determine actual chunk size from grid position and array bounds
    chunk_start = (Tuple(chunk_grid_idx) .- 1) .* chunk_dims_julia .+ 1
    chunk_end = min.(chunk_start .+ chunk_dims_julia .- 1, size(v))
    actual_chunk_size = chunk_end .- chunk_start .+ 1

    # Check if this is an edge chunk
    is_edge_chunk = actual_chunk_size != chunk_dims_julia

    # Create temporary array for reading chunk data
    # HDF5 Specification Compliance (Appendix C):
    # - Non-filtered chunks (Client ID = 0): Always padded to full chunk_dims
    # - Filtered chunks (Client ID = 1): May be unpadded (size explicitly stored)
    #
    # Strategy:
    # 1. Non-filtered edge chunks: Read full padded size, use only actual_chunk_size
    # 2. Filtered edge chunks: Check chunk_size_bytes to determine if unpadded
    # 3. Full chunks: Always read full chunk_dims
    vchunk = if is_edge_chunk && iscompressed(filters)
        # Filtered chunks may be unpadded - check size
        if chunk_size_bytes < prod(chunk_dims_julia) * sizeof(T)
            Array{T, N}(undef, actual_chunk_size...)
        else
            Array{T, N}(undef, chunk_dims_julia...)
        end
    else
        # Non-filtered chunks or full chunks: Always full size
        Array{T, N}(undef, chunk_dims_julia...)
    end

    # Read chunk data with filter support
    read_chunk_with_filters!(vchunk, f, rr, chunk_size_bytes, filters, filter_mask)

    # Assign to output array (handles edge chunks)
    assign_chunk_to_array!(v, vchunk, chunk_grid_idx, chunk_dims_julia)

    return nothing
end
