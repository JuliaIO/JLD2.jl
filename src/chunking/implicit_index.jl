# Implicit Index for HDF5 DataLayout v4
#
# Implements HDF5 Implicit chunk indexing (type 2) as specified in
# Section VII.B of the HDF5 format specification.
#
# Used when:
# - Dataset has fixed maximum dimension sizes
# - No filter applied to the dataset
# - Early space allocation (H5D_ALLOC_TIME_EARLY)
#
# All chunks are stored contiguously in the file starting at the base address.

"""
    read_implicit_index_chunks(f::JLDFile, v::Array{T}, dataspace::ReadDataspace,
                               rr::ReadRepresentation, layout::DataLayout,
                               filters::FilterPipeline, header_offset::RelOffset,
                               ndims::Int) where T

Read all chunks of a dataset using Implicit indexing into the preallocated array `v`.

The Implicit index stores chunks contiguously in the file. The chunk address for a given
chunk is calculated as:
    chunk_address = base_address + (chunk_size_bytes × chunk_index)

where chunk_index is the linear index calculated from N-dimensional chunk coordinates.
"""
function read_implicit_index_chunks(f::JLDFile, v::Array{T}, dataspace::ReadDataspace,
                                   @nospecialize(rr::ReadRepresentation), layout::DataLayout,
                                   filters::FilterPipeline, header_offset::RelOffset,
                                   ndims::Int) where T
    io = f.io

    # Get array dimensions from the preallocated array v
    # (construct_array has already reversed the dimensions from the file)
    array_dims_julia = size(v)

    # Get chunk dimensions from layout
    # layout.chunk_dimensions are in HDF5 order
    chunk_dims_hdf5 = layout.chunk_dimensions[1:ndims]
    # Convert to Julia order by reversing
    chunk_dims_julia = reverse(chunk_dims_hdf5)

    # Base address for chunks (stored directly in DataLayout message for implicit index)
    # For implicit index, data_offset is already an absolute file offset
    base_address = Int64(layout.data_offset)

    # Calculate chunk size in bytes
    chunk_size_bytes = Int(prod(chunk_dims_julia) * sizeof(T))

    # Create unified chunk grid iterator (automatically computes grid dimensions)
    chunk_grid = ChunkGrid(array_dims_julia, chunk_dims_julia)

    # Iterate over all chunks
    for (chunk_grid_idx, linear_idx) in chunk_grid
        # Calculate chunk address using linear index
        chunk_address = base_address + (linear_idx * chunk_size_bytes)

        # Compute chunk starting position (more efficient than computing inside read_and_assign_chunk!)
        chunk_start = chunk_start_from_index(chunk_grid_idx, chunk_dims_julia)

        # Read and assign chunk (implicit index doesn't use filters by definition)
        filter_mask = 0
        read_and_assign_chunk!(f, v, chunk_start, RelOffset(chunk_address),
                              chunk_size_bytes, chunk_dims_julia, rr, filters, filter_mask)
    end

    track_weakref!(f, header_offset, v)
    return v
end
