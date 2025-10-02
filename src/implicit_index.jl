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
    array_dims_hdf5 = reverse(array_dims_julia)

    # Get chunk dimensions from layout
    # layout.chunk_dimensions are in HDF5 order (matching array_dims_hdf5)
    chunk_dims_hdf5 = layout.chunk_dimensions[1:ndims]
    # Convert to Julia order by reversing
    chunk_dims_julia = reverse(chunk_dims_hdf5)

    # Calculate number of chunks in each dimension (HDF5 order)
    nchunks_hdf5 = ntuple(ndims) do i
        cld(array_dims_hdf5[i], chunk_dims_hdf5[i])
    end

    # Calculate number of chunks in Julia order for iteration
    nchunks_julia = cld.(array_dims_julia, chunk_dims_julia)

    # Base address for chunks (stored directly in DataLayout message for implicit index)
    # For implicit index, data_offset is already an absolute file offset
    base_address = Int64(layout.data_offset)

    # Calculate chunk size in bytes
    chunk_size_bytes = Int(prod(chunk_dims_julia) * sizeof(T))

    # Chunk indices for within-chunk addressing (Julia order)
    chunk_ids = CartesianIndices(tuple(chunk_dims_julia...))

    # Iterate over all chunks
    for chunk_grid_idx in CartesianIndices(tuple(nchunks_julia...))
        # Convert chunk grid index to element index (0-based)
        julia_coords = Tuple(chunk_grid_idx)

        # Compute element indices: multiply chunk grid index by chunk size
        julia_element_indices_0based = (julia_coords .- 1) .* chunk_dims_julia

        # Convert to HDF5 order (reverse)
        hdf5_element_indices_0based = reverse(julia_element_indices_0based)

        # Convert element indices to chunk grid coordinates (in HDF5 order)
        hdf5_chunk_coords = ntuple(ndims) do i
            div(hdf5_element_indices_0based[i], chunk_dims_hdf5[i])
        end

        # Compute linear chunk index using HDF5 algorithm
        chunk_index = compute_chunk_index(hdf5_chunk_coords, nchunks_hdf5)

        # Calculate chunk address
        chunk_address = base_address + (chunk_index * chunk_size_bytes)

        # Seek to chunk data
        seek(io, chunk_address)

        # Create temporary array for this chunk
        vchunk = Array{T, ndims}(undef, chunk_dims_julia...)

        # Read chunk data (implicit index doesn't use filters by definition)
        # But we still call read_chunk_with_filters! for consistency
        filter_mask = 0
        read_chunk_with_filters!(vchunk, f, rr, chunk_size_bytes, filters, filter_mask)

        # Calculate indices in the output array
        # chunk_root is 0-based offset to be added to 1-based chunk_ids
        chunk_root = CartesianIndex(Tuple((chunk_grid_idx.I .- 1) .* chunk_dims_julia))
        array_ids = CartesianIndices(v)

        # Compute intersection (handles edge chunks)
        vidxs = intersect(array_ids, chunk_ids .+ chunk_root)
        ch_idx = CartesianIndices(size(vidxs))

        # Copy chunk data to output array
        @views v[vidxs] .= vchunk[ch_idx]
    end

    track_weakref!(f, header_offset, v)
    return v
end
