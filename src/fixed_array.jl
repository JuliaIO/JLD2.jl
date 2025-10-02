# Fixed Array Index for HDF5 DataLayout v4
#
# Implements HDF5 Fixed Array chunk indexing (type 3) as specified in
# Section VII.C of the HDF5 format specification.
#
# Used when datasets have fixed maximum dimension sizes.

const FIXED_ARRAY_HEADER_SIGNATURE = htol(0x44484146)  # "FAHD"
const FIXED_ARRAY_DATABLOCK_SIGNATURE = htol(0x42444146)  # "FADB"

"""
    FixedArrayHeader

Header structure for HDF5 Fixed Array index.

# Fields
- `version::UInt8` - Version number (currently 0)
- `client_id::UInt8` - Client type (0=non-filtered, 1=filtered chunks)
- `entry_size::UInt8` - Size in bytes of each entry
- `page_bits::UInt8` - Bits for page size (0 if not paged)
- `max_num_entries::Int64` - Maximum number of entries (total chunks)
- `data_block_address::RelOffset` - Address of the data block
"""
struct FixedArrayHeader
    version::UInt8
    client_id::UInt8
    entry_size::UInt8
    page_bits::UInt8
    max_num_entries::Int64
    data_block_address::RelOffset
end

"""
    read_fixed_array_header(f::JLDFile, header_address::Int64) -> FixedArrayHeader

Read the Fixed Array header structure from the file.
"""
function read_fixed_array_header(f::JLDFile, header_address::Int64)
    io = f.io
    seek(io, header_address)

    # Read and verify signature
    signature = jlread(io, UInt32)
    if signature != FIXED_ARRAY_HEADER_SIGNATURE
        throw(InvalidDataException("Invalid Fixed Array header signature: expected 0x$(string(FIXED_ARRAY_HEADER_SIGNATURE, base=16)), got 0x$(string(signature, base=16))"))
    end

    # Read header fields
    version = jlread(io, UInt8)
    client_id = jlread(io, UInt8)
    entry_size = jlread(io, UInt8)
    page_bits = jlread(io, UInt8)

    # Max num entries is stored as "Length" type (8 bytes in JLD2)
    max_num_entries = Int64(jlread(io, Length))

    # Data block address
    data_block_address = jlread(io, RelOffset)

    # Skip checksum (4 bytes)
    skip(io, 4)

    return FixedArrayHeader(version, client_id, entry_size, page_bits,
                           max_num_entries, data_block_address)
end

"""
    compute_chunk_index(chunk_indices::Tuple, nchunks::Tuple) -> Int

Compute the linear chunk index from N-dimensional chunk coordinates.

# Arguments
- `chunk_indices`: Tuple of 0-based chunk indices in HDF5 order (fastest to slowest)
- `nchunks`: Tuple of number of chunks in each dimension (HDF5 order)

# Returns
Linear index into the chunk address array (0-based).

This implements the chunk index calculation from Section VII.B of the HDF5 spec:
1. Calculate down_chunks for each dimension
2. Compute chunk_index = sum(down_chunks[i] * chunk_indices[i])
"""
function compute_chunk_index(chunk_indices::Tuple, nchunks::Tuple)
    ndims = length(nchunks)

    # Calculate down_chunks (product of all faster-changing dimensions)
    down_chunks = zeros(Int, ndims)
    acc = 1
    for i in ndims:-1:1
        down_chunks[i] = acc
        acc *= nchunks[i]
    end

    # Compute linear index
    chunk_index = 0
    for i in 1:ndims
        chunk_index += down_chunks[i] * chunk_indices[i]
    end

    return chunk_index
end

"""
    lookup_chunk_address_fixed_array(f::JLDFile, layout::DataLayout,
                                     chunk_indices::Tuple, array_dims::Tuple) -> Union{RelOffset, Nothing}

Look up the file address of a chunk using the Fixed Array index.

# Arguments
- `f`: JLD2 file
- `layout`: DataLayout containing Fixed Array indexing info
- `chunk_indices`: 0-based chunk coordinates in HDF5 order (including element size dimension)
- `array_dims`: Array dimensions in HDF5 order (fastest to slowest)

# Returns
RelOffset of the chunk data, or nothing if chunk is not allocated.
"""
function lookup_chunk_address_fixed_array(f::JLDFile, layout::DataLayout,
                                         chunk_indices::Tuple, array_dims::Tuple)
    io = f.io

    # Read Fixed Array header
    header = read_fixed_array_header(f, layout.data_offset)

    # Calculate number of chunks in each dimension
    # chunk_indices are element indices (0-based), not chunk grid indices
    # We need to convert them to chunk grid indices
    # layout.chunk_dimensions are in HDF5 order (same order as array_dims)
    chunk_dims_hdf5 = layout.chunk_dimensions[1:end-1]  # exclude element size

    # Compute nchunks for each dimension (in HDF5 order)
    ndims = length(array_dims)
    nchunks = ntuple(ndims) do i
        cld(array_dims[i], chunk_dims_hdf5[i])
    end

    # chunk_indices includes element size dimension, so we drop the last one
    element_indices = chunk_indices[1:ndims]

    # Convert element indices to chunk grid indices by dividing by chunk size
    chunk_coords = ntuple(ndims) do i
        div(element_indices[i], chunk_dims_hdf5[i])
    end

    # Compute linear chunk index
    chunk_idx = compute_chunk_index(chunk_coords, nchunks)

    # Check bounds (chunk_idx is 0-based, so valid range is 0 to max_num_entries-1)
    if chunk_idx >= header.max_num_entries
        throw(InvalidDataException("Chunk index $chunk_idx exceeds maximum entries $(header.max_num_entries)"))
    end

    # Read data block header
    data_block_addr = fileoffset(f, header.data_block_address)
    seek(io, data_block_addr)

    # Read and verify data block signature
    db_signature = jlread(io, UInt32)
    if db_signature != FIXED_ARRAY_DATABLOCK_SIGNATURE
        throw(InvalidDataException("Invalid Fixed Array data block signature"))
    end

    # Skip version (1) and client_id (1)
    skip(io, 2)

    # Skip header address (8 bytes)
    skip(io, 8)

    # Check if paging is actually used
    # Paging is only active if number of entries exceeds page threshold
    # page_bits being set doesn't guarantee paging is used
    page_size = header.page_bits > 0 ? (1 << header.page_bits) : typemax(Int)
    is_paged = header.page_bits > 0 && header.max_num_entries > page_size

    if is_paged
        # Paged storage (page_size already calculated above)

        # Calculate which page contains our chunk
        page_num = div(chunk_idx, page_size)
        page_offset_within_page = chunk_idx % page_size

        # Calculate bitmap size (one bit per page)
        num_pages = cld(header.max_num_entries, page_size)
        bitmap_bytes = cld(num_pages, 8)

        # Read bitmap to check if page is initialized
        bitmap = zeros(UInt8, bitmap_bytes)
        read!(io, bitmap)

        # Check if the page is initialized
        byte_idx = div(page_num, 8)
        bit_idx = page_num % 8
        page_initialized = ((bitmap[byte_idx + 1] >> bit_idx) & 0x01) != 0

        if !page_initialized
            # Page not initialized - chunk not allocated
            return nothing
        end

        # Calculate offset to the start of pages
        # Pages start after the bitmap
        pages_start = position(io)

        # Calculate offset to our specific page
        # Each page contains page_size elements of entry_size bytes each
        # Plus 4 bytes for page checksum
        page_data_size = page_size * Int(header.entry_size)
        page_total_size = page_data_size + 4  # +4 for checksum

        page_offset = pages_start + (page_num * page_total_size)

        # Seek to our page
        seek(io, page_offset)

        # Within the page, seek to our entry
        entry_offset_in_page = page_offset_within_page * Int(header.entry_size)
        skip(io, entry_offset_in_page)

        # Read chunk address
        chunk_address = jlread(io, RelOffset)

        # UNDEFINED_ADDRESS means chunk not allocated
        if chunk_address == UNDEFINED_ADDRESS
            return nothing
        end

        return chunk_address
    else
        # Direct element storage (no paging)
        # Seek to the correct entry in the elements array
        # Entry position = current position + (chunk_idx * entry_size)
        entry_offset = chunk_idx * Int(header.entry_size)
        skip(io, entry_offset)

        # Read chunk address
        chunk_address = jlread(io, RelOffset)

        # UNDEFINED_ADDRESS means chunk not allocated
        if chunk_address == UNDEFINED_ADDRESS
            return nothing
        end

        return chunk_address
    end
end

"""
    read_fixed_array_chunks(f::JLDFile, v::Array{T}, dataspace::ReadDataspace,
                           rr::ReadRepresentation, layout::DataLayout,
                           filters::FilterPipeline, header_offset::RelOffset,
                           ndims::Int) where T

Read all chunks of a dataset using Fixed Array indexing into the preallocated array `v`.
"""
function read_fixed_array_chunks(f::JLDFile, v::Array{T}, dataspace::ReadDataspace,
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

    # Calculate number of chunks in each dimension (Julia order)
    nchunks_julia = cld.(array_dims_julia, chunk_dims_julia)

    # Iterate over all chunks
    chunk_ids = CartesianIndices(tuple(chunk_dims_julia...))

    for chunk_grid_idx in CartesianIndices(tuple(nchunks_julia...))
        # Convert chunk grid index to element index (0-based)
        julia_coords = Tuple(chunk_grid_idx)

        # Compute element indices: multiply chunk grid index by chunk size
        julia_element_indices_0based = (julia_coords .- 1) .* chunk_dims_julia

        # Convert to HDF5 order (reverse)
        hdf5_element_indices_0based = reverse(julia_element_indices_0based)

        # Add element size dimension (always 0)
        chunk_indices_with_elemsize = tuple(hdf5_element_indices_0based..., 0)

        # Look up chunk address
        chunk_address = lookup_chunk_address_fixed_array(f, layout,
                                                         chunk_indices_with_elemsize,
                                                         array_dims_hdf5)

        if isnothing(chunk_address)
            # Chunk not allocated - fill with zeros or fill value
            # For now, we'll skip unallocated chunks
            continue
        end


        # Seek to chunk data
        seek(io, fileoffset(f, chunk_address))

        # Create temporary array for this chunk
        vchunk = Array{T, ndims}(undef, chunk_dims_julia...)

        # Determine actual chunk size in bytes
        # For non-filtered chunks, size = prod(chunk_dims) * sizeof(T)
        chunk_size_bytes = Int(prod(chunk_dims_julia) * sizeof(T))

        # Read chunk data with filter support
        filter_mask = 0  # Fixed Array header.client_id tells us if filtered, but individual chunks have no filter mask
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
