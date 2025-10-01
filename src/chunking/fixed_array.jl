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
define_packed(FixedArrayHeader)

"""
    write_fixed_array_header(io, hdr::FixedArrayHeader) -> Int

Write Fixed Array header with signature and checksum, return total bytes written.
"""
function write_fixed_array_header(io, hdr::FixedArrayHeader)
    header_size = 4 + jlsizeof(FixedArrayHeader) + 4  # signature + header + checksum
    cio = begin_checksum_write(io, header_size - 4)
    jlwrite(cio, FIXED_ARRAY_HEADER_SIGNATURE)
    jlwrite(cio, hdr)
    jlwrite(io, end_checksum(cio))
    return header_size
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
    max_num_entries = Int64(jlread(io, Length))
    data_block_address = jlread(io, RelOffset)

    # Skip checksum (4 bytes)
    skip(io, 4)

    return FixedArrayHeader(version, client_id, entry_size, page_bits,
                           max_num_entries, data_block_address)
end

# Note: compute_chunk_index has been removed. Use ChunkLinearIndexer directly:
#   indexer = ChunkLinearIndexer(grid_dims_julia_order)
#   linear_idx = compute_linear_index(indexer, julia_chunk_idx)

"""
    read_chunk_entry_direct(io::IO, header::FixedArrayHeader, chunk_idx::Int) -> Tuple

Read chunk entry from direct (non-paged) storage.

Returns (chunk_address, chunk_size) or (nothing, nothing) if unallocated.
"""
function read_chunk_entry_direct(io::IO, header::FixedArrayHeader, chunk_idx::Integer)
    entry_offset = chunk_idx * Int(header.entry_size)
    skip(io, entry_offset)

    chunk_address = jlread(io, RelOffset)
    if chunk_address == UNDEFINED_ADDRESS
        return (nothing, nothing)
    end

    chunk_size = if header.client_id == 1
        jlread(io, UInt64)
    else
        nothing
    end

    return (chunk_address, chunk_size)
end

"""
    read_chunk_entry_paged(io::IO, header::FixedArrayHeader, chunk_idx::Int, page_size::Int) -> Tuple

Read chunk entry from paged storage.

Returns (chunk_address, chunk_size) or (nothing, nothing) if unallocated.
"""
function read_chunk_entry_paged(io::IO, header::FixedArrayHeader, chunk_idx::Integer, page_size::Int)
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
        return (nothing, nothing)
    end

    # Calculate offset to our page
    pages_start = position(io)
    page_data_size = page_size * Int(header.entry_size)
    page_total_size = page_data_size + 4  # +4 for checksum
    page_offset = pages_start + (page_num * page_total_size)

    # Seek to entry within page
    seek(io, page_offset)
    entry_offset_in_page = page_offset_within_page * Int(header.entry_size)
    skip(io, entry_offset_in_page)

    # Read chunk entry
    chunk_address = jlread(io, RelOffset)
    if chunk_address == UNDEFINED_ADDRESS
        return (nothing, nothing)
    end

    chunk_size = if header.client_id == 1
        jlread(io, UInt64)
    else
        nothing
    end

    return (chunk_address, chunk_size)
end

"""
    lookup_chunk_address_fixed_array(f::JLDFile, layout::DataLayout, chunk_indices::Tuple, array_dims::Tuple)

Look up the file address and size of a chunk using the Fixed Array index.

# Arguments
- `f`: JLD2 file
- `layout`: DataLayout containing Fixed Array indexing info
- `chunk_indices`: 0-based chunk coordinates in HDF5 order (including element size dimension)
- `array_dims`: Array dimensions in HDF5 order (fastest to slowest)

# Returns
(chunk_address, chunk_size) tuple where:
- chunk_address is RelOffset to chunk data (or nothing if not allocated)
- chunk_size is compressed size in bytes (or nothing if no compression or not allocated)
"""
function lookup_chunk_address_fixed_array(f::JLDFile, layout::DataLayout,
                                         chunk_indices::Tuple, array_dims::Tuple)
    io = f.io

    # Read Fixed Array header
    header = read_fixed_array_header(f, layout.data_offset)

    # Calculate number of chunks in each dimension
    chunk_dims_hdf5 = layout.chunk_dimensions[1:end-1]  # exclude element size

    # Compute nchunks for each dimension (in HDF5 order)
    ndims = length(array_dims)
    nchunks = ntuple(ndims) do i
        cld(array_dims[i], chunk_dims_hdf5[i])
    end

    # Convert element indices to chunk grid indices (0-based HDF5 coordinates)
    element_indices = chunk_indices[1:ndims]
    chunk_coords_0based = ntuple(ndims) do i
        div(element_indices[i], chunk_dims_hdf5[i])
    end

    # Convert to Julia 1-based CartesianIndex in Julia order (reverse)
    julia_chunk_idx = CartesianIndex(reverse(chunk_coords_0based) .+ 1)

    # Compute linear chunk index using ChunkLinearIndexer
    indexer = ChunkLinearIndexer(reverse(nchunks))
    chunk_idx = compute_linear_index(indexer, julia_chunk_idx)

    # Check bounds (chunk_idx is 0-based)
    if chunk_idx >= header.max_num_entries
        throw(InvalidDataException("Chunk index $chunk_idx exceeds maximum entries $(header.max_num_entries)"))
    end

    # Read data block header
    data_block_addr = fileoffset(f, header.data_block_address)
    seek(io, data_block_addr)

    # Verify data block signature
    db_signature = jlread(io, UInt32)
    if db_signature != FIXED_ARRAY_DATABLOCK_SIGNATURE
        throw(InvalidDataException("Invalid Fixed Array data block signature"))
    end

    # Skip version (1) and client_id (1)
    skip(io, 2)

    # Skip header address (8 bytes)
    skip(io, 8)

    # Determine if paging is active
    page_size = header.page_bits > 0 ? (1 << header.page_bits) : typemax(Int)
    is_paged = header.page_bits > 0 && header.max_num_entries > page_size

    if is_paged
        return read_chunk_entry_paged(io, header, chunk_idx, page_size)
    else
        return read_chunk_entry_direct(io, header, chunk_idx)
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

    # Get array dimensions
    array_dims_julia = size(v)
    array_dims_hdf5 = reverse(array_dims_julia)

    # Get chunk dimensions
    chunk_dims_hdf5 = layout.chunk_dimensions[1:ndims]
    chunk_dims_julia = reverse(chunk_dims_hdf5)

    # Calculate number of chunks
    nchunks_julia = cld.(array_dims_julia, chunk_dims_julia)

    # Iterate over all chunks
    chunk_ids = CartesianIndices(tuple(chunk_dims_julia...))

    for chunk_grid_idx in CartesianIndices(tuple(nchunks_julia...))
        # Convert chunk grid index to element index (0-based)
        julia_coords = Tuple(chunk_grid_idx)
        julia_element_indices_0based = (julia_coords .- 1) .* chunk_dims_julia

        # Convert to HDF5 order with element size dimension
        hdf5_element_indices_0based = reverse(julia_element_indices_0based)
        chunk_indices_with_elemsize = tuple(hdf5_element_indices_0based..., 0)

        # Look up chunk address and size
        chunk_address, compressed_size = lookup_chunk_address_fixed_array(f, layout,
                                                         chunk_indices_with_elemsize,
                                                         array_dims_hdf5)

        if isnothing(chunk_address)
            continue  # Chunk not allocated - skip
        end

        # Seek to chunk data
        seek(io, fileoffset(f, chunk_address))

        # Create temporary array for this chunk
        vchunk = Array{T, ndims}(undef, chunk_dims_julia...)

        # Determine chunk size in bytes
        chunk_size_bytes = if !isnothing(compressed_size)
            Int(compressed_size)
        else
            Int(prod(chunk_dims_julia) * sizeof(T))
        end

        # Read chunk data with filter support
        filter_mask = 0
        read_chunk_with_filters!(vchunk, f, rr, chunk_size_bytes, filters, filter_mask)

        # Calculate indices in the output array
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
