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
    read_fixed_array_header(f::JLDFile, header_pos::Int64) -> FixedArrayHeader

Read the Fixed Array header structure from the file.
"""
function read_fixed_array_header(f::JLDFile, header_pos::Int64)
    io = f.io
    seek(io, header_pos)

    cio = begin_checksum_read(io)
    # Read and verify signature
    signature = jlread(io, UInt32)
    if signature != FIXED_ARRAY_HEADER_SIGNATURE
        throw(InvalidDataException("Invalid Fixed Array header signature: expected 0x$(string(FIXED_ARRAY_HEADER_SIGNATURE, base=16)), got 0x$(string(signature, base=16))"))
    end

    # Read header fields
    hdr = jlread(cio, FixedArrayHeader)
    end_checksum(cio) == jlread(io, UInt32) || throw(InvalidDataException("Invalid Checksum"))

    return hdr
end

"""
    read_all_chunk_entries_fixed_array(io::IO, header::FixedArrayHeader, n_chunks::Int) -> Vector{Tuple}

Read all chunk entries from Fixed Array data block in one pass.

Returns a vector of (chunk_address::Union{RelOffset,Nothing}, chunk_size::Union{Int,Nothing})
tuples indexed by linear chunk index (0-based).
"""
function read_all_chunk_entries_fixed_array(io::IO, header::FixedArrayHeader, n_chunks::Int)
    # Determine if paging is active
    page_size = header.page_bits > 0 ? (1 << header.page_bits) : typemax(Int)
    is_paged = header.page_bits > 0 && header.max_num_entries > page_size

    chunk_entries = Vector{Tuple{Union{RelOffset,Nothing}, Union{Int,Nothing}}}(undef, n_chunks)

    if is_paged
        # For paged storage, we need to read each page separately
        entry_start_pos = position(io)
        num_pages = cld(header.max_num_entries, page_size)
        bitmap_bytes = cld(num_pages, 8)

        # Read bitmap
        bitmap = zeros(UInt8, bitmap_bytes)
        read!(io, bitmap)

        pages_start = position(io)
        page_data_size = page_size * Int(header.entry_size)
        page_total_size = page_data_size + 4  # +4 for checksum

        for chunk_idx in 0:(n_chunks-1)
            page_num = div(chunk_idx, page_size)
            page_offset_within_page = chunk_idx % page_size

            # Check if page is initialized
            byte_idx = div(page_num, 8)
            bit_idx = page_num % 8
            page_initialized = ((bitmap[byte_idx + 1] >> bit_idx) & 0x01) != 0

            if !page_initialized
                chunk_entries[chunk_idx + 1] = (nothing, nothing)
                continue
            end

            # Seek to entry within page
            page_offset = pages_start + (page_num * page_total_size)
            entry_offset_in_page = page_offset_within_page * Int(header.entry_size)
            seek(io, page_offset + entry_offset_in_page)

            # Read entry
            chunk_address = jlread(io, RelOffset)
            if chunk_address == UNDEFINED_ADDRESS
                chunk_entries[chunk_idx + 1] = (nothing, nothing)
            else
                chunk_size = if header.client_id == 1
                    Int(jlread(io, UInt64))
                else
                    nothing
                end
                chunk_entries[chunk_idx + 1] = (chunk_address, chunk_size)
            end
        end
    else
        # Non-paged: read all entries sequentially (most efficient)
        entry_start_pos = position(io)

        for chunk_idx in 0:(n_chunks-1)
            chunk_address = jlread(io, RelOffset)
            if chunk_address == UNDEFINED_ADDRESS
                chunk_entries[chunk_idx + 1] = (nothing, nothing)
                # Skip compressed size if present
                if header.client_id == 1
                    skip(io, 8)  # skip UInt64 chunk_size
                end
            else
                chunk_size = if header.client_id == 1
                    Int(jlread(io, UInt64))
                else
                    nothing
                end
                chunk_entries[chunk_idx + 1] = (chunk_address, chunk_size)
            end
        end
    end

    return chunk_entries
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

    # Get array and chunk dimensions
    array_dims_julia = size(v)
    chunk_dims_hdf5 = layout.chunk_dimensions[1:ndims]
    chunk_dims_julia = Tuple(Int.(reverse(chunk_dims_hdf5)))

    # Calculate number of chunks for reading entries
    n_chunks_total = prod(cld.(array_dims_julia, chunk_dims_julia))

    # Read Fixed Array header once
    header = read_fixed_array_header(f, layout.data_offset)

    # Position to start of chunk entries in data block
    data_block_addr = fileoffset(f, header.data_block_address)
    seek(io, data_block_addr)

    # Verify data block signature once
    db_signature = jlread(io, UInt32)
    if db_signature != FIXED_ARRAY_DATABLOCK_SIGNATURE
        throw(InvalidDataException("Invalid Fixed Array data block signature"))
    end

    # Skip version (1) + client_id (1) + header address (8)
    skip(io, 10)

    # Read all chunk entries in one pass
    chunk_entries = read_all_chunk_entries_fixed_array(io, header, n_chunks_total)

    # Create unified chunk grid iterator (automatically computes grid dimensions)
    chunk_grid = ChunkGrid(array_dims_julia, chunk_dims_julia)

    # Now load chunks using the pre-read entries
    for (chunk_grid_idx, linear_idx) in chunk_grid
        chunk_address, compressed_size = chunk_entries[linear_idx + 1]

        if isnothing(chunk_address)
            continue  # Chunk not allocated - skip
        end

        # Determine chunk size in bytes
        chunk_size_bytes = if !isnothing(compressed_size)
            compressed_size
        else
            Int(prod(chunk_dims_julia) * sizeof(T))
        end

        # Compute chunk starting position (more efficient than computing inside read_and_assign_chunk!)
        chunk_start = chunk_start_from_index(chunk_grid_idx, chunk_dims_julia)

        # Read and assign chunk
        filter_mask = 0
        read_and_assign_chunk!(f, v, chunk_start, chunk_address,
                              chunk_size_bytes, chunk_dims_julia, rr, filters, filter_mask)
    end

    track_weakref!(f, header_offset, v)
    return v
end
