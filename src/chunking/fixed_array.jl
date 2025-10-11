const FIXED_ARRAY_HEADER_SIGNATURE = htol(0x44484146)  # "FAHD"
const FIXED_ARRAY_DATABLOCK_SIGNATURE = htol(0x42444146)  # "FADB"

"""Fixed Array header for HDF5 chunk indexing (type 3)."""
struct FixedArrayHeader
    version::UInt8
    client_id::UInt8
    entry_size::UInt8
    page_bits::UInt8
    max_num_entries::Int64
    data_block_address::RelOffset
end
define_packed(FixedArrayHeader)

"""Write Fixed Array header with signature and checksum."""
function write_fixed_array_header(io, hdr::FixedArrayHeader)
    header_size = 4 + jlsizeof(FixedArrayHeader) + 4
    cio = begin_checksum_write(io, header_size - 4)
    jlwrite(cio, FIXED_ARRAY_HEADER_SIGNATURE)
    jlwrite(cio, hdr)
    jlwrite(io, end_checksum(cio))
    return header_size
end

"""Read and verify Fixed Array header."""
function read_fixed_array_header(f::JLDFile, header_pos::Int64)
    seek(f.io, header_pos)
    cio = begin_checksum_read(f.io)
    signature = jlread(f.io, UInt32)
    signature == FIXED_ARRAY_HEADER_SIGNATURE ||
        throw(InvalidDataException("Invalid Fixed Array header signature"))
    hdr = jlread(cio, FixedArrayHeader)
    end_checksum(cio) == jlread(f.io, UInt32) || throw(InvalidDataException("Invalid checksum"))
    return hdr
end

"""Read all chunk entries from Fixed Array data block (paged or non-paged)."""
function read_all_chunk_entries_fixed_array(io::IO, header::FixedArrayHeader, n_chunks::Int)
    page_size = header.page_bits > 0 ? (1 << header.page_bits) : typemax(Int)
    is_paged = header.page_bits > 0 && header.max_num_entries > page_size
    chunk_entries = Vector{Tuple{Union{RelOffset,Nothing}, Union{Int,Nothing}}}(undef, n_chunks)

    if is_paged
        bitmap = zeros(UInt8, cld(cld(header.max_num_entries, page_size), 8))
        read!(io, bitmap)
        pages_start = position(io)
        page_total_size = page_size * Int(header.entry_size) + 4

        for chunk_idx in 0:(n_chunks-1)
            page_num = div(chunk_idx, page_size)
            byte_idx, bit_idx = divrem(page_num, 8)
            page_initialized = ((bitmap[byte_idx + 1] >> bit_idx) & 0x01) != 0

            if !page_initialized
                chunk_entries[chunk_idx + 1] = (nothing, nothing)
            else
                seek(io, pages_start + page_num * page_total_size +
                         (chunk_idx % page_size) * Int(header.entry_size))
                chunk_address = jlread(io, RelOffset)
                chunk_entries[chunk_idx + 1] = chunk_address == UNDEFINED_ADDRESS ?
                    (nothing, nothing) :
                    (chunk_address, header.client_id == 1 ? Int(jlread(io, UInt64)) : nothing)
            end
        end
    else
        for chunk_idx in 0:(n_chunks-1)
            chunk_address = jlread(io, RelOffset)
            if chunk_address == UNDEFINED_ADDRESS
                chunk_entries[chunk_idx + 1] = (nothing, nothing)
                header.client_id == 1 && skip(io, 8)
            else
                chunk_entries[chunk_idx + 1] = (chunk_address,
                    header.client_id == 1 ? Int(jlread(io, UInt64)) : nothing)
            end
        end
    end

    return chunk_entries
end

"""Read chunks using Fixed Array indexing (type 3)."""
function read_fixed_array_chunks(f::JLDFile, v::Array{T}, dataspace::ReadDataspace,
                                @nospecialize(rr::ReadRepresentation), layout::DataLayout,
                                filters::FilterPipeline, header_offset::RelOffset,
                                ndims::Int) where T
    chunk_dims_julia = Tuple(Int.(reverse(layout.chunk_dimensions[1:ndims])))
    n_chunks_total = prod(cld.(size(v), chunk_dims_julia))
    header = read_fixed_array_header(f, layout.data_offset)

    seek(f.io, fileoffset(f, header.data_block_address))
    jlread(f.io, UInt32) == FIXED_ARRAY_DATABLOCK_SIGNATURE ||
        throw(InvalidDataException("Invalid Fixed Array data block signature"))
    skip(f.io, 10)

    chunk_entries = read_all_chunk_entries_fixed_array(f.io, header, n_chunks_total)

    for (chunk_grid_idx, linear_idx) in ChunkIndexIterator(size(v), chunk_dims_julia)
        chunk_address, compressed_size = chunk_entries[linear_idx + 1]
        isnothing(chunk_address) && continue

        chunk_size_bytes = isnothing(compressed_size) ? Int(prod(chunk_dims_julia) * sizeof(T)) : compressed_size
        chunk_start = chunk_start_from_index(chunk_grid_idx, chunk_dims_julia)
        read_and_assign_chunk!(f, v, chunk_start, chunk_address,
                              chunk_size_bytes, chunk_dims_julia, rr, filters, 0)
    end
    return v
end
