const EXTENSIBLE_ARRAY_HEADER_SIGNATURE = htol(0x44484145)  # "EAHD"
const EXTENSIBLE_ARRAY_INDEX_BLOCK_SIGNATURE = htol(0x42494145)  # "EAIB"
const EXTENSIBLE_ARRAY_DATA_BLOCK_SIGNATURE = htol(0x42444145)  # "EADB"

"""Extensible Array header for HDF5 chunk indexing (type 4, one unlimited dimension)."""
struct ExtensibleArrayHeader
    version::UInt8
    client_id::UInt8
    element_size::UInt8
    max_nelmts_bits::UInt8
    index_blk_elmts::UInt8
    data_blk_min_elmts::UInt8
    secondary_blk_min_data_ptrs::UInt8
    max_dblk_page_nelmts_bits::UInt8
    num_secondary_blks::UInt64
    secondary_blk_size::UInt64
    num_data_blks::UInt64
    data_blk_size::UInt64
    max_index_set::UInt64
    nelmts::UInt64
    index_blk_addr::RelOffset
end
define_packed(ExtensibleArrayHeader)

"""Read chunks using Extensible Array indexing (type 4, one unlimited dimension)."""
function read_extensible_array_chunks(f::JLDFile, v::Array, dataspace, rr,
                                       layout::DataLayout, filters,
                                       header_offset, ndims::Int)
    seek(f.io, layout.data_offset)
    jlread(f.io, UInt32) == EXTENSIBLE_ARRAY_HEADER_SIGNATURE ||
        throw(InvalidDataException("Invalid Extensible Array header signature"))

    hdr = jlread(f.io, ExtensibleArrayHeader)
    hdr.version == 0 || throw(UnsupportedVersionException("Unsupported version $(hdr.version)"))
    hdr.index_blk_addr.offset == typemax(UInt64) && return v

    read_extensible_array_index_block!(f, v, dataspace, rr, layout, filters,
                                        hdr.index_blk_addr, hdr.element_size,
                                        hdr.index_blk_elmts, hdr.num_data_blks, ndims)
    return v
end

"""
    read_extensible_array_index_block!(f, v, dataspace, rr, layout, filters,
                                        index_blk_addr, element_size,
                                        index_blk_elmts, num_data_blks, ndims)

Read the Extensible Array Index Block and navigate to chunk records.
"""
function read_extensible_array_index_block!(f::JLDFile, v::Array, dataspace, rr,
                                             layout::DataLayout, filters,
                                             index_blk_addr::RelOffset, element_size::UInt8,
                                             index_blk_elmts::UInt8, num_data_blks::UInt64,
                                             ndims::Int)

    offset = fileoffset(f, index_blk_addr)
    seek(f.io, offset)

    # Read index block header
    sig = jlread(f.io, UInt32)
    sig == htol(0x42494145) || throw(InvalidDataException("Invalid Extensible Array Index Block signature"))  # "EAIB"

    version = jlread(f.io, UInt8)
    version == 0 || throw(UnsupportedVersionException("Unsupported Index Block version $version"))

    client_id = jlread(f.io, UInt8)
    header_addr = jlread(f.io, RelOffset)

    # Read elements stored directly in index block
    num_direct_elements = Int(index_blk_elmts)

    # Get array dimensions for chunk navigation
    array_dims_julia = size(v)
    chunk_dims_hdf5 = layout.chunk_dimensions[1:ndims]
    chunk_dims_julia = Tuple(Int.(reverse(chunk_dims_hdf5)))

    # Calculate grid dimensions for indexing
    grid_dims = cld.(array_dims_julia, chunk_dims_julia)

    # First, read all chunk addresses/records from direct elements
    chunk_records = Vector{Tuple{RelOffset, UInt64, UInt32}}(undef, num_direct_elements)
    for i in 1:num_direct_elements
        if isempty(filters.filters)
            chunk_addr = jlread(f.io, RelOffset)
            chunk_size = prod(chunk_dims_julia) * sizeof(eltype(v))
            filter_mask = 0x00000000
        else
            chunk_addr = jlread(f.io, RelOffset)
            chunk_size = jlread(f.io, UInt64)
            filter_mask = jlread(f.io, UInt32)
        end

        chunk_records[i] = (chunk_addr, chunk_size, filter_mask)
    end

    # Read data block addresses from index block (immediately after direct elements)
    data_block_addrs = Vector{RelOffset}(undef, num_data_blks)
    for i in 1:num_data_blks

        data_block_addrs[i] = jlread(f.io, RelOffset)
    end

    # Now read actual chunk data from direct elements
    for i in 1:num_direct_elements
        chunk_addr, chunk_size, filter_mask = chunk_records[i]

        # Skip undefined chunks
        if isnothing(chunk_addr) || chunk_addr.offset == typemax(UInt64)
            continue
        end

        # Calculate which chunk this is (linear index to multi-dimensional coordinates)
        chunk_idx = i - 1  # 0-based

        # Convert linear chunk index to chunk coordinates
        chunk_grid_idx = linear_index_to_chunk_coords(chunk_idx, grid_dims)

        # Compute chunk starting position
        chunk_start = chunk_start_from_index(chunk_grid_idx, chunk_dims_julia)

        # Read chunk into array (using common function)
        read_and_assign_chunk!(f, v, chunk_start, chunk_addr, Int(chunk_size),
                              chunk_dims_julia, rr, filters, filter_mask)
    end

    # Read chunks from each data block
    chunk_idx = num_direct_elements  # Continue from where direct elements left off
    for data_block_addr in data_block_addrs
        if !isnothing(data_block_addr) && data_block_addr.offset != typemax(UInt64)
            chunk_idx = read_extensible_array_data_block!(f, v, dataspace, rr, layout,
                                                          filters, data_block_addr,
                                                          chunk_idx, grid_dims,
                                                          chunk_dims_julia)
        end
    end

    return nothing
end

"""
    read_extensible_array_data_block!(f, v, dataspace, rr, layout, filters,
                                       data_block_addr, chunk_idx_start,
                                       grid_dims, chunk_dims_julia)

Read an Extensible Array Data Block and its chunk records.
Returns the next chunk_idx after reading this block.
"""
function read_extensible_array_data_block!(f::JLDFile, v::Array, dataspace, rr,
                                           layout::DataLayout, filters,
                                           data_block_addr::RelOffset, chunk_idx_start::Int,
                                           grid_dims::NTuple{N,Int},
                                           chunk_dims_julia::NTuple{N,Int}) where N
    seek(f.io, fileoffset(f, data_block_addr))

    # Read data block header
    sig = jlread(f.io, UInt32)
    sig == htol(0x42444145) || throw(InvalidDataException("Invalid Extensible Array Data Block signature"))  # "EADB"

    version = jlread(f.io, UInt8)
    version == 0 || throw(UnsupportedVersionException("Unsupported Data Block version $version"))

    client_id = jlread(f.io, UInt8)
    header_addr = jlread(f.io, RelOffset)

    # Block offset (variable size based on max_nelmts_bits)
    # Size in bytes = ceil(max_nelmts_bits / 8)
    ea_info = layout.chunk_indexing_info::ExtensibleArrayInfo
    block_offset_size = cld(Int(ea_info.max_bits), 8)
    block_offset = if block_offset_size == 1
        UInt64(jlread(f.io, UInt8))
    elseif block_offset_size == 2
        UInt64(jlread(f.io, UInt16))
    elseif block_offset_size == 4
        UInt64(jlread(f.io, UInt32))
    else
        jlread(f.io, UInt64)
    end
    # Calculate total number of chunks needed
    total_chunks = prod(grid_dims)
    remaining_chunks = Int(total_chunks) - chunk_idx_start

    # Read all remaining chunks (not limited to min_elements)
    max_elements = remaining_chunks
    # Read chunk records until we hit EOF or max_elements
    elements_read = 0
    for i in 1:max_elements
        chunk_idx = chunk_idx_start + i - 1
        # Try to read chunk record - break on EOF
        try
            if isempty(filters.filters)
                chunk_addr = jlread(f.io, RelOffset)
                chunk_size = prod(chunk_dims_julia) * sizeof(eltype(v))
                filter_mask = 0x00000000
            else
                chunk_addr = jlread(f.io, RelOffset)
                chunk_size = jlread(f.io, UInt64)
                filter_mask = jlread(f.io, UInt32)
            end
            # Skip undefined chunks
            if isnothing(chunk_addr) || chunk_addr.offset == typemax(UInt64)
                elements_read += 1
                continue
            end

            # Convert linear chunk index to chunk coordinates
            chunk_grid_idx = linear_index_to_chunk_coords(chunk_idx, grid_dims)

            # Compute chunk starting position
            chunk_start = chunk_start_from_index(chunk_grid_idx, chunk_dims_julia)

            # Save position before reading chunk data
            saved_pos = position(f.io)

            # Read chunk into array (using common function)
            read_and_assign_chunk!(f, v, chunk_start, chunk_addr, Int(chunk_size),
                                  chunk_dims_julia, rr, filters, filter_mask)

            # Restore position to continue reading data block addresses
            seek(f.io, saved_pos)
            elements_read += 1
        catch e
            if e isa EOFError
                break
            else
                rethrow()
            end
        end
    end

    return chunk_idx_start + elements_read
end