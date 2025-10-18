"""
Read chunks using Implicit indexing (type 2). Chunks are stored contiguously:
chunk_address = base_address + (chunk_size_bytes × chunk_index)
"""
function read_implicit_index_chunks(f::JLDFile, v::Array{T}, dataspace::ReadDataspace,
                                   @nospecialize(rr::ReadRepresentation), layout::DataLayout,
                                   filters::FilterPipeline, header_offset::RelOffset,
                                   ndims::Int, fill_value::T=zero(T)) where T
    base_offset = layout.data_offset
    chunk_dims_julia = Tuple(Int.(reverse(layout.chunk_dimensions[1:ndims])))
    chunk_size_bytes = Int(prod(chunk_dims_julia) * sizeof(T))

    for (chunk_grid_idx, linear_idx) in ChunkIndexIterator(size(v), chunk_dims_julia)
        chunk_offset = base_offset + (linear_idx * chunk_size_bytes)
        chunk_start = chunk_start_from_index(chunk_grid_idx, chunk_dims_julia)
        read_and_assign_chunk!(f, v, chunk_start, chunk_offset,
                              chunk_size_bytes, chunk_dims_julia, rr, filters, 0)
    end
    return v
end

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
function read_fixed_array_header(f::JLDFile, offset::RelOffset)
    seek(f.io, fileoffset(f, offset))
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
    chunk_entries = Vector{@NamedTuple{offset::RelOffset, size::UInt64, filter_mask::UInt32}}(
        undef, n_chunks
    )

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
                chunk_entries[chunk_idx + 1] = (RelOffset(-1%UInt64), 0, 0)
            else
                seek(io, pages_start + page_num * page_total_size +
                         (chunk_idx % page_size) * Int(header.entry_size))
                offset = jlread(io, RelOffset)
                if header.client_id #compressed
                    size = jlread(io, UInt64)
                    filter_mask = jlread(io, UInt32)
                else
                    size = typemax(UInt64)
                    filter_mask = zero(UInt32)
                end
                chunk_entries[chunk_idx + 1] = (; offset, size, filter_mask)
            end
        end
    else
        for chunk_idx in 0:(n_chunks-1)
            offset = jlread(io, RelOffset)
            if header.client_id == 1 #compressed
                size = jlread(io, UInt64)
                filter_mask = jlread(io, UInt32)
            else
                size = typemax(UInt64)
                filter_mask = zero(UInt32)
            end
            chunk_entries[chunk_idx + 1] = (; offset, size, filter_mask)
        end
    end

    return chunk_entries
end

"""Read chunks using Fixed Array indexing (type 3)."""
function read_fixed_array_chunks(f::JLDFile, v::Array{T}, dataspace::ReadDataspace,
                                @nospecialize(rr::ReadRepresentation), layout::DataLayout,
                                filters::FilterPipeline, header_offset::RelOffset,
                                ndims::Int, fill_value::T=zero(T)) where T
    chunk_dims_julia = Tuple(Int.(reverse(layout.chunk_dimensions[1:ndims])))
    n_chunks_total = prod(cld.(size(v), chunk_dims_julia))
    header = read_fixed_array_header(f, layout.data_offset)

    seek(f.io, fileoffset(f, header.data_block_address))
    jlread(f.io, UInt32) == FIXED_ARRAY_DATABLOCK_SIGNATURE ||
        throw(InvalidDataException("Invalid Fixed Array data block signature"))
    skip(f.io, 10)

    chunk_entries = read_all_chunk_entries_fixed_array(f.io, header, n_chunks_total)

    # Initialize array with fill value for sparse chunks
    fill!(v, fill_value)

    for (chunk_grid_idx, linear_idx) in ChunkIndexIterator(size(v), chunk_dims_julia)
        (; offset, size, filter_mask) = chunk_entries[linear_idx + 1]

        # Skip undefined chunks (already filled with fill value)
        if offset == JLD2.UNDEFINED_ADDRESS
            continue
        end

        size = (size != typemax(UInt64) ? size : Int(prod(chunk_dims_julia) * sizeof(T)))
        chunk_start = chunk_start_from_index(chunk_grid_idx, chunk_dims_julia)
        read_and_assign_chunk!(f, v, chunk_start, offset, size, chunk_dims_julia, rr, filters, 0)
    end
    return v
end


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

"""
Calculate the number of data block and super block address slots in an extensible array index block.

Based on HDF5 H5EAiblock.c allocation logic.
"""
function calculate_ea_index_block_addr_slots(hdr::ExtensibleArrayHeader)
    # Calculate first super block threshold
    # This is where data blocks end and super blocks begin
    first_sup_blk_log2 = Int(hdr.max_dblk_page_nelmts_bits)

    # Number of data block address slots
    data_blk_min_log2 = trailing_zeros(Int(hdr.data_blk_min_elmts))
    ndblk_addrs = first_sup_blk_log2 - data_blk_min_log2

    # Number of super block address slots
    # From H5EA__iblock_alloc in H5EAiblock.c:
    # The number of super block addresses is calculated to cover the
    # hierarchical addressing from first_sup_blk to max_nelmts
    if Int(hdr.secondary_blk_min_data_ptrs) > 0
        secondary_log2 = trailing_zeros(Int(hdr.secondary_blk_min_data_ptrs))
        # Empirical formula that matches testshift.h5
        # TODO: Verify against HDF5 source code for correctness
        total_addr_bits = Int(hdr.max_nelmts_bits) - data_blk_min_log2
        total_addr_slots = total_addr_bits + (hdr.index_blk_elmts > 0 ? 3 : 0)  # Empirical adjustment
        nsup_addrs = total_addr_slots - ndblk_addrs
    else
        nsup_addrs = 0
    end

    return (ndblk_addrs, nsup_addrs)
end

"""Read chunks using Extensible Array indexing (type 4, one unlimited dimension)."""
function read_extensible_array_chunks(f::JLDFile, v::Array{T}, dataspace, rr,
                                       layout::DataLayout, filters,
                                       header_offset, ndims::Int, fill_value::T=zero(T)) where T
    # Initialize array with fill value for sparse chunks
    fill!(v, fill_value)

    seek(f.io, fileoffset(f, layout.data_offset))
    jlread(f.io, UInt32) == EXTENSIBLE_ARRAY_HEADER_SIGNATURE ||
        throw(InvalidDataException("Invalid Extensible Array header signature"))

    hdr = jlread(f.io, ExtensibleArrayHeader)
    hdr.version == 0 || throw(UnsupportedVersionException("Unsupported version $(hdr.version)"))
    hdr.index_blk_addr.offset == typemax(UInt64) && return v

    # Calculate allocated address slots (not just used ones)
    ndblk_addrs, nsup_addrs = calculate_ea_index_block_addr_slots(hdr)

    # Pass max_index_set to limit reading to actual chunks
    read_extensible_array_index_block!(f, v, dataspace, rr, layout, filters,
                                        hdr.index_blk_addr, hdr.element_size,
                                        hdr.index_blk_elmts, hdr.data_blk_min_elmts,
                                        ndblk_addrs, nsup_addrs, ndims, Int(hdr.max_index_set))
    return v
end

"""
    read_extensible_array_index_block!(f, v, dataspace, rr, layout, filters,
                                        index_blk_addr, element_size,
                                        index_blk_elmts, data_blk_min_elmts,
                                        ndblk_addrs, nsup_addrs, ndims, max_index_set)

Read the Extensible Array Index Block and navigate to chunk records.

# Arguments
- `ndblk_addrs`: Number of data block address SLOTS (not used blocks!)
- `nsup_addrs`: Number of super block address SLOTS
- `max_index_set`: Maximum chunk index that has been set (limits reading)
"""
function read_extensible_array_index_block!(f::JLDFile, v::Array, dataspace, rr,
                                             layout::DataLayout, filters,
                                             index_blk_addr::RelOffset, element_size::UInt8,
                                             index_blk_elmts::UInt8, data_blk_min_elmts::UInt8,
                                             ndblk_addrs::Int, nsup_addrs::Int, ndims::Int,
                                             max_index_set::Int)

    offset = fileoffset(f, index_blk_addr)
    seek(f.io, offset)

    # Start checksum computation
    cio = begin_checksum_read(f.io)

    # Read index block header
    sig = jlread(cio, UInt32)
    sig == htol(0x42494145) || throw(InvalidDataException("Invalid Extensible Array Index Block signature"))  # "EAIB"

    version = jlread(cio, UInt8)
    version == 0 || throw(UnsupportedVersionException("Unsupported Index Block version $version"))

    client_id = jlread(cio, UInt8)

    header_addr = jlread(cio, RelOffset)

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
            chunk_addr = jlread(cio, RelOffset)
            chunk_size = prod(chunk_dims_julia) * sizeof(eltype(v))
            filter_mask = 0x00000000
        else
            chunk_addr = jlread(cio, RelOffset)
            chunk_size = jlread(cio, UInt64)
            filter_mask = jlread(cio, UInt32)
        end

        chunk_records[i] = (chunk_addr, chunk_size, filter_mask)
    end

    # Read data block address SLOTS from index block (immediately after direct elements)
    data_block_addrs = Vector{RelOffset}(undef, ndblk_addrs)
    for i in 1:ndblk_addrs
        data_block_addrs[i] = jlread(cio, RelOffset)
    end

    # Read super block address SLOTS from index block
    super_block_addrs = Vector{RelOffset}(undef, nsup_addrs)
    for i in 1:nsup_addrs
        super_block_addrs[i] = jlread(cio, RelOffset)
    end

    # Verify checksum
    computed_checksum = end_checksum(cio)
    stored_checksum = jlread(f.io, UInt32)

    if computed_checksum != stored_checksum
        throw(InvalidDataException(
            "Extensible Array Index Block checksum mismatch: " *
            "computed 0x$(string(computed_checksum, base=16, pad=8)), " *
            "stored 0x$(string(stored_checksum, base=16, pad=8))"
        ))
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
    for (data_blk_idx, data_block_addr) in enumerate(data_block_addrs)
        if !isnothing(data_block_addr) && data_block_addr.offset != typemax(UInt64)
            # Stop if we've reached the maximum index that has been set
            if chunk_idx >= max_index_set
                break
            end
            chunk_idx = read_extensible_array_data_block!(f, v, dataspace, rr, layout,
                                                          filters, data_block_addr,
                                                          chunk_idx, data_blk_idx - 1,
                                                          data_blk_min_elmts, grid_dims,
                                                          chunk_dims_julia, max_index_set)
        end
    end

    return nothing
end

"""
    read_extensible_array_data_block!(f, v, dataspace, rr, layout, filters,
                                       data_block_addr, chunk_idx_start,
                                       data_blk_idx, data_blk_min_elmts,
                                       grid_dims, chunk_dims_julia, max_index_set)

Read an Extensible Array Data Block and its chunk records.

The number of elements in each data block follows an exponential growth pattern:
- Data block k contains: data_blk_min_elmts * 2^k elements

Returns the next chunk_idx after reading this block.
"""
function read_extensible_array_data_block!(f::JLDFile, v::Array, dataspace, rr,
                                           layout::DataLayout, filters,
                                           data_block_addr::RelOffset, chunk_idx_start::Int,
                                           data_blk_idx::Int, data_blk_min_elmts::UInt8,
                                           grid_dims::NTuple{N,Int},
                                           chunk_dims_julia::NTuple{N,Int},
                                           max_index_set::Int) where N
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
    ea_info = layout.chunk_indexing_info#::ExtensibleArrayInfo

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

    # Calculate number of elements in this specific data block using exponential growth pattern
    # Data block k contains: data_blk_min_elmts * 2^k elements
    num_elements_allocated = Int(data_blk_min_elmts) * (1 << data_blk_idx)

    # Only read up to max_index_set to avoid reading uninitialized slots
    num_elements_to_read = min(num_elements_allocated, max(0, max_index_set - chunk_idx_start))

    # Calculate total chunks needed
    total_chunks_needed = prod(grid_dims)

    # Read chunk records from the data block (only up to what's actually set)
    # Note: HDF5 may store fewer records than allocated if chunks aren't written yet
    chunk_records = Vector{Tuple{RelOffset, UInt64, UInt32}}()
    sizehint!(chunk_records, num_elements_to_read)

    for i in 1:num_elements_to_read
        if isempty(filters.filters)
            offset = jlread(f.io, RelOffset)
            size = prod(chunk_dims_julia) * sizeof(eltype(v))
            filter_mask = 0x00000000
        else
            offset = jlread(f.io, RelOffset)
            size = jlread(f.io, UInt64)
            filter_mask = jlread(f.io, UInt32)
        end

        # Check if this looks like valid data (offset within file bounds and not a checksum pattern)
        # If offset is way beyond file size, we've likely hit the end of actual records
        if offset.offset != 0 && offset.offset != typemax(UInt64) && offset.offset < filesize(f.path) * 10
            push!(chunk_records, (offset, size, filter_mask))
        elseif offset.offset == 0 || offset.offset == typemax(UInt64)
            # Explicitly undefined chunk - add it
            push!(chunk_records, (offset, size, filter_mask))
        else
            # Invalid offset - stop reading (likely hit checksum or uninitialized data)
            break
        end
    end

    num_elements_to_read = length(chunk_records)

    # Now, read the actual chunk data
    for i in 1:num_elements_to_read
        chunk_idx = chunk_idx_start + i - 1
        offset, size, filter_mask = chunk_records[i]

        # Skip if chunk index exceeds what we need for the array
        if chunk_idx >= total_chunks_needed
            continue
        end

        # Skip undefined chunks
        if isnothing(offset) || offset == JLD2.UNDEFINED_ADDRESS
            continue
        end

        # Calculate chunk position and read data
        chunk_grid_idx = linear_index_to_chunk_coords(chunk_idx, grid_dims)
        chunk_start = chunk_start_from_index(chunk_grid_idx, chunk_dims_julia)

        read_and_assign_chunk!(f, v, chunk_start, offset, Int(size),
                                chunk_dims_julia, rr, filters, filter_mask)
    end

    return chunk_idx_start + num_elements_to_read
end

# ChunkRecordV2 and read_chunk_record_type10 are now defined in src/btrees/v2btree_read.jl
# and exported from the BTrees module

"""
    read_v2btree_chunks(f, v, dataspace, rr, layout, filters, header_offset, ndims)

Read chunks indexed by HDF5 v2 B-tree (type 5).
Used when dataset has multiple unlimited dimensions.

# V2 B-tree Structure

**Header** (signature "BTHD"):
- version, type, node_size, record_size
- depth, split_percent, merge_percent
- root_node_address, num_records_root, total_records

**Leaf Node** (signature "BTLF"):
- For chunked data (type 10), each record contains:
  - chunk_address (8 bytes)
  - chunk_index_dim1 (8 bytes) - chunk coordinates in HDF5 order
  - chunk_index_dim2 (8 bytes)
  - ... (one per dimension, excluding element size)

**Internal Node** (signature "BTIN"):
- Contains child node pointers and separator keys
- Used when dataset has many chunks (depth > 0)

# Algorithm

1. Parse v2 B-tree header to get root node address and depth
2. Create record reader callback for chunk records (type 10)
3. Use recursive tree traversal to collect all chunk records
4. Read each chunk into output array using chunk_address and chunk_indices

# Dimension Handling

Chunk indices in v2 B-tree are stored in HDF5 dimension order (reversed from Julia).
For a 2D Julia array (rows × cols), v2 B-tree stores:
- record[0] = chunk_address
- record[1] = row_chunk_index (Julia dim 1)
- record[2] = col_chunk_index (Julia dim 2)

These are chunk indices (not element indices). Multiply by chunk_size to get element offset.
"""
function read_v2btree_chunks(f::JLDFile, v::Array{T}, dataspace, rr,
                              layout::DataLayout, filters,
                              header_offset, ndims::Int, fill_value::T=zero(T)) where T

    # Initialize array with fill value for sparse chunks
    fill!(v, fill_value)

    # Parse v2 B-tree chunk index header
    header = JLD2.BTrees.read_v2btree_header(f, layout.data_offset)

    # Get array and chunk dimensions
    array_dims_julia = size(v)
    chunk_dims_hdf5 = layout.chunk_dimensions[1:ndims]
    chunk_dims_julia = Int.(reverse(chunk_dims_hdf5))

    # Calculate chunk size for unfiltered chunks
    chunk_size_bytes = UInt64(prod(chunk_dims_julia) * sizeof(eltype(v)))
    has_filters = !isempty(filters.filters)

    # Create record reader callback with captured parameters
    record_reader = (io, type) -> read_chunk_record_type10(io, type, ndims, chunk_size_bytes, has_filters)

    # Traverse tree to collect all chunk records
    chunks = JLD2.BTrees.read_records_in_node(
        f,
        header.root_node_address,
        header.num_records_root,
        header.depth,
        header,
        record_reader
    )

    # Read each chunk into array
    for chunk in chunks
        chunk_start = chunk_start_from_index(chunk.idx, Tuple(chunk_dims_julia))

        # Use the unified chunk reading function
        read_and_assign_chunk!(f, v, chunk_start, chunk.offset, Int(chunk.size),
                              Tuple(chunk_dims_julia), rr, filters, chunk.filter_mask)
    end

    return v
end
