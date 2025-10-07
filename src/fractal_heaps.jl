const FRACTAL_HEAP_HEADER_SIGNATURE = htol(0x50485246) # UInt8['F','R','H','P']
const FRACTAL_HEAP_INDIRECT_BLOCK_SIGNATURE = htol(0x42494846) # UInt8['F','H','I','B']
const FRACTAL_HEAP_DIRECT_BLOCK_SIGNATURE = htol(0x42444846) # UInt8['F', 'H', 'D', 'B']

struct FractalHeapHeader
    offset::RelOffset
    table_width::Int
    starting_block_size::Int
    max_direct_block_size::Int
    max_heap_size::Int
    root_block_address::RelOffset
    cur_num_rows_in_root_iblock::Int
    has_io_filter::Bool
    max_dblock_rows::Int
    max_size_managed_objects::Int
    # could add the rest of the fields if they ever become necessary
end

struct FractalHeapDirectBlock
    offset::RelOffset # position of block in file
    # block offset in heaps address space
    # WARNING: don't use. sometimes wrong in long files
    block_offset::UInt64
    size::UInt64
    filtered_size::UInt64 # set to typemax if not filtered
    filter_mask::UInt32  # set to typemax if not filtered
end

struct FractalHeapIndirectBlock
    offset::RelOffset # position of iblock in file
    block_offset::UInt64 # block offset in heaps address space
    dblocks::Vector{FractalHeapDirectBlock}
    iblocks::Vector{FractalHeapIndirectBlock}
end

function blocksize(blocknum, starting_size, table_width)
    #block numbering starts at zero
    rownum = Int(blocknum ÷ table_width)
    (2^(max(0,rownum-1))) * starting_size
end

function block_num_size_start(offset, hh)
    width = hh.table_width
    # first compute row number
    r = Int(offset ÷ (hh.starting_block_size*width))
    r > 2 && (r = ceil(Int, log2(r+1)))
    # row start offset
    row_startoffset = (r>1 ? 2^(r-1) : r)*hh.starting_block_size*width
    block_size = (2^(max(0,r-1))) * hh.starting_block_size
    block_num = width*r + (offset-row_startoffset) ÷ block_size
    block_start = row_startoffset + block_size*(block_num-width*r)
    block_num, block_size, block_start
end


function read_fractal_heap_header(f, offset)
    io = f.io
    seek(io, fileoffset(f, offset)) # may need to compute fileoffset
    cio = begin_checksum_read(io)

    signature = jlread(cio, UInt32)
    signature == FRACTAL_HEAP_HEADER_SIGNATURE || throw(InvalidDataException("Signature does not match."))

    version = jlread(cio, UInt8)
    heap_id_length = jlread(cio, UInt16)
    io_filter_encoded_length = jlread(cio, UInt16)
    flags = jlread(cio, UInt8)
    max_size_managed_objects = jlread(cio, UInt32)
    next_huge_object_id = jlread(cio, Length)
    huge_object_v2btree_address = jlread(cio, RelOffset)
    free_space_in_managed_blocks = jlread(cio, Length)
    managed_block_free_space_manager = jlread(cio, RelOffset)
    managed_space_in_heap = jlread(cio, Length)
    allocated_space_in_heap = jlread(cio, Length)
    direct_block_allocation_iterator_offset = jlread(cio, Length)
    managed_objects_number_in_heap = jlread(cio, Length)
    huge_objects_size_in_heap = jlread(cio, Length)
    huge_objects_number_in_heap = jlread(cio, Length)
    tiny_objects_size_in_heap = jlread(cio, Length)
    tiny_objects_number_in_heap = jlread(cio, Length)

    table_width = jlread(cio, UInt16)
    starting_block_size = jlread(cio, Length)
    max_direct_block_size = jlread(cio, Length)
    max_heap_size = jlread(cio, UInt16)
    num_starting_rows_in_root_iblock = jlread(cio, UInt16)
    root_block_address = jlread(cio, RelOffset)
    cur_num_rows_in_root_iblock = jlread(cio, UInt16)

    has_io_filter = io_filter_encoded_length > 0
    if has_io_filter
        filtered_root_direct_block_size = jlread(cio, Length)
        io_filter_mask = jlread(cio, UInt32)
        io_filter_information = jlread(cio, UInt8, io_filter_encoded_length)
    else
        filtered_root_direct_block_size = typemax(Length)
        io_filter_mask = typemax(UInt32)
        io_filter_information = UInt8[]
    end

    # Checksum
    end_checksum(cio) == jlread(io, UInt32) || throw(InvalidDataException("Invalid Checksum"))

    max_dblock_rows = (log2(max_direct_block_size) - log2(starting_block_size))+2 |> Int

    FractalHeapHeader(offset, table_width, starting_block_size, max_direct_block_size, max_heap_size,
        root_block_address, cur_num_rows_in_root_iblock, has_io_filter, max_dblock_rows,
        max_size_managed_objects)
end

function read_indirect_block(f, offset, hh, nrows::Int)
    io = f.io
    seek(io, fileoffset(f, offset))
    cio = begin_checksum_read(io)

    signature = jlread(cio, UInt32)
    signature == FRACTAL_HEAP_INDIRECT_BLOCK_SIGNATURE || throw(InvalidDataException("Signature does not match."))

    version = jlread(cio, UInt8)
    heap_header_address = jlread(cio, RelOffset)
    # number of bytes for block offset
    offset_byte_num = ceil(Int, hh.max_heap_size / 8)
    block_offset = read_nb_uint(cio, offset_byte_num)

    # Read child direct blocks
    block_start = block_offset
    K = min(nrows, hh.max_dblock_rows)*hh.table_width
    dblocks = map(1:K) do k
        dblock_address = jlread(cio, RelOffset)
        dblock_size = blocksize(k-1, hh.starting_block_size, hh.table_width)
        if hh.has_io_filter > 0
            filtered_size = jlread(cio, Length)
            filter_mask = jlread(cio, UInt32)
        else
            filtered_size = typemax(Length)
            filter_mask = typemax(UInt32)
        end
        dblock = FractalHeapDirectBlock(dblock_address, block_start, dblock_size, filtered_size, filter_mask)
        block_start += dblock_size
        return dblock
    end
    N = (nrows <= hh.max_dblock_rows) ? 0 :  (nrows-hh.max_dblock_rows)*hh.table_width
    iblock_addresses = map(1:N) do n
        jlread(cio, RelOffset)
    end

    # Checksum
    end_checksum(cio) == jlread(io, UInt32) || throw(InvalidDataException())

    iblocks = Vector{FractalHeapIndirectBlock}(undef, N)
    for n=1:N
        iblock_offset = iblock_addresses[n]
        iblock_offset == UNDEFINED_ADDRESS && break
        # figure out iblock size / nrows
        block_num = K+(n-1)
        rownum = block_num ÷ hh.table_width
        block_size = (2^(max(0,rownum-1))) * hh.starting_block_size
        sub_iblock_nrows::Int = (log2(block_size)-log2(hh.starting_block_size* hh.table_width))+1
        iblocks[n] = read_indirect_block(f, iblock_offset , hh, sub_iblock_nrows)
    end
    FractalHeapIndirectBlock(offset, block_offset, dblocks, iblocks)
end



function get_block_offset(f, iblock, roffset, hh::FractalHeapHeader)
    block_num, block_size, block_start = block_num_size_start(roffset, hh)
    K = length(iblock.dblocks)
    if block_num < K
        dblock = iblock.dblocks[block_num+1]
        return dblock.offset + roffset - block_start
    end
    sub_iblock =  iblock.iblocks[block_num-K+1]
    get_block_offset(f, sub_iblock, roffset-block_start, hh)
end


"""
    read_btree(f, offset_hh, offset_bh)

Read a complete V2 B-tree for indexed groups with fractal heap storage.
Returns a vector of tuples: (link_name::String, target::RelOffset)
"""
function read_fractal_heap_group(f, offset_hh, offset_bh)
    hh = read_fractal_heap_header(f, offset_hh)
    bh = read_v2btree_header(f, offset_bh)

    # Create a record reader closure that captures the fractal heap header
    record_reader = (io, type) -> BTrees.read_record_type5(io, type, hh)

    # Use the generic B-tree reading function with our specific record reader
    records = BTrees.read_records_in_node(f, bh.root_node_address, bh.num_records_in_root_node, bh.depth, bh, record_reader)

    if hh.cur_num_rows_in_root_iblock > 0
        indirect_rb = read_indirect_block(f, hh.root_block_address, hh, hh.cur_num_rows_in_root_iblock)
        links = map(records) do r
            offset = get_block_offset(f, indirect_rb, r.offset, hh)
            m = HmWrap(HmLinkMessage, Message(HmLinkMessage, f, offset))
            m.link_name, m.target
        end
    else # there's only a single direct block at hh.root_block_address
        links = map(records) do r
            offset = hh.root_block_address + r.offset
            m = HmWrap(HmLinkMessage, Message(HmLinkMessage, f, offset))
            m.link_name, m.target
        end
    end
    links::Vector{Tuple{String, RelOffset}}
end


###########################################################################################
##                        Old Style Group: V1 B-Tree & Name Index Heap                   ##
###########################################################################################

function read_oldstyle_group(f, v1btree_address, name_index_heap)
    local_heap = read_local_heap_header(f, name_index_heap)
    links = read_v1btree(f, v1btree_address)
    map(links) do link
        link_name = read_in_local_heap(f, local_heap, link.link_name_offset)
        (link_name, link.obj_header_address::RelOffset)
    end::Vector{Tuple{String, RelOffset}}
end

const LOCAL_HEAP_SIGNATURE = htol(0x50414548) # UInt8['H', 'E', 'A', 'P']
function read_local_heap_header(f, offset)
    io = f.io
    seek(io, fileoffset(f, offset))

    signature = jlread(io, UInt32)
    signature == LOCAL_HEAP_SIGNATURE || throw(InvalidDataException("Signature does not match."))

    version = jlread(io, UInt8)
    version == 0 || throw(UnsupportedVersionException("Local heap with version $version detected."))
    skip(io, 3)
    data_segment_size = jlread(io, Length)

    # This field is important for computing where to add to the heap. Let's ignore that
    offset_head_free_list = jlread(io, Length)
    data_segment_offset = jlread(io, RelOffset)
    (; offset=data_segment_offset, size=data_segment_size)
end

function read_in_local_heap(f, local_heap, pos)
    io = f.io
    offset = local_heap.offset + pos
    seek(io, fileoffset(f, offset))
    return read_bytestring(io)
end

# V1 B-tree functions for groups moved to BTrees module
# See src/btrees/v1btree_groups.jl
