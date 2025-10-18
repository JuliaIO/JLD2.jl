# V2 B-tree Reading Functions
# Generic V2 B-tree reading with callbacks for record-specific logic

"""
    read_v2btree_header(f, offset)

Read a V2 B-tree header from the file.
Returns a V2BTreeHeader structure containing tree metadata.
Checksum is validated but not returned as part of the struct.
"""
function read_v2btree_header(f, offset)
    io = f.io
    seek(io, fileoffset(f, offset))

    # Read header with checksum validation
    cio = begin_checksum_read(io)

    # Read the struct using define_packed (checksum computed over these fields)
    hdr = jlread(cio, V2BTreeHeader)

    # Validate checksum
    computed_checksum = end_checksum(cio)
    stored_checksum = jlread(io, UInt32)
    computed_checksum == stored_checksum || throw(InvalidDataException("V2 B-tree header checksum mismatch"))

    # Validate signature and version
    hdr.signature == V2_BTREE_HEADER_SIGNATURE || throw(InvalidDataException("Invalid V2 B-tree header signature"))
    hdr.version == 0 || throw(UnsupportedVersionException("Unsupported V2 B-tree version $(hdr.version)"))

    return hdr
end

"""
    read_v2btree_node(f, offset, num_records, depth, bh, record_reader)

Read a V2 B-tree node (internal or leaf) from the file.
Uses a callback function `record_reader(io, type)` for reading records.

Arguments:
- `f`: JLDFile
- `offset`: File offset of the node
- `num_records`: Number of records in this node
- `depth`: Depth of this node (0 = leaf)
- `bh`: BTreeHeaderV2 for size calculations
- `record_reader`: Function to read a single record from IO
"""
function read_v2btree_node(f, offset, num_records, depth, bh, record_reader)
    if depth == 0
        return read_v2btree_leaf_node(f, offset, num_records, bh, record_reader)
    end
    io = f.io
    seek(io, fileoffset(f, offset))
    cio = begin_checksum_read(io)

    signature = jlread(cio, UInt32)
    signature == V2_BTREE_INTERNAL_NODE_SIGNATURE || throw(InvalidDataException("Signature does not match."))

    version = jlread(cio, UInt8)
    type = jlread(cio, UInt8)

    records = map(1:num_records) do n
        record_reader(cio, type)
    end

    # determine number of bytes used to encode `num_records`
    # this has to be done iteratively
    # leaf node:
    space = bh.node_size - 4 - 1 - 1 - 4
    max_records = space ÷ bh.record_size
    max_records_total = 0
    numbytes = size_size(max_records)
    numbytes_total = 0

    for d = 1:depth
        space = bh.node_size - 4-1-1-4 - sizeof(RelOffset) - (d>1)*numbytes_total
        max_records = space ÷ (bh.record_size + sizeof(RelOffset) + numbytes+(d>1)*numbytes_total)
        numbytes = size_size(max_records)
        max_records_total = max_records + (max_records+1)*max_records_total
        numbytes_total = size_size(max_records_total)
    end
    numbytes_total = size_size2(max_records_total)
    child_nodes = map(1:num_records+1) do _
        child_node_pointer = jlread(cio, RelOffset)
        num_records = Int(read_nb_uint(cio, numbytes))
        if depth > 1
            total_records = Int(read_nb_uint(cio, numbytes_total))
            return (; child_node_pointer, num_records, total_records)
        end
        (; child_node_pointer, num_records)
    end
    end_checksum(cio) == jlread(io, UInt32) || throw(InvalidDataException())

    BTreeInternalNodeV2(offset, type, records, child_nodes)
end

"""
    read_v2btree_leaf_node(f, offset, num_records, bh, record_reader)

Read a V2 B-tree leaf node from the file.
Uses a callback function `record_reader(io, type)` for reading records.
"""
function read_v2btree_leaf_node(f, offset, num_records, bh, record_reader)
    io = f.io
    seek(io, fileoffset(f, offset))
    cio = begin_checksum_read(io)

    signature = jlread(cio, UInt32)
    signature == V2_BTREE_LEAF_NODE_SIGNATURE || throw(InvalidDataException("Signature does not match."))
    version = jlread(cio, UInt8)
    type = jlread(cio, UInt8)
    records = map(1:num_records) do n
        record_reader(cio, type)
    end

    end_checksum(cio) == jlread(io, UInt32) || throw(InvalidDataException())
    BTreeLeafNodeV2(offset, type, records)
end

"""
    read_records_in_node(f, offset, num_records, depth, bh, record_reader)

Recursively read all records from a V2 B-tree node and its descendants.
Returns a flat vector of all records in the subtree.
Uses a callback function `record_reader(io, type)` for reading records.
"""
function read_records_in_node(f, offset, num_records, depth, bh, record_reader)
    if depth == 0
        return read_v2btree_leaf_node(f, offset, num_records, bh, record_reader).records
    end

    node = read_v2btree_node(f, offset, num_records, depth, bh, record_reader)::BTreeInternalNodeV2

    records = []
    for n=1:num_records+1
        child_offset = node.child_nodes[n].child_node_pointer
        child_records = node.child_nodes[n].num_records
        records_in_child = read_records_in_node(f, child_offset, child_records, depth-1, bh, record_reader)
        append!(records, records_in_child)
        n<=num_records && (push!(records, node.records[n]))
    end
    return records
end

"""
    read_record_type5(io, type, hh)

Read a Type 5 record (link name for indexed group) from a V2 B-tree.
This function is specific to fractal heap storage.
"""
function read_record_type5(io, type, hh)
    if type == 5 # link name for indexed group
        hash_of_name = jlread(io, UInt32)
        # read Heap id for managed object
        version_type = jlread(io, UInt8)

        offbytes = hh.max_heap_size÷8
        offset = Int(read_nb_uint(io, offbytes))
        lnbytes = min(hh.max_direct_block_size, hh.max_size_managed_objects) |> size_size2
        length = Int(read_nb_uint(io, lnbytes))
        skip(io, 6-offbytes-lnbytes)
        return BTreeType5RecordV2(hash_of_name, offset, length)
    else
        throw(error("Not implemented record type"))
    end
end

"""
    read_chunk_record_type10(io, type, ndims, chunk_size_bytes, has_filters)

Read a chunk record (type 10) from a V2 B-tree node for chunked datasets.
Returns a ChunkRecordV2 containing offset, size, filter mask, and chunk index.

# Arguments
- `io`: IO stream positioned at record location
- `type`: B-tree type (should be 10 for chunk records)
- `ndims`: Number of dataset dimensions
- `chunk_size_bytes`: Size of uncompressed chunk in bytes
- `has_filters`: Whether the dataset has filters (affects record format)
"""
function read_chunk_record_type10(io, type, ndims, chunk_size_bytes, has_filters)
    offset = jlread(io, RelOffset)

    if has_filters
        size = jlread(io, UInt64)
        filter_mask = jlread(io, UInt32)
    else
        size = chunk_size_bytes
        filter_mask = UInt32(0)
    end

    # Read chunk indices (in HDF5 order, 0-based) and convert to Julia 1-based
    idx = CartesianIndex(reverse(ntuple(_-> 1 + jlread(io, UInt64), ndims)))

    return ChunkRecordV2(offset, size, filter_mask, idx)
end
