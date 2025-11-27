# V2 B-tree Writing Functions
# Functions for writing V2 B-tree chunk index structures (type 5)

"""
    write_v2btree_leaf_node(io::IO, chunk_records, is_filtered::Bool) -> leaf_node_size

Write a V2 B-tree leaf node to the given IO stream.
"""
function write_v2btree_leaf_node(io::IO, chunk_records, is_filtered::Bool)
    ndims = length(chunk_records[1].idx)
    record_size = 8 + 8*ndims + 12*is_filtered

    leaf_node_size = 4 + 1 + 1 + length(chunk_records) * record_size + 4
    leaf_cio = begin_checksum_write(io, leaf_node_size - 4)
    jlwrite(leaf_cio, htol(0x464C5442)) # Signature "BTLF"
    jlwrite(leaf_cio, UInt8(0))  # version
    jlwrite(leaf_cio, UInt8(10))  # type = chunked dataset storage

    for record in chunk_records
        jlwrite(leaf_cio, record.offset)
        if is_filtered
            jlwrite(leaf_cio, record.chunk_size)
            jlwrite(leaf_cio, record.filter_mask)
        end
        # TODO: Is this meaningful ?? Could be wrong
        chunk_indices = collect(UInt64.(reverse(Tuple(record.idx) .- 1)))
        for idx in chunk_indices
            jlwrite(leaf_cio, idx)
        end
    end

    jlwrite(io, end_checksum(leaf_cio))
    return leaf_node_size
end

"""
    write_v2btree_chunked_dataset(f::JLDFile, chunk_records, is_filtered::Bool,
                                  node_size::UInt32=UInt32(2048),
                                  split_percent::UInt8=UInt8(100),
                                  merge_percent::UInt8=UInt8(40)) -> header_offset

Write a complete V2 B-tree chunk index structure (depth=0 only).
"""
function write_v2btree_chunked_dataset(f::JLDFile,
                                       chunk_records,
                                       is_filtered::Bool;
                                       node_size::UInt32=UInt32(2048),
                                       split_percent::UInt8=UInt8(100),
                                       merge_percent::UInt8=UInt8(40))
    n_chunks = length(chunk_records)

    # Calculate record size from first record
    ndims = length(chunk_records[1].idx)
    record_size = record_size = 8 + 8*ndims + 12*is_filtered

    # Validate chunk count fits in single leaf node (simplified implementation, depth=0)
    max_records_per_node = (node_size - 10) ÷ record_size

    if n_chunks > max_records_per_node
        throw(UnsupportedFeatureException(
            "V2 B-tree with >$max_records_per_node chunks requires internal nodes (depth>0), " *
            "which is not yet implemented. Current chunk count: $n_chunks. " *
            "Consider using larger chunks to reduce the number of chunks."
        ))
    end

    # Step 1: Write Leaf Node
    leaf_node_offset = f.end_of_data
    seek(f.io, leaf_node_offset)
    leaf_node_size = write_v2btree_leaf_node(f.io, chunk_records, is_filtered)
    f.end_of_data = leaf_node_offset + leaf_node_size

    # Step 2: Write B-tree Header
    hdr = V2BTreeHeader(
        UInt8(10),        # type = chunked dataset storage
        node_size,
        record_size,
        UInt16(0),        # depth (single leaf node)
        split_percent,
        merge_percent,
        h5offset(f, leaf_node_offset),
        UInt16(n_chunks), # num_records
        UInt64(n_chunks), # total_records
    )

    header_pos = f.end_of_data
    seek(f.io, header_pos)

    cio = begin_checksum_write(f.io, jlsizeof(V2BTreeHeader))
    jlwrite(cio, hdr)
    jlwrite(f.io, end_checksum(cio))
    f.end_of_data = header_pos + jlsizeof(V2BTreeHeader) + 4

    return h5offset(f, header_pos)
end
