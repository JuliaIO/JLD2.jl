# V2 B-tree Writing Functions
# Functions for writing V2 B-tree chunk index structures (type 5)

"""
    get_chunk_record_ndims(chunk_records, is_filtered::Bool) -> Int

Extract the number of dimensions from chunk records.

# Arguments
- `chunk_records` - Vector of chunk records
- `is_filtered::Bool` - Whether chunks use compression filters

# Returns
Number of spatial dimensions
"""
function get_chunk_record_ndims(chunk_records, is_filtered::Bool)
    first_record = chunk_records[1]
    if is_filtered
        length(first_record[4])  # chunk_indices is 4th element in filtered records
    else
        length(first_record[2])  # chunk_indices is 2nd element in unfiltered records
    end
end

"""
    calculate_v2btree_record_size(ndims::Int, is_filtered::Bool) -> Int

Calculate the size of a V2 B-tree chunk record.

# Arguments
- `ndims::Int` - Number of spatial dimensions
- `is_filtered::Bool` - Whether chunks use compression filters

# Returns
Record size in bytes
"""
function calculate_v2btree_record_size(ndims::Int, is_filtered::Bool)
    if is_filtered
        8 + 4 + 4 + 8 * ndims  # address + size + filter_mask + indices
    else
        8 + 8 * ndims  # address + indices
    end
end

"""
    write_v2btree_leaf_node(io::IO, chunk_records, is_filtered::Bool) -> leaf_node_size

Write a V2 B-tree leaf node to the given IO stream.

# Arguments
- `io::IO` - IO stream to write to
- `chunk_records` - Vector of chunk records (either unfiltered or filtered format)
- `is_filtered::Bool` - Whether chunks use compression filters

# Record Format
Unfiltered: `(address::RelOffset, chunk_indices::Vector{UInt64})`
Filtered: `(address::RelOffset, size::UInt32, filter_mask::UInt32, chunk_indices::Vector{UInt64})`

# Returns
Size of the written leaf node in bytes
"""
function write_v2btree_leaf_node(io::IO, chunk_records, is_filtered::Bool)
    n_chunks = length(chunk_records)

    # Calculate record size from first record
    ndims = get_chunk_record_ndims(chunk_records, is_filtered)
    record_size = calculate_v2btree_record_size(ndims, is_filtered)

    # Calculate leaf node size
    leaf_node_size = 4 +  # signature
                     1 +  # version
                     1 +  # type
                     n_chunks * record_size +  # chunk records
                     4    # checksum

    # Write leaf node with checksum
    leaf_cio = begin_checksum_write(io, leaf_node_size - 4)

    # Signature "BTLF" (0x464C5442)
    jlwrite(leaf_cio, htol(0x464C5442))

    jlwrite(leaf_cio, UInt8(0))  # version
    jlwrite(leaf_cio, UInt8(10))  # type = chunked dataset storage

    # Write chunk records in sorted order
    if is_filtered
        # Filtered format: address, size, filter_mask, indices
        for record in chunk_records
            chunk_addr, chunk_size, filter_mask, chunk_indices = record
            jlwrite(leaf_cio, chunk_addr.offset)  # UInt64
            jlwrite(leaf_cio, chunk_size)          # UInt32
            jlwrite(leaf_cio, filter_mask)         # UInt32
            for idx in chunk_indices
                jlwrite(leaf_cio, idx)              # UInt64
            end
        end
    else
        # Unfiltered format: address, indices
        for record in chunk_records
            chunk_addr, chunk_indices = record
            jlwrite(leaf_cio, chunk_addr.offset)  # UInt64
            for idx in chunk_indices
                jlwrite(leaf_cio, idx)              # UInt64
            end
        end
    end

    # Write checksum
    jlwrite(io, end_checksum(leaf_cio))

    return leaf_node_size
end

"""
    write_v2btree_header(io::IO, header::V2BTreeHeader) -> header_size

Write a V2 B-tree header to the given IO stream.

# Returns
Size of the written header in bytes (including checksum)
"""
function write_v2btree_header(io::IO, header::V2BTreeHeader)
    header_size = jlsizeof(V2BTreeHeader) + 4  # struct + checksum

    cio = begin_checksum_write(io, jlsizeof(V2BTreeHeader))
    jlwrite(cio, header)

    # Write checksum
    jlwrite(io, end_checksum(cio))

    return header_size
end

"""
    write_v2btree_chunked_dataset(f::JLDFile, chunk_records, is_filtered::Bool,
                                  node_size::UInt32=UInt32(2048),
                                  split_percent::UInt8=UInt8(100),
                                  merge_percent::UInt8=UInt8(40)) -> header_offset

Write a complete V2 B-tree chunk index structure (depth=0 only).

# Arguments
- `f::JLDFile` - File handle
- `chunk_records` - Vector of chunk records (from write_all_chunks_as_records)
- `is_filtered::Bool` - Whether chunks use compression filters
- `node_size::UInt32` - Node size in bytes (default: 2048, matching h5py)
- `split_percent::UInt8` - Split threshold percentage (default: 100)
- `merge_percent::UInt8` - Merge threshold percentage (default: 40)

# Structure Written
1. Leaf node with chunk records
2. B-tree header

# Returns
File offset of the B-tree header (for DataLayout message)
"""
function write_v2btree_chunked_dataset(f::JLDFile,
                                       chunk_records,
                                       is_filtered::Bool;
                                       node_size::UInt32=UInt32(2048),
                                       split_percent::UInt8=UInt8(100),
                                       merge_percent::UInt8=UInt8(40))
    n_chunks = length(chunk_records)

    # Calculate record size from first record
    ndims = get_chunk_record_ndims(chunk_records, is_filtered)
    record_size = UInt16(calculate_v2btree_record_size(ndims, is_filtered))

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
        leaf_node_offset,
        UInt16(n_chunks), # num_records
        UInt64(n_chunks), # total_records
    )

    header_offset = f.end_of_data
    seek(f.io, header_offset)
    header_size = write_v2btree_header(f.io, hdr)
    f.end_of_data = header_offset + header_size

    return header_offset
end
