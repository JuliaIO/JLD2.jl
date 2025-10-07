# V1 B-tree Writing Functions
# Functions for writing B-tree nodes and chunks
"""
    write_chunk_key(io::IO, key::V1ChunkKey)

Write a V1 chunk key to the given IO stream.
"""
function write_chunk_key(io::IO, key::V1ChunkKey)
    jlwrite(io, key.chunk_size)
    jlwrite(io, key.filter_mask)
    for index in key.indices
        jlwrite(io, index)
    end
end

"""
    write_v1btree_node(f::JLDFile, node::V1BTreeNode)::RelOffset

Write a V1 B-tree node to file and return its offset.
Allocates space at the end of the file for the node.
"""
function write_v1btree_node(f::JLDFile, node::V1BTreeNode)::RelOffset
    io = f.io
    offset = h5offset(f, f.end_of_data)
    seek(io, f.end_of_data)

    # 1. Write signature
    jlwrite(io, V1_BTREE_NODE_SIGNATURE)

    # 2. Write header
    jlwrite(io, node.node_type)
    jlwrite(io, node.node_level)
    jlwrite(io, node.entries_used)
    jlwrite(io, node.left_sibling)
    jlwrite(io, node.right_sibling)

    # 3. Write interleaved keys and children
    for i in 1:node.entries_used
        write_chunk_key(io, node.keys[i])
        jlwrite(io, node.children[i])
    end

    # 4. Write final key
    write_chunk_key(io, node.keys[node.entries_used + 1])

    f.end_of_data = position(io)
    return offset
end

"""
    update_sibling_pointer(f::JLDFile, node_offset::RelOffset, side::Symbol, new_offset::RelOffset)

Update the left or right sibling pointer of a node at the given offset.
Used during node splits to maintain doubly-linked sibling lists.
"""
function update_sibling_pointer(f::JLDFile, node_offset::RelOffset, side::Symbol, new_offset::RelOffset)
    io = f.io

    # Seek to sibling pointer location in node header
    sibling_offset_pos = fileoffset(f, node_offset) + 4 + 1 + 1 + 2  # Skip signature, type, level, entries_used

    if side == :left
        # Update left sibling pointer
        seek(io, sibling_offset_pos)
        jlwrite(io, new_offset)
    elseif side == :right
        # Update right sibling pointer (comes after left sibling)
        seek(io, sibling_offset_pos + jlsizeof(RelOffset))
        jlwrite(io, new_offset)
    else
        throw(ArgumentError("Side must be :left or :right"))
    end
end

"""
    insert_chunk!(btree::V1BTree, chunk_indices::Vector{Int}, chunk_address::RelOffset,
                 chunk_size::UInt32, filter_mask::UInt32 = 0x00000000)

Insert a chunk into the V1 B-tree, handling node splits and tree growth as needed.
This is the main entry point for adding chunks to the tree.

Chunks are accumulated in the btree's pending_chunks field and written out
when finalize_btree! is called.
"""
function insert_chunk!(btree::V1BTree, chunk_indices, chunk_address::RelOffset,
                      chunk_size::UInt32, filter_mask::UInt32 = 0x00000000)

    indices = UInt64[reverse(chunk_indices .- 1)..., 0]
    key =  V1ChunkKey(chunk_size, filter_mask, indices)
    # Store the chunk for later processing when finalize_btree! is called
    push!(btree.pending_chunks, (key, chunk_address))
end

"""
    finalize_btree!(btree::V1BTree)

Finalize the B-tree by writing all pending chunks. If there are more chunks
than can fit in a single node, creates multiple nodes with proper B-tree structure.
"""
function finalize_btree!(btree::V1BTree, boundary_index)
    if isempty(btree.pending_chunks)
        # No chunks to write
        btree.root = UNDEFINED_ADDRESS
        return
    end
    # Sort chunks by key for proper B-tree ordering
    sort!(btree.pending_chunks, by=chunk -> chunk[1])

    keys = V1ChunkKey[]
    children = RelOffset[]

    for (key, address) in btree.pending_chunks
        push!(keys, key)
        push!(children, address)
    end
    push!(keys, V1ChunkKey(UInt32(0), UInt32(0), boundary_index))
    # Clear pending chunks after writing
    empty!(btree.pending_chunks)

    btree.root = build_btree_from_chunks(btree.file, keys, children, btree.max_entries_per_node)
    nothing
end


"""
    build_btree_from_chunks(file::JLDFile, keys::Vector{V1ChunkKey}, children::Vector{RelOffset}, max_entries::UInt16)

Build a B-tree from a large number of chunks that need to be split across multiple nodes.
Returns the root node offset.
"""
function build_btree_from_chunks(file::JLDFile,
    keys::Vector{V1ChunkKey},
    children::Vector{RelOffset},
    max_entries::UInt16
    )

    node_level = 0
    while true
        keys, children = split_into_nodes(file, keys, children, max_entries, node_level)
        node_level += 1
        length(children) == 1 && break
    end
    return children[1]  # Root node
end

"""
    split_into_nodes(file::JLDFile, keys::Vector{V1ChunkKey}, children::Vector{RelOffset}, max_entries::UInt16)

Split chunks into multiple nodes, each containing at most max_entries chunks.
Returns a vector of node offsets.
"""
function split_into_nodes(file::JLDFile,
    keys::Vector{V1ChunkKey},
    children::Vector{RelOffset},
    max_entries::UInt16,
    node_level=0)

    offsets = RelOffset[]
    nkeys = V1ChunkKey[]
    # Split into groups of max_entries
    for i in 1:max_entries:length(children)
        end_idx = min(i + max_entries - 1, length(children))

        # Extract keys and children for this leaf
        leaf_keys = keys[i:(end_idx+1)]     # +1 because we need one extra key
        leaf_children = children[i:end_idx]

        # Create leaf node
        node = V1BTreeNode(
            UInt8(1),        # node_type (chunked datasets)
            UInt8(node_level),        # node_level (leaf)
            UInt16(length(leaf_children)), # entries_used
            UNDEFINED_ADDRESS, UNDEFINED_ADDRESS,  # no siblings for now
            leaf_keys, leaf_children
        )

        offset = write_v1btree_node(file, node)
        push!(offsets, offset)
        push!(nkeys, leaf_keys[1])
    end
    push!(nkeys, keys[end])
    return nkeys, offsets
end

"""
    extract_chunk(data, chunk_indices::Vector{Int}, chunk_dims::Vector{Int})

Extract a chunk of data at the given chunk starting positions with the specified chunk dimensions.
Returns the chunk data as a contiguous array.
"""
function extract_chunk(data, chunk_indices, chunk_dims)
    ndims(data) == length(chunk_indices) || throw(ArgumentError("chunk_indices dimensionality must match data"))

    chunk = typeof(data)(undef, chunk_dims...)
    avail_indices = range.(chunk_indices, min.(size(data), chunk_indices .+ chunk_dims .-1))
    indexview = (:).(1, length.(avail_indices))

    chunk[indexview...] .= @view data[avail_indices...]

    chunk
end

"""
    write_chunked_dataset_with_v1btree(f::JLDFile, data, odr, local_filters, wsession::JLDWriteSession,
                                           chunk_dims::Vector{Int})

Write a chunked dataset using V1 B-tree indexing.
This is the main integration function that replaces simple compression with proper chunking.
"""
function write_chunked_dataset_with_v1btree(f::JLDFile, data, odr, local_filters, wsession::JLDWriteSession,
                                           chunk_dims::Vector{Int})
    dimensionality = UInt8(length(chunk_dims))
    data_size = size(data)

    # Create V1 B-tree for chunk indexing
    max_entries = calculate_max_entries(f, dimensionality)
    btree = V1BTree(UNDEFINED_ADDRESS, dimensionality, max_entries, f)

    total_chunk_size = 0
    num_chunks = 0

    # Write chunks and populate B-tree
    chunk_indices = Iterators.product(StepRange.(1, chunk_dims, data_size)...)
    chunk_count = 0
    for chunk_idx in chunk_indices
        chunk_count += 1
        chunk_data = extract_chunk(data, chunk_idx, chunk_dims)
        compressed_chunk, filter_mask = Filters.compress(local_filters, chunk_data, odr, f, wsession)
        # Write chunk data to file
        chunk_address = h5offset(f, f.end_of_data)
        seek(f.io, f.end_of_data)

        jlwrite(f.io, compressed_chunk)
        f.end_of_data += length(compressed_chunk)

        # Insert chunk into B-tree
        insert_chunk!(btree, chunk_idx, chunk_address, UInt32(length(compressed_chunk)), filter_mask)

        total_chunk_size += length(compressed_chunk)
        num_chunks += 1
    end
    boundary_index = data_size .+ chunk_dims .- mod1.(data_size, chunk_dims)
    finalize_btree!(btree, UInt64[reverse(boundary_index)..., odr_sizeof(odr)])
    return btree, total_chunk_size, num_chunks
end
