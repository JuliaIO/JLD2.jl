# V1 B-tree Implementation for JLD2
# Implementation of HDF5 V1 B-tree writing for chunked dataset indexing

const V1_BTREE_NODE_SIGNATURE = htol(0x45455254)  # "TREE" - already defined in fractal_heaps.jl

"""
    V1ChunkKey

Key structure for chunked datasets in V1 B-trees.
Each key contains:
- chunk_size: Size of chunk in bytes (UInt32)
- filter_mask: Bitmask indicating which filters were applied (UInt32)
- indices: D+1 dimensional indices where D is dimensionality (Vector{UInt64})
  The last index is always 0 (datatype offset)

Example: For a 3D chunk at position [5,5,5], indices = [5, 5, 5, 0]
"""
struct V1ChunkKey
    chunk_size::UInt32         # Size of chunk in bytes
    filter_mask::UInt32        # Filter bitmask (0 if no filters)
    indices::Vector{UInt64}    # D+1 dimensional indices (last is always 0)

    # Inner constructor for validation
    function V1ChunkKey(chunk_size::UInt32, filter_mask::UInt32, indices::Vector{UInt64})
        length(indices) >= 2 || throw(ArgumentError("indices must have at least 2 elements (dimensions + datatype offset)"))
        indices[end] == 0 || throw(ArgumentError("last index (datatype offset) must be 0"))
        new(chunk_size, filter_mask, indices)
    end
end

"""
    V1BTreeNode

Core node structure for V1 B-trees.
- node_type: 1 for chunked datasets, 0 for groups
- node_level: 0 for leaf, >0 for internal nodes
- entries_used: Number of valid key/child pairs
- left_sibling: Left sibling node address (UNDEFINED_ADDRESS if none)
- right_sibling: Right sibling node address (UNDEFINED_ADDRESS if none)
- keys: Keys (length = entries_used + 1)
- children: Child addresses (length = entries_used)

HDF5 ordering rule for chunked datasets (type 1):
Key[i] describes the least chunk in Child[i]
"""
mutable struct V1BTreeNode
    node_type::UInt8           # 1 for chunked datasets, 0 for groups
    node_level::UInt8          # 0 for leaf, >0 for internal
    entries_used::UInt16       # Number of valid key/child pairs
    left_sibling::RelOffset    # Left sibling node address
    right_sibling::RelOffset   # Right sibling node address
    keys::Vector{V1ChunkKey}   # Keys (length = entries_used + 1)
    children::Vector{RelOffset} # Child addresses (length = entries_used)

    # Inner constructor for validation
    function V1BTreeNode(node_type::UInt8, node_level::UInt8, entries_used::UInt16,
                        left_sibling::RelOffset, right_sibling::RelOffset,
                        keys::Vector{V1ChunkKey}, children::Vector{RelOffset})
        node_type in (0, 1) || throw(ArgumentError("node_type must be 0 (groups) or 1 (chunks)"))
        length(keys) == entries_used + 1 ||
            throw(ArgumentError("keys length $(length(keys)) must equal entries_used + 1 ($(entries_used + 1))"))
        length(children) == entries_used ||
            throw(ArgumentError("children length $(length(children)) must equal entries_used ($entries_used)"))
        new(node_type, node_level, entries_used, left_sibling, right_sibling, keys, children)
    end
end

"""
    V1BTree

Helper structure for managing the entire V1 B-tree.
- root: Address of root node
- dimensionality: Number of dimensions in dataset
- max_entries_per_node: Based on desired node size
- file: Reference to file for I/O
- pending_chunks: Chunks waiting to be written to the tree
"""
mutable struct V1BTree
    root::RelOffset            # Address of root node
    dimensionality::UInt8      # Number of dimensions in dataset
    max_entries_per_node::UInt16 # Based on desired node size
    file::JLDFile             # Reference to file for I/O
    pending_chunks::Vector{Tuple{V1ChunkKey, RelOffset}}  # Pending chunks to write

    function V1BTree(root::RelOffset, dimensionality::UInt8, max_entries_per_node::UInt16, file::JLDFile)
        dimensionality > 0 || throw(ArgumentError("dimensionality must be > 0"))
        max_entries_per_node > 0 || throw(ArgumentError("max_entries_per_node must be > 0"))
        new(root, dimensionality, max_entries_per_node, file, Tuple{V1ChunkKey, RelOffset}[])
    end
end

"""
    calculate_max_entries(f::JLDFile, dimensionality::UInt8, target_node_size::Int = 4096)::UInt16

Calculate the maximum number of entries that can fit in a V1 B-tree node
for the given dimensionality and target node size.

Algorithm:
1. Calculate fixed overhead (signature, header, sibling pointers)
2. Calculate size per entry (key + child pointer)
3. Account for extra key at end
4. Determine max entries that fit in target size
"""
function calculate_max_entries(f::JLDFile, dimensionality::UInt8, target_node_size::Int = 4096)::UInt16
    # Fixed overhead per node
    header_size = 4 + 1 + 1 + 2 + 2*jlsizeof(RelOffset)  # signature + type + level + entries + siblings

    # Size per entry
    key_size = 4 + 4 + 8 * (dimensionality + 1)  # chunk_size + filter_mask + indices
    child_size = jlsizeof(RelOffset)
    entry_size = key_size + child_size

    # Extra key at end
    extra_key_size = key_size

    # Calculate max entries that fit in target size
    available_space = target_node_size - header_size - extra_key_size
    max_entries = available_space ÷ entry_size

    return UInt16(max(1, min(max_entries, 64)))  # At least 1, at most 64 (HDF5 V1 B-tree limit)
end

"""
    create_chunk_key(chunk_indices::Vector{Int}, chunk_size::UInt32, filter_mask::UInt32 = 0x00000000)::V1ChunkKey

Create a V1 B-tree chunk key from chunk indices and metadata.
Automatically appends the required datatype offset (always 0) to the indices.

NOTE: HDF5 stores dimensions in reverse order (fastest to slowest).
The chunk indices must be reversed to match HDF5 expectations before storing as 0-based.
The reading code in datasets.jl will reverse them back and convert to 1-based ranges.
"""
function create_chunk_key(chunk_indices, chunk_size::UInt32, filter_mask::UInt32 = 0x00000000)::V1ChunkKey
    # Convert to 0-based for HDF5 V1 B-tree format, reverse for HDF5 dimension order, and add datatype offset (always 0)
    # Reading code in datasets.jl will reverse back and convert to 1-based ranges
    reversed_0based_indices = reverse(chunk_indices .- 1)
    indices = UInt64[reversed_0based_indices..., 0]
    return V1ChunkKey(chunk_size, filter_mask, indices)
end

"""
    calculate_node_size(node::V1BTreeNode)::Int

Calculate the total size in bytes needed to serialize a V1 B-tree node.
"""
function calculate_node_size(node::V1BTreeNode)::Int
    # Fixed header: signature + type + level + entries_used + siblings
    header_size = 4 + 1 + 1 + 2 + 2*jlsizeof(RelOffset)

    # All entries are of the same size
    # It is not clear how big hdf5 assumes the node to be.
    # Guess
    K = 64
    # Keys and children (interleaved)
    entry_size = (K+1)*8 + K * calculate_key_size(node.keys[1])

    return header_size + entry_size
end

"""
    calculate_key_size(key::V1ChunkKey)::Int

Calculate the size in bytes needed to serialize a V1 chunk key.
"""
function calculate_key_size(key::V1ChunkKey)::Int
    return 4 + 4 + 8 * length(key.indices)  # chunk_size + filter_mask + indices
end

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
    # Calculate total size needed
    node_size = calculate_node_size(node)

    # Allocate space at end of file
    offset = h5offset(f, f.end_of_data)
    f.end_of_data += node_size

    # Write to file
    io = f.io
    seek(io, fileoffset(f, offset))

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

    return offset
end

"""
    write_v1btree_node_at_offset(f::JLDFile, node::V1BTreeNode, offset::RelOffset)

Write a V1 B-tree node to file at a specific offset.
Used when updating existing nodes.
"""
function write_v1btree_node_at_offset(f::JLDFile, node::V1BTreeNode, offset::RelOffset)
    # Write to file at specific offset
    io = f.io
    seek(io, fileoffset(f, offset))

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

    return offset
end

# ============================================================================
# Phase 2: Core Algorithms - Key comparison and node insertion logic
# ============================================================================

"""
    compare_chunk_keys(key1::V1ChunkKey, key2::V1ChunkKey)::Int

Compare two chunk keys for sorting in V1 B-trees.
Returns: -1 if key1 < key2, 0 if equal, +1 if key1 > key2

For chunked datasets: Key[i] describes the least chunk in Child[i]
Keys are compared lexicographically by dimension indices (excluding final datatype offset).
"""
function compare_chunk_keys(key1::V1ChunkKey, key2::V1ChunkKey)::Int
    # Compare dimension indices lexicographically
    # Skip last index (datatype offset, always 0)
    min_dims = min(length(key1.indices), length(key2.indices)) - 1

    for i in 1:min_dims
        if key1.indices[i] < key2.indices[i]
            return -1
        elseif key1.indices[i] > key2.indices[i]
            return 1
        end
    end

    # If all compared dimensions equal, shorter wins (shouldn't happen in practice)
    return (length(key1.indices) - 1) - (length(key2.indices) - 1)
end

"""
    find_insertion_point(node::V1BTreeNode, key::V1ChunkKey)::Int

Find the correct insertion point for a key in a node using binary search.
Returns the index where the key should be inserted (1-based indexing).

For V1 B-trees with chunked datasets, maintains the ordering invariant:
Key[i] describes the least chunk in Child[i]
"""
function find_insertion_point(node::V1BTreeNode, key::V1ChunkKey)::Int
    # Binary search to find correct insertion point
    left, right = 1, node.entries_used + 1

    while left <= right
        mid = (left + right) ÷ 2

        comparison = compare_chunk_keys(key, node.keys[mid])

        if comparison < 0
            right = mid - 1
        elseif comparison > 0
            left = mid + 1
        else
            # Exact match - this should not happen for chunk keys
            # (each chunk should have unique coordinates)
            throw(InvalidDataException("Duplicate chunk key: $(key.indices[1:end-1])"))
        end
    end

    return left
end

"""
    insert_into_node!(node::V1BTreeNode, key::V1ChunkKey, child::RelOffset, max_entries::UInt16)::Bool

Insert a key/child pair into a node if it has capacity.
Returns true if insertion succeeded, false if node is full.

Maintains the V1 B-tree ordering invariant for chunked datasets.
"""
function insert_into_node!(node::V1BTreeNode, key::V1ChunkKey, child::RelOffset, max_entries::UInt16)::Bool
    # Check if node has capacity
    if node.entries_used >= max_entries
        return false  # Node is full, need to split first
    end

    # Find insertion point
    insert_pos = find_insertion_point(node, key)

    # Insert key at position insert_pos
    insert!(node.keys, insert_pos, key)

    # Insert child at position insert_pos
    # Note: children array has same length as entries_used
    insert!(node.children, insert_pos, child)

    # Update entries count
    node.entries_used += 1
    return true
end

"""
    find_child_for_key(node::V1BTreeNode, key::V1ChunkKey)::Int

Find which child pointer to follow for a given key in an internal node.
Returns the child index (1-based) to traverse.

Uses the V1 B-tree ordering rule: Key[i] describes the least chunk in Child[i]
"""
function find_child_for_key(node::V1BTreeNode, key::V1ChunkKey)::Int
    # For chunked dataset B-trees, find the appropriate child
    # Key[i] describes the least chunk in Child[i]

    for i in 1:node.entries_used
        # If key is less than or equal to this key, follow this child
        if compare_chunk_keys(key, node.keys[i]) <= 0
            return i
        end
    end

    # If key is greater than all keys, it doesn't belong in this tree
    # This should not happen in a properly constructed tree
    throw(InvalidDataException("Key $(key.indices[1:end-1]) is greater than all keys in internal node"))
end

# ============================================================================
# Phase 3: Tree Management - Node splitting and balancing
# ============================================================================

"""
    split_node(f::JLDFile, node::V1BTreeNode, btree::V1BTree)::Tuple{V1BTreeNode, V1BTreeNode, V1ChunkKey, RelOffset}

Split a full node roughly in half and return the left node, right node,
promoted key, and the right node's file offset.

Updates sibling pointers and writes both nodes to file.
Returns tuple: (left_node, right_node, promoted_key, right_offset)
"""
function split_node(f::JLDFile, node::V1BTreeNode, btree::V1BTree)::Tuple{V1BTreeNode, V1BTreeNode, V1ChunkKey, RelOffset}
    # Split node roughly in half
    split_point = (node.entries_used + 1) ÷ 2

    # Create left node (keeps first part)
    left_keys = node.keys[1:split_point]
    left_children = node.children[1:(split_point-1)]  # One less child than keys
    left_node = V1BTreeNode(
        node.node_type, node.node_level, UInt16(split_point - 1),
        node.left_sibling, UNDEFINED_ADDRESS,  # Will set right sibling below
        left_keys, left_children
    )

    # Create right node (gets second part)
    right_keys = node.keys[(split_point+1):end]
    right_children = node.children[split_point:end]
    right_node = V1BTreeNode(
        node.node_type, node.node_level, UInt16(length(right_keys) - 1),
        UNDEFINED_ADDRESS, node.right_sibling,  # Will set left sibling below
        right_keys, right_children
    )

    # Promote middle key
    promoted_key = node.keys[split_point]

    # Write right node to get its offset (left will reuse original offset)
    right_offset = write_v1btree_node(f, right_node)

    # Update sibling pointers
    left_node.right_sibling = right_offset
    right_node.left_sibling = UNDEFINED_ADDRESS  # Will be set by caller

    # Update old sibling's pointers if they exist
    if node.right_sibling != UNDEFINED_ADDRESS
        update_sibling_pointer(f, node.right_sibling, :left, right_offset)
    end

    return left_node, right_node, promoted_key, right_offset
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
    create_initial_root(f::JLDFile, key::V1ChunkKey, child_address::RelOffset, btree::V1BTree)::RelOffset

Create the initial root node for an empty B-tree.
Returns the offset where the root node was written.
"""
function create_initial_root(f::JLDFile, key::V1ChunkKey, child_address::RelOffset, btree::V1BTree)::RelOffset
    # Create boundary key with proper indices (one higher than the actual key)
    # Key indices are 0-based, so add 2 to get 1-based boundary, then create_chunk_key converts to 0-based
    boundary_indices_1based = [Int(key.indices[i]) + 2 for i in 1:(btree.dimensionality)]
    boundary_key = create_chunk_key(boundary_indices_1based, UInt32(0))

    root_node = V1BTreeNode(
        UInt8(1),        # node_type (chunked datasets)
        UInt8(0),        # node_level (leaf)
        UInt16(1),       # entries_used
        UNDEFINED_ADDRESS, UNDEFINED_ADDRESS,  # no siblings
        [key, boundary_key],    # keys (entries_used + 1)
        [child_address]         # children (entries_used)
    )

    return write_v1btree_node(f, root_node)
end

"""
    create_new_root(f::JLDFile, old_root_offset::RelOffset, new_child_offset::RelOffset,
                   promoted_key::V1ChunkKey, btree::V1BTree)::RelOffset

Create a new root node when the old root was split.
The new root will have the old root and new child as its children.
Returns the offset where the new root was written.
"""
function create_new_root(f::JLDFile, old_root_offset::RelOffset, new_child_offset::RelOffset,
                        promoted_key::V1ChunkKey, btree::V1BTree)::RelOffset
    # Create boundary key with proper indices (one higher than promoted key)
    # Promoted key indices are 0-based, so add 2 to get 1-based boundary, then create_chunk_key converts to 0-based
    boundary_indices_1based = [Int(promoted_key.indices[i]) + 2 for i in 1:(btree.dimensionality)]
    boundary_key = create_chunk_key(boundary_indices_1based, UInt32(0))

    new_root = V1BTreeNode(
        UInt8(1),        # node_type (chunked datasets)
        UInt8(1),        # node_level (internal - one level up)
        UInt16(1),       # entries_used
        UNDEFINED_ADDRESS, UNDEFINED_ADDRESS,  # no siblings
        [promoted_key, boundary_key],          # keys (entries_used + 1)
        [old_root_offset]                      # children (entries_used) - points to old root
    )

    return write_v1btree_node(f, new_root)
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

    key = create_chunk_key(chunk_indices, chunk_size, filter_mask)

    # Store the chunk for later processing when finalize_btree! is called
    push!(btree.pending_chunks, (key, chunk_address))
end

"""
    finalize_btree!(btree::V1BTree)

Finalize the B-tree by writing all pending chunks. If there are more chunks
than can fit in a single node, creates multiple nodes with proper B-tree structure.
"""
function finalize_btree!(btree::V1BTree, max_indices)
    if isempty(btree.pending_chunks)
        # No chunks to write
        btree.root = UNDEFINED_ADDRESS
        return
    end

    # Sort chunks by key for proper B-tree ordering
    sort!(btree.pending_chunks, by=chunk -> chunk[1], lt=(a,b) -> compare_chunk_keys(a,b) < 0)

    keys = V1ChunkKey[]
    children = RelOffset[]

    for (key, address) in btree.pending_chunks
        push!(keys, key)
        push!(children, address)
    end

    # Add boundary key (one higher than max indices)
    # max_indices is already in 0-based HDF5 order: (dim2_max, dim1_max, 0)
    # Boundary key should be (dim2_max+1, dim1_max+1, 0)
    boundary_indices_0based_hdf5 = UInt64[max_indices[i] + (i < length(max_indices) ? 1 : 0) for i in 1:length(max_indices)]
    boundary_key = V1ChunkKey(UInt32(0), UInt32(0), boundary_indices_0based_hdf5)
    push!(keys, boundary_key)

    # Check if we need to split nodes
    if length(children) <= btree.max_entries_per_node
        # Simple case: everything fits in one node
        root_node = V1BTreeNode(
            UInt8(1),        # node_type (chunked datasets)
            UInt8(0),        # node_level (leaf)
            UInt16(length(children)), # entries_used
            UNDEFINED_ADDRESS, UNDEFINED_ADDRESS,  # no siblings
            keys, children
        )
        btree.root = write_v1btree_node(btree.file, root_node)
    else
        # Complex case: need to split into multiple nodes
        btree.root = build_btree_from_chunks(btree.file, keys, children, btree.max_entries_per_node)
    end

    # Clear pending chunks after writing
    empty!(btree.pending_chunks)
end

"""
    read_first_key_from_node(file::JLDFile, node_offset::RelOffset)::V1ChunkKey

Read the first key from a B-tree node (used for building internal nodes).
"""
function read_first_key_from_node(file::JLDFile, node_offset::RelOffset)::V1ChunkKey
    io = file.io
    seek(io, fileoffset(file, node_offset))

    # Skip node header (signature + type + level + entries_used + siblings)
    skip(io, 4 + 1 + 1 + 2 + 16)

    # Read first key
    chunk_size = jlread(io, UInt32)
    filter_mask = jlread(io, UInt32)
    # Note: dimensionality is implicit - we need to know it from context
    # For now, assume 2D + element size = 3 indices
    # TODO: Pass dimensionality as parameter if this becomes an issue
    indices = [jlread(io, UInt64) for _ in 1:3]  # Hardcoded for now

    return V1ChunkKey(chunk_size, filter_mask, indices)
end

"""
    read_last_key_from_node(file::JLDFile, node_offset::RelOffset)::V1ChunkKey

Read the last key from a B-tree node (used for boundary keys in internal nodes).
"""
function read_last_key_from_node(file::JLDFile, node_offset::RelOffset)::V1ChunkKey
    io = file.io
    seek(io, fileoffset(file, node_offset))

    # Read node header to get entries_used
    skip(io, 4 + 1 + 1)  # Skip signature, type, level
    entries_used = jlread(io, UInt16)
    skip(io, 16)  # Skip siblings

    # Skip all entries to get to the final key
    # Each entry is: key (4 + 4 + 3*8 = 32 bytes) + child offset (8 bytes) = 40 bytes
    skip(io, entries_used * 40)

    # Read final key
    chunk_size = jlread(io, UInt32)
    filter_mask = jlread(io, UInt32)
    indices = [jlread(io, UInt64) for _ in 1:3]

    return V1ChunkKey(chunk_size, filter_mask, indices)
end

"""
    build_btree_from_chunks(file::JLDFile, keys::Vector{V1ChunkKey}, children::Vector{RelOffset}, max_entries::UInt16)

Build a B-tree from a large number of chunks that need to be split across multiple nodes.
Returns the root node offset.
"""
function build_btree_from_chunks(file::JLDFile, keys::Vector{V1ChunkKey}, children::Vector{RelOffset}, max_entries::UInt16)

    # Split leaf nodes
    leaf_nodes = split_into_leaf_nodes(file, keys, children, max_entries)

    if length(leaf_nodes) == 1
        # Only one leaf node
        return leaf_nodes[1]
    end

    # Build internal nodes bottom-up
    current_level = leaf_nodes

    while length(current_level) > 1
        next_level = RelOffset[]

        # Group current level nodes into internal nodes
        for i in 1:max_entries:length(current_level)
            end_idx = min(i + max_entries - 1, length(current_level))
            group = current_level[i:end_idx]

            # Create internal node for this group
            internal_keys = V1ChunkKey[]
            internal_children = RelOffset[]

            # For internal nodes, we need to create keys that describe the least chunk in each child
            for j in 1:length(group)
                child_offset = group[j]
                # Read the first key from the child node
                child_first_key = read_first_key_from_node(file, child_offset)
                push!(internal_keys, child_first_key)
                push!(internal_children, child_offset)
            end

            # Add boundary key - read the last key from the last child
            last_child_offset = group[end]
            boundary_key = read_last_key_from_node(file, last_child_offset)
            push!(internal_keys, boundary_key)

            # Create internal node
            internal_node = V1BTreeNode(
                UInt8(1),        # node_type (chunked datasets)
                UInt8(1),        # node_level (internal)
                UInt16(length(internal_children)), # entries_used
                UNDEFINED_ADDRESS, UNDEFINED_ADDRESS,  # no siblings
                internal_keys, internal_children
            )

            internal_offset = write_v1btree_node(file, internal_node)
            push!(next_level, internal_offset)
        end

        current_level = next_level
    end
    return current_level[1]  # Root node
end

"""
    split_into_leaf_nodes(file::JLDFile, keys::Vector{V1ChunkKey}, children::Vector{RelOffset}, max_entries::UInt16)

Split chunks into multiple leaf nodes, each containing at most max_entries chunks.
Returns a vector of leaf node offsets.
"""
function split_into_leaf_nodes(file::JLDFile, keys::Vector{V1ChunkKey}, children::Vector{RelOffset}, max_entries::UInt16)
    leaf_offsets = RelOffset[]

    # Split into groups of max_entries
    for i in 1:max_entries:length(children)
        end_idx = min(i + max_entries - 1, length(children))

        # Extract keys and children for this leaf
        leaf_keys = keys[i:(end_idx+1)]     # +1 because we need one extra key
        leaf_children = children[i:end_idx]

        # Create leaf node
        leaf_node = V1BTreeNode(
            UInt8(1),        # node_type (chunked datasets)
            UInt8(0),        # node_level (leaf)
            UInt16(length(leaf_children)), # entries_used
            UNDEFINED_ADDRESS, UNDEFINED_ADDRESS,  # no siblings for now
            leaf_keys, leaf_children
        )

        leaf_offset = write_v1btree_node(file, leaf_node)
        push!(leaf_offsets, leaf_offset)
    end

    return leaf_offsets
end

"""
    insert_recursive!(f::JLDFile, node_offset::RelOffset, key::V1ChunkKey,
                     child_address::RelOffset, btree::V1BTree)::Tuple{Union{V1ChunkKey, Nothing}, Union{RelOffset, Nothing}}

Recursively insert a key/child pair into the B-tree, handling splits as needed.
Returns (promoted_key, promoted_child_offset) if the node was split, (nothing, nothing) otherwise.
"""
function insert_recursive!(f::JLDFile, node_offset::RelOffset, key::V1ChunkKey,
                          child_address::RelOffset, btree::V1BTree)::Tuple{Union{V1ChunkKey, Nothing}, Union{RelOffset, Nothing}}

    # Read current node (we'll need a read function for this)
    node = read_v1btree_node(f, node_offset)

    if node.node_level == 0
        # Leaf node - try direct insertion
        if node.entries_used < btree.max_entries_per_node
            insert_into_node!(node, key, child_address, btree.max_entries_per_node)
            write_v1btree_node_at_offset(f, node, node_offset)
            return (nothing, nothing)  # No promotion needed
        else
            # Node is full - must split
            left_node, right_node, promoted_key, right_offset = split_node(f, node, btree)

            # Insert into appropriate split
            if compare_chunk_keys(key, promoted_key) < 0
                insert_into_node!(left_node, key, child_address, btree.max_entries_per_node)
                write_v1btree_node_at_offset(f, left_node, node_offset)
            else
                insert_into_node!(right_node, key, child_address, btree.max_entries_per_node)
                write_v1btree_node_at_offset(f, right_node, right_offset)
            end

            return (promoted_key, right_offset)
        end
    else
        # Internal node - find correct child and recurse
        child_index = find_child_for_key(node, key)
        child_offset = node.children[child_index]

        promoted_key, promoted_child = insert_recursive!(f, child_offset, key, child_address, btree)

        if promoted_key === nothing
            return (nothing, nothing)  # No promotion from child
        end

        # Child was split - need to insert promoted key/child
        if node.entries_used < btree.max_entries_per_node
            insert_into_node!(node, promoted_key, promoted_child, btree.max_entries_per_node)
            write_v1btree_node_at_offset(f, node, node_offset)
            return (nothing, nothing)
        else
            # This node is also full - split it
            left_node, right_node, new_promoted_key, right_offset = split_node(f, node, btree)

            # Insert promoted key/child into appropriate split
            if compare_chunk_keys(promoted_key, new_promoted_key) < 0
                insert_into_node!(left_node, promoted_key, promoted_child, btree.max_entries_per_node)
                write_v1btree_node_at_offset(f, left_node, node_offset)
            else
                insert_into_node!(right_node, promoted_key, promoted_child, btree.max_entries_per_node)
                write_v1btree_node_at_offset(f, right_node, right_offset)
            end

            return (new_promoted_key, right_offset)
        end
    end
end

"""
    read_v1btree_node(f::JLDFile, offset::RelOffset)::V1BTreeNode

Read a V1 B-tree node from file at the given offset.
This is a wrapper around the existing reading functionality to create our mutable struct.
"""
function read_v1btree_node(f::JLDFile, offset::RelOffset)::V1BTreeNode
    io = f.io
    seek(io, fileoffset(f, offset))

    signature = jlread(io, UInt32)
    signature == V1_BTREE_NODE_SIGNATURE || throw(InvalidDataException("Invalid V1 B-tree signature"))

    node_type = jlread(io, UInt8)
    node_level = jlread(io, UInt8)
    entries_used = jlread(io, UInt16)
    left_sibling = jlread(io, RelOffset)
    right_sibling = jlread(io, RelOffset)

    # Read keys and children
    keys = V1ChunkKey[]
    children = RelOffset[]

    for _ in 1:entries_used
        # Read key
        chunk_size = jlread(io, UInt32)
        filter_mask = jlread(io, UInt32)
        # Determine dimensionality from node_type and calculate indices count
        # For now, we'll need to pass dimensionality - this is a limitation
        # In practice, we'd know this from the btree context
        throw(UnsupportedFeatureException("read_v1btree_node needs dimensionality parameter - use existing read functions for now"))
    end

    # This is incomplete - we need dimensionality info to read keys properly
    # For now, this is a placeholder that demonstrates the structure
end

# ============================================================================
# Phase 4: DataLayout Integration - Chunked dataset pipeline integration
# ============================================================================

"""
    enumerate_chunks(data_size::NTuple{N,Int}, chunk_dims::Vector{Int}) where N

Enumerate all chunk indices for a given data size and chunk dimensions.
Returns an iterator over chunk indices.

Example:
```
julia> JLD2.enumerate_chunks((10,14), [3,5]) |> collect
4×3 Matrix{Tuple{Int64, Int64}}:
 (1, 1)   (1, 6)   (1, 11)
 (4, 1)   (4, 6)   (4, 11)
 (7, 1)   (7, 6)   (7, 11)
 (10, 1)  (10, 6)  (10, 11)
````
"""
function enumerate_chunks(data_size, chunk_dims)
    Iterators.product(StepRange.(1, chunk_dims, data_size)...)
end

"""
    extract_chunk(data, chunk_indices::Vector{Int}, chunk_dims::Vector{Int})

Extract a chunk of data at the given chunk starting positions with the specified chunk dimensions.
Returns the chunk data as a contiguous array.
"""
function extract_chunk(data, chunk_indices, chunk_dims)
    ndims(data) == length(chunk_indices) || throw(ArgumentError("chunk_indices dimensionality must match data"))

    chunk = typeof(data)(undef, chunk_dims...)
    # # Calculate start and end indices for each dimension
    # # chunk_indices now contains starting positions, not chunk numbers
    # ranges = []
    # for (dim, start_idx, chunk_size) in zip(1:ndims(data), chunk_indices, chunk_dims)
    #     end_idx = min(start_pos + chunk_size - 1, size(data, dim))
    #     push!(ranges, start_idx:end_idx)
    # end

    # full_indices = range.(chunk_indices, chunk_indices .+ chunk_dims)
    avail_indices = range.(chunk_indices, min.(size(data), chunk_indices .+ chunk_dims .-1))
    indexview = (:).(1, length.(avail_indices))

    #chunk[indexview...] .= @view data[ranges...]
    chunk[indexview...] .= @view data[avail_indices...]

    chunk
end

"""
    write_chunked_dataset_with_v1btree(f::JLDFile, data, dataspace, datatype, chunk_dims::Vector{Int})

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
    chunk_count = 0
    for chunk_indices in enumerate_chunks(data_size, chunk_dims)
        chunk_count += 1
        chunk_data = extract_chunk(data, chunk_indices, chunk_dims)

        # Convert chunk data to raw bytes (like Filters.compress does)
        if !isempty(local_filters)
            # Get raw byte data from transposed chunk array
            GC.@preserve chunk_data begin
                raw_chunk_data = unsafe_wrap(
                    Vector{UInt8},
                    Ptr{UInt8}(pointer(chunk_data)),
                    odr_sizeof(odr) * length(chunk_data)
                )
                # Apply compression filters
                compressed_chunk, filter_mask = apply_chunk_filters(local_filters, raw_chunk_data)
            end
        else
            # Store raw array data without compression
            GC.@preserve chunk_data begin
                compressed_chunk = unsafe_wrap(
                    Vector{UInt8},
                    Ptr{UInt8}(pointer(chunk_data)),
                    odr_sizeof(odr) * length(chunk_data)
                )
                # Make a copy to avoid aliasing issues
                compressed_chunk = copy(compressed_chunk)
                filter_mask = 0x00000000
            end
        end

        # Write chunk data to file
        chunk_address = h5offset(f, f.end_of_data)
        seek(f.io, f.end_of_data)

        jlwrite(f.io, compressed_chunk)
        f.end_of_data += length(compressed_chunk)

        # Insert chunk into B-tree
        insert_chunk!(btree, chunk_indices, chunk_address, UInt32(length(compressed_chunk)), filter_mask)

        total_chunk_size += length(compressed_chunk)
        num_chunks += 1
    end

    # Finalize the B-tree by writing all pending chunks
    # The boundary key should be the array dimensions (0-based) in HDF5 order
    # Chunk indices are 0-based element positions of first element in each chunk
    # For a 30×80 array: max element index is (29, 79) in 0-based Julia order
    # Boundary key is one past: (30, 80, 0) in 0-based, then reverse to HDF5 order: (80, 30, 0)
    array_dims_0based = size(data)  # This gives us the boundary in 0-based indexing
    max_indices_hdf5 = (reverse(array_dims_0based)..., 0)  # HDF5 order with datatype offset

    finalize_btree!(btree, max_indices_hdf5)
    return btree, total_chunk_size, num_chunks
end

"""
    apply_chunk_filters(filters::FilterPipeline, chunk_data::Vector{UInt8})::Tuple{Vector{UInt8}, UInt32}

Apply compression filters to a single chunk and return the compressed data and filter mask.
Uses the JLD2.Filters module to apply compression correctly.
"""
function apply_chunk_filters(filters::Filters.FilterPipeline, chunk_data::Vector{UInt8})::Tuple{Vector{UInt8}, UInt32}
    if isempty(filters.filters)
        return chunk_data, 0x00000000
    end

    # Apply filters using the Filters module
    ref = Ref(chunk_data)
    retcodes = map(fil -> Filters.apply_filter!(fil, ref), filters)

    # Build filter mask from return codes
    # A filter is marked as skipped (bit set) if it failed (retcode != 0)
    filter_mask = UInt32(0)
    for (i, retcode) in enumerate(retcodes)
        if retcode != 0
            filter_mask |= (UInt32(1) << (i - 1))
        end
    end

    return ref[], filter_mask
end
