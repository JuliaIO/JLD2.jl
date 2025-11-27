# V1 B-tree Type Definitions
# Core data structures for V1 B-trees
const V1_BTREE_NODE_SIGNATURE = htol(0x45455254)  # "TREE"

struct V1ChunkKey
    chunk_size::UInt32         # Size of chunk in bytes
    filter_mask::UInt32        # Filter bitmask (0 if no filters)
    indices::Vector{UInt64}    # D+1 dimensional indices (last is always 0)

    # Inner constructor for validation
    function V1ChunkKey(chunk_size, filter_mask::UInt32, indices::Vector{UInt64})
        length(indices) >= 2 || throw(ArgumentError("indices must have at least 2 elements (dimensions + datatype offset)"))
        #indices[end] == 0 || throw(ArgumentError("last index (datatype offset) must be 0"))
        new(chunk_size, filter_mask, indices)
    end
end

Base.isless(k1,k2) = reverse(k1.indices) < reverse(k2.indices)

mutable struct V1BTreeNode
    node_type::UInt8           # 1 for chunked datasets, 0 for groups
    node_level::UInt8          # 0 for leaf, >0 for internal
    entries_used::UInt16       # Number of valid key/child pairs
    left_sibling::RelOffset    # Left sibling node address
    right_sibling::RelOffset   # Right sibling node address
    keys::Vector{V1ChunkKey}   # Keys (length = entries_used + 1)
    children::Vector{RelOffset} # Child addresses (length = entries_used)

    function V1BTreeNode(node_type, node_level, entries_used, left_sibling, right_sibling, keys, children)
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
