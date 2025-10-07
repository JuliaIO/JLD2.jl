# V2 B-tree Type Definitions
# Core data structures for V2 B-trees (used in HDF5 1.8+)

const V2_BTREE_HEADER_SIGNATURE = htol(0x44485442) # UInt8['B','T','H','D']
const V2_BTREE_INTERNAL_NODE_SIGNATURE = htol(0x4e495442) # UInt8['B', 'T', 'I', 'N']
const V2_BTREE_LEAF_NODE_SIGNATURE = htol(0x464c5442) # UInt8['B', 'T', 'L', 'F']

"""
    BTreeHeaderV2

Header structure for V2 B-trees.
V2 B-trees are used in HDF5 1.8+ for indexed groups and other structures.

Fields:
- `offset`: File offset of the header
- `type`: B-tree type (5 = link name for indexed group)
- `node_size`: Size of B-tree nodes in bytes
- `record_size`: Size of records in bytes
- `depth`: Depth of the tree (0 = root is leaf node)
- `split_percent`: Percent full before splitting (typically 100)
- `merge_percent`: Percent full before merging (typically 40)
- `root_node_address`: File offset of the root node
- `num_records_in_root_node`: Number of records in root node
- `num_records_in_tree`: Total number of records in the entire tree
"""
struct BTreeHeaderV2
    offset::RelOffset
    type::Int
    node_size::Int
    record_size::Int
    depth::Int
    split_percent::Int
    merge_percent::Int
    root_node_address::RelOffset
    num_records_in_root_node::Int
    num_records_in_tree::Int
end

"""
    BTreeNodeV2

Abstract base type for V2 B-tree nodes.
Concrete types: BTreeInternalNodeV2, BTreeLeafNodeV2
"""
abstract type BTreeNodeV2 end

"""
    BTreeRecordV2

Abstract base type for V2 B-tree records.
Different record types exist for different B-tree types.
Concrete types: BTreeType5RecordV2 (for link names in indexed groups)
"""
abstract type BTreeRecordV2 end

"""
    BTreeInternalNodeV2 <: BTreeNodeV2

Internal (non-leaf) node in a V2 B-tree.
Contains records and pointers to child nodes.

Fields:
- `offset`: File offset of this node
- `type`: B-tree type (matches header type)
- `records`: Vector of records stored in this node
- `child_nodes`: Vector of child node information (offsets and record counts)
"""
struct BTreeInternalNodeV2 <: BTreeNodeV2
    offset::RelOffset
    type::UInt8
    records::Vector{Any}
    child_nodes::Vector # Abstract to defer loading
end

"""
    BTreeLeafNodeV2 <: BTreeNodeV2

Leaf node in a V2 B-tree.
Contains records but no child pointers.

Fields:
- `offset`: File offset of this node
- `type`: B-tree type (matches header type)
- `records`: Vector of records stored in this node
"""
struct BTreeLeafNodeV2 <: BTreeNodeV2
    offset::RelOffset
    type::UInt8
    records::Vector{<:BTreeRecordV2}
end

"""
    BTreeType5RecordV2 <: BTreeRecordV2

Record type for B-tree type 5 (link names in indexed groups).
Used to locate link information in a fractal heap.

Fields:
- `hash`: Hash of the link name (for faster lookup)
- `offset`: Offset within the fractal heap where the link message is stored
- `length`: Length of the link message data
"""
struct BTreeType5RecordV2 <: BTreeRecordV2
    hash::UInt32
    offset::UInt64
    length::Int
end
