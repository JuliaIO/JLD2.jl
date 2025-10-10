# V2 B-tree Type Definitions
# Core data structures for V2 B-trees (used in HDF5 1.8+)

const V2_BTREE_HEADER_SIGNATURE = htol(0x44485442) # UInt8['B','T','H','D']
const V2_BTREE_INTERNAL_NODE_SIGNATURE = htol(0x4e495442) # UInt8['B', 'T', 'I', 'N']
const V2_BTREE_LEAF_NODE_SIGNATURE = htol(0x464c5442) # UInt8['B', 'T', 'L', 'F']

"""
    V2BTreeHeader

On-disk header structure for V2 B-trees (HDF5 1.8+).
Used for both reading and writing. Uses exact HDF5 field types.

Fields (in on-disk order):
- `signature`: Always V2_BTREE_HEADER_SIGNATURE (0x44485442 = "BTHD")
- `version`: Should be 0
- `type`: B-tree type (5 = link names, 10 = chunked dataset storage)
- `node_size`: Size of B-tree nodes in bytes
- `record_size`: Size of each record in bytes
- `depth`: Tree depth (0 = root is leaf node)
- `split_percent`: Percent full before splitting (typically 100)
- `merge_percent`: Percent full before merging (typically 40)
- `root_node_address`: File offset of the root node (UInt64, not RelOffset for v2 btrees)
- `num_records_root`: Number of records in root node
- `total_records`: Total records in entire tree (variable-size Length type)

Note: Checksum is NOT part of the struct - it's stored separately in the file
and validated during reading. This replaces both the old BTreeHeaderV2 and
the duplicate V2BTreeHeader.
"""
struct V2BTreeHeader
    signature::UInt32
    version::UInt8
    type::UInt8
    node_size::UInt32
    record_size::UInt16
    depth::UInt16
    split_percent::UInt8
    merge_percent::UInt8
    root_node_address::RelOffset  # Offset relative to superblock
    num_records_root::UInt16
    total_records::Length      # Variable-size field
end
define_packed(V2BTreeHeader)

# Constructor for writing (auto-fills signature)
V2BTreeHeader(type::UInt8, args...) = V2BTreeHeader(
    V2_BTREE_HEADER_SIGNATURE, 0x00, type, args...)

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
