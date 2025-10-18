# V2 B-tree Type Definitions
# Core data structures for V2 B-trees (used in HDF5 1.8+)

const V2_BTREE_HEADER_SIGNATURE = htol(0x44485442) # UInt8['B','T','H','D']
const V2_BTREE_INTERNAL_NODE_SIGNATURE = htol(0x4e495442) # UInt8['B', 'T', 'I', 'N']
const V2_BTREE_LEAF_NODE_SIGNATURE = htol(0x464c5442) # UInt8['B', 'T', 'L', 'F']

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


abstract type BTreeRecordV2 end

struct BTreeInternalNodeV2
    offset::RelOffset
    type::UInt8
    records::Vector{Any}
    child_nodes::Vector
end

struct BTreeLeafNodeV2
    offset::RelOffset
    type::UInt8
    records::Vector{<:BTreeRecordV2}
end

struct BTreeType5RecordV2 <: BTreeRecordV2
    hash::UInt32
    offset::UInt64
    length::Int
end

"""
    ChunkRecordV2

Represents a chunk record in a V2 B-tree for chunked datasets (type 10).
Contains chunk offset, size, filter mask, and multi-dimensional index.
"""
struct ChunkRecordV2 <: BTreeRecordV2
    offset::RelOffset
    size::UInt64
    filter_mask::UInt32
    idx::CartesianIndex
end
