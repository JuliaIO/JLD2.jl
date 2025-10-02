# V2 B-tree Index (v4 chunk indexing type 5)
# Used for datasets with MULTIPLE unlimited dimensions
#
# Based on HDF5 Specification Section III.A.2
# "Disk Format: Level 1A2 - Version 2 B-trees"

"""
    read_v2btree_chunks(f, v, dataspace, rr, layout, filters, header_offset, ndims)

Read chunks indexed by HDF5 v2 B-tree (type 5).
Used when dataset has multiple unlimited dimensions.

# V2 B-tree Structure

**Header** (signature "BTHD"):
- version, type, node_size, record_size
- depth, split_percent, merge_percent
- root_node_address, num_records_root, total_records

**Leaf Node** (signature "BTLF"):
- For chunked data (type 10), each record contains:
  - chunk_address (8 bytes)
  - chunk_index_dim1 (8 bytes) - chunk coordinates in HDF5 order
  - chunk_index_dim2 (8 bytes)
  - ... (one per dimension, excluding element size)

**Internal Node** (signature "BTIN"):
- Contains child node pointers and separator keys
- Not needed for small datasets (depth=0)

# Algorithm

1. Parse v2 B-tree header to get root node address and depth
2. If depth=0, root is leaf node - read all chunk records
3. If depth>0, traverse internal nodes to find leaf nodes
4. Read each chunk into output array using chunk_address and chunk_indices

# Dimension Handling

Chunk indices in v2 B-tree are stored in HDF5 dimension order (reversed from Julia).
For a 2D Julia array (rows × cols), v2 B-tree stores:
- record[0] = chunk_address
- record[1] = row_chunk_index (Julia dim 1)
- record[2] = col_chunk_index (Julia dim 2)

These are chunk indices (not element indices). Multiply by chunk_size to get element offset.
"""
function read_v2btree_chunks(f::JLDFile, v::Array, dataspace, rr,
                              layout::DataLayout, filters,
                              header_offset, ndims::Int)

    # Parse v2 B-tree chunk index header
    header = read_v2btree_chunk_index_header(f, layout.data_offset)

    # Get array and chunk dimensions
    array_dims_julia = size(v)
    chunk_dims_hdf5 = layout.chunk_dimensions[1:ndims]
    chunk_dims_julia = UInt64.(collect(reverse(chunk_dims_hdf5)))

    # Calculate chunk size for unfiltered chunks
    chunk_size_bytes = UInt64(prod(chunk_dims_julia) * sizeof(eltype(v)))

    # Traverse tree to collect all chunk records
    chunks = if header.depth == 0
        # Root is leaf node - read all chunks directly
        read_v2btree_leaf_node(f, header.root_node_address, header.record_size,
                               header.num_records_root, ndims, chunk_size_bytes,
                               filters)
    else
        # Need to traverse internal nodes (not implemented yet)
        throw(UnsupportedFeatureException("V2 B-tree with depth > 0 not yet implemented"))
    end

    # Read each chunk into array
    for chunk in chunks
        read_chunk_into_array!(f, v, chunk.address, chunk.size,
                              chunk.coords, chunk_dims_julia,
                              filters, chunk.filter_mask, rr)
    end

    return v
end

"""
    V2BTreeHeader

Header structure for v2 B-tree.

Fields match HDF5 specification exactly:
- signature: "BTHD" (0x44485442)
- version: Should be 0
- type: 10 for chunked dataset storage
- node_size: Size of tree nodes in bytes
- record_size: Size of each record in bytes
- depth: Tree depth (0 = root is leaf)
- split_percent: Percentage full for splitting
- merge_percent: Percentage full for merging
- root_node_address: Address of root node
- num_records_root: Number of records in root
- total_records: Total records in entire tree
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
    root_node_address::UInt64
    num_records_root::UInt16
    total_records::UInt64
    checksum::UInt32
end
define_packed(V2BTreeHeader)

V2BTreeHeader(type::UInt8, args...) = V2BTreeHeader(
    V2BTREE_HEADER_SIGNATURE, 0x0, type, args...)

# Signature constant for v2 B-tree header
const V2BTREE_HEADER_SIGNATURE = htol(0x44485442)  # "BTHD"

"""
    V2BTreeChunkRecord

Chunk record extracted from v2 B-tree leaf node.

Contains all information needed to read a chunk:
- address: File offset of chunk data
- size: Size of chunk in bytes (calculated from chunk dimensions and element type)
- filter_mask: Bitmask of which filters were applied
- coords: Chunk coordinates in Julia order, 1-based (for read_chunk_into_array!)
"""
struct V2BTreeChunkRecord
    address::RelOffset
    size::UInt64
    filter_mask::UInt32
    coords::Vector{Int}
end

"""
    read_v2btree_chunk_index_header(f, header_addr)

Parse v2 B-tree chunk index header at given address.

Returns V2BTreeHeader struct with all header fields.
"""
function read_v2btree_chunk_index_header(f::JLDFile, header_addr)
    # Convert RelOffset to absolute file offset if needed
    offset = header_addr isa RelOffset ? fileoffset(f, header_addr) : header_addr
    seek(f.io, offset)

    # Read entire header using define_packed
    hdr = jlread(f.io, V2BTreeHeader)

    # Validate signature and version
    hdr.signature == V2BTREE_HEADER_SIGNATURE ||
        throw(InvalidDataException("Invalid V2 B-tree header signature: expected BTHD, got 0x$(string(hdr.signature, base=16))"))
    hdr.version == 0 ||
        throw(UnsupportedVersionException("Unsupported V2 B-tree version $(hdr.version)"))

    return hdr
end

"""
    read_v2btree_leaf_node(f, node_addr, record_size, num_records, ndims, chunk_size_bytes, filters)

Read all chunk records from a v2 B-tree leaf node.

# Record Format for Type 10 (Chunked Data)

Each record contains:
- chunk_address (8 bytes)
- chunk_index_dim_N (8 bytes) for each spatial dimension
- (element size dimension is NOT stored - always 0)

For a 2D dataset, record_size = 24 bytes:
- chunk_address (8)
- chunk_index_dim1 (8) - row chunk index in HDF5 order
- chunk_index_dim2 (8) - col chunk index in HDF5 order

# Coordinate Conversion

V2 B-tree stores chunk indices (0-based) in HDF5 order.
read_chunk_into_array! expects chunk indices (1-based) in Julia order.

Example for 2D array:
- V2 B-tree record: [chunk_addr, row_chunk_idx, col_chunk_idx] (HDF5 order, 0-based)
- Needs conversion to: [col_chunk_idx+1, row_chunk_idx+1] (Julia order, 1-based)

Returns Vector{V2BTreeChunkRecord} with all chunks from this node.
"""
function read_v2btree_leaf_node(f::JLDFile, node_addr::UInt64, record_size::UInt16,
                                 num_records::UInt16, ndims::Int, chunk_size_bytes::UInt64,
                                 filters)
    seek(f.io, node_addr)

    # Read and verify node signature
    sig = jlread(f.io, UInt32)
    sig == htol(0x464C5442) || throw(InvalidDataException("Invalid V2 B-tree leaf node signature: expected BTLF (0x464C5442), got 0x$(string(sig, base=16))"))

    # Read node header
    version = jlread(f.io, UInt8)
    type_byte = jlread(f.io, UInt8)

    # Calculate number of chunk index fields
    # Unfiltered: record_size = 8 (address) + 8*N (chunk indices)
    # Filtered: record_size = 8 (address) + 4 (size) + 4 (filter_mask) + 8*N (chunk indices)
    # For 2D unfiltered: record_size = 24 = 8 + 8*2
    # For 2D filtered: record_size = 32 = 8 + 4 + 4 + 8*2
    header_bytes = if !isempty(filters.filters)
        16  # address + size + filter_mask
    else
        8   # address only
    end
    num_index_dims = (record_size - header_bytes) ÷ 8

    # Read all records
    chunks = Vector{V2BTreeChunkRecord}()

    for i in 1:num_records
        # Read chunk address
        chunk_addr_value = jlread(f.io, UInt64)
        chunk_addr = RelOffset(chunk_addr_value)

        # Read chunk size and filter mask if filters are present
        chunk_size = chunk_size_bytes
        filter_mask = UInt32(0)

        if !isempty(filters.filters)
            # Filtered record format: address, size, filter_mask, indices
            chunk_size = Int(jlread(f.io, UInt32))
            filter_mask = jlread(f.io, UInt32)
        end

        # Read chunk indices (in HDF5 order, 0-based)
        chunk_indices_hdf5_0based = UInt64[jlread(f.io, UInt64) for _ in 1:num_index_dims]

        # Convert chunk indices to Julia order and 1-based
        # V2 B-tree stores in HDF5 order (slowest to fastest varying)
        # Need to reverse for Julia order (fastest to slowest varying)
        chunk_coords_julia = reverse(chunk_indices_hdf5_0based .+ 1)

        # Store chunk record
        push!(chunks, V2BTreeChunkRecord(chunk_addr, chunk_size, filter_mask,
                                        Int.(chunk_coords_julia)))
    end

    return chunks
end
