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
    header = JLD2.BTrees.read_v2btree_header(f, h5offset(f, layout.data_offset))

    # Get array and chunk dimensions
    array_dims_julia = size(v)
    chunk_dims_hdf5 = layout.chunk_dimensions[1:ndims]
    chunk_dims_julia = Int.(reverse(chunk_dims_hdf5))

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
        # Chunk coords are already 1-based Julia coordinates - compute chunk_start directly
        chunk_grid_idx = CartesianIndex(chunk.coords...)
        chunk_start = chunk_start_from_index(chunk_grid_idx, Tuple(chunk_dims_julia))

        # Use the unified chunk reading function
        read_and_assign_chunk!(f, v, chunk_start, chunk.address, Int(chunk.size),
                              Tuple(chunk_dims_julia), rr, filters, chunk.filter_mask)
    end

    return v
end

# JLD2.BTrees.V2BTreeHeader now defined in v2btree_types.jl to avoid duplication

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

macro validate(obj, field)
    quote
        let
            val = getproperty($(esc(obj)), $(esc(field)))
            req_val = required_value(typeof($(esc(obj))), $(field))
            val == req_val || throw(InvalidDataException("""
                Invalid $($field) in " * string(typeof($($obj))).
                Expected: $req_val
                Found: $val
            """))
        end
    end
end

struct AA; ver::Float64; end
required_value(::Type{AA}, s::Symbol) = s==:ver ? 42.0 : 0

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
function read_v2btree_leaf_node(f::JLDFile, node_addr::RelOffset, record_size::UInt16,
                                 num_records::UInt16, ndims::Int, chunk_size_bytes::UInt64,
                                 filters)
    seek(f.io, fileoffset(f, node_addr))

    # Read and verify node signature
    sig = jlread(f.io, UInt32)
    sig == htol(0x464C5442) || throw(InvalidDataException("Invalid V2 B-tree leaf node signature: expected BTLF (0x464C5442), got 0x$(string(sig, base=16))"))

    # Read node header
    version = jlread(f.io, UInt8)
    type_byte = jlread(f.io, UInt8)

    # Calculate number of chunk index fields
    # Unfiltered: record_size = 8 (address) + 8*N (chunk indices)
    # Filtered: record_size = 8 (address) + 8 (size) + 4 (filter_mask) + 8*N (chunk indices)
    # For 2D unfiltered: record_size = 24 = 8 + 8*2
    # For 2D filtered: record_size = 32 = 8 + 8 + 4 + 8*2
    header_bytes = if !isempty(filters.filters)
        20  # address + size + filter_mask
    else
        8   # address only
    end
    num_index_dims = (record_size - header_bytes) ÷ 8

    # Read all records
    chunks = Vector{V2BTreeChunkRecord}()

    for i in 1:num_records
        # Read chunk address
        chunk_addr = jlread(f.io, RelOffset)

        # Read chunk size and filter mask if filters are present
        chunk_size = chunk_size_bytes
        filter_mask = UInt32(0)

        if !isempty(filters.filters)
            # Filtered record format: address, size, filter_mask, indices
            chunk_size = Int(jlread(f.io, UInt64))
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
