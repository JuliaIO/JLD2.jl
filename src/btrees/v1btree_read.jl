# V1 B-tree Reading Functions
# Functions for reading B-tree nodes and chunks

function read_v1btree(f::JLDFile, offset::RelOffset; dimensionality = -1)
    io = f.io
    seek(io, fileoffset(f, offset))

    signature = jlread(io, UInt32)
    signature == V1_BTREE_NODE_SIGNATURE || throw(InvalidDataException("Signature does not match."))

    # 0 for internal node, 1 for chunked datasets
    node_type = jlread(io, UInt8)
    if node_type == 1 && dimensionality == -1
        throw(ArgumentError("Passing the dataset dimensionality is required for decoding chunked dataset btrees"))
    end

    # level of node. 0 implies leaf node
    node_level = jlread(io, UInt8)
    # how many entries are used
    entries_used = jlread(io, UInt16)
    # maximum value appears to be the one from superblock
    # but this is irrelevant for reading
    left_sibling = jlread(io, RelOffset)
    right_sibling = jlread(io, RelOffset)

    if node_type == 0
        children = RelOffset[]
        for _ = 1:entries_used
            # First 8 bytes is reference into name list for fast traversal (not needed here)
            skip(io, 8)
            push!(children, jlread(io, RelOffset))
        end
        links = @NamedTuple{link_name_offset::UInt64, obj_header_address::RelOffset}[]
        for child in children
            if node_level > 0
                append!(links, read_v1btree(f, child))
            else
                append!(links, read_symbol_table_node(f, child))
            end
        end
        return links
    else
        children = Any[]
        # Read keys and children in the correct V1BTree format (interleaved)
        for entry_idx = 1:entries_used
            # Read key in V1ChunkKey format: chunk_size, filter_mask, then indices
            chunk_size = Int(jlread(io, UInt32))
            filter_mask = Int(jlread(io, UInt32))

            # Read exactly dimensionality indices (dimensionality includes element size dimension)
            idx = jlread(io, UInt64, Int(dimensionality)) .% Int |> splat(tuple)

            # Read child offset (comes immediately after the key)
            child_offset = jlread(io, RelOffset)
            push!(children, (; offset=child_offset, chunk_size, filter_mask, idx))
        end
        chunks = Any[]
        for child in children
            if node_level > 0
                # This is an internal node, so children point to other B-tree nodes
                append!(chunks, read_v1btree(f, child.offset; dimensionality))
            else
                # This is a leaf node, so children point to actual chunk data
                push!(chunks, child)
            end
        end
        return chunks
    end


end

# V1 B-tree Functions for Groups
# Reading V1 B-trees used for old-style group indexing

const SYMBOL_TABLE_NODE_SIGNATURE = htol(0x444f4e53) # UInt8['S', 'N', 'O', 'D']

"""
    read_symbol_table_node(f, offset)

Read a symbol table node which contains links to objects in old-style groups.
Symbol table nodes are the leaf nodes of group B-trees.
"""
function read_symbol_table_node(f, offset)
    io = f.io
    seek(io, fileoffset(f, offset))

    signature = jlread(io, UInt32)
    signature == SYMBOL_TABLE_NODE_SIGNATURE || throw(InvalidDataException("Signature does not match."))

    version = jlread(io, UInt8)
    skip(io, 1)
    num_symbols = jlread(io, UInt16)
    links = @NamedTuple{link_name_offset::UInt64, obj_header_address::RelOffset}[]

    for _=1:num_symbols
        link_name_offset =  jlread(io, Length)
        obj_header_address = jlread(io, RelOffset)
        skip(io, 24)
        push!(links, (; link_name_offset, obj_header_address))
    end
    return links
end
