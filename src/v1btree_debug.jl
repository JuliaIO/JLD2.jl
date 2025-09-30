# V1 B-tree Debug Utilities
# Functions for inspecting and pretty-printing V1 B-tree structure

"""
    print_v1btree(f::JLDFile, offset::RelOffset, dimensionality::UInt8)

Pretty print the V1 B-tree structure starting at the given offset.
This function mimics the output format of h5debug for comparison.
Uses granular reading approach that prints information as it's read.
"""
function print_v1btree(f::JLDFile, offset::RelOffset, dimensionality::UInt8)
    println("Reading signature at address $(offset.offset) (rel)")

    io = f.io
    seek(io, fileoffset(f, offset))

    # Read and validate signature immediately
    signature = jlread(io, UInt32)
    if signature == htol(0x45455254)  # "TREE"
        println("✅ Valid V1 B-tree signature: TREE")
    else
        println("❌ Invalid signature: 0x$(string(signature, base=16, pad=8))")
        print_raw_bytes(f, offset, 32)
        return
    end

    # Read header fields one by one with immediate printing
    node_type = jlread(io, UInt8)
    println("Tree type ID:                                      $(node_type == 1 ? "H5B_CHUNK_ID" : "H5B_SNODE_ID") (node_type=$node_type)")

    if node_type != 1
        println("❌ Expected node_type=1 for chunked datasets, got $node_type")
        return
    end

    node_level = jlread(io, UInt8)
    println("Level:                                             $node_level")

    entries_used = jlread(io, UInt16)
    println("Number of children (max):                          $entries_used ($(calculate_max_entries_theoretical(dimensionality)))")

    left_sibling = jlread(io, RelOffset)
    left_addr = left_sibling == UNDEFINED_ADDRESS ? "18446744073709551615" : string(left_sibling.offset)
    println("Address of left sibling:                           $left_addr")

    right_sibling = jlread(io, RelOffset)
    right_addr = right_sibling == UNDEFINED_ADDRESS ? "18446744073709551615" : string(right_sibling.offset)
    println("Address of right sibling:                          $right_addr")

    # Calculate key size for this dimensionality
    key_size = 4 + 4 + 8 * (dimensionality + 1)  # chunk_size + filter_mask + indices
    println("Size of raw (disk) key:                            $key_size")

    # Calculate total node size (estimated)
    header_size = 4 + 1 + 1 + 2 + 2*8  # signature + type + level + entries + siblings
    total_entries_size = entries_used * (key_size + 8) + key_size  # entries*(key+child) + final_key
    estimated_node_size = header_size + total_entries_size
    println("Size of node:                                      $estimated_node_size")

    println("Dirty flag:                                        False")

    # Read keys and children with detailed output for each child
    children_offsets = RelOffset[]

    try
        for i in 1:entries_used
            println("Child $(i-1)...")

            # Read left key (describes this child)
            chunk_size = jlread(io, UInt32)
            filter_mask = jlread(io, UInt32)
            indices = Vector{UInt64}()
            for j in 1:(dimensionality )
                push!(indices, jlread(io, UInt64))
            end

            # Read child address
            child_offset = jlread(io, RelOffset)
            push!(children_offsets, child_offset)
            println("   Address:                                        $(child_offset.offset)")

            # Print left key
            println("   Left Key:                                      ")
            println("      Chunk size:                                  $chunk_size bytes")
            println("      Filter mask:                                 0x$(string(filter_mask, base=16, pad=8))")
            logical_offset = "{" * join(string.(indices), ", ") * "}"
            println("      Logical offset:                              $logical_offset")

            # Look ahead to read right key for this child
            current_pos = position(io)

            if i < entries_used
                # There's another entry, so we can read its left key as our right key
                next_chunk_size = jlread(io, UInt32)
                next_filter_mask = jlread(io, UInt32)
                next_indices = Vector{UInt64}()
                for j in 1:(dimensionality)
                    push!(next_indices, jlread(io, UInt64))
                end

                println("   Right Key:                                     ")
                println("      Chunk size:                                  $next_chunk_size bytes")
                println("      Filter mask:                                 0x$(string(next_filter_mask, base=16, pad=8))")
                next_logical_offset = "{" * join(string.(next_indices), ", ") * "}"
                println("      Logical offset:                              $next_logical_offset")

                # Reset position to continue normal reading
                seek(io, current_pos)
            else
                # This is the last child, read the final boundary key
                final_chunk_size = jlread(io, UInt32)
                final_filter_mask = jlread(io, UInt32)
                final_indices = Vector{UInt64}()
                for j in 1:(dimensionality)
                    push!(final_indices, jlread(io, UInt64))
                end

                println("   Right Key:                                     ")
                println("      Chunk size:                                  $final_chunk_size bytes")
                println("      Filter mask:                                 0x$(string(final_filter_mask, base=16, pad=8))")
                final_logical_offset = "{" * join(string.(final_indices), ", ") * "}"
                println("      Logical offset:                              $final_logical_offset")
            end
        end

        # If this is an internal node, recursively print children
        if node_level > 0
            println("\n--- Recursively printing child nodes ---")
            for (i, child_offset) in enumerate(children_offsets)
                println("\n=== Child $i at offset $(child_offset.offset) ===")
                print_v1btree(f, child_offset, dimensionality)
            end
        end

    catch e
        println("❌ Error reading keys/children: $e")
        println("Current file position: $(position(io))")
        println("Raw bytes at current position:")
        print_raw_bytes_at_position(f, position(io), 64)
    end
end

"""
    read_v1btree_node_debug(f::JLDFile, offset::RelOffset, dimensionality::UInt8)

Read a V1 B-tree node with detailed error handling for debugging.
"""
function read_v1btree_node_debug(f::JLDFile, offset::RelOffset, dimensionality::UInt8)
    io = f.io
    seek(io, fileoffset(f, offset))

    # Read signature
    signature = jlread(io, UInt32)
    if signature != htol(0x45455254)  # "TREE"
        throw(InvalidDataException("Invalid V1 B-tree signature: 0x$(string(signature, base=16))"))
    end

    # Read header
    node_type = jlread(io, UInt8)
    node_level = jlread(io, UInt8)
    entries_used = jlread(io, UInt16)
    left_sibling = jlread(io, RelOffset)
    right_sibling = jlread(io, RelOffset)

    # Read keys and children
    keys = Vector{V1ChunkKey}()
    children = Vector{RelOffset}()

    # Read interleaved keys and children
    for i in 1:entries_used
        key = read_chunk_key_debug(io, dimensionality)
        push!(keys, key)

        child = jlread(io, RelOffset)
        push!(children, child)
    end

    # Read final key
    final_key = read_chunk_key_debug(io, dimensionality)
    push!(keys, final_key)

    return V1BTreeNode(node_type, node_level, entries_used, left_sibling, right_sibling, keys, children)
end

"""
    read_chunk_key_debug(io::IO, dimensionality::UInt8)

Read a chunk key with debug information.
"""
function read_chunk_key_debug(io::IO, dimensionality::UInt8)
    chunk_size = jlread(io, UInt32)
    filter_mask = jlread(io, UInt32)

    indices = Vector{UInt64}()
    for i in 1:(dimensionality + 1)
        index = jlread(io, UInt64)
        push!(indices, index)
    end

    return V1ChunkKey(chunk_size, filter_mask, indices)
end

"""
    print_v1btree_node(node::V1BTreeNode, offset::RelOffset, dimensionality::UInt8)

Pretty print a single V1 B-tree node in h5debug style.
"""
function print_v1btree_node(node::V1BTreeNode, offset::RelOffset, dimensionality::UInt8)
    println("Tree type ID:                                      H5B_CHUNK_ID")
    println("Size of node:                                      $(calculate_node_size(node))")
    println("Size of raw (disk) key:                            $(4 + 4 + 8 * (dimensionality + 1))")
    println("Dirty flag:                                        False")
    println("Level:                                             $(node.node_level)")

    # Format sibling addresses
    left_addr = node.left_sibling == UNDEFINED_ADDRESS ? "18446744073709551615" : string(node.left_sibling.offset)
    right_addr = node.right_sibling == UNDEFINED_ADDRESS ? "18446744073709551615" : string(node.right_sibling.offset)

    println("Address of left sibling:                           $left_addr")
    println("Address of right sibling:                          $right_addr")
    println("Number of children (max):                          $(node.entries_used) ($(calculate_max_entries_theoretical(dimensionality)))")

    # Print children with left/right keys
    for i in 1:node.entries_used
        println("Child $(i-1)...")
        println("   Address:                                        $(node.children[i].offset)")

        # Left key (this child's key)
        left_key = node.keys[i]
        println("   Left Key:                                      ")
        println("      Chunk size:                                  $(left_key.chunk_size) bytes")
        println("      Filter mask:                                 0x$(string(left_key.filter_mask, base=16, pad=8))")

        # Format logical offset (indices excluding the datatype offset)
        logical_offset = "{" * join(string.(left_key.indices[1:end-1]), ", ") * "}"
        println("      Logical offset:                              $logical_offset")

        # Right key (next key in sequence)
        right_key = node.keys[i+1]
        println("   Right Key:                                     ")
        println("      Chunk size:                                  $(right_key.chunk_size) bytes")
        println("      Filter mask:                                 0x$(string(right_key.filter_mask, base=16, pad=8))")

        # Format logical offset for right key
        right_logical_offset = "{" * join(string.(right_key.indices[1:end-1]), ", ") * "}"
        println("      Logical offset:                              $right_logical_offset")
    end
end

"""
    print_raw_bytes(f::JLDFile, offset::RelOffset, count::Int)

Print raw bytes at the given offset for debugging.
"""
function print_raw_bytes(f::JLDFile, offset::RelOffset, count::Int)
    io = f.io
    seek(io, fileoffset(f, offset))

    bytes = read(io, count)
    println("Raw bytes (hex):")
    for i in 1:min(count, length(bytes))
        if (i-1) % 16 == 0
            print("$(lpad(string(i-1, base=16), 4, '0')): ")
        end
        print("$(lpad(string(bytes[i], base=16), 2, '0')) ")
        if i % 16 == 0 || i == length(bytes)
            println()
        end
    end
end

"""
    print_raw_bytes_at_position(f::JLDFile, pos::Int, count::Int)

Print raw bytes at the current file position for debugging.
"""
function print_raw_bytes_at_position(f::JLDFile, pos::Int, count::Int)
    io = f.io
    current_pos = position(io)
    seek(io, pos)

    bytes = read(io, count)
    println("Raw bytes at position $pos (hex):")
    for i in 1:min(count, length(bytes))
        if (i-1) % 16 == 0
            print("$(lpad(string(i-1, base=16), 4, '0')): ")
        end
        print("$(lpad(string(bytes[i], base=16), 2, '0')) ")
        if i % 16 == 0 || i == length(bytes)
            println()
        end
    end

    # Restore original position
    seek(io, current_pos)
end

"""
    calculate_max_entries_theoretical(dimensionality::UInt8, node_size::Int = 4096)

Calculate theoretical maximum entries for comparison with h5debug output.
"""
function calculate_max_entries_theoretical(dimensionality::UInt8, node_size::Int = 4096)
    header_size = 4 + 1 + 1 + 2 + 2*8  # signature + type + level + entries + siblings
    key_size = 4 + 4 + 8 * (dimensionality + 1)
    child_size = 8
    entry_size = key_size + child_size
    extra_key_size = key_size

    available_space = node_size - header_size - extra_key_size
    max_entries = available_space ÷ entry_size

    return max_entries
end

"""
    debug_v1btree_file(filename::String, dataset_name::String)

Debug a complete JLD2 file's V1 B-tree structure.
"""
function debug_v1btree_file(filename::String, dataset_name::String)
    println("🔍 Debugging V1 B-tree in file: $filename")
    println("📊 Dataset: $dataset_name")
    println("=" ^ 60)

    jldopen(filename, "r") do f
        # Get the dataset's header messages
        println("📋 Dataset header messages:")
        JLD2.print_header_messages(f, dataset_name)

        # Find the DataLayout message
        dataset_link = f.root_group.written_links[dataset_name]
        dataset_offset = if dataset_link isa HardLink
            dataset_link.target
        else
            error("Only HardLinks are supported for V1 B-tree debugging")
        end

        data_layout = nothing
        hmitr = JLD2.HeaderMessageIterator(f, dataset_offset)
        for msg in hmitr
            if msg.type == JLD2.HmDataLayout
                layout = DataLayout(f, msg)
                if layout.storage_type == JLD2.LcChunked
                    data_layout = layout
                    break
                end
            end
        end

        if data_layout === nothing
            println("❌ No chunked DataLayout found")
            return
        end

        println("\n🌲 V1 B-tree structure:")
        println("Root offset: $(data_layout.data_offset)")
        println("Dimensionality: $(data_layout.dimensionality)")

        # Print the V1 B-tree
        try
            print_v1btree(f, h5offset(f, data_layout.data_offset), data_layout.dimensionality)
        catch e
            println("❌ Error reading V1 B-tree: $e")
        end
    end
end
