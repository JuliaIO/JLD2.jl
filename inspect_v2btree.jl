#!/usr/bin/env julia

"""
Inspect v2 B-tree header structure in test file.
Based on HDF5 spec Section III.A.2 "Disk Format: Level 1A2 - Version 2 B-trees"
"""

using Printf

function inspect_v2btree_header(filename, header_offset)
    println("Inspecting v2 B-tree header at offset $header_offset")
    println("=" ^ 70)

    open(filename, "r") do io
        # Seek to header (absolute offset from h5debug)
        seek(io, header_offset)

        # Read signature (4 bytes)
        sig_bytes = read(io, 4)
        sig = UInt32(sig_bytes[1]) | (UInt32(sig_bytes[2]) << 8) |
              (UInt32(sig_bytes[3]) << 16) | (UInt32(sig_bytes[4]) << 24)
        sig_str = String(Char.(sig_bytes))
        println("Signature: 0x$(string(sig, base=16, pad=8)) = \"$sig_str\"")

        # Expected signatures for v2 B-tree:
        # - Header: "BTHD" (0x44485442)
        # - Internal Node: "BTIN" (0x4E495442)
        # - Leaf Node: "BTLF" (0x464C5442)

        # Read version (1 byte)
        version = read(io, UInt8)
        println("Version: $version")

        # Read type (1 byte)
        # Type 5 = v2 B-tree for storing indirectly accessed, non-filtered dataspace chunks
        type_byte = read(io, UInt8)
        println("Type: $type_byte")

        # Read node size (4 bytes)
        node_size = read(io, UInt32)
        println("Node size: $node_size bytes")

        # Read record size (2 bytes)
        record_size = read(io, UInt16)
        println("Record size: $record_size bytes")

        # Read depth (2 bytes)
        depth = read(io, UInt16)
        println("Depth: $depth")

        # Read split percent (1 byte)
        split_percent = read(io, UInt8)
        println("Split percent: $split_percent")

        # Read merge percent (1 byte)
        merge_percent = read(io, UInt8)
        println("Merge percent: $merge_percent")

        # Read root node address (8 bytes)
        root_addr = read(io, UInt64)
        println("Root node address: $root_addr (0x$(string(root_addr, base=16, pad=16)))")

        # Read number of records in root (2 bytes)
        num_records_root = read(io, UInt16)
        println("Number of records in root: $num_records_root")

        # Read total number of records (8 bytes)
        total_records = read(io, UInt64)
        println("Total number of records: $total_records")

        # Checksum (4 bytes)
        checksum = read(io, UInt32)
        println("Checksum: 0x$(string(checksum, base=16, pad=8))")

        println()
        println("Expected total chunks: $(cld(25, 5) * cld(15, 5)) = 15 chunks")
        println("  (25×15 array with 5×5 chunks = 5×3 chunks)")

        return root_addr
    end
end

function inspect_v2btree_node(filename, node_offset, record_size, depth_level=0)
    println("\n" * "=" ^ 70)
    println("Inspecting v2 B-tree node at offset $node_offset (depth level $depth_level)")
    println("=" ^ 70)

    open(filename, "r") do io
        # Seek to node (absolute offset)
        seek(io, node_offset)

        # Read signature (4 bytes)
        sig_bytes = read(io, 4)
        sig_str = String(Char.(sig_bytes))
        println("Signature: \"$sig_str\"")

        # Read version (1 byte)
        version = read(io, UInt8)
        println("Version: $version")

        # Read type (1 byte)
        type_byte = read(io, UInt8)
        println("Type: $type_byte")

        if sig_str == "BTLF"
            println("\nThis is a LEAF node - contains chunk records")
            println("Record size: $record_size bytes")

            # Calculate number of dimensions from record size
            # Record format for type 10 (chunked data):
            # - scaled offset (8 bytes per dimension)
            # For 24 bytes: 24 / 8 = 3 dimensions (includes element size)
            ndims = record_size ÷ 8
            println("Number of dimensions (including element size): $ndims")

            # Read records - need to know how many
            # Try reading a few
            println("\nFirst 5 chunk records:")
            for i in 1:min(5, 15)
                # Read chunk scaled offsets (ndims * 8 bytes)
                offsets = [read(io, UInt64) for _ in 1:ndims]
                println("  Record $i: scaled offsets = $offsets")
            end

        elseif sig_str == "BTIN"
            println("\nThis is an INTERNAL node - contains child pointers")
            # For internal nodes, read child pointers and keys

        end
    end
end

# Main execution
filename = "test_v4_v2btree.h5"
header_offset = 463  # From h5debug output

println("Inspecting $filename")
println()

root_addr = inspect_v2btree_header(filename, header_offset)

# Inspect root node
if root_addr != typemax(UInt64)
    # The root_addr is absolute (confirmed by output)
    println("\nInspecting root node...")
    inspect_v2btree_node(filename, Int(root_addr), 24, 0)
end
