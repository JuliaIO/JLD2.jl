#!/usr/bin/env julia

"""
Decode v2 B-tree records by comparing with h5ls output.

From h5ls, we know the first few chunks are at addresses:
2048, 2148, 3148, 2248, 2348, 3248, 2448, 2548, 3348, 2648, 2748, 3448, 2848, 2948, 3548

And chunk indices are:
[0, 0, 0], [0, 5, 0], [0, 10, 0], [5, 0, 0], [5, 5, 0], [5, 10, 0], ...
"""

using Printf

filename = "test_v4_v2btree.h5"
node_offset = 4096

# Expected from h5ls
expected_addresses = [2048, 2148, 3148, 2248, 2348, 3248, 2448, 2548, 3348, 2648, 2748, 3448, 2848, 2948, 3548]
expected_indices = [
    (0, 0, 0), (0, 5, 0), (0, 10, 0),
    (5, 0, 0), (5, 5, 0), (5, 10, 0),
    (10, 0, 0), (10, 5, 0), (10, 10, 0),
    (15, 0, 0), (15, 5, 0), (15, 10, 0),
    (20, 0, 0), (20, 5, 0), (20, 10, 0),
]

println("Decoding v2 B-tree records...")
println()

open(filename, "r") do io
    seek(io, node_offset)

    # Skip node header (signature + version + type)
    skip(io, 4 + 1 + 1)

    println("Reading all 15 records:")
    println("Record | Field1     | Field2     | Field3     | Expected Addr | Expected Indices")
    println("-" ^ 90)

    for i in 1:15
        field1 = read(io, UInt64)
        field2 = read(io, UInt64)
        field3 = read(io, UInt64)

        expected_addr = expected_addresses[i]
        expected_idx = expected_indices[i]

        @printf("%6d | 0x%08x | 0x%08x | 0x%08x | %13d | %s\n",
                i, field1, field2, field3, expected_addr, expected_idx)
    end

    println()
    println("Analysis:")
    println("  Field1 values look like addresses (2048 = 0x0800)")
    println("  Let me check if field1 matches expected addresses...")
    println()

    # Re-read to compare
    seek(io, node_offset + 6)  # Skip header again

    matches = 0
    for i in 1:15
        field1 = read(io, UInt64)
        skip(io, 16)  # Skip field2 and field3

        if field1 == expected_addresses[i]
            matches += 1
        end
    end

    println("  Field1 matched expected addresses: $matches/15 times")

    if matches == 15
        println("\n✓ Field1 is the chunk address!")
        println("\nNow checking fields 2 and 3...")

        # Re-read to decode fields 2 and 3
        seek(io, node_offset + 6)

        for i in 1:3  # Just first 3 for analysis
            addr = read(io, UInt64)
            field2 = read(io, UInt64)
            field3 = read(io, UInt64)

            idx = expected_indices[i]

            println("\n  Chunk $i:")
            println("    Address: $addr")
            println("    Field2: 0x$(string(field2, base=16, pad=16)) = $field2")
            println("    Field3: 0x$(string(field3, base=16, pad=16)) = $field3")
            println("    Expected indices: $idx")

            # Try to decode field2 and field3 as scaled chunk indices
            # For a 25×15 array with 5×5 chunks:
            # Chunk (0,0) → indices [0,0,0]
            # Chunk (0,1) → indices [0,5,0]
            # Chunk (0,2) → indices [0,10,0]

            # In HDF5 order (reversed): [col, row, 0]
            # field2 might be the "key" - scaled chunk offset
        end
    end
end
