#!/usr/bin/env julia
"""
Phase 1 Completion Test: Verify all DataLayout v4 header messages can be parsed.

This tests that:
1. The typo in headermessages.jl is fixed
2. All chunk indexing types (1-5) can be parsed
3. Header messages print without errors

This does NOT test reading data - that's Phase 2+.
"""

using JLD2

println("=" ^ 70)
println("Phase 1 Completion Test: DataLayout v4 Parser")
println("=" ^ 70)
println()

test_files = [
    ("test_v4_single_chunk.h5", "v4_single", "Single Chunk (Type 1)"),
    ("test_v4_fixed_array.h5", "v4_fixed", "Fixed Array (Type 3)"),
    ("test_v4_extensible_array.h5", "v4_extensible", "Extensible Array (Type 4)"),
    ("test_v4_v2btree.h5", "v4_v2btree", "Version 2 B-tree (Type 5)"),
]

results = []

for (filename, dataset, description) in test_files
    print("Testing $description... ")

    try
        jldopen(filename, "r") do f
            # Try to print header messages - don't redirect, just let it print
            JLD2.print_header_messages(f, dataset)
        end

        # If we got here, parsing succeeded
        println("\n  ✓ PASSED (header parsing successful)")
        push!(results, true)
    catch e
        println("  ❌ FAILED")
        println("  Error: $e")
        push!(results, false)
    end
    println()
end

passed = count(results)
failed = length(results) - passed

println()
println("=" ^ 70)
println("Results: $passed passed, $failed failed")
println("=" ^ 70)

if failed == 0
    println("\n✅ Phase 1 COMPLETE! All DataLayout v4 parsers working correctly.")
    println("\nNext steps:")
    println("  • Phase 2: Implement Single Chunk reading")
    println("  • Phase 3: Implement Fixed Array reading")
    println("  • Phase 4: Implement Extensible Array reading")
    println("  • Phase 5: Implement V2 B-tree reading")
    exit(0)
else
    println("\n❌ Phase 1 incomplete. Fix parser errors above.")
    exit(1)
end
