#!/usr/bin/env julia

"""
Test v2 B-tree chunked array reading.

Expected results:
- h5py shape: (25, 15)
- JLD2 shape: (15, 25) - dimensions are reversed (Julia column-major vs HDF5 row-major)
- Sum: 70125.0
- First element: 0.0
- Last element: 274.0
"""

using JLD2, Test

println("Testing v2 B-tree chunk indexing implementation...")
println("=" ^ 70)

filename = "test_v4_v2btree.h5"

println("\nReading file: $filename")

try
    data = jldopen(filename) do f
        f["v4_v2btree"]
    end

    println("\n✓ File read successfully!")
    println("\nArray info:")
    println("  Size: $(size(data))")
    println("  Element type: $(eltype(data))")
    println("  Sum: $(sum(data))")
    println("  First element: $(data[1,1])")
    println("  Last element: $(data[end,end])")

    println("\nRunning tests...")

    # Test array dimensions (JLD2 reverses dimensions from HDF5)
    @test size(data) == (15, 25)
    println("  ✓ Array size correct (JLD2 dimension order)")

    # Test sum (from h5py)
    @test sum(data) ≈ 70125.0
    println("  ✓ Sum matches h5py")

    # Test specific elements (accounting for transposed layout)
    @test data[1,1] == 0.0f0
    println("  ✓ First element correct")

    @test data[end,end] == 274.0f0
    println("  ✓ Last element correct")

    # Test a few middle elements
    # Note: indices are (col, row) in JLD2's reversed dimension order
    @test data[5,5] ≈ 44.0f0
    @test data[10,10] ≈ 99.0f0
    println("  ✓ Middle elements correct")

    println("\n" * "=" ^ 70)
    println("✅ All tests passed!")
    println("\nV2 B-tree implementation is working correctly!")

catch e
    println("\n❌ Error reading file:")
    println("  $(typeof(e)): $e")
    if isa(e, MethodError) || isa(e, UndefVarError)
        println("\nStack trace:")
        for (exc, bt) in Base.catch_stack()
            showerror(stdout, exc, bt)
            println()
        end
    end
    rethrow(e)
end
