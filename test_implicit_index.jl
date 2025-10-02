using JLD2

println("Testing Type 2: Implicit Index reading...")

# Test file: test_v4_implicit.h5
# Array: 30×10 (h5py) → 10×30 (Julia)
# Chunks: 3×2 (h5py) → 2×3 (Julia)
# Data: 0.0 to 299.0

try
    data = jldopen("test_v4_implicit.h5", "r") do f
        f["implicit"]
    end

    println("✓ File read successfully")
    println("Array size: ", size(data))
    println("Expected: (10, 30)")

    # Verify dimensions
    if size(data) == (10, 30)
        println("✓ Dimensions correct")
    else
        println("✗ Dimensions incorrect!")
    end

    # Check first 10 elements
    first_10 = data[1, 1:10]
    println("First 10 elements: ", first_10)
    expected_first = Float32[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
    if first_10 == expected_first
        println("✓ First 10 elements correct")
    else
        println("✗ First 10 elements incorrect!")
        println("  Expected: ", expected_first)
    end

    # Check last element
    last_elem = data[10, 30]
    println("Last element [10,30]: ", last_elem)
    if last_elem == 299.0f0
        println("✓ Last element correct")
    else
        println("✗ Last element incorrect! Expected: 299.0")
    end

    # Sample a few middle elements
    println("Sample middle elements:")
    println("  data[5, 15] = ", data[5, 15], " (expected: ", 144.0f0, ")")
    println("  data[1, 20] = ", data[1, 20], " (expected: ", 190.0f0, ")")

    # Verify all data is sequential
    all_data = vec(data)
    expected_all = Float32.(0:299)
    if all_data == expected_all
        println("✓ All data matches expected sequence!")
    else
        # Find first mismatch
        for i in 1:length(all_data)
            if all_data[i] != expected_all[i]
                println("✗ First mismatch at index $i: got $(all_data[i]), expected $(expected_all[i])")
                break
            end
        end
    end

    println("\n✅ Implicit Index implementation test PASSED")

catch e
    println("\n❌ Test FAILED with error:")
    println(e)
    for (exc, bt) in Base.catch_stack()
        showerror(stdout, exc, bt)
        println()
    end
end
