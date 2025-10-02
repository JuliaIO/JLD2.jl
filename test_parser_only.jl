#!/usr/bin/env julia
using JLD2

println("Testing DataLayout v4 parser fixes...")
println()

test_files = [
    "test_v4_fixed_array.h5" => "v4_fixed",
    "test_v4_extensible_array.h5" => "v4_extensible",
    "test_v4_v2btree.h5" => "v4_v2btree",
]

for (filename, dataset) in test_files
    println("=" ^ 70)
    println("File: $filename / $dataset")
    println("=" ^ 70)

    try
        jldopen(filename, "r") do f
            # Get the dataset group
            grp = f[dataset]

            # Get the DataLayout via internal access
            println("Accessing DataLayout...")
            # This will fail for now since we can't read, but let's see the error
        end
    catch e
        println("\nError: $e")

        # Try to manually inspect
        println("\nManually inspecting header messages...")
        jldopen(filename, "r") do f
            JLD2.print_header_messages(f, dataset)
        end
    end
    println()
end
