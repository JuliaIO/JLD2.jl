#!/usr/bin/env julia
using JLD2

println("Testing JLD2 with DataLayout v4 files...")
println()

test_files = [
    "test_v4_single_chunk.h5" => "v4_single",
    "test_v4_fixed_array.h5" => "v4_fixed",
    "test_v4_extensible_array.h5" => "v4_extensible",
    "test_v4_v2btree.h5" => "v4_v2btree",
    "test_v4_filtered_single.h5" => "v4_filtered",
]

for (filename, dataset) in test_files
    println("=" ^ 70)
    println("File: $filename / $dataset")
    println("=" ^ 70)

    try
        jldopen(filename, "r") do f
            # Print header messages to see DataLayout version
            println("\nHeader Messages:")
            JLD2.print_header_messages(f, dataset)

            println("\nReading data...")
            data = f[dataset]
            println("✓ Successfully read!")
            println("  Shape: $(size(data))")
            println("  First values: $(data[1:min(5, length(data))])")
        end
    catch e
        println("\n❌ Error reading file:")
        showerror(stdout, e)
        println()
    end
    println()
end
