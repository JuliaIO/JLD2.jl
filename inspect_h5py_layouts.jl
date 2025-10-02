#!/usr/bin/env julia

using JLD2

function inspect_chunk_layout(filename, dataset_name)
    println("\n" * "="^70)
    println("Inspecting: $filename - '$dataset_name'")
    println("="^70)

    jldopen(filename, "r") do f
        # Get the dataset
        dataset = f[dataset_name]

        # Print header messages
        println("\nHeader Messages:")
        JLD2.print_header_messages(f, dataset_name)
    end
end

# Inspect each test file
files = [
    ("h5py_single_chunk.h5", "single"),
    ("h5py_fixed_array.h5", "fixed"),
    ("h5py_extensible.h5", "ext"),
    ("h5py_v2btree.h5", "btree"),
    ("h5py_compressed.h5", "comp"),
    ("h5py_fillvalue.h5", "filled"),
]

for (filename, dataset_name) in files
    if isfile(filename)
        inspect_chunk_layout(filename, dataset_name)
    else
        println("File not found: $filename")
    end
end

println("\n" * "="^70)
println("Inspection complete!")
println("="^70)
