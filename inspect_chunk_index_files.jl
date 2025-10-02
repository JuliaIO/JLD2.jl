#!/usr/bin/env julia
"""
Inspect HDF5 test files to understand chunk indexing mechanisms.

This script uses JLD2's debugging utilities to examine the DataLayout
messages and chunk indexing information in the test files.
"""

using JLD2

# Color codes for output
const BOLD = "\033[1m"
const GREEN = "\033[32m"
const BLUE = "\033[34m"
const RESET = "\033[0m"

function print_separator(title)
    println()
    println("=" ^ 70)
    println("$BOLD$title$RESET")
    println("=" ^ 70)
end

function inspect_file(filename, dataset_name)
    print_separator("$filename / $dataset_name")

    try
        # Try to open and read with JLD2
        println("\n$(BLUE)Opening with JLD2...$(RESET)")
        jldopen(filename, "r") do f
            if haskey(f, dataset_name)
                data = f[dataset_name]
                println("  ✓ Successfully read dataset")
                println("  Shape: $(size(data))")
                println("  Type: $(eltype(data))")
                println("  First few values: $(data[1:min(5, length(data))])")

                # Try to print header messages
                println("\n$(BLUE)Header Messages:$(RESET)")
                try
                    JLD2.print_header_messages(f, dataset_name)
                catch e
                    println("  Error printing header messages: $e")
                end
            else
                println("  ✗ Dataset not found: $dataset_name")
                println("  Available keys: $(keys(f))")
            end
        end
    catch e
        println("  $(GREEN)Error opening/reading file:$(RESET)")
        println("  $e")

        # Show stack trace for debugging
        if isa(e, Exception)
            for (exc, bt) in Base.catch_stack()
                showerror(stdout, exc, bt)
                println()
            end
        end
    end
end

function main()
    println("$(BOLD)Inspecting HDF5 Chunk Indexing Test Files with JLD2$(RESET)")

    # List of test files and their datasets
    test_files = [
        ("test_single_chunk_index.h5", "single_chunk", "Single Chunk Index (Type 1)"),
        ("test_implicit_index.h5", "implicit_chunks", "Implicit Index (Type 2)"),
        ("test_fixed_array_index.h5", "fixed_array", "Fixed Array Index (Type 3)"),
        ("test_extensible_array_index.h5", "extensible_array", "Extensible Array Index (Type 4)"),
        ("test_v2_btree_index.h5", "v2_btree", "Version 2 B-tree Index (Type 5)"),
        ("test_v1_btree_index.h5", "v1_btree", "Version 1 B-tree Index (Legacy)"),
        ("test_filtered_single_chunk.h5", "filtered_single", "Filtered Single Chunk"),
    ]

    for (filename, dataset, description) in test_files
        if isfile(filename)
            inspect_file(filename, dataset)
        else
            println("\n$(GREEN)File not found: $filename$(RESET)")
        end
    end

    print_separator("Summary")
    println("\nInspection complete. Check output above for:")
    println("  • Which files JLD2 can currently read")
    println("  • Error messages for unsupported features")
    println("  • DataLayout message versions")
    println("  • Chunk indexing types used")
    println()
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
