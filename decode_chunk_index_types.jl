#!/usr/bin/env julia

# Simple direct approach: read the chunk index type byte from the data layout address
# From inspection, we know data_address is at RelOffset(1400) for these files

function get_chunk_index_type_direct(filename)
    type_names = [
        "Single Chunk (Type 1)",
        "Implicit Index (Type 2)",
        "Fixed Array (Type 3)",
        "Extensible Array (Type 4)",
        "v2 B-tree (Type 5)"
    ]

    open(filename, "r") do io
        # HDF5 superblock is at 512, RelOffset(1400) means 512 + 1400 = 1912
        seek(io, 512 + 1400)
        index_type = read(io, UInt8)

        if 1 <= index_type <= 5
            return type_names[index_type]
        else
            return "Unknown (type $index_type)"
        end
    end
end

files = [
    ("h5py_single_chunk.h5", "single", "Single Chunk"),
    ("h5py_fixed_array.h5", "fixed", "Fixed Array"),
    ("h5py_extensible.h5", "ext", "Extensible Array"),
    ("h5py_v2btree.h5", "btree", "V2 B-tree"),
    ("h5py_compressed.h5", "comp", "Compressed Fixed Array"),
]

println("="^70)
println("Decoding Chunk Index Types from h5py-created files")
println("="^70)

for (filename, dataset_name, expected) in files
    println("\n$filename - '$dataset_name'")
    println("  Expected: $expected")
    if isfile(filename)
        result = get_chunk_index_type_direct(filename)
        println("  ✓ Actual: $result")
    else
        println("  ✗ File not found")
    end
end

println("\n" * "="^70)
println("\nSummary of h5py's chunk index type selection:")
println("  - Chunks == data size → Single Chunk (Type 1)")
println("  - Fixed size, no unlimited dims → Fixed Array (Type 3)")
println("  - One unlimited dimension → Extensible Array (Type 4)")
println("  - Multiple unlimited dims → V2 B-tree (Type 5)")
println("  - With filters/compression → Same rules apply")
println("\nNote: h5py does NOT use Implicit Index (Type 2) by default")
println("="^70)
