# Comparative File Analysis Tools
# Tools for comparing JLD2-generated files with reference implementations
# to identify format compliance issues

using JLD2

"""
    create_reference_chunked_file(data, chunk_dims, output_path; tool="h5py")

Create a reference HDF5 file with chunking using external tools for comparison.
"""
function create_reference_chunked_file(data, chunk_dims, output_path; tool="h5py")
    if tool == "h5py"
        create_h5py_reference(data, chunk_dims, output_path)
    elseif tool == "h5dump"
        error("h5dump cannot create files, only read them")
    else
        error("Unsupported tool: $tool")
    end
end

"""
    create_h5py_reference(data, chunk_dims, output_path)

Create reference file using h5py.
"""
function create_h5py_reference(data, chunk_dims, output_path)
    # Generate Python script to create reference file
    python_script = """
import h5py
import numpy as np

def create_reference_chunked_file():
    # Recreate the data array
    data_shape = $(size(data))
    chunk_shape = $(chunk_dims)

    print(f"Creating reference file with data shape: {data_shape}, chunk shape: {chunk_shape}")

    # Create data array - use same values as JLD2 test
    if len(data_shape) == 1:
        data_array = np.arange(1, np.prod(data_shape) + 1, dtype=np.float64)
    elif len(data_shape) == 2:
        data_array = np.arange(1, np.prod(data_shape) + 1, dtype=np.float64).reshape(data_shape)
    elif len(data_shape) == 3:
        data_array = np.arange(1, np.prod(data_shape) + 1, dtype=np.float64).reshape(data_shape)
    else:
        raise ValueError(f"Unsupported dimensionality: {len(data_shape)}")

    # Create HDF5 file with chunking
    with h5py.File('$output_path', 'w') as f:
        dataset = f.create_dataset('test',
                                 data=data_array,
                                 chunks=chunk_shape,
                                 compression=None,  # No compression for cleaner comparison
                                 shuffle=False,
                                 fletcher32=False)

        print(f"Created dataset with:")
        print(f"  Shape: {dataset.shape}")
        print(f"  Chunks: {dataset.chunks}")
        print(f"  Dtype: {dataset.dtype}")
        print(f"  Layout: {dataset.is_chunked}")

    print(f"Reference file created: $output_path")

if __name__ == "__main__":
    create_reference_chunked_file()
"""

    # Write Python script
    script_path = "create_reference_$(basename(output_path)).py"
    open(script_path, "w") do io
        write(io, python_script)
    end

    println("Python script created: $script_path")
    println("Run: python $script_path")
    println("This will create: $output_path")

    return script_path
end

"""
    compare_file_structures(jld2_file, reference_file)

Compare the overall structure of two HDF5 files.
"""
function compare_file_structures(jld2_file, reference_file)
    println("=== File Structure Comparison ===")
    println("JLD2 file: $jld2_file")
    println("Reference file: $reference_file")
    println()

    # Compare file sizes
    jld2_size = filesize(jld2_file)
    ref_size = filesize(reference_file)

    println("File sizes:")
    println("  JLD2: $jld2_size bytes")
    println("  Reference: $ref_size bytes")
    println("  Difference: $(jld2_size - ref_size) bytes")
    println()

    # Test external tool compatibility
    println("External tool compatibility:")

    # Test h5dump header
    jld2_h5dump_header = test_h5dump_header(jld2_file)
    ref_h5dump_header = test_h5dump_header(reference_file)
    println("  h5dump -H:")
    println("    JLD2: $(jld2_h5dump_header ? "✅" : "❌")")
    println("    Reference: $(ref_h5dump_header ? "✅" : "❌")")

    # Test h5dump data
    jld2_h5dump_data = test_h5dump_data(jld2_file, "test")
    ref_h5dump_data = test_h5dump_data(reference_file, "test")
    println("  h5dump -d:")
    println("    JLD2: $(jld2_h5dump_data ? "✅" : "❌")")
    println("    Reference: $(ref_h5dump_data ? "✅" : "❌")")

    # Test h5debug
    jld2_h5debug = test_h5debug(jld2_file)
    ref_h5debug = test_h5debug(reference_file)
    println("  h5debug:")
    println("    JLD2: $(jld2_h5debug ? "✅" : "❌")")
    println("    Reference: $(ref_h5debug ? "✅" : "❌")")

    println()
end

"""
    test_h5dump_header(file_path)

Test if h5dump can read the file header.
"""
function test_h5dump_header(file_path)
    try
        result = run(pipeline(`h5dump -H $file_path`, stdout=devnull, stderr=devnull))
        return success(result)
    catch
        return false
    end
end

"""
    test_h5dump_data(file_path, dataset_name)

Test if h5dump can read dataset data.
"""
function test_h5dump_data(file_path, dataset_name)
    try
        result = run(pipeline(`h5dump -d /$dataset_name $file_path`, stdout=devnull, stderr=devnull))
        return success(result)
    catch
        return false
    end
end

"""
    test_h5debug(file_path)

Test if h5debug can analyze the file.
"""
function test_h5debug(file_path)
    try
        result = run(pipeline(`h5debug $file_path`, stdout=devnull, stderr=devnull))
        return success(result)
    catch
        return false
    end
end

"""
    compare_dataset_layouts(jld2_file, reference_file, dataset_name="test")

Compare the DataLayout messages of datasets in two files.
"""
function compare_dataset_layouts(jld2_file, reference_file, dataset_name="test")
    println("=== Dataset Layout Comparison ===")

    println("JLD2 file layout:")
    analyze_dataset_layout(jld2_file, dataset_name)

    println("\nReference file layout:")
    analyze_dataset_layout(reference_file, dataset_name)
end

"""
    analyze_dataset_layout(file_path, dataset_name)

Analyze the DataLayout message of a dataset.
"""
function analyze_dataset_layout(file_path, dataset_name)
    try
        jldopen(file_path, "r") do f
            JLD2.print_header_messages(f, dataset_name)
        end
    catch e
        println("Error analyzing layout: $e")
    end
end

"""
    extract_v1btree_info(file_path, dataset_name="test")

Extract V1 B-tree information from a file for comparison.
"""
function extract_v1btree_info(file_path, dataset_name="test")
    println("=== V1 B-tree Information Extraction ===")
    println("File: $file_path")
    println("Dataset: $dataset_name")

    # Find V1 B-tree locations
    btree_locations = find_hdf5_structure(file_path, "v1btree")

    if isempty(btree_locations)
        println("No V1 B-tree structures found")
        return
    end

    println("Found V1 B-tree nodes at offsets:")
    for (i, offset) in enumerate(btree_locations)
        println("  $i: 0x$(string(offset, base=16, pad=8)) ($offset)")
    end

    # Analyze each B-tree node
    for (i, offset) in enumerate(btree_locations)
        println("\n--- B-tree Node $i (offset 0x$(string(offset, base=16, pad=8))) ---")
        analyze_v1btree_node(file_path, offset)
    end
end

"""
    analyze_v1btree_node(file_path, offset)

Analyze a specific V1 B-tree node at given offset.
"""
function analyze_v1btree_node(file_path, offset)
    open(file_path, "r") do io
        seek(io, offset)

        # Read V1 B-tree node header
        signature = read(io, 4)
        if signature != b"TREE"
            println("Warning: Expected TREE signature, got: $(signature)")
            return
        end

        node_type = read(io, UInt8)
        node_level = read(io, UInt8)
        entries_used = read(io, UInt16)
        sizeof_addr = read(io, UInt8)
        sizeof_size = read(io, UInt8)

        println("Node signature: $(String(signature))")
        println("Node type: $node_type")
        println("Node level: $node_level")
        println("Entries used: $entries_used")
        println("Sizeof addr: $sizeof_addr")
        println("Sizeof size: $sizeof_size")

        # Read sibling pointers
        left_sibling = read(io, UInt64)
        right_sibling = read(io, UInt64)

        println("Left sibling: 0x$(string(left_sibling, base=16, pad=16))")
        println("Right sibling: 0x$(string(right_sibling, base=16, pad=16))")

        # For leaf nodes, show some key information
        if node_level == 0 && entries_used > 0
            println("First few keys:")
            for i in 1:min(3, entries_used)
                key_size = read(io, UInt32)  # Size of key
                # This is simplified - actual key structure depends on dimensionality
                println("  Key $i size: $key_size")
                seek(io, position(io) + key_size + 8)  # Skip key data and address
            end
        end
    end
end

"""
    comprehensive_comparison(jld2_file, reference_file, dataset_name="test")

Perform comprehensive comparison between JLD2 and reference files.
"""
function comprehensive_comparison(jld2_file, reference_file, dataset_name="test")
    println("=== Comprehensive File Comparison ===")
    println("Analyzing format compliance differences between JLD2 and reference implementations")
    println()

    # 1. Overall structure comparison
    compare_file_structures(jld2_file, reference_file)

    # 2. Dataset layout comparison
    compare_dataset_layouts(jld2_file, reference_file, dataset_name)

    # 3. V1 B-tree structure comparison
    println("\n=== V1 B-tree Structure Analysis ===")
    println("JLD2 file:")
    extract_v1btree_info(jld2_file, dataset_name)

    println("\nReference file:")
    extract_v1btree_info(reference_file, dataset_name)

    # 4. Specific recommendations
    println("\n=== Debugging Recommendations ===")
    generate_debugging_recommendations(jld2_file, reference_file)
end

"""
    generate_debugging_recommendations(jld2_file, reference_file)

Generate specific debugging recommendations based on comparison results.
"""
function generate_debugging_recommendations(jld2_file, reference_file)
    println("Based on the comparison, here are specific areas to investigate:")

    # Check basic compatibility
    jld2_data_ok = test_h5dump_data(jld2_file, "test")
    ref_data_ok = test_h5dump_data(reference_file, "test")

    if !jld2_data_ok && ref_data_ok
        println("1. 🔍 PRIORITY: h5dump cannot read JLD2 data but can read reference")
        println("   → Focus on chunk data format and addressing")
        println("   → Check chunk storage format (raw bytes vs serialized)")
        println("   → Verify chunk key format in V1 B-tree")
    end

    println("2. 🔍 Compare hex dumps of V1 B-tree nodes")
    println("   → Use: compare_hex_dumps(\"$jld2_file\", \"$reference_file\", ...)")
    println("   → Look for differences in:")
    println("     - Node headers and signatures")
    println("     - Key format and ordering")
    println("     - Address/offset calculations")

    println("3. 🔍 Examine DataLayout messages")
    println("   → Check dimension ordering (HDF5 vs Julia)")
    println("   → Verify chunk dimension values")
    println("   → Validate address field pointing to B-tree root")

    println("4. 🔍 Test with minimal cases")
    println("   → Create smallest possible chunked dataset")
    println("   → Single chunk vs multiple chunks")
    println("   → Compare chunk enumeration and storage")

    println("\n=== Recommended Investigation Commands ===")
    println("# Find V1 B-tree locations")
    println("jld2_btrees = find_hdf5_structure(\"$jld2_file\", \"v1btree\")")
    println("ref_btrees = find_hdf5_structure(\"$reference_file\", \"v1btree\")")
    println()
    println("# Compare B-tree node bytes")
    println("compare_hex_dumps(\"$jld2_file\", \"$reference_file\", offset1=jld2_btrees[1], offset2=ref_btrees[1], length=256)")
    println()
    println("# Detailed B-tree analysis")
    println("analyze_v1btree_node(\"$jld2_file\", jld2_btrees[1])")
    println("analyze_v1btree_node(\"$reference_file\", ref_btrees[1])")
end

# Export functions
export create_reference_chunked_file, compare_file_structures,
       extract_v1btree_info, comprehensive_comparison,
       compare_dataset_layouts, analyze_dataset_layout