"""
JLD2 Chunked Array Writing Examples

This file demonstrates the chunked array writing API with various configurations.

NOTE: Phase 0 implementation - actual writing not yet implemented.
      Examples show API usage; write operations will throw UnsupportedFeatureException.

Run with: julia --project examples/chunked_writing_examples.jl
"""

using JLD2
using Printf

println("="^70)
println("JLD2 Chunked Array Writing API Examples")
println("="^70)
println("\nNOTE: Phase 0 - API design complete, writing implementation pending")
println("Examples demonstrate API usage. Write operations will report configuration.")
println("="^70)

#-------------------------------------------------------------------------------
# Example 1: Basic Fixed Array Chunking
#-------------------------------------------------------------------------------
println("\n📦 Example 1: Basic Fixed Array Chunking")
println("-"^70)

data1 = reshape(1.0f0:10000.0f0, 100, 100)
ca1 = WriteChunkedArray(data1, chunks=(10, 10))

println("Data size: $(size(ca1))")
println("Chunk size: $(ca1.chunks)")
println("Maxshape: $(ca1.maxshape)")
println("Inferred index type: $(JLD2.select_chunk_index_type(size(ca1.data), ca1.chunks, ca1.maxshape, ca1.fill_value))")

# Would write like this:
# jldsave("example1_fixed.jld2"; data=ca1)
println("✓ Configuration ready (would use Fixed Array indexing - Type 3)")

#-------------------------------------------------------------------------------
# Example 2: Image Stack (Extensible Array)
#-------------------------------------------------------------------------------
println("\n📸 Example 2: Image Stack - Extensible in Time Dimension")
println("-"^70)

# Simulate image stack: 512×512 pixels, 10 time points
image_stack = rand(Float32, 512, 512, 10)
ca2 = WriteChunkedArray(image_stack,
                       chunks=(128, 128, 1),  # Process one time point at a time
                       maxshape=(512, 512, nothing))  # Can add more time points

println("Data size: $(size(ca2))")
println("Chunk size: $(ca2.chunks)")
println("Maxshape: $(ca2.maxshape) (nothing = unlimited)")
println("Inferred index type: $(JLD2.select_chunk_index_type(size(ca2.data), ca2.chunks, ca2.maxshape, ca2.fill_value))")
println("✓ Configuration ready (would use Extensible Array indexing - Type 4)")

#-------------------------------------------------------------------------------
# Example 3: Large Dataset with Compression
#-------------------------------------------------------------------------------
println("\n🗜️  Example 3: Compressed Large Dataset")
println("-"^70)

large_data = randn(Float32, 1000, 1000)  # ~4MB
ca3 = WriteChunkedArray(large_data,
                       chunks=(100, 100),
                       filters=:gzip)  # Could also use :gzip => 9 for level 9

println("Data size: $(size(ca3))")
println("Chunk size: $(ca3.chunks)")
println("Compression: $(ca3.filters)")
println("Number of chunks: $(prod(cld.(size(ca3.data), ca3.chunks)))")
println("Inferred index type: $(JLD2.select_chunk_index_type(size(ca3.data), ca3.chunks, ca3.maxshape, ca3.fill_value))")
println("✓ Configuration ready (would use Fixed Array + gzip compression)")

#-------------------------------------------------------------------------------
# Example 4: Sparse Data with Fill Value
#-------------------------------------------------------------------------------
println("\n⬜ Example 4: Sparse Data with Fill Value")
println("-"^70)

# Create sparse data (mostly zeros)
sparse_data = zeros(Float32, 1000, 1000)
sparse_data[100:200, 100:200] .= rand(Float32, 101, 101)  # Small non-zero region

ca4 = WriteChunkedArray(sparse_data,
                       chunks=(100, 100),
                       fill_value=0.0f0)

nonzero_fraction = count(!iszero, sparse_data) / length(sparse_data)
println("Data size: $(size(ca4))")
println("Chunk size: $(ca4.chunks)")
println("Fill value: $(ca4.fill_value)")
println("Non-zero fraction: $(@sprintf("%.1f%%", nonzero_fraction * 100))")
println("Inferred index type: $(JLD2.select_chunk_index_type(size(ca4.data), ca4.chunks, ca4.maxshape, ca4.fill_value))")
println("✓ Configuration ready (would optimize storage for sparse data)")

#-------------------------------------------------------------------------------
# Example 5: Multi-dimensional Growth (V2 B-tree)
#-------------------------------------------------------------------------------
println("\n🌳 Example 5: Multi-dimensional Growth - V2 B-tree")
println("-"^70)

# 3D simulation grid that can grow in all dimensions
grid_data = rand(Float32, 100, 100, 20)
ca5 = WriteChunkedArray(grid_data,
                       chunks=(25, 25, 5),
                       maxshape=(nothing, nothing, nothing))  # All unlimited

println("Data size: $(size(ca5))")
println("Chunk size: $(ca5.chunks)")
println("Maxshape: $(ca5.maxshape) (all unlimited)")
println("Inferred index type: $(JLD2.select_chunk_index_type(size(ca5.data), ca5.chunks, ca5.maxshape, ca5.fill_value))")
println("✓ Configuration ready (would use V2 B-tree indexing - Type 5)")

#-------------------------------------------------------------------------------
# Example 6: Auto Chunking
#-------------------------------------------------------------------------------
println("\n🤖 Example 6: Automatic Chunk Size Selection")
println("-"^70)

test_sizes = [
    (100, 50),
    (1000, 1000),
    (10000, 10000),
    (100, 100, 100)
]

for data_size in test_sizes
    element_size = sizeof(Float32)
    auto_chunks = JLD2.auto_chunk_size(data_size, element_size)
    chunk_bytes = prod(auto_chunks) * element_size
    total_mb = prod(data_size) * element_size / 1024^2

    println("\nData size: $data_size ($(round(total_mb, digits=2)) MB)")
    println("  Auto chunks: $auto_chunks")
    println("  Chunk size: $(round(chunk_bytes / 1024, digits=1)) KB")
    println("  Total chunks: $(prod(cld.(data_size, auto_chunks)))")
end

println("\n✓ Auto-chunking targets ~32KB chunks for balanced I/O")

#-------------------------------------------------------------------------------
# Example 7: Different Access Patterns
#-------------------------------------------------------------------------------
println("\n📊 Example 7: Chunk Size for Different Access Patterns")
println("-"^70)

matrix_data = rand(Float32, 1000, 1000)

# Row-oriented access
ca_row = WriteChunkedArray(matrix_data, chunks=(1, 1000))
println("Row-oriented chunks: $(ca_row.chunks)")
println("  Use case: Reading entire rows at once")

# Column-oriented access (Julia's default)
ca_col = WriteChunkedArray(matrix_data, chunks=(1000, 1))
println("\nColumn-oriented chunks: $(ca_col.chunks)")
println("  Use case: Reading entire columns at once (Julia column-major)")

# Balanced chunks
ca_bal = WriteChunkedArray(matrix_data, chunks=(100, 100))
println("\nBalanced chunks: $(ca_bal.chunks)")
println("  Use case: Random block access, good for most cases")

println("\n✓ Choose chunks based on your typical access pattern")

#-------------------------------------------------------------------------------
# Example 8: Manual Index Type Override
#-------------------------------------------------------------------------------
println("\n🔧 Example 8: Manual Chunk Index Type Override")
println("-"^70)

data8 = rand(Float32, 100, 100)

# Automatic selection
ca_auto = WriteChunkedArray(data8, chunks=(10, 10))
auto_type = JLD2.select_chunk_index_type(size(ca_auto.data), ca_auto.chunks, ca_auto.maxshape, ca_auto.fill_value)

# Manual override
ca_manual = WriteChunkedArray(data8, chunks=(10, 10), indexing=:v2btree)

println("Automatic selection: $auto_type")
println("Manual override: $(ca_manual.indexing)")
println("✓ Use manual override for specific requirements")

#-------------------------------------------------------------------------------
# Example 9: Low-Level write_chunked Function
#-------------------------------------------------------------------------------
println("\n⚙️  Example 9: Low-Level write_chunked Function")
println("-"^70)

println("The write_chunked function provides direct control:")
println("""
  jldopen("file.jld2", "w") do f
      write_chunked(f, "dataset", data;
                   chunks=(100, 100),
                   maxshape=(nothing, 1000),
                   filters=:gzip => 6)
  end
""")
println("✓ Useful for programmatic control and integration")

#-------------------------------------------------------------------------------
# Summary
#-------------------------------------------------------------------------------
println("\n" * "="^70)
println("Summary of Examples")
println("="^70)
println("""
1. ✓ Fixed Array (Type 3) - Default for fixed-size arrays
2. ✓ Extensible Array (Type 4) - For time series and growable data
3. ✓ Compression - Reduce file size with filters
4. ✓ Sparse Data - Optimize storage with fill values
5. ✓ V2 B-tree (Type 5) - Multi-dimensional growth
6. ✓ Auto Chunking - Let JLD2 choose optimal chunk size
7. ✓ Access Patterns - Align chunks with usage
8. ✓ Manual Override - Expert control when needed
9. ✓ Low-Level API - Direct write_chunked function

Implementation Status: Phase 0 Complete (API Design)
Next Phases: Implement actual writing for each chunk index type
""")
println("="^70)

#-------------------------------------------------------------------------------
# Interoperability Demo (Conceptual)
#-------------------------------------------------------------------------------
println("\n🔄 Interoperability with h5py")
println("-"^70)
println("""
Files written by JLD2 will be readable with h5py:

  Python:
  ```python
  import h5py
  with h5py.File('output.jld2', 'r') as f:
      data = f['chunked'][:]
      print(f"Chunks: {f['chunked'].chunks}")
  ```

  Julia:
  ```julia
  data = jldopen("output.jld2", "r") do f
      f["chunked"]
  end
  ```

✓ Cross-language compatibility through HDF5 format
""")

println("\n" * "="^70)
println("Examples Complete!")
println("="^70)
println("\nFor more information:")
println("  - Documentation: docs/src/chunked_writing.md")
println("  - Tests: test/chunked_writing_api_test.jl")
println("  - Source: src/chunked_writing_api.jl")
println("="^70)
