# Chunked Arrays

JLD2.jl provides experimental support for chunked arrays, which break large datasets into smaller chunks for efficient storage and partial access patterns. This feature is particularly useful for large multi-dimensional arrays that exceed memory limits or when you only need to access portions of the data.

## What are Chunked Arrays?

Chunked arrays split large datasets into smaller, regularly-sized pieces called "chunks." Each chunk is stored separately in the HDF5 file and can be:

- **Compressed independently** for better storage efficiency
- **Accessed individually** without loading the entire dataset

## Basic Usage

### Writing Chunked Arrays

The simplest way to create chunked arrays is using the `chunk` keyword argument:

```@example chunks
using JLD2

# Create a large 2D array
data = rand(1000, 2000)

# Save with chunking - chunks of 100×200 elements each
jldopen("chunked_data.jld2", "w") do f
    f["large_array"] = data
    write(f, "chunked_array", data; chunk=(100, 200))
end

rm("chunked_data.jld2", force=true) # hide
nothing # hide
```

### Chunking with Compression

Combine chunking with compression for optimal storage:

```@example chunks
# Create repetitive data that compresses well
pattern_data = repeat([1.0, 2.0, 3.0, 4.0], 250, 500)

jldopen("compressed_chunks.jld2", "w") do f
    # Regular storage
    f["uncompressed"] = pattern_data

    # Chunked with compression
    write(f, "compressed_chunked", pattern_data;
          chunk=(100, 200), compress=Deflate())
end

# Check file sizes
info_uncompressed = stat("compressed_chunks.jld2")
println("File size: $(info_uncompressed.size) bytes")
rm("compressed_chunks.jld2", force=true) # hide
nothing # hide
```

## Reading Chunked Arrays

### Standard Reading

Chunked arrays can be read normally - chunking is transparent to the user:

```@example chunks
# Create chunked data
data = collect(reshape(1:120, 10, 12))

jldopen("read_example.jld2", "w") do f
    write(f, "chunked", data; chunk=(5, 6))
end

# Read normally - chunking is transparent
result = jldopen("read_example.jld2", "r") do f
    f["chunked"]
end

println("Original data size: $(size(data))")
println("Read data size: $(size(result))")
println("Data matches: $(data == result)")

rm("read_example.jld2", force=true) # hide
nothing # hide
```

### Accessing Individual Chunks

For advanced use cases, access individual chunks without loading the entire array:

```@example chunks
# Create test data
test_data = collect(reshape(1.0:60.0, 6, 10))

jldopen("chunk_access.jld2", "w") do f
    write(f, "data", test_data; chunk=(3, 5))
end

# Access chunks individually
jldopen("chunk_access.jld2", "r") do f
    chunked = JLD2.get_chunked_array(f, "data")

    println("Chunk dimensions: $(JLD2.chunk_dimensions(chunked))")
    println("Total chunks: $(JLD2.num_chunks(chunked))")
    println("Chunk grid: $(JLD2.chunk_grid_size(chunked))")

    # Access first chunk
    first_chunk = chunked[1, 1]
    println("First chunk data:\n$(first_chunk.data)")
    println("Chunk position: $(first_chunk.indices)")
end

rm("chunk_access.jld2", force=true) # hide
nothing # hide
```

### Iterating Over Chunks

Process large datasets chunk by chunk to manage memory usage:

```@example chunks
# Create larger dataset
large_data = collect(reshape(1:200, 10, 20))

jldopen("iterate_chunks.jld2", "w") do f
    write(f, "large", large_data; chunk=(5, 10))
end

jldopen("iterate_chunks.jld2", "r") do f
    chunked = JLD2.get_chunked_array(f, "large")

    println("Processing $(length(chunked)) chunks...")

    total_sum = 0.0
    for (i, chunk) in enumerate(chunked)
        chunk_sum = sum(chunk.data)
        total_sum += chunk_sum
        println("Chunk $i: sum = $chunk_sum, size = $(size(chunk.data))")
    end

    println("Total sum across all chunks: $total_sum")
    println("Verification - direct sum: $(sum(large_data))")
end

rm("iterate_chunks.jld2", force=true) # hide
nothing # hide
```

## Advanced Features

### Error Handling and Validation

JLD2 validates chunk parameters and provides helpful error messages:

```@example chunks
invalid_data = [1, 2, 3, 4, 5]

try
    jldopen("validation.jld2", "w") do f
        # This will fail - wrong number of chunk dimensions
        write(f, "data", invalid_data; chunk=(2, 3))
    end
catch e
    println("Expected error: $(typeof(e))")
    println("Message: $(e.msg)")
end

# Clean up any created file
rm("validation.jld2", force=true)
nothing # hide
```

## Performance Considerations

### Choosing Chunk Sizes

Optimal chunk sizes depend on your access patterns:

- **Sequential access**: Use larger chunks (closer to 64KB-1MB per chunk)
- **Random access**: Use smaller chunks for faster individual chunk reads
- **Compression**: Smaller chunks compress less efficiently but allow finer-grained access

```@example chunks
# Example: comparing different chunk strategies
data_matrix = rand(1000, 1000)

jldopen("chunk_strategies.jld2", "w") do f
    # Strategy 1: Large chunks for sequential access
    write(f, "sequential", data_matrix; chunk=(500, 500))

    # Strategy 2: Smaller chunks for random access
    write(f, "random_access", data_matrix; chunk=(100, 100))

    # Strategy 3: Strip chunks for row-wise access
    write(f, "row_strips", data_matrix; chunk=(50, 1000))
end

println("Different chunking strategies saved successfully")

rm("chunk_strategies.jld2", force=true)
nothing # hide
```

## When to Use Chunking

**Use chunking when:**

- Working with arrays larger than available memory
- Need to access only portions of large datasets
- Want to apply compression to reduce file size
- Building incremental processing pipelines

**Avoid chunking when:**

- Arrays are small (< 64KB)
- Always need to access the complete dataset
- Working with 1D arrays (minimal benefit)

## Compatibility

Chunked arrays created by JLD2 are fully compatible with:

- **HDF5 ecosystem tools** (`h5dump`, `h5debug`)
- **Python h5py** for cross-language access
- **Other HDF5 libraries** in C/C++, R, etc.

The implementation follows the HDF5 specification for V1 B-tree indexed chunked datasets.

## Current Status

⚠️ **Note**: Chunked array support is experimental. While the core functionality is stable, API and advanced features are still under development.
