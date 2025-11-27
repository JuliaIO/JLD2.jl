# Chunked Arrays

## What is Chunking?

Chunking divides large arrays into fixed-size rectangular blocks stored separately in the file. This enables:

- **Partial I/O**: Read only the chunks you need without loading the entire array
- **Compression**: Each chunk compresses independently, improving both speed and ratio
- **Memory efficiency**: Process arrays larger than RAM by working chunk-by-chunk
- **Optimized access patterns**: Align chunks with how you'll access the data

### Visual Example

Consider a small 4×6 array chunked into 2×3 blocks:

```@example chunks
using JLD2 # hide
# Create a 4×6 array with values 1-24
data = collect(reshape(1:24, 4, 6))
println("Complete 4×6 array:")
println(data)
println()

# Show how it divides into chunks with chunk=(2,3)
println("Chunked with chunk=(2,3) → creates a 2×2 grid of chunks:")
println()
for row in 1:2
    for col in 1:2
        r_start, r_end = 2(row-1)+1, 2row
        c_start, c_end = 3(col-1)+1, 3col
        println("Chunk ($row,$col) = data[$r_start:$r_end, $c_start:$c_end]:")
        println(data[r_start:r_end, c_start:c_end])
        println()
    end
end
nothing # hide
```

When you save with `chunk=(2,3)`, the file stores these 4 chunks separately. Reading `data[2:3, 4:5]` loads only chunk (2,2) - not the entire array.

## Quick Start

### Writing Chunked Arrays

```@example chunks
using JLD2

data = rand(1000, 1000)

# Simple chunking with write()
jldopen("example.jld2", "w") do f
    write(f, "data", data; chunk=(100, 100))
end

# With compression
jldopen("compressed.jld2", "w") do f
    write(f, "data", data; chunk=(100, 100), compress=Deflate())
end

rm("example.jld2", force=true) # hide
rm("compressed.jld2", force=true) # hide
nothing # hide
```

### Reading Chunked Arrays

Reading is transparent - chunked arrays work like normal arrays:

```@example chunks
data = collect(reshape(1.0:60.0, 6, 10))

jldopen("test.jld2", "w") do f
    write(f, "data", data; chunk=(3, 5))
end

result = jldopen("test.jld2", "r") do f
    f["data"]  # Loads entire array
end

println("Data matches: ", result == data)
rm("test.jld2", force=true) # hide
nothing # hide
```

## Writing Chunked Arrays

### Basic Syntax

Three equivalent ways to write chunked arrays:

```@example chunks
data = rand(Float32, 100, 50)

jldopen("methods.jld2", "w") do f
    # Method 1: write() with chunk keyword
    write(f, "v1", data; chunk=(20, 10))

    # Method 2: setindex! with chunk keyword
    f["v2", chunk=(20, 10)] = data

    # Method 3: WriteChunkedArray wrapper
    wca = WriteChunkedArray(data; chunk=(20, 10))
    f["v3"] = wca
end

rm("methods.jld2", force=true) # hide
nothing # hide
```

### Compression

Apply compression filters to chunks:

```@example chunks
# Create repetitive integer data
# Make a 1000×1000 array with repeating pattern of small integers
base_pattern = reshape([1, 2, 3, 4, 5, 6, 7, 8], 2, 4)
compressible_data = repeat(base_pattern, inner=(500, 250))  # 1000×1000 Int64 array

# Write with different compression settings
jldopen("uncompressed.jld2", "w") do f
    write(f, "data", compressible_data; chunk=(100, 50))
end

jldopen("deflate.jld2", "w") do f
    write(f, "data", compressible_data; chunk=(100, 50), compress=Deflate())
end

jldopen("shuffle_deflate.jld2", "w") do f
    write(f, "data", compressible_data; chunk=(100, 50),
          compress=[Shuffle(), Deflate()])
end

# Compare file sizes
raw_size = sizeof(compressible_data) / 1024
uncompressed_size = filesize("uncompressed.jld2") / 1024
deflate_size = filesize("deflate.jld2") / 1024
shuffle_deflate_size = filesize("shuffle_deflate.jld2") / 1024

println("Raw data size: ", round(raw_size, digits=1), " KB")
println("Uncompressed file: ", round(uncompressed_size, digits=1), " KB")
println("Deflate only: ", round(deflate_size, digits=1), " KB (",
        round(raw_size/deflate_size, digits=1), "× smaller)")
println("Shuffle + Deflate: ", round(shuffle_deflate_size, digits=1), " KB (",
        round(raw_size/shuffle_deflate_size, digits=1), "× smaller)")

rm("uncompressed.jld2", force=true) # hide
rm("deflate.jld2", force=true) # hide
rm("shuffle_deflate.jld2", force=true) # hide
nothing # hide
```

### Extensible Arrays

Create datasets that can grow along specified dimensions:

```@example chunks
timeseries = rand(Float32, 100, 50)

jldopen("extensible.jld2", "w") do f
    # Can grow in first dimension (time), fixed in second dimension
    wca = WriteChunkedArray(timeseries;
                           chunk=(10, 50),
                           maxshape=(nothing, 50))
    f["timeseries"] = wca
end

rm("extensible.jld2", force=true) # hide
nothing # hide
```

The `maxshape` parameter determines which dimensions can grow:

- `maxshape=nothing` - fixed size (default)
- `maxshape=(nothing, 100)` - first dim unlimited, second fixed at 100
- `maxshape=(nothing, nothing)` - both dims unlimited

## Reading Chunked Arrays

### Standard Loading

```@example chunks
demo_data = collect(reshape(1:120, 10, 12))

jldopen("read.jld2", "w") do f
    write(f, "data", demo_data; chunk=(5, 6))
end

# Load entire array
result = jldopen("read.jld2", "r") do f
    f["data"]
end

println("Size: ", size(result))
println("Matches: ", result == demo_data)
rm("read.jld2", force=true) # hide
nothing # hide
```

### Chunk-by-Chunk Access

For large datasets, access individual chunks without loading everything:

```@example chunks
# Use a small 4×6 array for demonstration
small_data = collect(reshape(1:24, 4, 6))

jldopen("chunks.jld2", "w") do f
    write(f, "data", small_data; chunk=(2, 3))
end

jldopen("chunks.jld2", "r") do f
    chunked = JLD2.get_chunked_array(f, "data")

    println("Dataset: ", size(small_data))
    println("Chunk size: ", JLD2.chunk_dimensions(chunked))
    println("Chunk grid: ", JLD2.chunk_grid_size(chunked))
    println()

    # Access a specific chunk
    chunk = chunked[2, 1]  # Bottom-left chunk
    println("Accessing chunk (2,1):")
    println(chunk.data)
    println("\nThis is data[3:4, 1:3] without loading the full array")
end

rm("chunks.jld2", force=true) # hide
nothing # hide
```

### Iterating Chunks

Process large datasets chunk-by-chunk to limit memory usage:

```@example chunks
# Save the 4×6 array
jldopen("iterate.jld2", "w") do f
    write(f, "data", small_data; chunk=(2, 3))
end

jldopen("iterate.jld2", "r") do f
    chunked = JLD2.get_chunked_array(f, "data")

    println("Processing all chunks:")
    for (i, chunk) in enumerate(chunked)
        println("\nChunk $i:")
        println(chunk.data)
        println("Sum = ", sum(chunk.data))
    end

    println("\n---")
    println("Total sum across chunks: ", sum(c -> sum(c.data), chunked))
    println("Verified: ", sum(small_data))
end

rm("iterate.jld2", force=true) # hide
nothing # hide
```

## Choosing Chunk Sizes

Good chunk sizes balance I/O performance with access patterns:

**General Guidelines:**

- **Target 10-100 KB per chunk** for most use cases
- **Match your access pattern:**
  - Row access → `(1, ncols)` or `(small, ncols)`
  - Column access → `(nrows, 1)` or `(nrows, small)`
  - Block access → `(sqrt(n), sqrt(n))`
- **Larger chunks** compress better but reduce access flexibility
- **Smaller chunks** increase metadata overhead

```@example chunks
data = rand(Float32, 10000, 1000)

# For row-wise access (reading rows at a time)
row_chunks = (1, 1000)

# For column-wise access (Julia's default)
col_chunks = (10000, 1)

# Balanced for block access
block_chunks = (500, 500)

println("Row chunk size: ", prod(row_chunks) * sizeof(Float32), " bytes")
println("Block chunk size: ", prod(block_chunks) * sizeof(Float32), " bytes")
nothing # hide
```

## Advanced Features

### Fill Values

Specify values for unallocated sparse data:

```@example chunks
# Sparse array with mostly zeros
sparse_data = zeros(Float32, 100, 100)
sparse_data[25:50, 25:50] .= rand(Float32, 26, 26)

jldopen("sparse.jld2", "w") do f
    wca = WriteChunkedArray(sparse_data;
                           chunk=(25, 25),
                           fill_value=0.0f0)
    f["sparse"] = wca
end

rm("sparse.jld2", force=true) # hide
nothing # hide
```

### Manual Index Type Selection

JLD2 automatically selects the optimal chunk index type, but you can override:

```@example chunks
data = rand(Float32, 100, 100)

jldopen("manual.jld2", "w") do f
    # Force V1 B-tree indexing
    wca = WriteChunkedArray(data;
                           chunk=(20, 20),
                           indexing=:v1btree)
    f["data"] = wca
end

rm("manual.jld2", force=true) # hide
nothing # hide
```

Available index types:

- `:single_chunk` - Single chunk (no chunking overhead)
- `:implicit_index` - Simple fixed grid with fill value
- `:fixed_array` - Default for fixed-size arrays
- `:extensible_array` - For arrays with one unlimited dimension
- `:v2btree` - For multiple unlimited dimensions
- `:v1btree` - Legacy format (default for compatibility)

## Compatibility

Chunked arrays are fully compatible with:

- **HDF5 tools** (`h5dump`, `h5ls`, `h5debug`)
- **Python h5py** for cross-language access
- **Other HDF5 libraries** (C, C++, R, MATLAB)

```python
# Reading JLD2 chunked arrays in Python
import h5py

with h5py.File('data.jld2', 'r') as f:
    data = f['dataset'][:]
    print(f"Shape: {data.shape}")
    print(f"Chunks: {f['dataset'].chunks}")
```

## When to Use Chunking

**Use chunking when:**

- Arrays are larger than available memory
- You need partial array access (slicing)
- Compression is beneficial
- Building incremental processing pipelines

**Avoid chunking when:**

- Arrays are small (< 64 KB)
- You always load the complete dataset
- Working with 1D vectors (minimal benefit)

## Performance Tips

```@example chunks
# Good: Balanced chunks (~50KB)
data = rand(Float32, 1000, 1000)
jldopen("good.jld2", "w") do f
    write(f, "data", data; chunk=(100, 125))  # 50KB chunks
end

# Bad: Tiny chunks (overhead dominates)
jldopen("bad.jld2", "w") do f
    write(f, "data", data; chunk=(5, 5))  # Only 100 bytes per chunk!
end

rm("good.jld2", force=true) # hide
rm("bad.jld2", force=true) # hide
nothing # hide
```

**Tips:**

1. Benchmark different chunk sizes for your workload
2. Use compression for archival (trades speed for space)
3. Only use unlimited dimensions when actually needed
4. Pre-allocate with `fill_value` for sparse datasets
5. Keep chunks between 10 KB and 1 MB

## API Reference

```@docs
JLD2.Chunking.WriteChunkedArray
JLD2.Chunking.write_chunked
```

## See Also

- [Compression](@ref) - Details on compression filters
- [HDF5 Compatibility](@ref) - Cross-platform file access
- [Advanced Usage](@ref) - Performance optimization techniques
