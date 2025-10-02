# Chunked Array Writing

JLD2 provides automatic chunked dataset writing with intelligent chunk index type selection matching h5py's behavior.

## Overview

Chunked storage divides large arrays into fixed-size blocks (chunks), enabling:
- **Efficient partial I/O**: Read/write specific regions without loading entire array
- **Compression**: Apply filters to individual chunks for better compression ratios
- **Extensibility**: Grow datasets along unlimited dimensions
- **Performance**: Optimize storage layout for your access patterns

## Quick Start

```julia
using JLD2

data = rand(Float32, 1000, 1000)

# Basic chunking - JLD2 automatically selects Fixed Array indexing
jldsave("output.jld2";
    chunked = WriteChunkedArray(data, chunks=(100, 100))
)

# Extensible dataset - can grow in first dimension
timeseries = rand(Float32, 100, 50)
jldsave("timeseries.jld2";
    data = WriteChunkedArray(timeseries,
                            chunks=(10, 50),
                            maxshape=(nothing, 50))
)
```

## API Reference

### WriteChunkedArray

```julia
WriteChunkedArray(data; chunks, maxshape=nothing, fill_value=nothing,
                  indexing=nothing, filters=nothing)
```

Wrap an array for chunked writing to JLD2 files.

**Parameters:**
- `data::AbstractArray` - Array data to write
- `chunks::Union{NTuple{N,Int}, Symbol}` - Chunk dimensions or `:auto`
- `maxshape::Union{Nothing, NTuple{N,Union{Int,Nothing}}}` - Maximum dimensions
  - `nothing` = fixed size dataset
  - `nothing` in tuple = unlimited dimension (e.g., `(nothing, 100)`)
- `fill_value::Union{Nothing, T}` - Fill value for unallocated chunks
- `indexing::Union{Symbol, Nothing}` - Manual override: `:single_chunk`, `:implicit_index`,
  `:fixed_array`, `:extensible_array`, or `:v2btree`
- `filters` - Compression filters (`:gzip`, `:gzip => level`, filter objects, or pipeline)

### write_chunked

```julia
write_chunked(f::JLDFile, name::String, data; kwargs...)
```

Low-level function to write chunked datasets. Same parameters as `WriteChunkedArray`.

**Example:**
```julia
jldopen("file.jld2", "w") do f
    write_chunked(f, "dataset", data;
                 chunks=(100, 100),
                 maxshape=(nothing, 1000))
end
```

## Chunk Index Type Selection

JLD2 automatically selects the optimal chunk indexing strategy based on your data characteristics,
matching h5py's behavior for cross-compatibility:

| Configuration | Index Type | Use Case |
|---------------|------------|----------|
| `chunks == data_size` | Single Chunk (Type 1) | Small arrays, no chunking overhead |
| No unlimited dims | Fixed Array (Type 3) | Fixed-size chunked arrays (most common) |
| 1 unlimited dim | Extensible Array (Type 4) | Time series, growable along one axis |
| 2+ unlimited dims | V2 B-tree (Type 5) | Multi-dimensional growth, sparse data |

### Manual Override

```julia
# Force V2 B-tree even for fixed size
jldsave("file.jld2";
    data = WriteChunkedArray(data, chunks=(10, 10), indexing=:v2btree)
)
```

## Usage Examples

### Fixed Array (Default)

```julia
data = rand(Float32, 1000, 1000)
jldsave("fixed.jld2";
    data = WriteChunkedArray(data, chunks=(100, 100))
)
# Uses Type 3: Fixed Array (most efficient for fixed-size data)
```

### Extensible Array (Time Series)

```julia
# Initial data
timeseries = rand(Float32, 100, 50)

# Can grow in first dimension (time)
jldsave("timeseries.jld2";
    data = WriteChunkedArray(timeseries,
                            chunks=(10, 50),
                            maxshape=(nothing, 50))
)
# Uses Type 4: Extensible Array
```

### V2 B-tree (Multi-dimensional Growth)

```julia
# 3D volume that can grow in all dimensions
volume = rand(Float32, 100, 100, 20)

jldsave("volume.jld2";
    data = WriteChunkedArray(volume,
                            chunks=(25, 25, 5),
                            maxshape=(nothing, nothing, nothing))
)
# Uses Type 5: V2 B-tree
```

### With Compression

```julia
data = randn(Float32, 1000, 1000)

# Gzip compression (default level 6)
jldsave("compressed1.jld2";
    data = WriteChunkedArray(data, chunks=(100, 100), filters=:gzip)
)

# Custom compression level
jldsave("compressed2.jld2";
    data = WriteChunkedArray(data, chunks=(100, 100), filters=:gzip => 9)
)
```

### Sparse Data with Fill Value

```julia
# Mostly zeros
sparse_data = zeros(Float32, 1000, 1000)
sparse_data[100:200, 100:200] .= rand(Float32, 101, 101)

jldsave("sparse.jld2";
    data = WriteChunkedArray(sparse_data,
                            chunks=(100, 100),
                            fill_value=0.0f0)
)
```

### Auto Chunking

```julia
large_data = rand(Float32, 10000, 10000)

# JLD2 automatically determines optimal chunk size (~32KB chunks)
jldsave("auto.jld2";
    data = WriteChunkedArray(large_data, chunks=:auto)
)
```

## Choosing Chunk Sizes

Good chunk size selection is crucial for performance:

### General Guidelines

- **Target 10-100 KB per chunk** for general use
- **Align with access patterns**:
  - Row access → row-oriented chunks: `(1, size(data, 2))`
  - Column access → column-oriented chunks: `(size(data, 1), 1)`
  - Block access → balanced chunks: `(sqrt(n), sqrt(n))`
- **Consider compression**: Larger chunks compress better but reduce flexibility
- **Balance I/O overhead**: Very small chunks increase metadata overhead

### Examples

```julia
data = rand(Float32, 10000, 10000)

# Row-oriented access
ca1 = WriteChunkedArray(data, chunks=(1, 10000))

# Column-oriented access (Julia's default)
ca2 = WriteChunkedArray(data, chunks=(10000, 1))

# Balanced chunks (~100 chunks total)
ca3 = WriteChunkedArray(data, chunks=(1000, 1000))

# Very small chunks (may have overhead)
ca4 = WriteChunkedArray(data, chunks=(10, 10))  # 1,000,000 chunks!
```

## Interoperability with h5py

Files written by JLD2 with chunked arrays are fully compatible with Python's h5py:

```python
import h5py

# Read JLD2-written file
with h5py.File('output.jld2', 'r') as f:
    data = f['chunked'][:]
    print(f"Shape: {data.shape}")
    print(f"Chunks: {f['chunked'].chunks}")
    print(f"Dtype: {f['chunked'].dtype}")
```

## Performance Tips

1. **Chunk size matters**: Benchmark different sizes for your access pattern
2. **Compression trade-off**: Better compression but slower I/O (use for archival)
3. **Unlimited dimensions**: Only use when needed (adds indexing overhead)
4. **Pre-allocation**: Use `fill_value` for sparse datasets to avoid allocating empty chunks
5. **Avoid tiny chunks**: Metadata overhead dominates for very small chunks (<1KB)

## Current Limitations

**Phase 0 Status**: API design and validation complete. Actual writing implementation in progress:

- ⏳ Phase 1: Single Chunk writing
- ⏳ Phase 2: Fixed Array writing
- ⏳ Phase 3: Implicit Index writing
- ⏳ Phase 4: Extensible Array writing
- ⏳ Phase 5: V2 B-tree writing

Currently, all write operations will throw `UnsupportedFeatureException` with information
about when the feature will be available.

## Reading Chunked Data

JLD2 fully supports reading all chunked dataset types created by h5py or other HDF5 tools:

```julia
# Read entire array (loads all chunks)
data = jldopen("file.jld2", "r") do f
    f["chunked_dataset"]
end

# For lazy chunk-by-chunk reading, see JLD2.get_chunked_array documentation
```

## See Also

- [h5py Chunked Storage Documentation](https://docs.h5py.org/en/stable/high/dataset.html#chunked-storage)
- [HDF5 Chunking Tutorial](https://portal.hdfgroup.org/display/HDF5/Chunking+in+HDF5)
- JLD2.jl Source: `src/chunked_writing_api.jl`

---

**Note**: This API was designed to match h5py's semantics for cross-language compatibility
while maintaining Julia idioms and type safety.
