# JLD2 Chunked Array Writing API Design

## Design Goals

1. **Julia-idiomatic**: Natural Julia syntax and patterns
2. **h5py-compatible**: Match h5py semantics for interoperability
3. **Automatic by default**: Smart chunk index type selection
4. **Manual override available**: Power users can specify exact behavior
5. **Type-safe**: Leverage Julia's type system
6. **Composable**: Works with existing JLD2 patterns

## API Options Evaluated

### Option A: Wrapper Type (RECOMMENDED)

```julia
jldsave("file.jld2";
    data = ChunkedArray(data, chunks=(5,5), maxshape=(nothing, nothing))
)
```

**Pros**:
- Clean, declarative syntax
- Type carries configuration
- Can implement AbstractArray interface
- Natural composition with jldsave

**Cons**:
- New type to learn
- Overhead of wrapper (minimal)

### Option B: Explicit Function

```julia
jldopen("file.jld2", "w") do f
    write_chunked(f, "dataset", data; chunks=(5, 5), maxshape=(nothing, nothing))
end
```

**Pros**:
- Explicit and clear
- No wrapper type needed
- Direct control

**Cons**:
- Doesn't work with jldsave (most common usage)
- More verbose
- Breaks from JLD2's idiomatic patterns

### Option C: Configuration Object

```julia
config = ChunkConfig(chunks=(5,5), indexing=:auto, filters=nothing)
jldsave("file.jld2"; data = (data, config))
```

**Pros**:
- Separates config from data
- Explicit configuration

**Cons**:
- Awkward tuple syntax
- Not idiomatic Julia
- Confusing with existing JLD2 patterns

### **Decision: Option A (Wrapper Type)**

Use `ChunkedArray` wrapper type for its clean syntax and composability.

## Core Type: ChunkedArray

```julia
struct ChunkedArray{T,N,A<:AbstractArray{T,N}} <: AbstractArray{T,N}
    data::A
    chunks::NTuple{N,Int}
    maxshape::Union{Nothing, NTuple{N,Union{Int,Nothing}}}
    fill_value::Union{Nothing, T}
    indexing::Union{Symbol, Nothing}
    filters::Union{Nothing, FilterPipeline}
end
```

### Constructor

```julia
function ChunkedArray(data::AbstractArray{T,N};
                     chunks::NTuple{N,Int},
                     maxshape::Union{Nothing, NTuple{N,Union{Int,Nothing}}}=nothing,
                     fill_value::Union{Nothing, T}=nothing,
                     indexing::Union{Symbol, Nothing}=nothing,
                     filters=nothing) where {T,N}
    # Validation
    validate_chunks(chunks, size(data))
    validate_maxshape(maxshape, size(data))

    new{T,N,typeof(data)}(data, chunks, maxshape, fill_value, indexing, filters)
end
```

### AbstractArray Interface

```julia
Base.size(ca::ChunkedArray) = size(ca.data)
Base.getindex(ca::ChunkedArray, i...) = getindex(ca.data, i...)
Base.IndexStyle(::Type{<:ChunkedArray}) = IndexLinear()
# etc.
```

This allows ChunkedArray to be used like a regular array for reading while carrying write configuration.

## Parameter Design Decisions

### 1. Unlimited Dimensions: `nothing` (RECOMMENDED)

**Syntax**: `maxshape=(nothing, 10)` - first dimension unlimited, second fixed at 10

**Rationale**:
- Matches Julia's `missing`/`nothing` convention
- Similar to Python's `None`
- Natural meaning: "no maximum" = unlimited
- Type-stable: `Union{Int, Nothing}`

**Alternatives considered**:
- `:unlimited` symbol - less clear
- `Inf` - mathematically appealing but awkward for indexing
- `-1` - HDF5 internal representation, not user-friendly

### 2. Chunk Sizes: Tuple (REQUIRED)

**Syntax**: `chunks=(5, 5)` or `chunks=:auto`

**Rationale**:
- Julia standard for multi-dimensional sizes
- Type-safe and validated at construction
- `:auto` symbol for automatic selection

**Auto-chunking** heuristic:
```julia
function auto_chunk_size(data_size::NTuple{N,Int}, element_size::Int) where N
    target_bytes = 32 * 1024  # 32KB target
    total_elements = prod(data_size)
    target_elements = max(1, target_bytes ÷ element_size)

    if total_elements <= target_elements
        return data_size  # Single chunk
    end

    # Balanced division: divide each dimension by same factor
    scale_factor = (total_elements / target_elements) ^ (1/N)
    return ntuple(i -> max(1, data_size[i] ÷ round(Int, scale_factor)), N)
end
```

### 3. Filters: Flexible Union Type

**Syntax**:
```julia
filters = nothing                    # No filters
filters = :gzip                      # Named filter with defaults
filters = :gzip => 6                 # Named filter with level
filters = DeflateFilter(level=6)     # Explicit filter object
filters = [ShuffleFilter(), DeflateFilter(6)]  # Filter pipeline
```

**Rationale**:
- Flexible for common cases (`:gzip`)
- Explicit for advanced cases (filter objects)
- Pipeline support for multiple filters

**Implementation**:
```julia
function parse_filter_spec(filters)
    if isnothing(filters)
        return nothing
    elseif filters isa Symbol
        return default_filter(filters)  # :gzip -> DeflateFilter(6)
    elseif filters isa Pair{Symbol, Int}
        return filter_with_level(filters.first, filters.second)
    elseif filters isa AbstractVector
        return FilterPipeline(filters)
    elseif filters isa Filter
        return FilterPipeline([filters])
    else
        throw(ArgumentError("Invalid filter specification: $filters"))
    end
end
```

### 4. Chunk Index Type: Automatic with Override

**Syntax**:
```julia
indexing = nothing           # Automatic selection
indexing = :fixed_array      # Manual override
```

**Valid symbols**:
- `:single_chunk` - Type 1
- `:implicit_index` - Type 2
- `:fixed_array` - Type 3
- `:extensible_array` - Type 4
- `:v2btree` - Type 5

**Automatic selection logic** (matches h5py):
```julia
function select_chunk_index_type(data_size::NTuple{N,Int},
                                 chunks::NTuple{N,Int},
                                 maxshape::Union{Nothing, NTuple{N,Union{Int,Nothing}}},
                                 fill_value) where N
    # Single chunk optimization
    if chunks == data_size
        return :single_chunk
    end

    # Count unlimited dimensions
    n_unlimited = isnothing(maxshape) ? 0 : count(isnothing, maxshape)

    # Select based on unlimited dimensions
    if n_unlimited == 0
        # Fixed size dataset - Type 2 vs Type 3 decision
        if !isnothing(fill_value)
            # Could use implicit index, but h5py doesn't
            # Keep consistent with h5py: use fixed array
            return :fixed_array
        else
            return :fixed_array  # Default for fixed size
        end
    elseif n_unlimited == 1
        return :extensible_array
    else  # 2 or more unlimited
        return :v2btree
    end
end
```

### 5. Fill Value: Type-Matched Optional

**Syntax**: `fill_value=0.0f0` for Float32 arrays

**Validation**:
```julia
function validate_fill_value(fill_value, data_eltype)
    if !isnothing(fill_value)
        if !(fill_value isa data_eltype)
            throw(ArgumentError("fill_value type $(typeof(fill_value)) doesn't match data eltype $data_eltype"))
        end
    end
end
```

## Low-Level Function: write_chunked

For advanced users who don't want the wrapper:

```julia
function write_chunked(f::JLDFile, name::String, data::AbstractArray{T,N};
                      chunks::Union{NTuple{N,Int}, Symbol},
                      maxshape::Union{Nothing, NTuple{N,Union{Int,Nothing}}}=nothing,
                      fill_value::Union{Nothing, T}=nothing,
                      indexing::Union{Symbol, Nothing}=nothing,
                      filters=nothing) where {T,N}

    !f.writable && throw(ArgumentError("File must be opened in write mode"))

    # Handle auto-chunking
    chunk_dims = if chunks === :auto
        auto_chunk_size(size(data), sizeof(T))
    else
        chunks
    end

    # Validate parameters
    validate_chunks(chunk_dims, size(data))
    validate_maxshape(maxshape, size(data))
    validate_fill_value(fill_value, T)

    # Select or validate chunk index type
    index_type = if !isnothing(indexing)
        validate_index_type(indexing)
        indexing
    else
        select_chunk_index_type(size(data), chunk_dims, maxshape, fill_value)
    end

    # Parse filter specification
    filter_pipeline = parse_filter_spec(filters)

    # Dispatch to appropriate writer
    _write_chunked_dispatch(f, name, data, chunk_dims, maxshape, fill_value,
                           index_type, filter_pipeline)
end
```

## Integration with jldsave

Modify `jldsave` to recognize and handle `ChunkedArray`:

```julia
function jldsave(filename::String; kwargs...)
    jldopen(filename, "w") do f
        for (name, value) in pairs(kwargs)
            if value isa ChunkedArray
                # Extract configuration and write chunked
                write_chunked(f, string(name), value.data;
                            chunks=value.chunks,
                            maxshape=value.maxshape,
                            fill_value=value.fill_value,
                            indexing=value.indexing,
                            filters=value.filters)
            else
                # Standard write path
                f[string(name)] = value
            end
        end
    end
end
```

## Usage Examples

### Basic Chunking

```julia
using JLD2

data = rand(Float32, 1000, 1000)

# Automatic chunk index type selection
jldsave("output.jld2";
    chunked = ChunkedArray(data, chunks=(100, 100))
)
# Uses Type 3 (Fixed Array) automatically
```

### Extensible Dataset

```julia
# Time series that can grow in first dimension
timeseries_data = rand(Float32, 100, 50)

jldsave("timeseries.jld2";
    data = ChunkedArray(timeseries_data,
                       chunks=(10, 50),
                       maxshape=(nothing, 50))  # First dim unlimited
)
# Uses Type 4 (Extensible Array) automatically
```

### Multi-Dimensional Growth

```julia
# 3D grid that can grow in all dimensions
grid = rand(Float32, 100, 100, 20)

jldsave("grid.jld2";
    volume = ChunkedArray(grid,
                         chunks=(25, 25, 5),
                         maxshape=(nothing, nothing, nothing))
)
# Uses Type 5 (V2 B-tree) automatically
```

### With Compression

```julia
data = randn(Float32, 1000, 1000)

jldsave("compressed.jld2";
    data = ChunkedArray(data,
                       chunks=(100, 100),
                       filters=:gzip => 6)  # Gzip level 6
)
```

### Multiple Filters

```julia
using JLD2.Filters

data = rand(Float32, 1000, 1000)

jldsave("filtered.jld2";
    data = ChunkedArray(data,
                       chunks=(100, 100),
                       filters=[ShuffleFilter(), DeflateFilter(9)])
)
```

### With Fill Value

```julia
# Sparse dataset
sparse_data = zeros(Float32, 1000, 1000)
sparse_data[100:200, 100:200] .= rand(Float32, 101, 101)

jldsave("sparse.jld2";
    sparse = ChunkedArray(sparse_data,
                         chunks=(100, 100),
                         fill_value=0.0f0)
)
# Still uses Type 3 (Fixed Array) like h5py
```

### Manual Index Type Override

```julia
data = rand(Float32, 100, 100)

# Force V2 B-tree even for fixed size
jldsave("manual.jld2";
    data = ChunkedArray(data,
                       chunks=(10, 10),
                       indexing=:v2btree)  # Manual override
)
```

### Auto Chunking

```julia
data = rand(Float32, 10000, 10000)

jldsave("auto.jld2";
    data = ChunkedArray(data, chunks=:auto)  # JLD2 chooses chunk size
)
```

### Low-Level API

```julia
jldopen("output.jld2", "w") do f
    data = rand(Float32, 1000, 1000)

    write_chunked(f, "dataset", data;
                 chunks=(100, 100),
                 maxshape=(nothing, 1000),
                 filters=:gzip)
end
```

## Error Handling

### Validation Errors

```julia
# Chunk dimensions mismatch
ChunkedArray(rand(10, 10), chunks=(5,))
# ArgumentError: chunks dimensions must match data dimensions

# Non-positive chunk size
ChunkedArray(rand(10, 10), chunks=(0, 5))
# ArgumentError: chunk sizes must be positive

# maxshape smaller than data
ChunkedArray(rand(100, 100), chunks=(10, 10), maxshape=(50, 100))
# ArgumentError: maxshape[1]=50 is less than data size 100

# Wrong fill_value type
ChunkedArray(rand(Float32, 10, 10), chunks=(5, 5), fill_value=0.0)  # Float64
# ArgumentError: fill_value type Float64 doesn't match data eltype Float32
```

### Runtime Errors

```julia
# Write to read-only file
jldopen("readonly.jld2", "r") do f
    write_chunked(f, "data", data; chunks=(5, 5))
end
# ArgumentError: File must be opened in write mode
```

## Type System Design

```julia
# Abstract type hierarchy (if needed for dispatch)
abstract type AbstractChunkedArray{T,N} <: AbstractArray{T,N} end

struct ChunkedArray{T,N,A<:AbstractArray{T,N}} <: AbstractChunkedArray{T,N}
    data::A
    chunks::NTuple{N,Int}
    maxshape::Union{Nothing, NTuple{N,Union{Int,Nothing}}}
    fill_value::Union{Nothing, T}
    indexing::Union{Symbol, Nothing}
    filters::Union{Nothing, FilterPipeline}
end

# Potential future: MappedChunkedArray for lazy loading
# struct MappedChunkedArray{T,N} <: AbstractChunkedArray{T,N}
#     file::JLDFile
#     dataset_name::String
#     ... chunk cache ...
# end
```

## Comparison with h5py

| Feature | h5py | JLD2 |
|---------|------|------|
| Chunk specification | `chunks=(5,5)` | `chunks=(5,5)` ✓ |
| Unlimited dims | `maxshape=(None,10)` | `maxshape=(nothing,10)` ✓ |
| Compression | `compression='gzip'` | `filters=:gzip` ✓ |
| Compression level | `compression_opts=6` | `filters=:gzip=>6` ✓ |
| Fill value | `fillvalue=-999` | `fill_value=-999` ✓ |
| Auto chunking | `chunks=True` | `chunks=:auto` ✓ |
| Index type | Automatic | Automatic + override ✓ |

## Implementation Phases

### Phase 0: API Design (Current)
- ✓ Design ChunkedArray type
- ✓ Design parameter specifications
- ✓ Design integration with jldsave

### Phase 1: Single Chunk (Type 1)
- Implement write_single_chunk
- Simplest case for testing infrastructure

### Phase 2: Fixed Array (Type 3)
- Implement write_fixed_array
- Most common use case
- Match h5py default behavior

### Phase 3: Implicit Index (Type 2)
- Optional: May skip or low priority
- h5py doesn't use it by default

### Phase 4: Extensible Array (Type 4)
- Implement write_extensible_array
- Important for time series data

### Phase 5: V2 B-tree (Type 5)
- Implement write_v2btree
- Most complex, most flexible

### Phase 6: Integration & Optimization
- Performance tuning
- Filter integration
- Documentation
- Examples

## Open Questions

### 1. Should ChunkedArray be mutable?

**Current**: Immutable struct (simpler, safer)

**Alternative**: Mutable to allow resizing

**Decision**: Start immutable, add MutableChunkedArray later if needed

### 2. Should we support chunk reading/writing API?

```julia
# Potential future API
ca = load_chunked("file.jld2", "dataset")  # Lazy loading
chunk = ca.chunks[1, 1]  # Access specific chunk
ca.chunks[1, 2] = new_chunk_data  # Write specific chunk
```

**Decision**: Phase 7+ feature, not in initial implementation

### 3. Auto-chunking: What target size?

**Options**:
- 32 KB (current proposal) - conservative, many chunks
- 1 MB (h5py-like) - fewer chunks, better compression
- Adaptive based on data size

**Decision**: Start with 32 KB, add heuristics later based on benchmarks

### 4. Validation: Strict or permissive?

**Current**: Strict validation (fail fast)

**Alternative**: Warnings for non-critical issues

**Decision**: Strict for correctness, can relax later if needed

## Summary

**Recommended API**:
- Use `ChunkedArray` wrapper type
- `nothing` for unlimited dimensions
- Tuple for chunk sizes, `:auto` for automatic
- Flexible filter specification
- Automatic chunk index type selection matching h5py
- Manual override available via `indexing` parameter

This design balances:
- ✓ Julia idioms and type safety
- ✓ h5py compatibility and familiarity
- ✓ Ease of use for common cases
- ✓ Power and flexibility for advanced users
- ✓ Clear upgrade path for future features
