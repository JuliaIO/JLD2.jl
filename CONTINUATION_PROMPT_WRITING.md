# Continuation Prompt: Chunked Array Writing Support - Phase 0 (API Design)

## Current Status (2025-10-01)

**Location**: `/workspace/JLD2.jl` (branch: `version4_chunking`)

**Latest Commit**: `5f23394` - "Add HDF5 v2 B-tree chunk indexing (type 5) support"

**Reading Support**: ✅ **COMPLETE** for all 5 v4 chunk indexing types!
- Type 1: Single Chunk ✅
- Type 2: Implicit Index ✅
- Type 3: Fixed Array ✅
- Type 4: Extensible Array ✅
- Type 5: V2 B-tree ✅

**Writing Support**: ⏳ **YOU ARE HERE** - Starting Phase 0 (API Design)

**Recent Achievement**: Successfully completed reading support for all HDF5 v4 chunk indexing types. All test files read correctly and match h5py output exactly.

---

## Your Task: Design the Chunked Writing API (Phase 0)

You are starting a multi-phase project to add **writing support** for all chunked array types. Phase 0 focuses on designing a clean, user-friendly API that will serve as the foundation for all subsequent implementation phases.

### Context

JLD2 currently has:
- **Reading**: Complete support for all 5 v4 chunk indexing types
- **Writing**: Only V1 B-tree (old format) - needs modernization

We need to design an API that:
1. **Automatically selects** the optimal chunk indexing type (like h5py does)
2. **Allows manual override** for advanced users
3. **Is Julia-idiomatic** and easy to use
4. **Matches h5py semantics** for interoperability

---

## Phase 0 Tasks (Estimated: 2-3 days)

### Task 1: Study h5py API (3-4 hours)

**Goal**: Understand how h5py handles chunked dataset creation.

**Steps**:

1. **Install h5py** (if not already available):
```bash
python3 -m pip install h5py numpy
```

2. **Create test scripts** to explore h5py API:

```python
# test_h5py_chunking.py
import h5py
import numpy as np

# Create test data
data = np.arange(300).reshape(30, 10).astype('f')

# Test 1: Single chunk
with h5py.File('h5py_single_chunk.h5', 'w') as f:
    f.create_dataset('single', data=data, chunks=data.shape)
    print(f"Single chunk: chunks={f['single'].chunks}")

# Test 2: Fixed array (default chunked)
with h5py.File('h5py_fixed_array.h5', 'w') as f:
    f.create_dataset('fixed', data=data, chunks=(5, 5))
    print(f"Fixed array: chunks={f['fixed'].chunks}")

# Test 3: Extensible array (one unlimited)
with h5py.File('h5py_extensible.h5', 'w') as f:
    f.create_dataset('ext', data=data, chunks=(5, 5), maxshape=(None, 10))
    print(f"Extensible: maxshape={f['ext'].maxshape}")

# Test 4: V2 B-tree (multiple unlimited)
with h5py.File('h5py_v2btree.h5', 'w') as f:
    f.create_dataset('btree', data=data, chunks=(5, 5), maxshape=(None, None))
    print(f"V2 B-tree: maxshape={f['btree'].maxshape}")

# Test 5: Compressed
with h5py.File('h5py_compressed.h5', 'w') as f:
    f.create_dataset('comp', data=data, chunks=(5, 5), compression='gzip', compression_opts=6)
    print(f"Compressed: compression={f['comp'].compression}")
```

3. **Inspect created files** with h5debug:
```bash
h5debug h5py_single_chunk.h5 <offset> | grep -A10 "Index Type"
h5debug h5py_fixed_array.h5 <offset> | grep -A10 "Index Type"
# etc.
```

4. **Document findings** in a file `H5PY_API_ANALYSIS.md`:
   - How does h5py select chunk index type?
   - What parameters affect the selection?
   - What are the default chunk sizes?
   - How are filters specified?

### Task 2: Design JLD2 API (4-6 hours)

**Goal**: Create a Julia-idiomatic API that matches h5py's capabilities.

**Design Principles**:
1. **Automatic is default**: Users shouldn't need to know about chunk index types
2. **Sensible defaults**: Follow h5py conventions
3. **Manual override available**: For power users
4. **Type-safe**: Use Julia's type system effectively
5. **Composable**: Works with existing JLD2 patterns

**Proposed API Design**:

```julia
# Option A: Wrapper type (recommended)
jldsave("file.jld2";
    data = ChunkedArray(data, chunks=(5,5), maxshape=(nothing, nothing))
)

# Option B: Explicit function
jldopen("file.jld2", "w") do f
    write_chunked(f, "dataset", data; chunks=(5, 5), maxshape=(nothing, nothing))
end

# Option C: Configuration object
config = ChunkConfig(chunks=(5,5), indexing=:auto, filters=nothing)
jldsave("file.jld2"; data = (data, config))
```

**Key Questions to Answer**:

1. **How to specify unlimited dimensions?**
   - Option A: `maxshape=(nothing, nothing)` (like Python None)
   - Option B: `maxshape=(:unlimited, :unlimited)`
   - Option C: `maxshape=(Inf, Inf)`

2. **How to specify chunk sizes?**
   - Tuple: `chunks=(5, 5)`
   - Named: `chunks=ChunkSize(5, 5)`
   - Auto: `chunks=:auto` (let JLD2 decide)

3. **How to specify filters?**
   - Symbol: `filters=:gzip`
   - Object: `filters=GzipFilter(level=6)`
   - Pipeline: `filters=[GzipFilter(6), ShuffleFilter()]`

4. **How to override automatic selection?**
   - Explicit: `indexing=:fixed_array`
   - Type parameter: `ChunkedArray{FixedArray}(...)`

**Create a design document**: `CHUNKED_API_DESIGN.md` with:
- Proposed API signatures
- Examples for each use case
- Comparison with h5py
- Rationale for design choices

### Task 3: Implement API Module Structure (2-3 hours)

**Goal**: Create the API skeleton with dispatch logic.

**File**: `src/chunked_writing_api.jl`

```julia
"""
    ChunkedArray{T,N}

Wrapper type for arrays that should be written with chunking.

# Fields
- `data::AbstractArray{T,N}` - The array data
- `chunks::NTuple{N,Int}` - Chunk dimensions
- `maxshape::Union{Nothing, NTuple{N,Union{Int,Nothing}}}` - Maximum dimensions
- `fill_value::Union{Nothing, T}` - Fill value for unallocated chunks
- `indexing::Union{Symbol, Nothing}` - Manual chunk index type override
- `filters::Union{Nothing, FilterPipeline}` - Compression filters

# Examples

```julia
# Auto-select chunking type
arr = ChunkedArray(data, chunks=(5,5))

# Extensible array (one unlimited dimension)
arr = ChunkedArray(data, chunks=(5,5), maxshape=(nothing, 10))

# V2 B-tree (multiple unlimited dimensions)
arr = ChunkedArray(data, chunks=(5,5), maxshape=(nothing, nothing))

# With compression
arr = ChunkedArray(data, chunks=(5,5), filters=:gzip)

# Manual override
arr = ChunkedArray(data, chunks=(5,5), indexing=:fixed_array)
```
"""
struct ChunkedArray{T,N} <: AbstractArray{T,N}
    data::AbstractArray{T,N}
    chunks::NTuple{N,Int}
    maxshape::Union{Nothing, NTuple{N,Union{Int,Nothing}}}
    fill_value::Union{Nothing, T}
    indexing::Union{Symbol, Nothing}
    filters::Union{Nothing, FilterPipeline}

    function ChunkedArray(data::AbstractArray{T,N};
                         chunks::NTuple{N,Int},
                         maxshape::Union{Nothing, NTuple{N,Union{Int,Nothing}}}=nothing,
                         fill_value::Union{Nothing, T}=nothing,
                         indexing::Union{Symbol, Nothing}=nothing,
                         filters=nothing) where {T,N}
        # Validate parameters
        validate_chunks(chunks, size(data))
        validate_maxshape(maxshape, size(data))

        new{T,N}(data, chunks, maxshape, fill_value, indexing, filters)
    end
end

# Make ChunkedArray behave like an array for reading
Base.size(ca::ChunkedArray) = size(ca.data)
Base.getindex(ca::ChunkedArray, i...) = getindex(ca.data, i...)
# etc.

"""
    select_chunk_index_type(data_size, chunks, maxshape, fill_value)

Automatically select the optimal chunk indexing type based on dataset characteristics.

# Selection Logic

1. **Single Chunk**: If chunks == data_size
2. **Implicit Index**: If no unlimited dims AND fill_value specified
3. **Fixed Array**: If no unlimited dims (default for fixed size)
4. **Extensible Array**: If exactly 1 unlimited dimension
5. **V2 B-tree**: If 2+ unlimited dimensions

# Returns
Symbol indicating chunk index type: `:single_chunk`, `:implicit_index`,
`:fixed_array`, `:extensible_array`, or `:v2btree`
"""
function select_chunk_index_type(data_size::NTuple{N,Int},
                                 chunks::NTuple{N,Int},
                                 maxshape::Union{Nothing, NTuple{N,Union{Int,Nothing}}},
                                 fill_value) where N
    # Single chunk optimization
    if chunks == data_size
        return :single_chunk
    end

    # Count unlimited dimensions
    n_unlimited = if isnothing(maxshape)
        0  # No unlimited dimensions
    else
        count(isnothing, maxshape)
    end

    # Select based on unlimited dimensions
    if n_unlimited == 0
        # Fixed size dataset
        if !isnothing(fill_value)
            return :implicit_index  # Early allocation with fill
        else
            return :fixed_array     # Most common case
        end
    elseif n_unlimited == 1
        return :extensible_array
    else  # 2 or more unlimited
        return :v2btree
    end
end

"""
    write_chunked(f::JLDFile, name::String, data::AbstractArray; kwargs...)

Write a chunked dataset to a JLD2 file.

This is a low-level function. For most use cases, use `ChunkedArray` with `jldsave`.

# Arguments
- `f::JLDFile` - Open JLD2 file in write mode
- `name::String` - Dataset name
- `data::AbstractArray` - Data to write

# Keyword Arguments
- `chunks::NTuple{N,Int}` - Chunk dimensions (required)
- `maxshape=nothing` - Maximum dimensions (nothing for each unlimited dim)
- `fill_value=nothing` - Fill value for implicit index
- `indexing=nothing` - Manual override for chunk index type
- `filters=nothing` - Compression filters

# Examples

```julia
jldopen("file.jld2", "w") do f
    # Basic chunking
    write_chunked(f, "data1", data; chunks=(10, 10))

    # With unlimited dimension
    write_chunked(f, "data2", data; chunks=(10, 10), maxshape=(nothing, 100))

    # With compression
    write_chunked(f, "data3", data; chunks=(10, 10), filters=:gzip)
end
```
"""
function write_chunked(f::JLDFile, name::String, data::AbstractArray{T,N};
                      chunks::NTuple{N,Int},
                      maxshape::Union{Nothing, NTuple{N,Union{Int,Nothing}}}=nothing,
                      fill_value::Union{Nothing, T}=nothing,
                      indexing::Union{Symbol, Nothing}=nothing,
                      filters=nothing) where {T,N}

    !f.writable && throw(ArgumentError("File must be opened in write mode"))

    # Select or validate chunk index type
    index_type = if !isnothing(indexing)
        validate_index_type(indexing)
        indexing
    else
        select_chunk_index_type(size(data), chunks, maxshape, fill_value)
    end

    # Dispatch to appropriate writer (stubs for now)
    if index_type == :single_chunk
        write_single_chunk(f, name, data, chunks, filters)
    elseif index_type == :implicit_index
        write_implicit_index(f, name, data, chunks, maxshape, fill_value, filters)
    elseif index_type == :fixed_array
        write_fixed_array(f, name, data, chunks, filters)
    elseif index_type == :extensible_array
        write_extensible_array(f, name, data, chunks, maxshape, filters)
    elseif index_type == :v2btree
        write_v2btree(f, name, data, chunks, maxshape, filters)
    else
        throw(ArgumentError("Unknown chunk index type: $index_type"))
    end

    return nothing
end

# Integration with jldsave
function jldsave(filename::String, compress::Bool, args::Pair...)
    # ... existing code ...

    # Handle ChunkedArray
    for (name, value) in args
        if value isa ChunkedArray
            write_chunked(f, string(name), value.data;
                         chunks=value.chunks,
                         maxshape=value.maxshape,
                         fill_value=value.fill_value,
                         indexing=value.indexing,
                         filters=value.filters)
        else
            # ... existing write logic ...
        end
    end
end

# Stub implementations (to be filled in later phases)
function write_single_chunk(f, name, data, chunks, filters)
    @info "write_single_chunk stub called" name size(data) chunks
    throw(UnsupportedFeatureException("Single chunk writing not yet implemented"))
end

function write_implicit_index(f, name, data, chunks, maxshape, fill_value, filters)
    @info "write_implicit_index stub called" name size(data) chunks
    throw(UnsupportedFeatureException("Implicit index writing not yet implemented"))
end

function write_fixed_array(f, name, data, chunks, filters)
    @info "write_fixed_array stub called" name size(data) chunks
    throw(UnsupportedFeatureException("Fixed array writing not yet implemented"))
end

function write_extensible_array(f, name, data, chunks, maxshape, filters)
    @info "write_extensible_array stub called" name size(data) chunks
    throw(UnsupportedFeatureException("Extensible array writing not yet implemented"))
end

function write_v2btree(f, name, data, chunks, maxshape, filters)
    @info "write_v2btree stub called" name size(data) chunks
    throw(UnsupportedFeatureException("V2 B-tree writing not yet implemented"))
end

# Validation helpers
function validate_chunks(chunks, data_size)
    length(chunks) == length(data_size) ||
        throw(ArgumentError("chunks dimensions must match data dimensions"))

    all(chunks .> 0) ||
        throw(ArgumentError("chunk sizes must be positive"))

    # Warn if chunks are larger than data
    if any(chunks .> data_size)
        @warn "Some chunk dimensions are larger than data dimensions" chunks data_size
    end
end

function validate_maxshape(maxshape, data_size)
    isnothing(maxshape) && return  # OK - fixed size

    length(maxshape) == length(data_size) ||
        throw(ArgumentError("maxshape dimensions must match data dimensions"))

    for (i, (max_dim, data_dim)) in enumerate(zip(maxshape, data_size))
        if !isnothing(max_dim) && max_dim < data_dim
            throw(ArgumentError("maxshape[$i]=$max_dim is less than data size $data_dim"))
        end
    end
end

function validate_index_type(index_type::Symbol)
    valid_types = [:single_chunk, :implicit_index, :fixed_array, :extensible_array, :v2btree]
    index_type in valid_types ||
        throw(ArgumentError("Invalid index type: $index_type. Must be one of $valid_types"))
end
```

**Add to `src/JLD2.jl`**:
```julia
include("chunked_writing_api.jl")
export ChunkedArray, write_chunked
```

### Task 4: Write API Tests (2-3 hours)

**Goal**: Test the API logic before implementing the writers.

**File**: `test/chunked_writing_api_test.jl`

```julia
using JLD2, Test

@testset "Chunked Writing API" begin
    @testset "ChunkedArray construction" begin
        data = rand(Float32, 30, 10)

        # Basic construction
        ca = ChunkedArray(data, chunks=(5, 5))
        @test size(ca) == (30, 10)
        @test ca.chunks == (5, 5)
        @test isnothing(ca.maxshape)

        # With maxshape
        ca = ChunkedArray(data, chunks=(5, 5), maxshape=(nothing, 10))
        @test ca.maxshape == (nothing, 10)

        # Invalid chunks
        @test_throws ArgumentError ChunkedArray(data, chunks=(5,))  # Wrong dimensions
        @test_throws ArgumentError ChunkedArray(data, chunks=(0, 5))  # Non-positive
    end

    @testset "Automatic index type selection" begin
        data = rand(Float32, 30, 10)

        # Single chunk
        @test JLD2.select_chunk_index_type((30, 10), (30, 10), nothing, nothing) == :single_chunk

        # Fixed array
        @test JLD2.select_chunk_index_type((30, 10), (5, 5), nothing, nothing) == :fixed_array

        # Implicit index
        @test JLD2.select_chunk_index_type((30, 10), (5, 5), nothing, 0.0f0) == :implicit_index

        # Extensible array
        @test JLD2.select_chunk_index_type((30, 10), (5, 5), (nothing, 10), nothing) == :extensible_array

        # V2 B-tree
        @test JLD2.select_chunk_index_type((30, 10), (5, 5), (nothing, nothing), nothing) == :v2btree
    end

    @testset "API stubs" begin
        # Test that stubs are called correctly
        data = rand(Float32, 10, 10)

        jldopen("test_stub.jld2", "w") do f
            # Should call stub and throw UnsupportedFeatureException
            @test_throws JLD2.UnsupportedFeatureException begin
                write_chunked(f, "test", data; chunks=(5, 5))
            end
        end

        rm("test_stub.jld2", force=true)
    end

    @testset "ChunkedArray with jldsave" begin
        data = rand(Float32, 10, 10)
        ca = ChunkedArray(data, chunks=(5, 5))

        # Currently should fail with stub exception
        @test_throws JLD2.UnsupportedFeatureException begin
            jldsave("test_chunked.jld2"; chunked_data = ca)
        end

        rm("test_chunked.jld2", force=true)
    end
end
```

Run tests to verify API logic works:
```bash
julia --project -e 'using Pkg; Pkg.test()'
```

### Task 5: Write Documentation (2-3 hours)

**Goal**: Document the API before implementation begins.

**File**: `docs/src/chunked_writing.md`

````markdown
# Chunked Array Writing

JLD2 supports writing chunked datasets with automatic selection of optimal chunk indexing strategies.

## Overview

Chunked storage divides arrays into fixed-size blocks, enabling:
- **Efficient partial I/O**: Read/write specific regions without loading entire array
- **Compression**: Apply filters to individual chunks
- **Extensibility**: Grow datasets along unlimited dimensions
- **Performance**: Optimize access patterns for different use cases

## Quick Start

```julia
using JLD2

# Create chunked dataset
data = rand(Float32, 1000, 1000)

# Automatic chunking
jldsave("output.jld2";
    chunked = ChunkedArray(data, chunks=(100, 100))
)

# With unlimited dimensions (for later extension)
jldsave("extensible.jld2";
    timeseries = ChunkedArray(data,
                             chunks=(100, 100),
                             maxshape=(nothing, 1000))  # Can grow in first dimension
)
```

## Chunk Index Types

JLD2 automatically selects the optimal chunk indexing strategy:

### 1. Single Chunk (Type 1)
Used when chunk size equals array size.
```julia
data = rand(100, 100)
arr = ChunkedArray(data, chunks=(100, 100))  # Stored as single chunk
```

### 2. Fixed Array (Type 3)
Default for fixed-size chunked arrays.
```julia
data = rand(1000, 1000)
arr = ChunkedArray(data, chunks=(100, 100))  # 100 chunks in 10×10 grid
```

### 3. Implicit Index (Type 2)
For pre-allocated arrays with fill values.
```julia
data = rand(1000, 1000)
arr = ChunkedArray(data, chunks=(100, 100), fill_value=0.0f0)
```

### 4. Extensible Array (Type 4)
For arrays with one unlimited dimension.
```julia
data = rand(1000, 1000)
arr = ChunkedArray(data, chunks=(100, 100), maxshape=(nothing, 1000))
```

### 5. V2 B-tree (Type 5)
For arrays with multiple unlimited dimensions.
```julia
data = rand(1000, 1000)
arr = ChunkedArray(data, chunks=(100, 100), maxshape=(nothing, nothing))
```

## Advanced Usage

### Manual Index Type Selection

```julia
# Force specific index type
arr = ChunkedArray(data, chunks=(100, 100), indexing=:fixed_array)
```

### Compression

```julia
# With gzip compression
arr = ChunkedArray(data, chunks=(100, 100), filters=:gzip)

# Custom filter pipeline
arr = ChunkedArray(data, chunks=(100, 100),
                  filters=[ShuffleFilter(), DeflateFilter(level=9)])
```

### Choosing Chunk Sizes

General guidelines:
- **Target 10-100 KB per chunk** for general use
- **Align with access patterns**: If reading rows, use row-oriented chunks
- **Consider compression**: Larger chunks compress better but reduce flexibility
- **Balance I/O**: Very small chunks increase overhead, very large reduce granularity

```julia
# For row-oriented access
arr = ChunkedArray(data, chunks=(100, size(data, 2)))

# For column-oriented access
arr = ChunkedArray(data, chunks=(size(data, 1), 100))

# Balanced chunks
chunk_size = round(Int, sqrt(prod(size(data)) / 100))  # ~100 chunks
arr = ChunkedArray(data, chunks=(chunk_size, chunk_size))
```

## API Reference

### ChunkedArray

```julia
ChunkedArray(data; chunks, maxshape=nothing, fill_value=nothing, indexing=nothing, filters=nothing)
```

Create a chunked array wrapper for writing to JLD2.

**Arguments:**
- `data::AbstractArray` - Array data to write
- `chunks::NTuple{N,Int}` - Chunk dimensions (required)
- `maxshape::Union{Nothing, NTuple{N,Union{Int,Nothing}}}` - Maximum dimensions
- `fill_value` - Fill value for unallocated chunks
- `indexing::Union{Symbol,Nothing}` - Manual index type override
- `filters` - Compression filters

### write_chunked

```julia
write_chunked(f::JLDFile, name::String, data; chunks, maxshape=nothing, ...)
```

Low-level function to write chunked dataset.

## Examples

See `examples/chunked_writing_examples.jl` for complete examples.

## Interoperability

Files written by JLD2 are fully compatible with h5py and HDF5 tools:

```python
import h5py
import numpy as np

# Read JLD2-written file
with h5py.File('output.jld2', 'r') as f:
    data = f['chunked'][:]
    print(f"Chunks: {f['chunked'].chunks}")
```

## Performance Tips

1. **Chunk size matters**: Test different sizes for your access pattern
2. **Compression trade-off**: Better compression vs. slower I/O
3. **Unlimited dimensions**: Only use when needed (adds overhead)
4. **Pre-allocation**: Use `fill_value` for sparse data

## Limitations

Current limitations:
- Cannot extend existing datasets (append operation not yet supported)
- Parallel chunk writing not yet implemented
- Filter auto-selection not available

Future enhancements planned in roadmap.
````

### Task 6: Create Examples (1-2 hours)

**File**: `examples/chunked_writing_examples.jl`

```julia
"""
Chunked Array Writing Examples

Run with: julia --project examples/chunked_writing_examples.jl
"""

using JLD2
using Printf

println("JLD2 Chunked Array Writing Examples")
println("=" ^ 70)

# Example 1: Basic chunking
println("\nExample 1: Basic Fixed Array Chunking")
data1 = reshape(1.0f0:10000.0f0, 100, 100)
jldsave("example1_fixed.jld2";
    data = ChunkedArray(data1, chunks=(10, 10))
)
println("✓ Created example1_fixed.jld2 with 10×10 chunks")

# Example 2: Image stack (extensible in time)
println("\nExample 2: Image Stack (Extensible Array)")
image_stack = rand(Float32, 512, 512, 10)  # 10 time points
jldsave("example2_images.jld2";
    images = ChunkedArray(image_stack,
                         chunks=(128, 128, 1),
                         maxshape=(512, 512, nothing))  # Can add more time points
)
println("✓ Created example2_images.jld2 - extensible along time axis")

# Example 3: Large dataset with compression
println("\nExample 3: Compressed Data")
large_data = randn(Float32, 1000, 1000)
jldsave("example3_compressed.jld2";
    compressed = ChunkedArray(large_data,
                             chunks=(100, 100),
                             filters=:gzip)
)
println("✓ Created example3_compressed.jld2 with gzip compression")

# Example 4: Sparse data with fill value
println("\nExample 4: Sparse Data (Implicit Index)")
sparse_data = zeros(Float32, 1000, 1000)
sparse_data[100:200, 100:200] .= rand(Float32, 101, 101)
jldsave("example4_sparse.jld2";
    sparse = ChunkedArray(sparse_data,
                         chunks=(100, 100),
                         fill_value=0.0f0)
)
println("✓ Created example4_sparse.jld2 with fill value")

# Example 5: Multi-dimensional extensible (V2 B-tree)
println("\nExample 5: Multi-dimensional Growth (V2 B-tree)")
grid_data = rand(Float32, 100, 100, 20)
jldsave("example5_grid.jld2";
    grid = ChunkedArray(grid_data,
                       chunks=(25, 25, 5),
                       maxshape=(nothing, nothing, nothing))  # All dimensions unlimited
)
println("✓ Created example5_grid.jld2 with V2 B-tree indexing")

println("\n" * "=" ^ 70)
println("All examples created successfully!")
println("\nVerify with h5py:")
println("  python3 -c \"import h5py; f = h5py.File('example1_fixed.jld2'); print(f['data'].chunks)\"")
println("\nVerify structure:")
println("  h5debug example1_fixed.jld2 <offset>")
```

### Task 7: Review and Iterate (2-3 hours)

**Checklist**:

- [ ] API is intuitive and easy to use
- [ ] Automatic selection logic is correct
- [ ] Parameter validation is comprehensive
- [ ] Error messages are helpful
- [ ] Documentation is complete
- [ ] Examples cover common use cases
- [ ] Matches h5py semantics where appropriate
- [ ] Julia idioms are followed
- [ ] Code is well-commented
- [ ] Tests pass

**Get feedback**:
- Test API with sample use cases
- Check ergonomics
- Verify dispatch logic
- Ensure stub implementations are called correctly

---

## Success Criteria for Phase 0

✅ **Phase 0 Complete When**:

1. **API Design**: Complete and documented in `CHUNKED_API_DESIGN.md`
2. **Implementation**: `src/chunked_writing_api.jl` with working dispatch logic
3. **Tests**: API tests pass (stubs throw expected exceptions)
4. **Documentation**: User guide written in `docs/src/chunked_writing.md`
5. **Examples**: Example file demonstrates all use cases
6. **h5py Analysis**: Understanding documented in `H5PY_API_ANALYSIS.md`
7. **Review**: Design reviewed and approved for next phases

---

## Deliverables

When Phase 0 is complete, you should have:

1. **`H5PY_API_ANALYSIS.md`** - Analysis of h5py chunking API
2. **`CHUNKED_API_DESIGN.md`** - JLD2 API design document
3. **`src/chunked_writing_api.jl`** - API implementation (~300-400 lines)
4. **`test/chunked_writing_api_test.jl`** - API tests
5. **`docs/src/chunked_writing.md`** - User documentation
6. **`examples/chunked_writing_examples.jl`** - Example code
7. **Test files created by h5py** for reference

---

## Next Steps After Phase 0

Once Phase 0 is approved:
- **Phase 1**: Implement Single Chunk writing (1-2 days)
- **Phase 2**: Implement Fixed Array writing (2-3 days)
- **Phase 3**: Implement Implicit Index writing (1-2 days)
- **Phase 4**: Implement Extensible Array writing (3-4 days)
- **Phase 5**: Implement V2 B-tree writing (4-5 days)
- **Phase 6**: Integration and optimization (2-3 days)

See `CHUNKED_WRITING_PLAN.md` for complete roadmap.

---

## Important Files to Reference

**Reading Implementations** (use as templates for writing):
- `src/implicit_index.jl` - Implicit index reading
- `src/fixed_array.jl` - Fixed array reading
- `src/extensible_array.jl` - Extensible array reading
- `src/v2btree_chunking.jl` - V2 B-tree reading

**Existing Writing Infrastructure**:
- `src/v1btree.jl` - V1 B-tree writing (older format, good reference)
- `src/chunked_array.jl` - Existing chunked writing function
- `src/datasets.jl` - Dataset writing infrastructure

**Documentation**:
- `CHUNKED_WRITING_PLAN.md` - Overall project plan
- `CLAUDE.md` - Project conventions
- Phase completion docs (PHASE*.md) - Patterns from reading implementation

**HDF5 Specification**:
- Keep HDF5 1.10+ spec handy for reference
- Section VII covers data storage formats

---

## Quick Start Commands

```bash
cd /workspace/JLD2.jl

# 1. Study h5py
python3 test_h5py_chunking.py
h5debug h5py_fixed_array.h5 <offset>

# 2. Create API implementation
# Edit src/chunked_writing_api.jl

# 3. Add to main module
# Edit src/JLD2.jl to add include

# 4. Write tests
# Create test/chunked_writing_api_test.jl

# 5. Test
julia --project -e 'using Pkg; Pkg.test()'

# 6. Write documentation
# Edit docs/src/chunked_writing.md

# 7. Create examples
# Create examples/chunked_writing_examples.jl
```

---

**Current Branch**: `version4_chunking`
**Status**: Ready to start Phase 0 (API Design)

Good luck! Focus on creating a clean, intuitive API that will make the subsequent implementation phases straightforward. 🚀
