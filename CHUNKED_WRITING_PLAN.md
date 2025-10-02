# Chunked Array Writing Implementation Plan

## Current Status (2025-10-01)

**Branch**: `version4_chunking`

**Reading Support**: ✅ Complete for all 5 v4 chunk indexing types
- Type 1: Single Chunk ✅
- Type 2: Implicit Index ✅
- Type 3: Fixed Array ✅
- Type 4: Extensible Array ✅
- Type 5: V2 B-tree ✅

**Writing Support**: ⏳ Needs implementation

**Current Writing Capability**:
- V1 B-tree writing exists (`src/v1btree.jl`) - used for large single-unlimited datasets
- Basic chunked writing function exists but uses V1 B-tree only
- No support for v4 chunk indexing types in writing

---

## Overview

Implement writing support for all HDF5 v4 chunk indexing types, allowing JLD2 to create files with optimal chunking strategies based on dataset characteristics.

**Key Principle**: Match h5py's API design philosophy - automatic chunk index selection based on dataset properties, with manual override options.

---

## Phase 0: API Design and User Interface (2-3 days)

### Goal
Design a clean, Julia-idiomatic API for chunked dataset creation that automatically selects optimal chunk indexing based on dataset characteristics.

### Tasks

#### 1. Study h5py API Design (3-4 hours)

**h5py patterns to analyze**:
```python
# Single chunk (type 1)
f.create_dataset('data', data=arr, chunks=arr.shape)

# Implicit index (type 2) - early allocation
f.create_dataset('data', data=arr, chunks=(5,5),
                 fillvalue=0, track_times=False)

# Fixed array (type 3) - default for chunked
f.create_dataset('data', data=arr, chunks=(5,5))

# Extensible array (type 4) - one unlimited
f.create_dataset('data', data=arr, chunks=(5,5), maxshape=(None, 10))

# V2 B-tree (type 5) - multiple unlimited
f.create_dataset('data', data=arr, chunks=(5,5), maxshape=(None, None))
```

**Key observations**:
1. Chunk index type is **automatically selected** based on:
   - `chunks` parameter
   - `maxshape` parameter
   - Dataset size
   - Number of unlimited dimensions
2. User doesn't explicitly specify index type
3. Sensible defaults apply

#### 2. Design JLD2 API (4-6 hours)

**Proposed API design**:

```julia
# High-level API - automatic chunking
jldsave("file.jld2";
    data = ChunkedArray(data, chunk_size=(5,5), max_dims=(nothing, nothing))
)

# Or with options
jldopen("file.jld2", "w") do f
    # Auto-select chunk indexing based on parameters
    write_chunked(f, "dataset", data;
                  chunks=(5, 5),           # Chunk dimensions
                  max_dims=nothing,        # nothing = all unlimited, or tuple
                  fill_value=0.0,          # For implicit index
                  filters=nothing)         # Compression filters
end

# Lower-level API - explicit index type selection
jldopen("file.jld2", "w") do f
    write_chunked(f, "dataset", data;
                  chunks=(5, 5),
                  indexing=:v2btree,       # :single, :implicit, :fixed, :extensible, :v2btree
                  max_dims=(nothing, nothing))
end
```

**Decision tree for automatic selection**:
```julia
function select_chunk_index_type(data_size, chunk_size, max_dims, fill_value)
    # Single chunk if chunks == data size
    if chunk_size == data_size
        return :single_chunk
    end

    # Count unlimited dimensions
    n_unlimited = count(isnothing, max_dims)

    if n_unlimited == 0
        # No unlimited dimensions
        if !isnothing(fill_value)
            return :implicit_index  # Early allocation
        else
            return :fixed_array     # Most common case
        end
    elseif n_unlimited == 1
        return :extensible_array
    else
        return :v2btree
    end
end
```

#### 3. Create API Module Structure (2-3 hours)

**New file**: `src/chunked_writing_api.jl`

```julia
# Public API exports
export ChunkedArray, write_chunked

# Helper types
struct ChunkedArray{T,N}
    data::AbstractArray{T,N}
    chunk_size::NTuple{N,Int}
    max_dims::Union{Nothing, NTuple{N,Union{Int,Nothing}}}
    fill_value::Union{Nothing, T}
    indexing::Union{Symbol, Nothing}  # Manual override
end

# Convenience constructor
ChunkedArray(data; chunks, max_dims=nothing, fill_value=nothing, indexing=nothing) = ...

# Main writing function
function write_chunked(f::JLDFile, name::String, data::AbstractArray;
                       chunks::NTuple{N,Int},
                       max_dims=nothing,
                       fill_value=nothing,
                       indexing=nothing,
                       filters=nothing) where N
    # Select chunk index type
    index_type = if !isnothing(indexing)
        indexing  # User override
    else
        select_chunk_index_type(size(data), chunks, max_dims, fill_value)
    end

    # Dispatch to appropriate writer
    if index_type == :single_chunk
        write_single_chunk(f, name, data, chunks, filters)
    elseif index_type == :implicit_index
        write_implicit_index(f, name, data, chunks, max_dims, fill_value, filters)
    # ... etc
    end
end
```

#### 4. Write API Documentation (2-3 hours)

**File**: `docs/src/chunked_writing.md`

- Explain chunking concepts
- Show examples for each index type
- Document automatic selection logic
- Provide performance guidelines
- Show h5py interoperability examples

#### 5. API Review and Testing (2-3 hours)

- Create minimal test cases for API
- Verify dispatch logic
- Test with stub implementations
- Get feedback on ergonomics

**Deliverables**:
- `src/chunked_writing_api.jl` - API implementation (with stubs)
- `docs/src/chunked_writing.md` - Documentation
- `test/chunked_writing_api_test.jl` - API tests
- Design review document

---

## Phase 1: Single Chunk Writing (Type 1) (1-2 days)

### Goal
Implement writing for datasets that fit in a single chunk (simplest case).

### Background
Already exists in `src/chunked_array.jl` but needs integration with new API.

### Tasks

#### 1. Implement Writer (2-3 hours)

**File**: `src/writers/single_chunk_writer.jl`

```julia
function write_single_chunk(f::JLDFile, name::String, data::AbstractArray,
                            chunk_size::NTuple, filters)
    # Write data as single chunk
    # Set DataLayout version 4, indexing type 1
    # Store chunk address directly in layout
end
```

**Key implementation details**:
- DataLayout version 4
- `chunk_indexing_type = 1`
- `data_address` = chunk data location
- Optional: `data_size` and filter info if filtered
- No separate index structure needed

#### 2. Create Test Files (1-2 hours)

**Python script**: `test/create_single_chunk_test.py`
```python
import h5py
import numpy as np

data = np.arange(100).reshape(10, 10).astype('f')
with h5py.File('single_chunk_test.h5', 'w') as f:
    # Single chunk = data size
    f.create_dataset('single', data=data, chunks=(10, 10))
```

#### 3. Write Tests (2-3 hours)

**File**: `test/single_chunk_writing_test.jl`

```julia
@testset "Single Chunk Writing" begin
    # Test basic writing
    jldopen("test_single.jld2", "w") do f
        data = reshape(1.0f0:100.0f0, 10, 10)
        write_chunked(f, "single", data; chunks=(10, 10))
    end

    # Verify with JLD2 reading
    result = jldopen("test_single.jld2") do f
        f["single"]
    end
    @test result == reshape(1.0f0:100.0f0, 10, 10)

    # Verify with h5py
    run(`python3 -c "import h5py; ..."`)

    # Verify structure with h5debug
    output = read(`h5debug test_single.jld2 ...`, String)
    @test contains(output, "Index Type: Single Chunk")
end
```

#### 4. Verify HDF5 Compliance (1-2 hours)

Use h5debug/h5dump to verify:
- Correct DataLayout message structure
- Chunk indexing type = 1
- Data is readable by h5py
- Proper filter pipeline if compressed

**Deliverables**:
- `src/writers/single_chunk_writer.jl`
- `test/create_single_chunk_test.py`
- `test/single_chunk_writing_test.jl`
- Verified HDF5 compliance

---

## Phase 2: Fixed Array Writing (Type 3) (2-3 days)

### Goal
Implement writing for fixed-size chunked datasets (most common case).

### Tasks

#### 1. Study Fixed Array Structure (2-3 hours)

**Reference files**:
- Read `PHASE3_COMPLETE.md` for structure details
- Analyze existing test files
- Review HDF5 spec Section VII.C

**Fixed Array components**:
```
Header (signature "FAHD")
  ├─ version, client_id
  ├─ element_size (chunk record size)
  ├─ nelmts (total number of chunks)
  ├─ data_block_address
  └─ checksum

Data Block (signature "FADB")
  ├─ header_address (back-pointer)
  ├─ Chunk records (address only for unfiltered)
  └─ Optional: size + filter_mask if filtered
```

#### 2. Implement Fixed Array Writer (6-8 hours)

**File**: `src/writers/fixed_array_writer.jl`

```julia
struct FixedArrayHeader
    version::UInt8
    client_id::UInt8
    element_size::UInt8
    nelmts::UInt64
    data_block_address::RelOffset
end

function write_fixed_array_header(f::JLDFile, nelmts, element_size)
    # Allocate space for header
    offset = allocate_space(f, calculate_header_size())

    # Write header fields
    seek(f.io, fileoffset(f, offset))
    jlwrite(f.io, htol(0x44484146))  # "FAHD"
    jlwrite(f.io, UInt8(0))          # version
    jlwrite(f.io, UInt8(0))          # client_id
    jlwrite(f.io, element_size)
    jlwrite(f.io, nelmts)
    jlwrite(f.io, data_block_offset)
    jlwrite(f.io, calculate_checksum(...))

    return offset
end

function write_fixed_array_data_block(f::JLDFile, chunk_records, header_offset)
    # Write signature "FADB"
    # Write header back-pointer
    # Write all chunk records
    # Write checksum
end

function write_fixed_array(f::JLDFile, name::String, data::AbstractArray,
                           chunk_size::NTuple, filters)
    # 1. Calculate number of chunks
    nchunks = calculate_nchunks(size(data), chunk_size)

    # 2. Write all chunks, collect addresses
    chunk_records = []
    for chunk_idx in enumerate_chunks(size(data), chunk_size)
        chunk_data = extract_chunk(data, chunk_idx, chunk_size)
        chunk_addr = write_chunk_data(f, chunk_data, filters)
        push!(chunk_records, chunk_addr)
    end

    # 3. Write data block
    data_block_offset = write_fixed_array_data_block(f, chunk_records, UNDEFINED_ADDRESS)

    # 4. Write header
    header_offset = write_fixed_array_header(f, nchunks, element_size)

    # 5. Update header with data block address
    update_header_data_block_address(f, header_offset, data_block_offset)

    # 6. Update data block with header address
    update_data_block_header_address(f, data_block_offset, header_offset)

    # 7. Create DataLayout message
    layout = create_datalayout_v4(header_offset, chunk_indexing_type=3, ...)

    # 8. Write dataset with layout
    write_dataset(f, name, data, layout)
end
```

**Key challenges**:
- Circular references (header ↔ data block)
- Checksum calculation
- Element size determination
- Chunk enumeration order

#### 3. Create Test Files (1-2 hours)

```python
# test/create_fixed_array_test.py
data = np.arange(300).reshape(30, 10).astype('f')
with h5py.File('fixed_array_test.h5', 'w') as f:
    f.create_dataset('fixed', data=data, chunks=(3, 2))
```

#### 4. Write Tests (3-4 hours)

```julia
@testset "Fixed Array Writing" begin
    # Test basic writing
    # Test different chunk sizes
    # Test with filters
    # Cross-validate with h5py
    # Verify structure with h5debug

    # Test round-trip
    original = rand(Float32, 30, 10)
    jldopen("test.jld2", "w") do f
        write_chunked(f, "data", original; chunks=(3, 2))
    end

    loaded = jldopen("test.jld2") do f
        f["data"]
    end

    @test loaded ≈ original

    # Verify h5py can read it
    run(`python3 -c "import h5py; assert h5py.File('test.jld2')['data'][:].sum() == $(sum(original))"`)
end
```

#### 5. Debug and Verify (2-3 hours)

- Use h5debug to inspect structure
- Verify checksums are correct
- Test with various array sizes
- Ensure h5py compatibility

**Deliverables**:
- `src/writers/fixed_array_writer.jl` (~300-400 lines)
- `test/create_fixed_array_test.py`
- `test/fixed_array_writing_test.jl`
- Verified HDF5 compliance

---

## Phase 3: Implicit Index Writing (Type 2) (1-2 days)

### Goal
Write datasets with early space allocation (all chunks pre-allocated).

### Tasks

#### 1. Implement Implicit Index Writer (3-4 hours)

**File**: `src/writers/implicit_index_writer.jl`

```julia
function write_implicit_index(f::JLDFile, name::String, data::AbstractArray,
                              chunk_size::NTuple, max_dims, fill_value, filters)
    # Similar to Fixed Array but simpler
    # No separate index structure
    # Chunks stored contiguously
    # Address = base_address + chunk_linear_index * chunk_size

    # 1. Calculate space needed
    nchunks = calculate_nchunks(max_dims, chunk_size)
    total_size = nchunks * chunk_element_size

    # 2. Allocate contiguous block
    base_address = allocate_space(f, total_size)

    # 3. Write chunks in order
    for (idx, chunk_coords) in enumerate(enumerate_chunks(max_dims, chunk_size))
        chunk_data = if chunk_in_bounds(chunk_coords, size(data))
            extract_chunk(data, chunk_coords, chunk_size)
        else
            fill_chunk(chunk_size, fill_value)
        end

        chunk_offset = base_address + (idx - 1) * chunk_element_size
        write_chunk_at_offset(f, chunk_data, chunk_offset)
    end

    # 4. Create DataLayout with base address
    layout = create_datalayout_v4(base_address, chunk_indexing_type=2, ...)
end
```

**Key points**:
- No index header needed
- Contiguous storage
- Fill value for uninitialized chunks
- Simple address calculation

#### 2. Test and Verify (2-3 hours)

Create tests similar to Fixed Array but with:
- Fill value specification
- Partial array initialization
- Verification of contiguous layout

**Deliverables**:
- `src/writers/implicit_index_writer.jl` (~150-200 lines)
- Tests
- HDF5 compliance verification

---

## Phase 4: Extensible Array Writing (Type 4) (3-4 days)

### Goal
Write datasets with one unlimited dimension.

### Tasks

#### 1. Study Extensible Array Structure (3-4 hours)

**Complex multi-level structure**:
```
Header (EAHD)
  ├─ Index Block (EAIB)
  │   ├─ Direct elements (chunk records)
  │   └─ Data block pointers
  └─ Data Blocks (EADB)
      └─ Chunk records
```

Review `PHASE5_TYPE4_COMPLETE.md` for details.

#### 2. Implement Extensible Array Writer (8-12 hours)

**File**: `src/writers/extensible_array_writer.jl`

This is the most complex writer due to:
- Multi-level index structure
- Parameter calculation (index_blk_elmts, data_blk_min_elmts, etc.)
- Multiple blocks to write and link
- Complex addressing

```julia
function calculate_extensible_array_parameters(nchunks_max)
    # Calculate optimal parameters based on expected max chunks
    # Parameters from HDF5 library defaults
    index_blk_elmts = ...
    data_blk_min_elmts = ...
    secondary_blk_min_data_ptrs = ...
    # etc.
end

function write_extensible_array(f::JLDFile, name::String, data, chunk_size, max_dims, filters)
    # Complex multi-step process
    # 1. Calculate parameters
    # 2. Write chunks
    # 3. Write data blocks
    # 4. Write index block
    # 5. Write header
    # 6. Link everything with back-pointers
end
```

#### 3. Test Extensively (4-6 hours)

- Multiple data sizes
- Growing datasets (if extending support added)
- Cross-validation with h5py
- Structure verification

**Deliverables**:
- `src/writers/extensible_array_writer.jl` (~400-500 lines)
- Comprehensive tests
- HDF5 compliance verification

---

## Phase 5: V2 B-tree Writing (Type 5) (4-5 days)

### Goal
Write datasets with multiple unlimited dimensions.

### Tasks

#### 1. Study V2 B-tree Structure (3-4 hours)

Review implementation from reading side:
- Node structure (header, internal, leaf)
- Key format
- Tree balancing (not needed for static writing)
- Record format

#### 2. Implement V2 B-tree Writer (10-14 hours)

**File**: `src/writers/v2btree_writer.jl`

```julia
function write_v2btree(f::JLDFile, name::String, data, chunk_size, max_dims, filters)
    # 1. Write all chunks, collect records
    chunk_records = []
    for chunk_coords in enumerate_chunks(size(data), chunk_size)
        chunk_data = extract_chunk(data, chunk_coords, chunk_size)
        chunk_addr = write_chunk_data(f, chunk_data, filters)
        push!(chunk_records, V2BTreeRecord(chunk_addr, chunk_coords))
    end

    # 2. Sort records by chunk coordinates (HDF5 order)
    sort!(chunk_records, by=r -> r.coords)

    # 3. Build tree from sorted records
    if length(chunk_records) <= max_records_per_leaf
        # Single leaf node
        root = create_leaf_node(chunk_records)
    else
        # Build tree bottom-up
        leaves = split_into_leaves(chunk_records, max_records_per_leaf)
        root = build_tree_from_leaves(leaves)
    end

    # 4. Write tree nodes to file
    root_offset = write_v2btree_nodes(f, root)

    # 5. Write header
    header_offset = write_v2btree_header(f, root_offset, nrecords, depth)

    # 6. Create DataLayout
    layout = create_datalayout_v4(header_offset, chunk_indexing_type=5, ...)
end

function build_tree_from_leaves(leaves, max_internal_children)
    # Build internal nodes level by level
    current_level = leaves

    while length(current_level) > 1
        next_level = []
        for group in partition(current_level, max_internal_children)
            internal_node = create_internal_node(group)
            push!(next_level, internal_node)
        end
        current_level = next_level
    end

    return current_level[1]  # Root
end
```

**Key challenges**:
- Tree construction algorithm
- Key extraction from child nodes
- Node size calculation
- Checksums for each node

#### 3. Test and Verify (3-4 hours)

- Multiple unlimited dimensions
- Various array sizes
- Compare with h5py output
- Verify tree structure with h5debug

**Deliverables**:
- `src/writers/v2btree_writer.jl` (~500-600 lines)
- Comprehensive tests
- HDF5 compliance verification

---

## Phase 6: Integration and Optimization (2-3 days)

### Tasks

#### 1. Integrate All Writers (3-4 hours)

- Wire up all writers to main API
- Test automatic selection logic
- Ensure consistent error handling
- Add progress callbacks for large datasets

#### 2. Performance Testing (4-6 hours)

```julia
# Benchmark each indexing type
@benchmark begin
    jldopen("test.jld2", "w") do f
        write_chunked(f, "data", large_array; chunks=optimal_chunks)
    end
end

# Compare with h5py
```

#### 3. Write Comprehensive Documentation (4-6 hours)

**File**: `docs/src/chunked_arrays.md`

- Complete guide to chunked arrays
- When to use each indexing type
- Performance characteristics
- Examples for common use cases
- Interoperability with HDF5 tools

#### 4. Add Examples (2-3 hours)

**File**: `examples/chunked_writing_examples.jl`

```julia
# Example 1: Image stack (extensible in time)
# Example 2: Simulation output (v2 btree for 3D growth)
# Example 3: Compressed data (fixed array with filters)
# Example 4: Sparse data (implicit with fill value)
```

#### 5. Final Testing (3-4 hours)

- Run full test suite
- Test on different platforms
- Verify h5py interoperability
- Test with external HDF5 tools (HDFView, etc.)

**Deliverables**:
- Complete, tested implementation
- Comprehensive documentation
- Example code
- Performance benchmarks

---

## Testing Strategy

### Unit Tests
- Test each writer independently
- Test parameter calculation
- Test helper functions

### Integration Tests
- Test API with all index types
- Test automatic selection
- Test with filters
- Test with various data types

### Compatibility Tests
- Write with JLD2, read with h5py
- Write with h5py, read with JLD2
- Verify with h5debug/h5dump
- Test with HDFView

### Performance Tests
- Benchmark writing speed
- Measure file size
- Compare with h5py
- Test with large datasets

---

## File Structure

```
src/
├── chunked_writing_api.jl          # Phase 0: Public API
└── writers/
    ├── single_chunk_writer.jl      # Phase 1
    ├── fixed_array_writer.jl       # Phase 2
    ├── implicit_index_writer.jl    # Phase 3
    ├── extensible_array_writer.jl  # Phase 4
    └── v2btree_writer.jl           # Phase 5

test/
├── chunked_writing_api_test.jl
├── writers/
│   ├── single_chunk_test.jl
│   ├── fixed_array_test.jl
│   ├── implicit_index_test.jl
│   ├── extensible_array_test.jl
│   └── v2btree_test.jl
└── create_tests/
    ├── create_single_chunk_test.py
    ├── create_fixed_array_test.py
    ├── create_implicit_index_test.py
    ├── create_extensible_array_test.py
    └── create_v2btree_test.py

docs/src/
├── chunked_arrays.md
└── chunked_writing.md

examples/
└── chunked_writing_examples.jl
```

---

## Estimated Timeline

| Phase | Duration | Complexity |
|-------|----------|------------|
| Phase 0: API Design | 2-3 days | Medium |
| Phase 1: Single Chunk | 1-2 days | Low |
| Phase 2: Fixed Array | 2-3 days | Medium |
| Phase 3: Implicit Index | 1-2 days | Low |
| Phase 4: Extensible Array | 3-4 days | High |
| Phase 5: V2 B-tree | 4-5 days | Very High |
| Phase 6: Integration | 2-3 days | Medium |
| **Total** | **15-22 days** | **~3-4 weeks** |

---

## Success Criteria

✅ **Must Have**:
1. All 5 chunk indexing types can be written
2. Files are HDF5-compliant (verified with h5debug)
3. Files are readable by h5py
4. Round-trip tests pass (write with JLD2, read with JLD2)
5. Cross-compatibility tests pass (write with JLD2, read with h5py)
6. Automatic chunk index selection works correctly
7. Full test coverage (>90%)
8. Documentation complete

✅ **Should Have**:
1. Performance comparable to h5py
2. Progress callbacks for large datasets
3. Comprehensive examples
4. Performance benchmarks

✅ **Nice to Have**:
1. Compression filter auto-selection
2. Optimal chunk size recommendations
3. Dataset extension support (append to existing datasets)
4. Parallel chunk writing

---

## Dependencies and Prerequisites

**Required Knowledge**:
- HDF5 specification (have it ready)
- Existing JLD2 codebase structure
- Understanding of all 5 chunk indexing types from reading implementation

**Required Tools**:
- h5py for cross-validation
- h5debug/h5dump for structure verification
- Python 3 for test file creation

**Code Prerequisites**:
- All reading implementations complete ✅
- Understanding of existing writing infrastructure
- Familiarity with JLD2 I/O patterns

---

## Risk Mitigation

**High-Risk Areas**:
1. **Circular references** (Fixed Array, Extensible Array)
   - Mitigation: Write in correct order, update back-pointers

2. **Checksum calculation**
   - Mitigation: Study h5py source, verify with h5debug

3. **Tree construction** (V2 B-tree)
   - Mitigation: Start simple (single leaf), build up complexity

4. **Performance issues**
   - Mitigation: Benchmark early, optimize hot paths

**Medium-Risk Areas**:
1. Parameter calculation (Extensible Array)
2. HDF5 compliance edge cases
3. Cross-platform compatibility

---

## Notes

- **Incremental approach**: Each phase builds on previous ones
- **Test-driven**: Create h5py test files first, then implement
- **Verify early**: Use h5debug after each phase
- **Document as you go**: Don't defer documentation
- **Reuse patterns**: Reading implementations provide templates

---

## References

- HDF5 Specification v1.10: Section VII (Data Storage)
- `PHASE3_COMPLETE.md` - Fixed Array patterns
- `PHASE5_TYPE4_COMPLETE.md` - Extensible Array patterns
- `src/v1btree.jl` - Existing B-tree writing
- h5py source code - Reference implementation
