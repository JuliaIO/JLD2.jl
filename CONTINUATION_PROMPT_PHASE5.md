# Continuation Prompt: HDF5 v4 Chunk Indexing - Phase 5 (Remaining Types)

## Current Status (2025-10-01)

**Location**: `/workspace/JLD2.jl` (branch: `version4_chunking`)

**Completed Phases**:
- ✅ Phase 1: All v4 parsers (SingleChunkInfo, FixedArrayInfo, etc.) - COMPLETE
- ✅ Phase 2: Single Chunk Index (type 1) reading - COMPLETE
- ✅ Phase 3: Fixed Array Index (type 3) reading - COMPLETE
- ✅ Phase 4: Implicit Index (type 2) reading - COMPLETE

**Progress**: 3 of 5 v4 indexing types fully implemented and tested

## Next Task: Phase 5 - Remaining v4 Indexing Types

Implement reading support for:

### Priority 1: Type 4 - Extensible Array Index (RECOMMENDED START)
- **Complexity**: Medium (4-6 hours estimated)
- **Use case**: Datasets with ONE unlimited dimension (time-series data)
- **Key challenge**: Block navigation and dynamic array growth
- **Test file**: Create with `h5py` using `maxshape=(None, 10)` parameter

### Priority 2: Type 5 - V2 B-tree Index
- **Complexity**: High (6-10 hours estimated)
- **Use case**: Datasets with MULTIPLE unlimited dimensions
- **Key challenge**: Different B-tree format from existing V1 implementation
- **Test file**: Create with `h5py` using `maxshape=(None, None)`

## Essential Reading (in order)

### 1. Quick Start - Phase 4 Summary
- **`PHASE4_IMPLICIT_INDEX_COMPLETE.md`** - Most recent implementation (read first!)
  - Validates patterns from Phase 3
  - **Critical**: Address type handling insights (line 774-799)
  - Code reuse patterns confirmed
  - Testing workflow validated again

### 2. Pattern Reference - Phase 3 Details
- **`PHASE3_COMPLETE.md`** - Fixed Array implementation patterns
  - Dimension handling (CRITICAL - lines 86-99)
  - Chunk index calculation algorithm
  - Integration pattern for new types

### 3. Development Workflow
- **`DEVELOPMENT_INSIGHTS.md`** - Critical patterns and tooling
  - **NEW**: Implicit Index section (lines 753-1044) - read for latest insights
  - Fixed Array section (lines 431-751) - dimension handling
  - Use `-O1` flag for faster iteration
  - Strategic debug output patterns
  - h5py validation workflow

### 4. Overall Plan
- **`CHUNK_INDEXING_ANALYSIS.md`** - Complete roadmap
  - Phase 4+ section for Extensible Array details
  - HDF5 spec references

### 5. Project Guidelines
- **`CLAUDE.md`** + **`JLD2.jl/CLAUDE.md`** - Project-specific patterns
  - **Critical**: Array indexing section (avoid "row-major" confusion)
  - Julia constructor patterns
  - Error handling guidelines

## Key Patterns Established (Phases 3-4)

### Address Type Handling (NEW from Phase 4!)
```julia
# Different v4 types store addresses differently:
# - Fixed Array (type 3): layout.data_offset is RelOffset → use fileoffset(f, ...)
# - Implicit Index (type 2): layout.data_offset is Int64 → use Int64(...)
# - Extensible Array (type 4): Likely RelOffset (verify!)
```

### Dimension Handling (Validated in Phases 3-4)
```julia
# Trust construct_array - it already reversed dimensions
array_dims_julia = size(v)
array_dims_hdf5 = reverse(array_dims_julia)

# layout.chunk_dimensions are in HDF5 order
chunk_dims_hdf5 = layout.chunk_dimensions[1:ndims]
chunk_dims_julia = reverse(chunk_dims_hdf5)
```

### Chunk Index Calculation (Reusable)
```julia
# compute_chunk_index() from fixed_array.jl works for all v4 types
# Converts N-dimensional coords to linear index following HDF5 spec
```

### Integration Pattern (Consistent)
1. Create `src/extensible_array.jl` (or v2btree.jl)
2. Implement `read_*_chunks(f, v, dataspace, rr, layout, filters, header_offset, ndims)`
3. Add dispatch in `src/chunked_array.jl`:
   ```julia
   elseif layout.version == 4 && layout.chunk_indexing_type == 4
       return read_extensible_array_chunks(...)
   ```
4. Add `include("extensible_array.jl")` to `src/JLD2.jl`
5. Test with h5py-generated file

## Testing Workflow (Validated Twice)

### 1. Create Test File with h5py
```python
import h5py, numpy as np

# Extensible Array (type 4)
f = h5py.File('test_v4_extensible.h5', 'w', libver='v110')
data = np.arange(300, dtype='float32').reshape(30, 10)
f.create_dataset('extensible', data=data, chunks=(3, 2), maxshape=(None, 10))
f.close()
```

### 2. Implement Following Pattern
- Use `src/fixed_array.jl` as template (most similar structure)
- Parse header structure with signature validation
- Handle block navigation
- Reuse chunk reading infrastructure

### 3. Test with Fast Iteration
```bash
julia -O1 --project -e 'using JLD2; data = jldopen("test.h5") do f; f["dataset"]; end; println(size(data), " sum=", sum(data))'
```

### 4. Validate Against h5py
```python
import h5py
f = h5py.File('test.h5', 'r')
data = f['dataset'][:]
print('Shape:', data.shape, 'Sum:', data.sum())  # Compare with Julia output
```

## Development Efficiency Tips

### From Phase 4 Success (1 hour implementation!)
1. **Read documentation first** - saves debugging time
2. **Use `-O1` flag** - faster precompilation during development
3. **Sequential test data** - easy to spot corruption (0, 1, 2, ..., 299)
4. **Validate early** - compare with h5py after each milestone
5. **Strategic debug output** - only at key decision points, remove before commit

### Code Reuse Opportunities
- `compute_chunk_index()` - reuse if applicable
- Dimension handling patterns - identical
- Chunk reading with `read_chunk_with_filters!()` - identical
- Array intersection logic - identical

## Quick Start Commands

```bash
cd /workspace/JLD2.jl
git status  # Verify on version4_chunking branch

# Read latest insights
cat PHASE4_IMPLICIT_INDEX_COMPLETE.md | grep -A 20 "Address Type"
cat PHASE3_COMPLETE.md | grep -A 30 "Key Implementation"
cat DEVELOPMENT_INSIGHTS.md | tail -300  # Latest insights

# Start implementation
# 1. Read HDF5 spec: hdf5_format/07_appendix_c.md (Section VII.D for Extensible Array)
# 2. Create test file with h5py
# 3. Implement following fixed_array.jl pattern
# 4. Test incrementally with -O1 flag
```

## Success Criteria

For each indexing type:
- ✅ Reads test file without errors
- ✅ Data matches h5dump/h5py output exactly
- ✅ All chunks read correctly (verify counts and sum)
- ✅ No segfaults or memory corruption
- ✅ Existing JLD2 tests still pass
- ✅ Documentation updated (PHASE*_COMPLETE.md)

## Time Estimates (Based on Phases 3-4)

- **Type 4 (Extensible Array)**: 4-6 hours (moderate complexity)
  - Header parsing: ~1 hour
  - Block navigation: ~2 hours
  - Testing and validation: ~1-2 hours

- **Type 5 (V2 B-tree)**: 6-10 hours (most complex)
  - V2 B-tree structure: ~3 hours
  - Chunk record handling: ~2 hours
  - Testing and validation: ~2-3 hours
  - May need debugging time

## Key Files to Reference

**Implementation Examples**:
- `src/fixed_array.jl` (334 lines) - Header parsing, paging, chunk lookup
- `src/implicit_index.jl` (109 lines) - Simplest pattern, address handling

**Integration Points**:
- `src/chunked_array.jl` - Where to add dispatch (around line 337-343)
- `src/JLD2.jl` - Where to add include (around line 528-529)

**Testing Examples**:
- `create_implicit_index_test.py` - How to create test files
- `test_implicit_index.jl` - Validation test pattern

**HDF5 Specification**:
- `hdf5_format/07_appendix_c.md` - All chunk indexing specifications
  - Section VII.D: Extensible Array
  - Section VII.E: V2 B-tree

## Critical Gotchas (Learned in Phases 3-4)

1. **Address type varies by indexing type** - always check what `layout.data_offset` contains
2. **Use `size(v)` not dataspace dimensions** - construct_array already reversed them
3. **layout.chunk_dimensions are in HDF5 order** - reverse for Julia
4. **Element indices ≠ chunk grid indices** - multiply by chunk size for element indices
5. **chunk_root is 0-based offset** - don't add 1
6. **Validate with h5py early and often** - catches bugs immediately

---

**Ready to Continue**: All infrastructure in place, patterns validated twice (Phases 3-4), clear path forward.

**Current v4 Support**: 3/5 types complete (Single Chunk, Implicit, Fixed Array)
**Remaining**: Extensible Array (type 4) → V2 B-tree (type 5)

**Start with Type 4 (Extensible Array)** - medium complexity, builds on Fixed Array patterns.
