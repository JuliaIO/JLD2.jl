# Continuation Prompt: HDF5 v4 Chunk Indexing Implementation

## Current Status

**Completed**: Phases 1-3 of HDF5 DataLayout v4 chunk indexing support
- ✅ Phase 1: All v4 parsers (SingleChunkInfo, FixedArrayInfo, etc.) - COMPLETE
- ✅ Phase 2: Single Chunk Index (type 1) reading - COMPLETE
- ✅ Phase 3: Fixed Array Index (type 3) reading - COMPLETE

**Location**: `/workspace/JLD2.jl` (branch: `version4_chunking`)

## Next Task: Phase 4 - Remaining v4 Indexing Types

Implement reading support for the remaining HDF5 v4 chunk indexing types:

### Priority Order

1. **Type 2: Implicit Index** (EASIEST - start here)
   - Chunks stored contiguously in file
   - No index structure, just calculate addresses
   - Similar to contiguous storage but chunked

2. **Type 4: Extensible Array Index** (MEDIUM)
   - For arrays with variable maximum dimensions
   - More complex than Fixed Array
   - Test file: Create with h5py using `maxshape` parameter

3. **Type 5: V2 B-tree Index** (COMPLEX)
   - For sparse or variable-size chunks
   - Most complex indexing type
   - Reuses B-tree v2 infrastructure

### Essential Documentation to Read First

**Implementation Plan**:
- `CHUNK_INDEXING_ANALYSIS.md` - Complete roadmap (read Phase 4+ section)
- `CHUNK_INDEXING_QUICKSTART.md` - Quick reference

**Completed Phase Reports**:
- `PHASE1_COMPLETE.md` - Parser infrastructure
- `PHASE2_COMPLETE.md` - Single Chunk insights
- `PHASE3_COMPLETE.md` - Fixed Array patterns (MOST RELEVANT)

**Development Insights**:
- `DEVELOPMENT_INSIGHTS.md` - Critical patterns discovered (especially Fixed Array section)
- Contains debugging strategies, dimension handling, tooling tips

**Project Guidelines**:
- `/workspace/CLAUDE.md` - General Julia/JLD2 patterns
- `/workspace/JLD2.jl/CLAUDE.md` - JLD2-specific guidelines
- **Critical**: Array indexing section added in Phase 3 (avoid "row-major" confusion)

**HDF5 Specification**:
- `hdf5_format/04_data_objects.md` - DataLayout v4 specification
- `hdf5_format/07_appendix_c.md` - Chunk indexing details (Section VII.C)

### Key Implementation Patterns from Phase 3

**Dimension Handling** (CRITICAL):
```julia
# layout.chunk_dimensions are in HDF5 order
chunk_dims_hdf5 = layout.chunk_dimensions[1:ndims]
chunk_dims_julia = reverse(chunk_dims_hdf5)

# Use size(v) - construct_array already reversed dimensions!
array_dims_julia = size(v)
array_dims_hdf5 = reverse(array_dims_julia)
```

**Element vs Chunk Indices**:
```julia
# Chunk grid index (1-based Julia) → Element index (0-based HDF5)
element_indices = (chunk_grid_idx.I .- 1) .* chunk_dims_julia
hdf5_element_indices = reverse(element_indices)
chunk_indices = tuple(hdf5_element_indices..., 0)  # Add element size dim
```

**Integration Pattern**:
1. Create `src/implicit_index.jl` (or equivalent)
2. Add reading function: `read_implicit_index_chunks(f, v, ...)`
3. Update `src/chunked_array.jl:read_chunked_array()` with dispatch:
   ```julia
   elseif layout.version == 4 && layout.chunk_indexing_type == 2
       return read_implicit_index_chunks(f, v, dataspace, rr, layout, filters, header_offset, ndims)
   ```
4. Add `include("implicit_index.jl")` to `src/JLD2.jl`

### Testing Approach

**Create Test Files** with h5py:
```python
import h5py
import numpy as np

# Type 2: Implicit Index (contiguous chunks)
f = h5py.File('test_v4_implicit.h5', 'w')
data = np.arange(300, dtype='float32').reshape(30, 10)
f.create_dataset('implicit', data=data, chunks=(3, 2))

# Type 4: Extensible Array (expandable dimensions)
f.create_dataset('extensible', data=data, chunks=(3, 2), maxshape=(None, 10))
```

**Validation**:
```bash
julia -O1 --project -e 'using JLD2; data = jldopen("test.h5") do f; f["dataset"]; end; println(size(data), " ", data[1:10])'
python3 -c "import h5py; print(h5py.File('test.h5')['dataset'][:10].flatten())"
```

### Development Workflow

**Fast Iteration** (from Phase 3 insights):
```bash
# Use -O1 for faster compilation during development
julia -O1 --project -e 'using JLD2; ...'

# Strategic debug output (remove before commit)
if chunk_idx == 0
    println("DEBUG: first chunk at offset=$offset")
end

# Validate with external tools
h5dump -H test.h5  # Structure
h5ls -r -v --address test.h5  # Offsets
```

**Key Development Insights** (read DEVELOPMENT_INSIGHTS.md for details):
- Use predictable test data (0, 1, 2, ...) for easy corruption detection
- Compare with h5py at every stage
- Strategic debug output > verbose logging
- Read raw bytes to verify chunk storage
- Trust size(v) - don't re-read dimensions

### Success Criteria

For each indexing type:
- ✅ Reads test file without errors
- ✅ Data matches h5dump/h5py output exactly
- ✅ All chunks read correctly (verify counts)
- ✅ No segfaults or memory corruption
- ✅ Existing JLD2 tests still pass

### Implementation Time Estimates

- **Type 2 (Implicit)**: 2-3 hours (simple calculation-based)
- **Type 4 (Extensible Array)**: 4-6 hours (moderate complexity)
- **Type 5 (V2 B-tree)**: 6-10 hours (most complex)

**Start with Type 2** - it's the simplest and will provide a quick win to build momentum.

### Quick Start Command

```bash
cd /workspace/JLD2.jl
git status  # Verify on version4_chunking branch

# Read key documentation
cat CHUNK_INDEXING_ANALYSIS.md | grep -A 30 "Phase 4"
cat PHASE3_COMPLETE.md | grep -A 20 "Key Implementation"
cat DEVELOPMENT_INSIGHTS.md | grep -A 50 "Fixed Array"

# Start implementation
# 1. Create test file for Type 2 (Implicit Index)
# 2. Create src/implicit_index.jl following Fixed Array pattern
# 3. Test incrementally with -O1 flag
```

### References

- **Implementation**: `src/fixed_array.jl` - Use as template for structure
- **Integration**: `src/chunked_array.jl` - Where to add dispatch
- **Testing**: `test_v4_fixed_array.h5` - Example test file format
- **HDF5 Spec**: `hdf5_format/07_appendix_c.md` - Sections VII.C.2, VII.C.4, VII.C.5

---

**Ready to continue**: All infrastructure in place, Phase 3 patterns established, clear path forward for remaining indexing types. Start with Type 2 (Implicit Index) for quick progress.
