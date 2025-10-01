# Continuation Prompt: V2 B-tree Index (Type 5)

## Current Status (2025-10-01)

**Location**: `/workspace/JLD2.jl` (branch: `version4_chunking`)

**Latest Commit**: `aefd185` - "Add HDF5 v4 Extensible Array chunk indexing (type 4)"

**v4 Chunk Indexing Progress**: 4 of 5 types complete
- Type 1: Single Chunk ✅
- Type 2: Implicit Index ✅ (1 hour, 109 lines)
- Type 3: Fixed Array ✅ (6 hours, 334 lines)
- Type 4: Extensible Array ✅ (6 hours, 308 lines)
- **Type 5: V2 B-tree ⏳ YOU ARE HERE**

## Your Task: Implement V2 B-tree Chunk Indexing

Implement reading for datasets with **multiple unlimited dimensions** using HDF5 v2 B-tree indexing.

### Expected Complexity

**High** - Most complex v4 indexing type
- **Estimated time**: 8-12 hours
- **Estimated lines**: 400-600 lines
- **Complexity factors**:
  - Tree node navigation (internal + leaf nodes)
  - Variable-size keys based on dimensionality
  - Recursive or iterative traversal
  - Multiple node types and layouts

### Implementation Approach

#### 1. Create Test File First (30-60 min)

```python
# create_v2btree_test.py
import h5py
import numpy as np

# V2 B-tree requires multiple unlimited dimensions
data = np.arange(300, dtype='float32').reshape(10, 30)
f = h5py.File('test_v2btree.h5', 'w')

# Use multiple unlimited dimensions to trigger v2 B-tree
f.create_dataset('btree', data=data, chunks=(2, 3),
                 maxshape=(None, None))  # Both dimensions unlimited
f.close()

# Verify with h5dump
# h5dump -H test_v2btree.h5 | grep -A20 "DATASPACE"
```

**Verification**:
```bash
# Check it's actually v2 B-tree (not Extensible Array)
h5dump -H test_v2btree.h5 | grep -i "chunk"
# Should show v2 B-tree indexing info

# Get expected output
python3 -c "import h5py; print(h5py.File('test_v2btree.h5')['btree'][:].sum())"
```

#### 2. Study V2 B-tree Structure (30-60 min)

**Key files to read**:
- `src/v1btree.jl` - Existing B-tree implementation (similar patterns)
- `DEVELOPMENT_INSIGHTS.md` (lines 753-1044) - Fixed/Extensible Array patterns
- `PHASE3_COMPLETE.md` - Fixed Array as reference
- HDF5 spec: Section III.A.2 "Disk Format: Level 1A2 - Version 2 B-trees"

**V2 B-tree Structure**:
```
Header (signature "BTHD")
  ├─ version, type, node_size, split_percent, merge_percent
  ├─ root_node_address
  └─ depth

Internal Node (signature "BTIN")
  ├─ Keys: chunk indices in HDF5 order
  └─ Child pointers: addresses of child nodes

Leaf Node (signature "BTLF")
  ├─ Keys: chunk indices in HDF5 order
  └─ Chunk records: (address, size, filter_mask) tuples
```

**Critical Pattern**: Keys are **element indices** (not chunk counts):
- For 10×30 array with 2×3 chunks
- Chunk at Julia (1,1) → key = (0, 0, 0) in HDF5 order
- Chunk at Julia (3,1) → key = (0, 4, 0) in HDF5 order
- Chunk at Julia (1,4) → key = (9, 0, 0) in HDF5 order

#### 3. Implementation Pattern (4-8 hours)

**File structure**:
```julia
# src/v2btree_chunking.jl (new file)

"""
    read_v2btree_chunks(f, v, dataspace, rr, layout, filters, header_offset, ndims)

Read chunks indexed by HDF5 v2 B-tree (type 5).
Used when dataset has multiple unlimited dimensions.
"""
function read_v2btree_chunks(...)
    # Parse header
    header = read_v2btree_header(f, layout.data_offset)

    # Traverse tree to collect all chunk records
    chunks = traverse_v2btree(f, header, layout, ndims)

    # Read each chunk into array (reuse existing infrastructure)
    for chunk in chunks
        read_chunk_into_array!(f, v, chunk.addr, chunk.size,
                              chunk.coords, chunk_dims_julia,
                              filters, chunk.filter_mask, rr)
    end

    return v
end

function read_v2btree_header(f, header_addr)
    # Read "BTHD" signature, version, type, etc.
end

function traverse_v2btree(f, header, layout, ndims)
    # Recursive or iterative tree traversal
    # Return vector of chunk records
end

function read_internal_node(f, node_addr, layout, ndims)
    # Read "BTIN" node with keys and child pointers
end

function read_leaf_node(f, node_addr, layout, ndims)
    # Read "BTLF" node with keys and chunk records
end
```

**Key reuse opportunities**:
- Chunk reading: Reuse `read_chunk_into_array!()` from Extensible Array
- Dimension handling: Reuse patterns from Fixed/Extensible Array
- Linear to coords: Reuse or adapt existing utilities

#### 4. Testing Strategy (2-3 hours)

**Quick validation**:
```julia
# test_v2btree.jl
using JLD2, Test

data = jldopen("test_v2btree.h5") do f
    f["btree"]
end

@test size(data) == (10, 30)
@test sum(data) ≈ 44850.0  # Same as other tests
@test data[1,1] == 0.0f0
@test data[end,end] == 299.0f0

# Verify specific chunks
@test data[1,1] == 0.0f0    # First chunk
@test data[3,4] == 63.0f0   # Middle chunk
@test data[9,28] == 298.0f0 # Near end
```

**Integration test**: Run full test suite
```bash
julia --project -e 'using Pkg; Pkg.test()'  # 5-7 minutes
```

### Success Criteria

✅ **Implementation complete when**:
1. Test file reads correctly (sum matches h5py)
2. All chunk data validated element-by-element
3. Full JLD2 test suite passes
4. No debug output in final code
5. Test added to `test/chunked_arrays.jl`
6. Documentation added to `DEVELOPMENT_INSIGHTS.md`

### Critical Patterns from Previous Phases

**From Fixed Array** (PHASE3_COMPLETE.md):
- Use `size(v)` for array dimensions (already transformed)
- Calculate chunk indices in HDF5 order (reversed + 0-based)
- Element size dimension always 0 in chunk keys

**From Extensible Array** (PHASE5_TYPE4_COMPLETE.md):
- Variable-size fields: Check existing patterns (Fractal Heap, etc.)
- Multi-level navigation: Read addresses first, then data
- Default values: Fields after conditionals need `= UNDEFINED_ADDRESS`

**From Implicit Index** (PHASE4_IMPLICIT_INDEX_COMPLETE.md):
- Address type varies: Check if `layout.data_offset` is RelOffset or Int64
- Pattern reuse works: Don't reinvent common algorithms

### Common Pitfalls to Avoid

1. **Dimension ordering**: HDF5 keys are reversed from Julia, 0-based
2. **Key format**: Element indices (0, 4, 0), NOT chunk counts (0, 1, 0)
3. **Tree depth**: Don't assume depth=1, handle multiple levels
4. **Node types**: Internal vs leaf nodes have different layouts
5. **Type fields**: Use `chunk_addr::RelOffset` for type safety

### Development Workflow

```bash
# Fast iteration with -O1 flag
julia -O1 --project test_v2btree.jl  # ~7 seconds

# Compare with h5py frequently
python3 -c "import h5py; data = h5py.File('test_v2btree.h5')['btree'][:]; print(f'Sum: {data.sum()}, First: {data[0,0]}, Last: {data[-1,-1]}')"

# When implementation works
julia --project -e 'using Pkg; Pkg.test()'  # Full suite

# Debug with h5dump if needed
h5dump -d /btree test_v2btree.h5  # Show data
h5dump -H test_v2btree.h5         # Show structure only
```

### Files to Create/Modify

**New files**:
- `src/v2btree_chunking.jl` (400-600 lines)
- `create_v2btree_test.py` (test file generator)
- `test_v2btree.jl` (validation script)

**Modify**:
- `src/JLD2.jl` - Add `include("v2btree_chunking.jl")`
- `src/chunked_array.jl` - Add dispatch for type 5
- `test/chunked_arrays.jl` - Add test case
- `DEVELOPMENT_INSIGHTS.md` - Add Type 5 section

### Quick Reference Documentation

**Must read before starting**:
1. **DEVELOPMENT_INSIGHTS.md** (lines 753-1339) - Patterns from Types 2-4
2. **PHASE3_COMPLETE.md** - Fixed Array reference implementation
3. **CLAUDE.md** - Project conventions and testing patterns
4. **src/v1btree.jl** - Existing B-tree patterns (v1 vs v2 differences)

**For debugging**:
5. **CHUNK_INDEXING_ANALYSIS.md** - Overview of all v4 types
6. **DEVELOPMENT_INSIGHTS.md** (lines 1-192) - HDF5 debugging tools

### Estimated Timeline

| Phase | Time | Description |
|-------|------|-------------|
| Setup | 1 hour | Create test file, verify with h5py |
| Study | 1 hour | Read v1btree.jl, understand structure |
| Header | 1-2 hours | Implement header parsing |
| Traversal | 3-5 hours | Implement tree navigation |
| Integration | 1-2 hours | Connect to chunk reading |
| Testing | 2-3 hours | Validate and run full suite |
| **Total** | **8-12 hours** | Complete implementation |

### Next Steps After Type 5

Once V2 B-tree is complete, all 5 v4 chunk indexing types will be implemented! 🎉

**Potential follow-up work**:
1. Write comprehensive documentation for all v4 types
2. Performance benchmarking across indexing types
3. Consider implementing v4 **writing** (currently read-only)
4. Clean up temporary test files and scripts

---

## Quick Start Commands

```bash
cd /workspace/JLD2.jl

# 1. Create test file
python3 create_v2btree_test.py

# 2. Verify test file
python3 -c "import h5py; print(h5py.File('test_v2btree.h5')['btree'][:].sum())"
h5dump -H test_v2btree.h5 | grep -A20 "CHUNK"

# 3. Implement in src/v2btree_chunking.jl

# 4. Test quickly
julia -O1 --project test_v2btree.jl

# 5. Run full suite when ready
julia --project -e 'using Pkg; Pkg.test()'
```

---

**Current Branch**: `version4_chunking`
**Last Commit**: `aefd185` - Extensible Array (Type 4)
**Status**: Ready to implement final v4 indexing type

Good luck! 🚀
