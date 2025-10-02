# HDF5 Chunk Indexing - Quick Start Guide

## Creating Test Files

```bash
# Create v3 (legacy) test files
python3 create_chunk_index_test_files.py

# Create v4 (modern) test files
python3 create_v4_layout_test_files.py
```

## Inspecting Files

```bash
# View storage layout
h5dump -p -H test_file.h5

# Debug specific structures
h5debug test_file.h5 <offset> <ndims> <dim1> <dim2>...

# Test with JLD2
julia --project test_v4_reading.jl
```

## Current Support Status

| Indexing Type | DataLayout Ver | Status | Test File |
|---------------|----------------|--------|-----------|
| V1 B-tree | v3 | ✅ SUPPORTED | test_v1_btree_index.h5 |
| Single Chunk | v4 type 1 | ⚠️ PARTIAL | test_v4_single_chunk.h5 |
| Implicit | v4 type 2 | ❌ NOT TESTED | - |
| Fixed Array | v4 type 3 | ❌ ERROR | test_v4_fixed_array.h5 |
| Extensible Array | v4 type 4 | ❌ PARSER ERROR | test_v4_extensible_array.h5 |
| V2 B-tree | v4 type 5 | ❌ PARSER ERROR | test_v4_v2btree.h5 |

## Implementation Phases

### Phase 1: Fix Parser (PRIORITY) - 2-4 hrs
**Goal**: Read DataLayout v4 indexing info correctly
**File**: `src/datalayouts.jl`
**Test**: All test files print header messages without errors

### Phase 2: Single Chunk - 2-3 hrs
**Goal**: Fix data corruption in single chunk reading
**Files**: `src/datalayouts.jl`, `src/datasets.jl`
**Test**: `test_v4_single_chunk.h5` reads correctly

### Phase 3: Fixed Array - 6-8 hrs
**Goal**: Most common v4 indexing type
**New file**: `src/fixed_array.jl`
**Test**: `test_v4_fixed_array.h5` reads all 50 chunks

### Phase 4: Extensible Array - 10-12 hrs
**Goal**: One unlimited dimension support
**New file**: `src/extensible_array.jl`
**Test**: `test_v4_extensible_array.h5` with extensions

### Phase 5: V2 B-tree - 12-15 hrs
**Goal**: Multiple unlimited dimensions
**New file**: `src/btree_v2.jl`
**Test**: `test_v4_v2btree.h5` with multi-dim growth

## Key Code Locations

```julia
# DataLayout parsing
src/datalayouts.jl:read_layout_msg()  # Add v4 parsing

# Chunk reading
src/datasets.jl:read_dataset()  # Dispatch to new indexing types

# V1 B-tree (reference)
src/btree.jl  # Existing implementation
```

## Testing Pattern

```julia
# 1. Create test file (Python)
# 2. Verify header messages (Julia)
jldopen("test.h5") do f
    JLD2.print_header_messages(f, "dataset")
end

# 3. Read data and verify
data = jldopen("test.h5") do f
    f["dataset"]
end

# 4. Compare with h5py
# python3 -c "import h5py; print(h5py.File('test.h5')['dataset'][:])"
```

## Chunk Indexing Selection Logic (for Writing)

```julia
function select_chunk_indexing_type(dims, maxdims, chunk_dims)
    num_chunks = prod(cld.(dims, chunk_dims))

    if num_chunks == 1
        return SingleChunkIndex  # Type 1
    elseif all(dims .== maxdims)  # Fixed dimensions
        return FixedArrayIndex  # Type 3
    elseif count(maxdims .== -1) == 1  # One unlimited dim
        return ExtensibleArrayIndex  # Type 4
    else  # Multiple unlimited dimensions
        return V2BTreeIndex  # Type 5
    end
end
```

## HDF5 Specification Quick Reference

**DataLayout Message**: Section IV.A.2.i
**Chunk Indexing Types**: Appendix C (Section VII)

### Version 4 DataLayout Structure

```
Byte offset | Field
------------|------------------
0           | Version (4)
1           | Layout Class (2 = Chunked)
2           | Flags
3           | Dimensionality
4           | Dimension Size Encoded Length
5+          | Dimension Sizes (variable)
...         | Chunk Indexing Type (1 byte)
...         | Indexing Type Information (variable)
...         | Address (8 bytes)
```

### Indexing Type Information Sizes

| Type | Name | Extra Fields | Size |
|------|------|--------------|------|
| 1 | Single Chunk | filter_size (8), filters (var) | 8+ if filtered |
| 2 | Implicit | none | 0 |
| 3 | Fixed Array | page_bits (1) | 1 |
| 4 | Extensible Array | 5 fields (1 byte each) | 5 |
| 5 | V2 B-tree | node_size (4), split% (1), merge% (1) | 6 |

## Common Errors and Fixes

### "Unknown chunk indexing type"
**Location**: `src/datalayouts.jl`, reading function
**Fix**: Add support for indexing type parsing and chunk lookup

### Parser MethodError
**Issue**: Trying to read symbol instead of bytes
**Fix**: Correct field type in read call

### Data Corruption
**Issue**: Incorrect chunk address or data layout interpretation
**Fix**: Verify address calculation, check endianness, validate chunk boundaries

## Resources

- Full analysis: `CHUNK_INDEXING_ANALYSIS.md`
- Test scripts: `create_*.py`, `test_v4_reading.jl`
- HDF5 spec: `hdf5_format/04_data_objects.md`
- Debugging guide: `DEVELOPMENT_INSIGHTS.md`
