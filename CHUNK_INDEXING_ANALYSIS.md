# HDF5 Chunk Indexing Mechanisms: Analysis and Implementation Plan

**Date**: 2025-10-01
**Author**: Claude Code Investigation
**Status**: Test files created, implementation plan drafted

## Executive Summary

This document analyzes HDF5 chunk indexing mechanisms and provides a comprehensive implementation plan for adding support to JLD2.jl. The investigation created test files for all chunk indexing types and identified current JLD2 support gaps.

## Background

HDF5 supports multiple chunk indexing mechanisms for efficient storage and retrieval of chunked datasets. These mechanisms evolved from the original V1 B-tree (DataLayout v1-3) to modern indexed structures (DataLayout v4) introduced in HDF5 1.10.0.

### HDF5 Format Specification Reference

- **DataLayout Message**: Section IV.A.2.i of HDF5 format specification
- **Chunk Indexing Types**: Appendix C (Section VII)
- **Version History**:
  - Versions 1-3: Use V1 B-tree for all chunked datasets
  - Version 4: Introduces 5 specialized indexing types

## Chunk Indexing Types

### DataLayout Version 3 (Legacy)

**V1 B-tree Index** - Currently **FULLY SUPPORTED** in JLD2
- Used for all chunked datasets in HDF5 files before v1.10
- Binary tree structure with variable-size keys
- Keys contain: `chunk_size`, `filter_mask`, and N-dimensional indices
- See existing implementation in `src/btree.jl` and `src/datalayouts.jl`

### DataLayout Version 4 (Modern)

Introduced in HDF5 1.10.0 for optimized chunking based on dataset properties.

#### 1. Single Chunk Index (Type 1)

**Status**: **PARTIALLY SUPPORTED** (reads but may have data issues)

**When used**: Dataset fits in exactly one chunk
- Example: 10×10 array with chunk size 10×10

**File format**:
- Chunk address stored directly in DataLayout message
- Optional filter information if chunk is compressed
- No separate index structure needed

**Current JLD2 behavior**:
- Reads without error
- May have data corruption issues (garbage values observed)
- Needs verification of data address calculation

**Test file**: `test_v4_single_chunk.h5`

#### 2. Implicit Index (Type 2)

**Status**: **NOT YET TESTED**

**When used**: Chunks stored contiguously in file
- All chunk addresses can be computed from base address
- Rare in practice, requires specific allocation settings

**File format**:
- Base address in DataLayout message
- No index structure needed
- Chunks stored sequentially

**Test file**: `test_implicit_index.h5` (created but uses v3, not v4)

#### 3. Fixed Array Index (Type 3)

**Status**: **NOT SUPPORTED** - Error: "Unknown chunk indexing type"

**When used**: Datasets with fixed dimensions (no unlimited)
- Number of chunks known at creation time
- Examples: 30×10 array with 3×2 chunks = 50 chunks total

**File format**:
```
DataLayout Message:
  - chunk_indexing_type: 3
  - page_bits: Number of bits for page size
  - data_address: Points to Fixed Array index structure

Fixed Array Structure (at data_address):
  - Signature: "FHFA" (Fixed Array)
  - Header with array parameters
  - Direct chunk address array
  - Optional paging for large arrays
```

**Implementation required**:
- Parse Fixed Array header structure
- Read chunk address array
- Handle paging if present

**Test file**: `test_v4_fixed_array.h5`

#### 4. Extensible Array Index (Type 4)

**Status**: **NOT SUPPORTED** - Parser error (MethodError reading symbol)

**When used**: Datasets with ONE unlimited dimension
- Common for time-series data growing along one axis
- Example: (∞, 10) array with (5, 5) chunks

**File format**:
```
DataLayout Message:
  - chunk_indexing_type: 4
  - max_bits: Max elements in array
  - index_elements: Elements in index block
  - min_pointers: Min data block pointers for superblock
  - min_elements: Min elements per data block
  - page_bits: Bits for page size
  - data_address: Points to Extensible Array structure

Extensible Array Structure:
  - Signature: "EAHD" (Extensible Array Header)
  - Index block with pointers
  - Data blocks and secondary blocks
  - Supperblocks for large arrays
```

**Implementation required**:
- Fix parser error in reading indexing type info
- Parse Extensible Array header
- Navigate index/data/secondary block structure
- Handle dynamic array growth

**Test file**: `test_v4_extensible_array.h5`

#### 5. Version 2 B-tree Index (Type 5)

**Status**: **NOT SUPPORTED** - Parser error

**When used**: Datasets with MULTIPLE unlimited dimensions
- Most flexible but higher overhead
- Example: (∞, ∞) array with (5, 5) chunks

**File format**:
```
DataLayout Message:
  - chunk_indexing_type: 5
  - node_size: Size of B-tree nodes
  - split_percent: Node split threshold
  - merge_percent: Node merge threshold
  - data_address: Points to V2 B-tree root

V2 B-tree Structure:
  - Different from V1 B-tree (already implemented)
  - Generic B-tree with specialized record types
  - Chunk records contain N-dimensional keys
  - See HDF5 spec Section III.A.2 for V2 B-tree format
```

**Implementation required**:
- Parse V2 B-tree structure (different from existing V1 code)
- Handle generic B-tree records
- Navigate chunk-specific record format

**Test files**: `test_v2_btree_index.h5`, `test_v4_v2btree.h5`

## Current JLD2 Implementation Status

### Supported Features
✅ **DataLayout v1-3** (V1 B-tree) - Full support
✅ **DataLayout v4 parsing** - Basic structure reading
✅ **Single Chunk Index (Type 1)** - Reads but needs data verification

### Unsupported Features
❌ **Fixed Array Index (Type 3)** - Recognized but throws "Unknown chunk indexing type"
❌ **Extensible Array Index (Type 4)** - Parser error in DataLayout message reading
❌ **V2 B-tree Index (Type 5)** - Parser error in DataLayout message reading
❌ **Implicit Index (Type 2)** - Not tested with v4 layout

### Code Locations

**DataLayout parsing**: `src/datalayouts.jl`
- `read_layout_msg(io, f, _=nothing)` - Main parsing function
- Currently handles versions 1-3
- Version 4 has basic field reading but incomplete indexing info parsing

**Chunk reading**: `src/datasets.jl`
- `read_dataset(f::JLDFile, dataspace, datatype_class, datatype_offset, layout, filters)`
- Dispatches based on layout class
- Chunked reading uses V1 B-tree lookup

## Test File Generation

### Tools Used
- **h5py** (Python): Create HDF5 files with specific indexing types
- **libver parameter**: Controls HDF5 format version
  - `'earliest'` → DataLayout v3 (V1 B-tree)
  - `'v110'` → DataLayout v4 (modern indexing)
  - `'latest'` → Latest HDF5 features

### Test Files Created

| File | Dataset | Indexing Type | Status |
|------|---------|---------------|--------|
| `test_v4_single_chunk.h5` | v4_single | Type 1 (Single) | Reads with data issues |
| `test_v4_fixed_array.h5` | v4_fixed | Type 3 (Fixed Array) | Unsupported error |
| `test_v4_extensible_array.h5` | v4_extensible | Type 4 (Ext Array) | Parser error |
| `test_v4_v2btree.h5` | v4_v2btree | Type 5 (V2 B-tree) | Parser error |
| `test_v4_filtered_single.h5` | v4_filtered | Type 1 (compressed) | Not tested yet |

### Verification Commands

```bash
# View storage layout
h5dump -p -H <filename>

# Debug B-tree structure
h5debug <filename> <offset> <ndims> <dim1> <dim2> ...

# Test with JLD2
julia --project test_v4_reading.jl
```

## Implementation Plan

### Phase 1: Fix DataLayout v4 Parsing (PRIORITY)

**Goal**: Read all DataLayout v4 fields correctly

**Tasks**:
1. Fix parser error for Extensible Array indexing info
   - Error: `MethodError: no method matching read(::JLD2.MmapIO, ::Symbol)`
   - Location: `src/datalayouts.jl`, DataLayout v4 parsing
   - Issue: Likely trying to read a symbol instead of bytes

2. Complete parsing for all indexing type information fields:
   - Single Chunk: filter info (if filtered)
   - Implicit: no extra fields
   - Fixed Array: `page_bits`
   - Extensible Array: `max_bits`, `index_elements`, `min_pointers`, `min_elements`, `page_bits`
   - V2 B-tree: `node_size`, `split_percent`, `merge_percent`

3. Create proper Julia types for each indexing info structure

**Testing**: Verify header messages print correctly for all test files

**Estimated effort**: 2-4 hours

### Phase 2: Implement Single Chunk Index Reading

**Goal**: Correctly read datasets using Single Chunk indexing

**Tasks**:
1. Fix data corruption issue in current implementation
2. Handle filtered single chunks (with compression)
3. Verify chunk address calculation
4. Add tests for various single-chunk datasets

**Testing**:
- Read `test_v4_single_chunk.h5` correctly
- Read `test_v4_filtered_single.h5` with decompression
- Compare data with h5py-read values

**Estimated effort**: 2-3 hours

### Phase 3: Implement Fixed Array Index Reading

**Goal**: Support most common v4 indexing type for fixed-size arrays

**Tasks**:
1. Parse Fixed Array header structure
   - Signature: "FHFA"
   - Client ID, header size, chunk size
   - Array parameters and statistics

2. Read chunk address array
   - Direct addresses for small arrays
   - Paged storage for large arrays

3. Implement chunk lookup function
   - Convert N-dimensional chunk index to array offset
   - Retrieve chunk address from array

4. Handle fill values for unallocated chunks

**New code**:
- `src/fixed_array.jl` (new file)
- Types: `FixedArrayHeader`, `FixedArrayIndex`
- Functions: `read_fixed_array_header()`, `lookup_chunk_fixed_array()`

**Testing**:
- Read `test_v4_fixed_array.h5` completely
- Verify all 50 chunks read correctly
- Test with various array sizes and chunk configurations

**Estimated effort**: 6-8 hours

### Phase 4: Implement Extensible Array Index Reading

**Goal**: Support time-series datasets with one unlimited dimension

**Tasks**:
1. Parse Extensible Array header structure
   - Signature: "EAHD"
   - Parameters controlling block structure
   - Index block pointer

2. Navigate index block
   - Contains pointers to data blocks and secondary blocks
   - Handle different element sizes

3. Read data blocks
   - Contain actual chunk addresses
   - May be paged for large blocks

4. Handle secondary blocks
   - Used for very large arrays
   - Contains pointers to data blocks

**New code**:
- `src/extensible_array.jl` (new file)
- Types: `ExtensibleArrayHeader`, `IndexBlock`, `DataBlock`, `SecondaryBlock`
- Functions: `read_extensible_array_header()`, `lookup_chunk_extensible()`

**Testing**:
- Read `test_v4_extensible_array.h5`
- Test with arrays extended to various sizes
- Verify performance with large unlimited dimensions

**Estimated effort**: 10-12 hours

### Phase 5: Implement V2 B-tree Index Reading

**Goal**: Support datasets with multiple unlimited dimensions

**Tasks**:
1. Parse V2 B-tree header
   - Generic B-tree structure (different from V1)
   - Node type, depth, split/merge parameters

2. Navigate V2 B-tree nodes
   - Internal nodes with record pointers
   - Leaf nodes with chunk records

3. Parse chunk record format
   - N-dimensional chunk address
   - Chunk file address
   - Filter mask (if filtered)

4. Implement chunk lookup
   - Binary search through B-tree
   - Compare N-dimensional keys

**New code**:
- `src/btree_v2.jl` (new file, separate from existing `btree.jl`)
- Types: `V2BTreeHeader`, `V2BTreeNode`, `ChunkRecord`
- Functions: `read_v2_btree_header()`, `lookup_chunk_v2btree()`

**Testing**:
- Read `test_v4_v2btree.h5`
- Test with datasets extended in multiple dimensions
- Compare performance with V1 B-tree

**Estimated effort**: 12-15 hours

### Phase 6: Implement Implicit Index Reading

**Goal**: Complete coverage of all indexing types

**Tasks**:
1. Detect implicit indexing conditions
2. Calculate chunk addresses from base address
3. Handle edge cases

**Estimated effort**: 2-3 hours

### Phase 7: Writing Support

**Goal**: Enable JLD2 to write DataLayout v4 files

**For each indexing type**:
1. Determine when to use (dataset properties analysis)
2. Implement structure creation
3. Implement chunk address storage
4. Add to dataset writing pipeline

**Decision logic for chunk indexing selection**:
```julia
function select_chunk_indexing_type(dims, maxdims, chunk_dims)
    num_chunks = prod(cld.(dims, chunk_dims))

    if num_chunks == 1
        return SingleChunkIndex
    elseif all(dims .== maxdims)  # No unlimited dimensions
        return FixedArrayIndex
    elseif count(maxdims .== -1) == 1  # One unlimited
        return ExtensibleArrayIndex
    else  # Multiple unlimited
        return V2BTreeIndex
    end
end
```

**Testing**:
- Write datasets using each indexing type
- Verify with h5debug and h5dump
- Round-trip test (write then read)
- Interoperability test (verify h5py can read)

**Estimated effort**: 15-20 hours

### Phase 8: Documentation and Tests

**Goal**: Production-ready implementation

**Tasks**:
1. Add comprehensive docstrings
2. Create unit tests for each indexing type
3. Add integration tests with real-world scenarios
4. Document chunk indexing selection logic
5. Add examples to JLD2 documentation
6. Performance benchmarking

**Testing categories**:
- Small datasets (single chunk)
- Medium datasets (fixed array)
- Large datasets (extensible/v2btree)
- Filtered chunks (compression)
- Edge cases (empty datasets, single-element chunks, etc.)

**Estimated effort**: 8-10 hours

## Implementation Order Rationale

1. **Parser fixes first** - Unblocks all subsequent work
2. **Read before write** - Easier to test against known-good files
3. **Simple to complex** - Single Chunk → Fixed Array → Extensible → V2 B-tree
4. **Common cases prioritized** - Fixed Array likely most common in practice

## Testing Strategy

### Phase-Based Testing

Each implementation phase includes:
- **Unit tests**: Individual function correctness
- **Integration tests**: Full dataset read/write
- **Format verification**: h5debug/h5dump validation
- **Interoperability**: h5py compatibility

### Test Data Generation

Use provided Python scripts:
- `create_chunk_index_test_files.py` - DataLayout v3 files
- `create_v4_layout_test_files.py` - DataLayout v4 files

### Validation Commands

```bash
# Inspect header messages
julia --project -e 'using JLD2; jldopen("file.h5") do f; JLD2.print_header_messages(f, "dataset"); end'

# Verify with HDF5 tools
h5dump -H file.h5
h5debug file.h5

# Compare with h5py
python3 -c "import h5py; f = h5py.File('file.h5'); print(f['dataset'][:])"
```

### Critical Validation Points

From `CLAUDE.md` testing guidelines:
1. **Always verify file format** with `print_header_messages()` before testing functionality
2. **Independent end-to-end tests** that exercise complete file format
3. **Verify storage path** - Confirm expected DataLayout version and indexing type used

## Technical Challenges and Solutions

### Challenge 1: DataLayout v4 Field Parsing

**Problem**: Variable-length indexing info based on type
**Solution**: Use tagged union or multiple read functions per type

### Challenge 2: Chunk Address Calculation

**Problem**: Different indexing types use different addressing schemes
**Solution**: Trait-based dispatch or type-specific lookup functions

### Challenge 3: Format Compliance

**Problem**: Subtle HDF5 specification details
**Solution**:
- Reference `DEVELOPMENT_INSIGHTS.md` debugging patterns
- Use h5debug and h5dump for validation
- Compare byte-level output with h5py-generated files

### Challenge 4: Performance

**Problem**: Some indexing types have overhead
**Solution**:
- Cache index structures
- Lazy loading of chunk addresses
- Benchmarking against V1 B-tree performance

## HDF5 Specification References

- **Section III.A.2**: Version 2 B-trees
- **Section IV.A.2.i**: Data Layout Message
- **Section VII**: Appendix C - Types of Indexes for Dataset Chunks
  - **VII.A**: Single Chunk Index
  - **VII.B**: Implicit Index
  - **VII.C**: Fixed Array Index
  - **VII.D**: Extensible Array Index
  - **VII.E**: Version 2 B-trees Index

## Success Criteria

### Reading Support
✅ All test files read without errors
✅ Data matches h5py-read values
✅ h5debug shows correct structure interpretation
✅ Performance comparable to V1 B-tree for similar datasets

### Writing Support
✅ Files validated by h5debug
✅ h5py can read JLD2-written files
✅ Correct indexing type selected automatically
✅ Round-trip testing passes

### Documentation
✅ All public functions documented
✅ Examples in docs
✅ Testing guide for chunk indexing

## Timeline Estimate

| Phase | Description | Effort | Dependencies |
|-------|-------------|--------|--------------|
| 1 | Parser fixes | 2-4 hrs | None |
| 2 | Single Chunk reading | 2-3 hrs | Phase 1 |
| 3 | Fixed Array reading | 6-8 hrs | Phase 1 |
| 4 | Extensible Array reading | 10-12 hrs | Phase 1 |
| 5 | V2 B-tree reading | 12-15 hrs | Phase 1 |
| 6 | Implicit index reading | 2-3 hrs | Phase 1 |
| 7 | Writing support | 15-20 hrs | Phases 2-6 |
| 8 | Documentation & tests | 8-10 hrs | All phases |

**Total estimated effort**: 57-75 hours

**Recommended approach**: Implement in phases with testing between each phase. Phase 1-3 provide significant value (40-50% of use cases) for 10-15 hours effort.

## References

- Test file generation: `create_chunk_index_test_files.py`, `create_v4_layout_test_files.py`
- Test file inspection: `inspect_chunk_index_files.jl`, `test_v4_reading.jl`
- Current implementation: `src/datalayouts.jl`, `src/datasets.jl`, `src/btree.jl`
- Debugging insights: `DEVELOPMENT_INSIGHTS.md`
- Format specification: `hdf5_format/04_data_objects.md`

## Next Steps

1. **Immediate**: Fix Phase 1 parser errors (blocks all other work)
2. **Short-term**: Implement Phases 2-3 (Single Chunk + Fixed Array) for immediate value
3. **Medium-term**: Phases 4-5 (Extensible Array + V2 B-tree) for complete read support
4. **Long-term**: Phases 6-8 (Implicit, writing, docs) for production completeness

---

**Investigation complete**: Test files created, current support assessed, implementation plan drafted. Ready to begin Phase 1 implementation.
