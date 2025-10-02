# Continuation Prompt: Chunked Array Writing - Phase 4 (Extensible Array)

## Current Status

**Location**: `/workspace/JLD2.jl` (branch: `version4_chunking`)

**Latest Completed**: Phase 3 (Implicit Index - Type 2) ✅ COMPLETE

**Reading Support**: ✅ COMPLETE for all 5 v4 chunk indexing types (Types 1-5)

**Writing Support**:
- ✅ **Phase 0 (API Design)** - Complete
- ✅ **Phase 1 (Single Chunk - Type 1)** - Complete
- ✅ **Phase 2 (Fixed Array - Type 3)** - Complete
- ✅ **Phase 3 (Implicit Index - Type 2)** - Complete
- ⏳ **Phase 4 (Extensible Array - Type 4)** - YOU ARE HERE

---

## Your Task: Implement Extensible Array Writing (Phase 4)

Implement writing support for **Extensible Array (Type 4)** chunk indexing - used for chunked datasets with unlimited dimensions where the dataset can grow over time.

### What is Extensible Array?

**When Used**: Chunked datasets with at least one unlimited dimension (e.g., `maxshape=(nothing, 100)` means first dimension can grow indefinitely).

**Structure**: Multi-level index structure supporting dynamic expansion:
- **Header**: Metadata about the array (version, client_id, element_size, max_nelmts, etc.)
- **Index Blocks**: Hold chunk addresses and can expand via secondary blocks
- **Secondary Blocks**: Additional storage when index blocks fill up
- **Data Blocks**: Store actual chunk data

**Key Characteristics**:
- Supports unlimited dimensions (extensibility)
- More complex than Fixed Array (multi-level structure)
- Efficient for append-only datasets
- Index can grow dynamically via secondary blocks

---

## Essential Context Files

### Read These FIRST:

1. **`PHASE3_IMPLICIT_INDEX_COMPLETE.md`** - Just completed, recent patterns
2. **`PHASE2_FIXED_ARRAY_COMPLETE.md`** - Fixed Array implementation patterns
3. **`CHUNKED_WRITING_PLAN.md`** - Overall roadmap (see Phase 4 section)
4. **`DEVELOPMENT_INSIGHTS.md`** - Critical development best practices
5. **`CHUNKED_API_DESIGN.md`** - API design and selection logic

### Implementation References:

6. **`src/chunked_writing_api.jl`** - Where to implement `_write_extensible_array()` (stub exists)
7. **`src/extensible_array.jl`** - READING implementation (your primary template)
8. **`src/headermessages.jl`** - HmDataLayout structure (lines 122-148)
9. **`src/fixed_array.jl`** - Reference for chunk iteration patterns
10. **`src/implicit_index.jl`** - Reference for simple contiguous storage

### Test References:

11. **`test/implicit_index_writing_test.jl`** - Phase 3 test pattern (36 tests)
12. **`test/fixed_array_writing_test.jl`** - Phase 2 test pattern (30 tests)

---

## Key Differences from Previous Phases

| Aspect | Fixed Array (Type 3) | Implicit (Type 2) | Extensible Array (Type 4) |
|--------|---------------------|-------------------|--------------------------|
| Index Structure | Fixed Array header + data block | None | Multi-level (header + index + secondary) |
| Chunk Storage | Random access via index | Sequential/contiguous | Random access via expandable index |
| Unlimited Dims | No | No | **Yes** |
| Complexity | Medium | Low | **High** |
| Disk Overhead | ~850 bytes + 8×N_chunks | ~20 bytes | Variable, grows with data |
| Use Case | Fixed-size chunked | Sparse with fill value | **Extensible datasets** |

---

## Implementation Approach

### Step 1: Understand Extensible Array Structure (2-3 hours)

Study `src/extensible_array.jl` thoroughly:

**Key Components**:
- **Header**: Contains metadata and address of index block
- **Index Block**: Holds chunk addresses (or addresses to secondary blocks)
- **Secondary Blocks**: Used when index blocks need more capacity
- **Data Blocks**: Contain actual chunk data

**HDF5 Spec Reference**: Section VII.C - Extensible Array

**Critical Concepts**:
- `max_nelmts_bits`: Determines index block size
- `nelmts`: Current number of elements
- `max_nelmts`: Maximum number of elements (based on unlimited dims)
- Secondary blocks activated when index exceeds initial capacity

### Step 2: Implement `_write_extensible_array()` (5-7 hours)

**Location**: `src/chunked_writing_api.jl` (replace stub)

**Required Signature**:
```julia
function _write_extensible_array(f, name, data, chunks, maxshape, filters)
```

**Implementation Steps**:

1. **Validate inputs**:
   - At least one dimension in `maxshape` must be `nothing` (unlimited)
   - `chunks` must be specified
   - Current `size(data)` must fit within non-unlimited dimensions

2. **Calculate index parameters**:
   ```julia
   # Determine which dimensions are unlimited
   unlimited_dims = [i for i in 1:N if isnothing(maxshape[i])]

   # Calculate max_nelmts (maximum number of chunks)
   # For unlimited dims, use a reasonable upper bound or HDF5 default
   max_nelmts = calculate_max_chunks(chunks, maxshape)

   # Calculate index block size
   max_nelmts_bits = calculate_index_bits(max_nelmts)
   ```

3. **Write chunks and collect addresses**:
   - Similar to Fixed Array, iterate through chunks
   - Write chunk data and collect RelOffsets
   - Pad partial chunks as needed

4. **Create index block**:
   ```julia
   # Index block contains chunk addresses
   # Structure depends on max_nelmts_bits
   index_block_size = calculate_index_block_size(max_nelmts_bits)

   # Write index block with chunk addresses
   # If more chunks than index block capacity, use secondary blocks
   ```

5. **Create header**:
   ```julia
   # Extensible Array Header
   # Signature: EXTENSIBLE_ARRAY_HEADER_SIGNATURE
   # Version, client_id, element_size, max_nelmts_bits, etc.
   # Index block address
   ```

6. **Create DataLayout message (version 4, type 4)**:
   ```julia
   write_header_message(cio, Val(HmDataLayout);
       version = 4,
       layout_class = LcChunked,
       flags = layout_flags,
       dimensionality = N + 1,
       dimensions = UInt64.((reverse(chunks)..., odr_sizeof(odr))),
       chunk_indexing_type = 4,
       data_address = h5offset(f, header_offset)
   )
   ```

### Step 3: Handle Unlimited Dimensions (3-4 hours)

**Challenge**: How to represent "unlimited" in max_nelmts calculation?

**Options**:
1. Use HDF5 default (2^64 - 1 for unlimited)
2. Use a reasonable upper bound (e.g., 2^32)
3. Calculate based on available space

**Recommendation**: Study `extensible_array.jl` reading code to see how it handles this.

### Step 4: Testing (4-5 hours)

Create `test/extensible_array_writing_test.jl`:

**Test Cases**:
1. Basic 2D with first dimension unlimited
2. Basic 2D with second dimension unlimited
3. Multiple unlimited dimensions
4. 1D, 2D, 3D arrays with various unlimited patterns
5. Write, then append more data (if API supports)
6. Edge cases: single unlimited chunk, all unlimited

**Pattern from Phase 3**:
```julia
@testset "Extensible Array Writing (Type 4)" begin
    @testset "First dimension unlimited" begin
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(3, 3),
                                       maxshape=(nothing, 10))
                write_chunked(f, "extensible", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["extensible"]
                @test data_read == data
            end
        finally
            rm(filename, force=true)
        end
    end
end
```

### Step 5: Validation (2-3 hours)

Create `validate_phase4.jl` for external validation.

**Key Validations**:
1. JLD2-to-JLD2 roundtrip
2. Various unlimited dimension patterns
3. Check with h5ls if available
4. Verify header structure with JLD2.print_header_messages

---

## Critical Insights from Previous Phases

**From DEVELOPMENT_INSIGHTS.md and Phase 2-3 experience**:

1. **Chunk Padding**: All chunks MUST be padded to full `chunks` size, even edge chunks.

2. **HDF5 Dimension Ordering**:
   - Julia `(N, M)` → HDF5 `{M, N}` (reversed)
   - Chunk dimensions in DataLayout: reversed from Julia order
   - Linear index calculation must match reading code

3. **DataLayout Dimensions Field**:
   - Contains **chunk dimensions**, NOT array dimensions
   - Array dimensions go in HmDataspace message

4. **RelOffset Conversion**:
   - Must use `h5offset(f, offset)` to convert Int64 to RelOffset
   - HardLink requires RelOffset, not Int64

5. **Testing Strategy**:
   - Start with simple cases (one unlimited dim)
   - Test multiple unlimited dims separately
   - Validate roundtrip before external tools

6. **Index Structure Validation**:
   - Use `JLD2.print_header_messages(f, "dataset")` to verify structure
   - Compare with reading code expectations
   - Validate signatures and checksums

---

## Estimated Timeline

- **Day 1-2**: Understand Extensible Array structure (3h), start implementation (5h)
- **Day 3**: Complete implementation (4h), handle unlimited dims (3h)
- **Day 4**: Create tests (4h), debug issues (3h)
- **Day 5**: Validation (3h), documentation (2h)
- **Total**: 4-5 days (most complex phase yet)

---

## Known Challenges

### 1. Index Block Size Calculation

**Issue**: Determining correct `max_nelmts_bits` for given dataset.

**Solution**: Study reading code in `extensible_array.jl` for size calculation logic. Look for `calculate_index_bits` or similar functions.

### 2. Secondary Blocks

**Question**: When are secondary blocks needed?

**Answer**: When number of chunks exceeds index block capacity. May be able to simplify by always allocating sufficient index block initially.

### 3. Multiple Unlimited Dimensions

**Challenge**: How to calculate max_nelmts when multiple dimensions are unlimited?

**Solution**: Use product of maximum chunk counts for non-unlimited dims, and upper bound for unlimited dims.

### 4. Append Support (Optional)

**Future Enhancement**: Supporting appending data to existing dataset.

**For Phase 4**: Focus on initial write only. Appending can be future work.

---

## Success Criteria

Phase 4 is complete when:

✅ **Implementation**
- `_write_extensible_array()` fully functional
- Handles unlimited dimensions correctly
- Supports 1D, 2D, 3D+ arrays
- Index structure properly created

✅ **Testing**
- All tests pass (aim for 25+ tests)
- Various unlimited dimension patterns tested
- Edge cases covered
- Roundtrip verified

✅ **Validation**
- JLD2 roundtrip works ✅
- Data integrity verified ✅
- Unlimited dimension handling correct ✅

✅ **Documentation**
- Create `PHASE4_EXTENSIBLE_ARRAY_COMPLETE.md`
- Document implementation details
- Include validation results
- Update continuation prompt for Phase 5

---

## Quick Start Commands

```bash
cd /workspace/JLD2.jl

# Study extensible array reading code
cat src/extensible_array.jl | less

# Search for key structures
grep -r "EXTENSIBLE_ARRAY" src/

# Study chunk indexing selection
grep -A30 "function select_chunk_index_type" src/chunked_writing_api.jl

# Edit implementation
# Add _write_extensible_array() to src/chunked_writing_api.jl

# Test as you go
julia -O1 --project -e 'using JLD2; include("test/extensible_array_writing_test.jl")'

# Validate
julia -O1 --project validate_phase4.jl
```

---

## Important Development Practices

**From DEVELOPMENT_INSIGHTS.md**:

1. **Use `-O1` flag**: Faster compilation during development
2. **Test incrementally**: Don't wait to test everything at once
3. **Avoid try-catch**: Return well-defined values instead
4. **Use specific type signatures**: Don't over-constrain function parameters
5. **Validate file format**: Use `JLD2.print_header_messages(f, dataset_name)` to verify
6. **Julia conventions**:
   - `isnothing(x)` not `x === nothing`
   - `!isnothing(x)` not `x !== nothing`
   - Concise keyword args: `fun(; variable)` when var name matches parameter

---

## Extensible Array Specifics from HDF5 Spec

**Key Constants** (check src/extensible_array.jl):
- `EXTENSIBLE_ARRAY_HEADER_SIGNATURE`
- `EXTENSIBLE_ARRAY_INDEX_SIGNATURE`
- `EXTENSIBLE_ARRAY_SECONDARY_SIGNATURE`

**Header Fields**:
- `version`: Usually 0
- `client_id`: 0 for non-filtered chunks
- `element_size`: Size of each index entry (jlsizeof(RelOffset))
- `max_nelmts_bits`: Bits needed to represent max_nelmts
- `index_block_elements`: Number of elements in index block
- `data_block_elements`: (if applicable)
- `max_nelmts`: Maximum number of elements

**Index Block**:
- Signature
- Version, client_id
- Header address (back-reference)
- Chunk addresses array
- Checksum

---

## After Phase 4

**Phase 5**: V2 B-tree (Type 5) - Most complex, supports >64K chunks and all unlimited patterns

---

## Files to Reference

**Must Read**:
- `DEVELOPMENT_INSIGHTS.md` - Critical best practices
- `PHASE3_IMPLICIT_INDEX_COMPLETE.md` - Just completed
- `CHUNKED_WRITING_PLAN.md` - Overall multi-phase plan

**Implementation**:
- `src/chunked_writing_api.jl` - Where to add code
- `src/extensible_array.jl` - **PRIMARY REFERENCE** for structure
- `src/headermessages.jl` - Message structures

**Testing**:
- `test/implicit_index_writing_test.jl` - Latest test pattern (36 tests)
- `test/fixed_array_writing_test.jl` - Earlier pattern (30 tests)

---

## Notes from Phase 3

- Phase 3 achieved 36/36 passing tests for JLD2 roundtrip ✅
- h5ls compatibility works for Implicit Index (unlike Fixed Array) ✅
- Fill value handling uses version 3 format with flags 0x20 ✅
- RelOffset conversion via `h5offset()` is critical for HardLink ✅
- Extensible Array will be more complex but builds on these patterns

---

**Branch**: `version4_chunking`
**Current Commit**: Latest from Phase 3 completion
**Status**: Phase 3 ✅ Complete, Phase 4 ⏳ Ready to start

Good luck! This is the most complex phase yet, but you have solid foundations from Phases 1-3. 🚀

## Key Resources

**HDF5 Format Spec**: https://docs.hdfgroup.org/hdf5/develop/_f_m_t3.html (Section VII.C)

**Reading Code**: `src/extensible_array.jl` is your primary reference - understand it thoroughly before writing.

**Debug Tools**:
- `JLD2.print_header_messages(f, "dataset")` - View dataset structure
- `h5debug file.jld2 <offset>` - Low-level debugging
- `h5ls -rv file.jld2` - View file hierarchy

**Remember**: When in doubt, follow the patterns from reading code exactly. The reading implementation has been tested and validated extensively.
