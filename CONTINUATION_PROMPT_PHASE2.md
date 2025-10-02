# Continuation Prompt: Chunked Array Writing - Phase 2 (Fixed Array)

## Current Status

**Location**: `/workspace/JLD2.jl` (branch: `version4_chunking`)

**Latest Commit**: `c21199f` - "Add chunked array writing support - Phase 1 (Single Chunk) complete"

**Reading Support**: ✅ COMPLETE for all 5 v4 chunk indexing types (Types 1-5)

**Writing Support**:
- ✅ **Phase 0 (API Design)** - Complete
- ✅ **Phase 1 (Single Chunk - Type 1)** - Complete
- ⏳ **Phase 2 (Fixed Array - Type 3)** - YOU ARE HERE

---

## Your Task: Implement Fixed Array Writing (Phase 2)

Implement writing support for **Fixed Array (Type 3)** chunk indexing - the most common chunking type for fixed-size datasets with multiple chunks.

### What is Fixed Array?

**When Used**: Default for fixed-size chunked arrays where `chunks != size(data)` and no unlimited dimensions.

**Structure**: Index array storing chunk addresses in a fixed grid:
```
Grid: [chunks_in_dim_N, ..., chunks_in_dim_1] (reversed HDF5 order)
Each element: chunk address (RelOffset) or UNDEFINED_ADDRESS if absent
```

**Key Characteristics**:
- Pre-allocated index array at known size
- Direct chunk address lookup via calculated indices
- Most efficient for regular chunked access patterns
- No tree structure needed (unlike V1 B-tree)

---

## Essential Context Files

### Read These FIRST:

1. **`PHASE1_SINGLE_CHUNK_COMPLETE.md`** - Phase 1 summary, patterns to follow
2. **`CHUNKED_WRITING_PLAN.md`** - Overall multi-phase roadmap (see Phase 2 section)
3. **`H5PY_API_ANALYSIS.md`** - h5py's Fixed Array behavior and test files
4. **`CHUNKED_API_DESIGN.md`** - API design rationale and selection logic

### Implementation References:

5. **`src/fixed_array.jl`** - READING implementation (your template!)
6. **`src/chunked_writing_api.jl`** - Where to add `_write_fixed_array()` (line ~461)
7. **`src/headermessages.jl`** - HmDataLayout structure (lines 122-148)
8. **`src/datasets.jl`** - Dataset writing patterns (lines 257-347)

### Test References:

9. **`test/single_chunk_writing_test.jl`** - Test pattern to follow
10. **`create_v4_layout_test_files.py`** - h5py test file generator
11. **Python h5py files**: `h5py_fixed_array*.h5` (from Phase 0)

---

## Implementation Steps

### Step 1: Understand Fixed Array Reading (1 hour)

Study `src/fixed_array.jl` to understand the structure:

```julia
# Fixed Array structure (from reading code)
struct FixedArrayIndex
    index_offset::RelOffset        # Where index array is stored
    chunk_addresses::Array{RelOffset, N}  # Pre-allocated grid
end
```

**Key insight**: Index array dimensions are `cld.(data_size, chunk_size)` (ceiling division for grid dimensions).

### Step 2: Implement `_write_fixed_array()` (4-6 hours)

**Location**: `src/chunked_writing_api.jl` (replace stub at line ~461)

**Required steps**:

```julia
function _write_fixed_array(f::JLDFile, name::String, data::AbstractArray{T,N},
                            chunks, filters) where {T,N}
    # 1. Calculate chunk grid dimensions
    grid_dims = cld.(size(data), chunks)
    n_chunks = prod(grid_dims)

    # 2. Write all chunks and collect addresses
    chunk_addresses = Array{RelOffset,N}(undef, grid_dims...)
    for chunk_idx in CartesianIndices(grid_dims)
        # Calculate data range for this chunk
        # Write chunk data (with filters if specified)
        # Store chunk address in array
        chunk_addresses[chunk_idx] = ...
    end

    # 3. Allocate and write index array
    index_offset = f.end_of_data
    # Write chunk addresses in HDF5 order (reversed)
    # Each address is 8 bytes (jlsizeof(RelOffset))

    # 4. Create DataLayout message (version 4, type 3)
    layout_flags = isnothing(filters) ? 0x00 : 0x02
    psz += jlsizeof(Val(HmDataLayout);
        version = 4,
        layout_class = LcChunked,
        flags = layout_flags,
        dimensionality = N + 1,
        dimensions = UInt64.((reverse(size(data))..., odr_sizeof(odr))),
        chunk_indexing_type = 3,
        page_bits = UInt8(0),  # Fixed Array parameter
        data_address = h5offset(f, index_offset)
    )

    # 5. Create object header with messages
    # 6. Link to file hierarchy
end
```

**Critical details**:
- **Chunk iteration**: Use `CartesianIndices(grid_dims)` for N-dimensional iteration
- **Index array ordering**: Must reverse dimensions for HDF5 (use `reverse(collect(...))`)
- **Partial chunks**: Last chunks may be smaller than `chunks` size
- **Filter application**: Same pattern as Phase 1 (normalize, apply per chunk)

### Step 3: Handle Chunk Data Writing (2-3 hours)

**Helper function**:
```julia
function write_chunk_at_index(f, data, chunk_idx, chunk_dims, odr, filters, wsession)
    # Calculate data range
    start_idx = Tuple((chunk_idx - oneunit(chunk_idx)) .* chunk_dims .+ 1)
    end_idx = min.(start_idx .+ chunk_dims .- 1, size(data))

    # Extract chunk data
    chunk_data = data[map(:, start_idx, end_idx)...]

    # Apply filters if present
    if !isnothing(filters)
        compressed, _ = Filters.compress(filters, chunk_data, odr, f, wsession)
        chunk_bytes = compressed
    else
        io_buf = IOBuffer()
        write_data(io_buf, f, chunk_data, odr, datamode(odr), wsession)
        chunk_bytes = take!(io_buf)
    end

    # Write to file
    chunk_offset = f.end_of_data
    seek(f.io, chunk_offset)
    write(f.io, chunk_bytes)
    f.end_of_data += sizeof(chunk_bytes)

    return h5offset(f, chunk_offset)
end
```

### Step 4: Testing (3-4 hours)

Create `test/fixed_array_writing_test.jl`:

```julia
@testset "Fixed Array Writing (Type 3)" begin
    @testset "Basic 2D chunking" begin
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        jldopen(filename, "w") do f
            wca = WriteChunkedArray(data, chunks=(3, 3))
            write_chunked(f, "chunked", wca)
        end

        # Verify readback
        jldopen(filename, "r") do f
            @test f["chunked"] == data
        end

        # Validate chunk count
        # Grid should be 4×4 chunks (ceiling(10/3) = 4)

        rm(filename, force=true)
    end

    @testset "1D chunking" begin
        # Test 1D array with chunks
    end

    @testset "3D chunking" begin
        # Test 3D array
    end

    @testset "With compression" begin
        # Test chunked + compressed
    end

    @testset "Partial chunks" begin
        # Test edge chunks that are smaller
    end
end
```

**Run tests**:
```bash
julia -O1 --project -e 'include("test/fixed_array_writing_test.jl")'
```

### Step 5: Validation (2-3 hours)

**Create validation script** `test_phase2_fixed_array.jl`:

```julia
using JLD2

data_2d = reshape(Float32.(1:100), 10, 10)

jldopen("phase2_fixed_array_test.jld2", "w") do f
    wca = WriteChunkedArray(data_2d, chunks=(3, 3))
    write_chunked(f, "chunked_2d", wca)
end

# Validate with h5py
println("Validate with:")
println("  python3 -c \"import h5py; f=h5py.File('phase2_fixed_array_test.jld2'); print(f['chunked_2d'].chunks)\"")
println("  h5ls -rv phase2_fixed_array_test.jld2")
```

**h5py validation**:
```bash
python3 -c "import h5py; f = h5py.File('phase2_fixed_array_test.jld2'); \
  print('Chunks:', f['chunked_2d'].chunks); \
  print('Data matches:', (f['chunked_2d'][:] == expected).all())"
```

**h5debug validation**:
```bash
# Get dataset offset
h5ls -rv --address phase2_fixed_array_test.jld2 | grep chunked_2d

# Check structure
h5debug phase2_fixed_array_test.jld2 <offset> | grep -A10 "Index Type"
# Should show: "Index Type: Fixed Array"
```

---

## Key Implementation Patterns from Phase 1

### 1. Filter Handling
```julia
# Normalize filters
normalized_filters = isnothing(filters) ? nothing : Filters.normalize_filters(filters)

# Apply to chunk
if !isnothing(normalized_filters)
    local_filters = FilterPipeline(map(normalized_filters) do filter
        Filters.set_local(filter, odr, dataspace, ())
    end)
    compressed, _ = Filters.compress(local_filters, chunk_data, odr, f, wsession)
end
```

### 2. Object Header Creation
```julia
# Calculate payload size
psz = jlsizeof(Val(HmDataspace); dataspace.dataspace_type, dimensions=dataspace.size)
psz += jlsizeof(Val(HmDatatype), 1; dt=datatype)
psz += jlsizeof(Val(HmDataLayout); version=4, layout_class=LcChunked, ...)
psz += CONTINUATION_MSG_SIZE

# Write header
fullsz = jlsizeof(ObjectStart) + size_size(psz) + psz + 4
header_offset = f.end_of_data
f.end_of_data = header_offset + fullsz

cio = begin_checksum_write(f.io, fullsz - 4)
jlwrite(cio, ObjectStart(size_flag(psz)))
write_size(cio, psz)

# Write messages...
write_continuation_placeholder(cio)
jlwrite(f.io, end_checksum(cio))
```

### 3. Linking to Hierarchy
```julia
parent_group = f.root_group
dataset_offset = h5offset(f, header_offset)
parent_group.unwritten_links[name] = HardLink(dataset_offset)
```

---

## Common Pitfalls to Avoid

1. **Index Array Ordering**: Must write in HDF5 order (reversed dimensions)
   ```julia
   # WRONG: write(f.io, chunk_addresses)
   # RIGHT: write(f.io, reverse(chunk_addresses, dims=ndims(chunk_addresses)))
   ```

2. **Partial Chunks**: Last chunks along each dimension may be smaller
   ```julia
   # Must calculate actual chunk size, not assume `chunks`
   actual_size = min.(start_idx .+ chunk_dims .- 1, size(data)) .- start_idx .+ 1
   ```

3. **Page Bits Parameter**: Fixed Array requires `page_bits` in DataLayout
   ```julia
   # Set to 0 for simplicity (no paging)
   page_bits = UInt8(0)
   ```

4. **Compression Per Chunk**: Each chunk compressed independently
   ```julia
   # Apply filters to EACH chunk, not entire dataset
   for chunk_idx in CartesianIndices(grid_dims)
       chunk_data = data[...]
       compressed_chunk = Filters.compress(filters, chunk_data, ...)
   end
   ```

---

## Success Criteria

Phase 2 is complete when:

✅ **Implementation**
- `_write_fixed_array()` fully functional
- Handles 1D, 2D, 3D+ arrays correctly
- Compression works per-chunk
- Partial chunks handled properly

✅ **Testing**
- All tests pass (aim for 20+ tests)
- Various chunk sizes validated
- Compression confirmed working
- Edge cases covered

✅ **Validation**
- h5py reads files correctly ✅
- Data integrity verified ✅
- h5debug shows "Index Type: Fixed Array" ✅
- Chunk addresses in index array are valid ✅

✅ **Documentation**
- Create `PHASE2_FIXED_ARRAY_COMPLETE.md`
- Document implementation details
- Include validation results
- Update continuation prompt for Phase 3

---

## Estimated Timeline

- **Day 1**: Study reading code (1h), implement core function (4h), initial tests (2h)
- **Day 2**: Complete tests (2h), validation (2h), fix issues (3h)
- **Day 3**: Documentation (2h), final validation (1h), prepare for Phase 3 (1h)

**Total**: 2-3 days

---

## Next After Phase 2

**Phase 3**: Implicit Index Writing (Type 2)
- Simpler than Fixed Array (no index structure)
- Chunks stored contiguously with fill value
- Estimated: 1-2 days

---

## Quick Start Commands

```bash
cd /workspace/JLD2.jl

# Study reading implementation
cat src/fixed_array.jl

# Edit implementation
# Add _write_fixed_array() to src/chunked_writing_api.jl around line 461

# Test as you go
julia -O1 --project -e 'using JLD2; include("test/fixed_array_writing_test.jl")'

# Validate
julia -O1 --project test_phase2_fixed_array.jl
python3 -c "import h5py; ..."
h5debug phase2_fixed_array_test.jld2 <offset>
```

---

## Important Notes

- **Use `-O1` flag**: Faster compilation during development
- **Test incrementally**: Don't wait to test everything at once
- **Validate early**: Check h5debug output as soon as you have a file
- **Follow Phase 1 patterns**: Object header structure is identical
- **Check CLAUDE.md**: Project-specific conventions and best practices

---

**Branch**: `version4_chunking`
**Current Commit**: `c21199f`
**Status**: Phase 1 ✅ Complete, Phase 2 ⏳ Ready to start

Good luck! 🚀
