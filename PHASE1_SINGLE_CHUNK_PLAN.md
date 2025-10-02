# Phase 1: Single Chunk Writing Implementation

## Overview

**Goal**: Implement writing support for Single Chunk (Type 1) indexing - the simplest chunk index type where the entire dataset is stored as a single chunk.

**Status**: Starting Phase 1 (2025-10-01)

**Estimated Duration**: 1-2 days

---

## What is Single Chunk Indexing?

Single Chunk (Type 1) is used when the chunk dimensions exactly match the dataset dimensions. Instead of dividing the data into multiple chunks, the entire dataset is stored as one contiguous block.

### When It's Used

Automatically selected when:
- `chunks == size(data)`
- Dataset fits entirely in one chunk

### HDF5 Structure

**Data Layout Message (Version 4)**:
```
Layout Class: CHUNKED (2)
Version: 4
Layout Flags: Single Chunk (0x01)
Dimensionality: N+1 (includes element size)
Dimension Sizes: [size_1, size_2, ..., size_N, element_size]
Chunk Index Type: 1 (Single Chunk)
Chunk Address: offset to chunk data
Chunk Size: total bytes (product of dimensions)
```

**No Additional Index Structure**: Unlike other types, single chunk doesn't need a separate index - the chunk address is directly in the layout message.

---

## Implementation Strategy

### Step 1: Understand Existing Infrastructure

Files to study:
- `src/data_layouts.jl` - DataLayout message structure
- `src/headermessages.jl` - Header message writing
- `src/v1btree.jl` - Example of chunk writing (V1 B-tree)
- `src/datasets.jl` - Dataset creation infrastructure

Key functions:
- `write_dataset()` - High-level dataset writing
- `write_header_message()` - Write HDF5 header messages
- `jlsizeof()` - Calculate message sizes

### Step 2: Implement write_single_chunk

**Location**: `src/chunked_writing_api.jl` (replace stub)

**Required Steps**:
1. Allocate space for chunk data
2. Write chunk data (with optional compression)
3. Create DataLayout message (version 4, type 1)
4. Create object header with messages
5. Link object to file hierarchy

**Function Signature**:
```julia
function write_single_chunk(f::JLDFile, name::String, data::AbstractArray{T,N},
                           chunks::NTuple{N,Int}, filters) where {T,N}
```

### Step 3: Handle Compression

If filters are specified:
1. Apply filter pipeline to chunk data
2. Update chunk_size to compressed size
3. Include Filter Pipeline message in object header

### Step 4: Create Object Header

Messages to include:
1. **Datatype Message** - Describes element type
2. **Dataspace Message** - Describes dimensions
3. **Data Layout Message** - Version 4, Type 1, with chunk address
4. **Filter Pipeline Message** (if filters specified)
5. **Link Info Message** (if needed for compatibility)

### Step 5: Testing Strategy

**Test Cases**:
1. Basic single chunk (no compression)
2. With compression (gzip, zstd)
3. Different element types (Float32, Int64, etc.)
4. Different dimensions (1D, 2D, 3D)
5. Edge cases (very small, very large)

**Validation**:
1. Read back with JLD2 - verify data matches
2. Read with h5py - verify interoperability
3. Check structure with h5debug
4. Verify with h5dump

---

## Implementation Checklist

### Core Implementation
- [ ] Study existing chunk writing code (v1btree.jl)
- [ ] Study DataLayout message structure
- [ ] Implement write_single_chunk function
- [ ] Handle compression/filters
- [ ] Create proper object header
- [ ] Link to file hierarchy

### Testing
- [ ] Test basic single chunk writing
- [ ] Test with compression
- [ ] Test different data types
- [ ] Test different dimensions
- [ ] Validate with h5py read
- [ ] Validate with h5debug/h5dump
- [ ] Edge case testing

### Documentation
- [ ] Add docstrings to implementation
- [ ] Update chunked_writing.md with implementation notes
- [ ] Create PHASE1_COMPLETE.md summary
- [ ] Add implementation notes for next phases

---

## Technical Considerations

### Memory Layout

Julia arrays are column-major, HDF5 expects row-major for dimensions:
- Julia: `data[i,j,k]` where `i` is fastest-changing
- HDF5: Dimensions reported as `(k_max, j_max, i_max, element_size)`
- Chunk data stored as-is (memory layout identical)

### Offset Calculations

```julia
# Allocate space for chunk
chunk_offset = f.end_of_data
f.end_of_data += chunk_size

# Convert to RelOffset for HDF5
rel_offset = RelOffset(chunk_offset - f.base_address)
```

### Filter Pipeline

If compression specified:
```julia
# Apply filters
compressed_data = apply_filters(data, filters)
chunk_size = sizeof(compressed_data)

# Create filter pipeline message
filter_msg = create_filter_pipeline_message(filters)
```

### DataLayout Message (Version 4, Type 1)

```julia
layout = DataLayout(
    layout_class = LC_CHUNKED,
    version = 4,
    flags = 0x01,  # Single chunk flag
    dimensionality = N + 1,  # +1 for element size
    dimension_sizes = [reverse(size(data))..., sizeof(T)],
    index_type = 1,  # Single chunk
    chunk_address = rel_offset,
    chunk_size = chunk_size
)
```

---

## Success Criteria

Phase 1 is complete when:

✅ **Implementation**
- write_single_chunk function fully implemented
- Handles all data types correctly
- Compression works properly
- Creates valid HDF5 structure

✅ **Testing**
- All tests pass
- Data reads back correctly with JLD2
- Files readable with h5py
- Structure validates with h5debug

✅ **Validation**
- h5py can read files without errors
- Data integrity verified (checksums match)
- Compression ratios reasonable
- Performance acceptable

✅ **Documentation**
- Implementation documented
- Test cases documented
- Known limitations documented
- PHASE1_COMPLETE.md created

---

## Code Structure

### File Organization

```julia
# src/chunked_writing_api.jl

# Replace stub with full implementation
function write_single_chunk(f::JLDFile, name::String, data::AbstractArray{T,N},
                           chunks::NTuple{N,Int}, filters) where {T,N}
    # 1. Validate inputs
    @assert chunks == size(data) "Single chunk requires chunks == size(data)"

    # 2. Prepare data (apply filters if needed)
    chunk_data, chunk_size = prepare_chunk_data(data, filters)

    # 3. Allocate space and write chunk
    chunk_offset = allocate_and_write_chunk(f, chunk_data)

    # 4. Create messages
    messages = create_single_chunk_messages(f, data, chunk_offset, chunk_size, filters)

    # 5. Create object header
    header_offset = write_object_header(f, messages)

    # 6. Link to hierarchy
    link_dataset(f, name, header_offset)
end

# Helper functions
function prepare_chunk_data(data, filters)
    if isnothing(filters)
        return data, sizeof(data)
    else
        compressed = apply_filters(data, filters)
        return compressed, sizeof(compressed)
    end
end

function allocate_and_write_chunk(f::JLDFile, chunk_data)
    chunk_offset = f.end_of_data
    # Write chunk data
    seek(f.io, chunk_offset)
    write(f.io, chunk_data)
    f.end_of_data += sizeof(chunk_data)
    return chunk_offset
end

function create_single_chunk_messages(f, data, chunk_offset, chunk_size, filters)
    messages = HeaderMessage[]

    # Datatype message
    push!(messages, create_datatype_message(eltype(data)))

    # Dataspace message
    push!(messages, create_dataspace_message(size(data)))

    # DataLayout message (version 4, type 1)
    push!(messages, create_single_chunk_layout_message(data, chunk_offset, chunk_size))

    # Filter pipeline (if filters specified)
    if !isnothing(filters)
        push!(messages, create_filter_pipeline_message(filters))
    end

    return messages
end
```

---

## Reference Files

**Study these for implementation**:
- `src/data_layouts.jl` - Lines with DataLayout definitions
- `src/v1btree.jl` - Chunk writing example
- `src/headermessages.jl` - Message writing
- `src/datasets.jl` - Dataset creation

**Test against**:
- `test/chunked_writing_api_test.jl` - Extend with Phase 1 tests
- h5py reference files from Phase 0

---

## Next Steps After Phase 1

Once Phase 1 is complete:
- **Phase 2**: Fixed Array (Type 3) - Most common case
- **Phase 3**: Implicit Index (Type 2) - With fill values
- **Phase 4**: Extensible Array (Type 4) - One unlimited dimension
- **Phase 5**: V2 B-tree (Type 5) - Multiple unlimited dimensions

Single Chunk establishes the pattern that other phases will follow.

---

**Current Status**: Ready to begin implementation
**Branch**: `version4_chunking`
**Starting Date**: 2025-10-01
