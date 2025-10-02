# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

JLD2 is a Julia package for saving and loading data in the Julia Data format. It provides HDF5-compatible serialization with high performance and complex nested structure support.

## Development Commands

### Testing

- `julia -e 'using Pkg; Pkg.test("JLD2.jl")'` - Run all tests
- `julia --project=test -e 'include("test/specific_test.jl")'` - Run individual test files

The test suite includes multiple test modules that must be developed and installed:

- JLD2Bzip2 and JLD2Lz4 filter packages (automatically handled in runtests.jl)
- Uses TestItemRunner for package-level tests
- Includes Aqua.jl quality assurance tests

### Documentation

- `julia --project=docs docs/make.jl` - Build documentation locally
- Documentation uses Documenter.jl and deploys to GitHub Pages

### Package Management

- `julia --project -e 'using Pkg; Pkg.instantiate()'` - Install dependencies
- `julia --project -e 'using Pkg; Pkg.precompile()'` - Precompile package

## Architecture Overview

### Core File Structure

- `src/JLD2.jl` - Main module file with core types (JLDFile, Group) and file operations
- `src/types.jl` - Core type definitions and data structures
- `src/datasets.jl` - Dataset handling and serialization
- `src/groups.jl` - Group (folder-like) functionality
- `src/loadsave.jl` - High-level load/save API (`jldsave`, `load`)

### Data Handling

- `src/data/` - Subdirectory containing type-specific serialization:
  - `custom_serialization.jl` - User-defined serialization hooks
  - `specialcased_types.jl` - Built-in type optimizations
  - `writing_datatypes.jl` - HDF5 datatype generation
  - `reconstructing_datatypes.jl` - Deserialization logic

### HDF5 Implementation

- `src/file_header.jl`, `src/superblock.jl` - HDF5 file format structure
- `src/object_headers.jl`, `src/headermessages.jl` - HDF5 object metadata
- `src/global_heaps.jl`, `src/fractal_heaps.jl` - HDF5 heap management
- `src/dataspaces.jl`, `src/datalayouts.jl` - HDF5 data organization

### I/O and Performance

- `src/io/` - I/O backends:
  - `mmapio.jl` - Memory-mapped I/O (default on most platforms)
  - `bufferedio.jl` - Buffered I/O fallback
  - `dataio.jl` - Core data reading/writing functions

### Filters and Compression

- `src/Filters.jl` - Compression filter system (Deflate, Zstd, Shuffle)
- `filterpkgs/` - External filter package definitions

## Key Design Patterns

### File Handles

- `JLDFile{T<:IO}` - Main file handle, parameterized by I/O backend type
- `Group{T}` - Represents HDF5 groups (directories), can be nested
- Files track whether they're writable, what compression is used, and maintain reference counting

### Serialization System

- Custom serialization via `writeas` and `readas` functions
- Automatic struct handling with upgrade mechanisms for version compatibility
- Type introspection for automatic HDF5 datatype generation

### Memory Management

- Automatic mmap fallback to IOStream on Windows 7
- Reference tracking for open files to prevent conflicts
- Finalizers handle cleanup when files go out of scope

## Testing Patterns

- Each test file focuses on a specific aspect (groups, compression, etc.)
- `test/runtests.jl` orchestrates all tests and handles filter package setup
- Uses `@testitem` macros for package-level testing with TestItemRunner
- Tests include compatibility with external HDF5 tools and legacy formats

## Dependencies and Extensions

### Core Dependencies

- FileIO.jl - File format detection and dispatch
- OrderedCollections.jl - Ordered dictionaries for group contents
- MacroTools.jl - Macro utilities for code generation
- Mmap.jl - Memory mapping support

### Compression Filters

- ChunkCodecLibZlib.jl, ChunkCodecLibZstd.jl - Built-in compression
- Optional filter packages in `filterpkgs/` for additional compression formats

### Weak Dependencies

- UnPack.jl - Optional struct unpacking support via extension

## Development Best Practices

### Julia Constructor Patterns

- Inner constructors with validation should use `new()` inside struct definition
- Avoid recursive constructor calls that create infinite loops
- Use `isdefined(obj, :field)` to check field existence for conditional HDF5 fields
- Ensure all fields are initialized in constructors to prevent UndefRefError

### Code Quality Guidelines

- Comprehensive documentation for all public types and functions
- Consistent error handling with descriptive messages
- Unit tests covering edge cases and error conditions
- Validation functions separated from core constructors
- Use specific error types (InvalidDataException, UnsupportedFeatureException) with context

### Testing Strategies

- Phase-based testing approach helps isolate functionality
- Test both success and failure cases with flexible string matching
- Use `@test_throws ErrorType expression` for expected exceptions
- Use `Union{ErrorType1, ErrorType2}` for platform-dependent error types
- OrderedCollections must be imported through JLD2 module structure
- Test type checking with `eltype()` rather than hardcoded type assertions

### HDF5 Compatibility

- Must maintain compatibility with standard HDF5 tools
- Validate with: `h5dump -H file.jld2`, `h5debug file.jld2`
- Cross-validation possible with h5py
- Use `@pseudostruct` system for HDF5 message format compliance
- Conditional fields in messages require explicit flag setting

### File I/O and Performance

- Different I/O backends (mmap vs buffered) affect performance
- Windows 7 has automatic fallback behavior
- Reference counting prevents file conflicts
- Groups use OrderedDict for maintaining insertion order
- Memory-mapped I/O is default, with buffered fallback

### Error Handling Patterns

- Include context (file paths, operation type) in all error messages
- Preserve original error types for proper exception handling
- Use `hasfield()` and `getfield()` for safe error message enhancement
- Different error handling strategies based on operation type
- Fast error detection with minimal overhead
- **Avoid try-catch blocks**: Usage of try-catch environments is usually a sign of bad code design. Functions should return well-defined values for expected conditions rather than throwing exceptions for normal control flow

### Testing New JLD2 Features - Critical Validation Pattern

**ALWAYS validate file format before testing functionality:**

When building new JLD2 features (especially file format features like V1 B-trees, chunking, filters):

1. **File Format Validation First**: Use `JLD2.print_header_messages(f, dataset_name)` to verify the expected header messages are present and correct
2. **Independent End-to-End Tests**: Create tests that truly exercise the complete file format, not just individual processing functions
3. **Verify Storage Path**: Confirm the intended storage mechanism was actually used (e.g., DataLayout version 3 for V1 B-tree, not regular storage)

**Common Testing Mistake**: Calling feature processing functions but then storing with `f[name] = data`, which bypasses the feature entirely and uses regular JLD2 format instead.

**Example - V1 B-tree Testing**:

```julia
# ❌ WRONG - This tests chunk processing but stores normally
write_chunked_dataset_with_v1btree(f, data, odr, filters, session, chunk_dims)
f["test"] = data  # Uses regular storage, not V1 B-tree format!

# ✅ CORRECT - Test complete V1 B-tree file format
JLD2.write_chunked_array(f, "test", large_array)  # Triggers natural V1 B-tree path
JLD2.print_header_messages(f, "test")  # Verify DataLayout v3 present
```

**File Format Debugging Tools**:

- `JLD2.print_header_messages(f, dataset_name)` - Shows all header messages for a dataset
- `h5debug filename.jld2` - Validates HDF5 format compliance
- `h5dump -H filename.jld2` - Shows HDF5 structure without data

This validation pattern prevents debugging "phantom bugs" where tests pass on isolated functions but fail on actual file format implementation.

### Complex Data Corruption Debugging Methodology

When implementing complex file format features (like V1 B-trees, chunking, links), data corruption bugs can be particularly challenging to isolate. Use this systematic approach:

**Threshold Analysis**:

- Test incrementally increasing data sizes to find exact corruption boundaries
- Identify which code paths are triggered at specific thresholds (e.g., 64KB for chunking)
- Isolate feature activation as the corruption trigger

**Element-Level Analysis**:

- Examine specific array positions (first, second, middle, last elements)
- Use predictable data like `reshape(1.0:N, dims...)` for easy corruption detection
- Look for patterns (e.g., first element correct suggests indexing/offset issues)

**Component Isolation**:

- Test core algorithms separately from integration pipelines
- Verify file format compliance with external tools (h5debug, h5dump)
- Narrow corruption to specific pipeline stages

**Index/Dimension Issues - Array Indexing in Julia vs HDF5**:

**CRITICAL UNDERSTANDING**: Both Julia and HDF5 store arrays as contiguous blocks of memory with one element after another. The difference is only in how they report dimensions and indices:

- **Julia indexing**: `arr[i,j,k,l]` where `i` is the **fastest-changing** index, then `j`, then `k`, then `l`
- **HDF5 dimensionality**: Reports the same array as `{l_max, k_max, j_max, i_max, elementsize}` - **reversed order**
- **HDF5 chunk indices**: Use 0-based indices in HDF5 order: `{l0, k0, j0, i0, 0}`
  - The last (fastest) dimension in HDF5 is always 0 because you must read whole elements at a time
- **Memory layout is identical** - only the dimension reporting is reversed

**Example**: A Julia array of size `30×10`:
- Julia sees: 30 rows (fastest) × 10 columns (slowest)
- HDF5 reports: dimensions `(10, 30)` plus element size dimension
- First chunk at Julia `(1,1)` → HDF5 indices `(0, 0, 0)` (0-based, reversed, plus element size)

**DO NOT** talk about "row-major" vs "column-major" - this creates confusion. The memory layout is the same; only dimension indexing differs.

- **0-based vs 1-based confusion**: Common at HDF5↔Julia interface boundaries
- **Dimension ordering**: HDF5 reports dimensions in reversed order from Julia
- **Chunk indices**: Must be in HDF5 convention (reversed, 0-based)
- Use `reverse()` operations carefully - track through write→storage→read pipeline

**Memory Safety vs Data Integrity**:

- Memory safety (no segfaults) can be maintained while data integrity fails
- `unsafe_wrap()` operations are prime suspects for corruption
- High corruption rates (>90%) indicate systematic bugs, not random corruption

### HDF5 Message Format Implementation

**Critical Implementation Requirements**:

- Conditional fields in `HmLinkMessage` require both `link_info_size` and data blob
- Bit 3 must be set for `link_type` field: `flags = UInt8(0x10 | 0x08 | size_flag(sizeof(name)))`
- `write_link_message` and `message_size_for_link` must use identical parameters
- External link payload requires 0x00 reserved byte at start (HDF5 spec compliance)

**@pseudostruct Usage Patterns**:

- Direct field access works; avoid `isdefined()` checks on message fields
- Use explicit flag setting for conditional message fields
- DataLayout version 3 requires specific field requirements and bit flags

**V1 B-tree Specifics**:

- Key ordering rule: Key[i] describes the least chunk in Child[i]
- Variable-size keys based on dimensionality (D+1 indices per key)
- Node signature: `htol(0x45455254)` ("TREE")
- Use `jlsizeof(RelOffset)` for offset sizes (8 bytes in JLD2)

### Module Loading Order Dependencies

External file management and complex features require careful module loading order:

```julia
# Correct include order for complex features:
include("types.jl")          # JLDFile type defined first
include("links.jl")          # Core link hierarchy
# ... other modules ...
include("path_resolution.jl") # Path resolution (needs link types)
include("external_files.jl")  # External file management (needs JLDFile)
include("groups.jl")           # Group functionality (needs all above)
```

### Link System and External Files Architecture

**Link Type Hierarchy**:

- `AbstractLink` hierarchy: `HardLink`, `SoftLink`, `ExternalLink`
- Security validation prevents directory traversal attacks in constructors
- Use `lookup_offset` for backward compatibility, `lookup_link` for new functionality

**External File Management Patterns**:

- WeakRef-based caching allows garbage collection (max 32 external files with LRU eviction)
- Task-local reference chain tracking for circular reference detection
- Exponential backoff retry logic for transient failures (network filesystems)
- Path validation limits "../" traversal for security

**HDF5 Path Resolution**:

- Absolute paths start with "/", relative paths resolve from current group
- Context-aware soft link resolution optimized for common cases
- Complex upward navigation ("../") has limitations for disk-loaded groups

### Common Code Patterns and Gotchas

**Predicate Functions**:

```julia
# ✅ CORRECT
count(x -> x == "..", path_components)

# ❌ WRONG
count("..", split(...))
```

**Error Context Patterns**:

- Re-attempt path resolution in catch blocks to avoid `UndefVarError` in error messages
- Maintain original error types (SystemError, KeyError) while adding context
- Include file paths and object paths in error messages for debugging

**Performance Considerations**:

- Groups don't inherently know their file hierarchy path (architectural limitation)
- Use space allocation via `f.end_of_data` increment pattern
- Target 32KB chunks for optimal I/O performance in chunked datasets

### Cross-Platform Considerations

- System error codes vary between platforms
- Use errno constants where possible, test error messages for robustness
- Use `normpath()` and `abspath()` for cross-platform compatibility
- Windows file locking is more aggressive than Unix
- Path separators handled uniformly by Julia's path functions

## HDF5 Binary Format Debugging Tools

When implementing or debugging HDF5 file format features, use these CLI tools and techniques for deep inspection:

### Essential HDF5 Command-Line Tools

**h5dump** - Primary tool for viewing HDF5 file contents:

```bash
h5dump -H file.jld2              # Header only (structure without data)
h5dump -d /dataset file.jld2     # Dump specific dataset with data
h5dump -p file.jld2              # Show all properties
h5dump -A file.jld2              # Show attributes only
```

**h5debug** - Low-level format debugging:

```bash
h5debug file.jld2                            # Basic file structure
h5debug file.jld2 <offset>                   # Inspect object at offset
h5debug file.jld2 <btree_offset> <ndims> <dim1> <dim2> ...  # Debug B-tree node
```

**Example B-tree debugging**:

```bash
# For a 2D chunked dataset with chunk dims [3, 2] and element size 8:
h5debug file.jld2 37496 3 2 3 8   # Inspect B-tree node at offset 37496
```

### Binary File Inspection with Julia

**Reading raw file structure**:

```julia
# Read at specific offset (RelOffset is relative to superblock at 512)
io = open("file.jld2", "r")
seek(io, 512 + rel_offset)  # Convert RelOffset to absolute position

# Read and verify signatures
sig = read(io, UInt32)
println("Signature: 0x$(string(sig, base=16))")

# For B-tree nodes, expected: htol(0x45455254) = "TREE"
# For object headers, expected: 0x5244484f = "OHDR"

# Read node header
node_type = read(io, UInt8)
node_level = read(io, UInt8)
entries_used = read(io, UInt16)

close(io)
```

**Hex dump analysis**:

```julia
# Read and display as hex for manual inspection
io = open("file.jld2", "r")
seek(io, absolute_offset)
bytes = read(io, 64)  # Read 64 bytes
println("Hex: $(join(string.(bytes, base=16, pad=2), " "))")
close(io)
```

### Debugging Workflow for File Format Issues

1. **Verify structure with h5dump first**:

   - Check that dataset exists and has expected properties
   - Verify chunking is enabled and chunk dimensions are correct
   - Confirm DataLayout version (v3 for V1 B-tree)

2. **Inspect B-tree structure with h5debug**:

   - Verify root node offset matches DataLayout message
   - Check node levels and entry counts
   - Validate child offsets point to valid nodes
   - Examine key values for correctness

3. **Binary inspection for detailed debugging**:

   - Read raw bytes at specific offsets to verify what was written
   - Compare written vs expected values byte-by-byte
   - Check signature bytes match expected format
   - Validate offset calculations (RelOffset + 512 = absolute)

4. **Cross-validate with external tools**:
   - If h5dump shows zeros, chunks aren't being found (B-tree navigation issue)
   - If h5dump shows correct data, reading logic is the problem
   - If h5debug reports structure errors, writing logic is the problem

### Common Offset Calculations

**RelOffset to absolute position**:

```julia
fileoffset(f, rel_offset) = rel_offset.offset + f.base_address  # base_address = 512
absolute_pos = 512 + rel_offset.offset
```

**B-tree node structure sizes**:

```julia
# Node header: 24 bytes
# - Signature: 4 bytes (UInt32)
# - Node type: 1 byte (UInt8)
# - Node level: 1 byte (UInt8)
# - Entries used: 2 bytes (UInt16)
# - Left sibling: 8 bytes (RelOffset)
# - Right sibling: 8 bytes (RelOffset)

# Entry (interleaved format):
# - Key: 4 (chunk_size) + 4 (filter_mask) + 8*N (indices) bytes
# - Child offset: 8 bytes (RelOffset)

# For 2D array (3 indices including element size):
entry_size = 4 + 4 + 8*3 + 8 = 40 bytes
```

### Debugging Data Corruption

**Systematic approach**:

1. **Verify chunks are written correctly**:

```julia
# Check first chunk data at its offset
io = open("file.jld2", "r")
seek(io, 512 + chunk_offset)
chunk_data = read(io, chunk_size)
# Convert to expected type and verify values
```

2. **Verify B-tree keys are correct**:

```julia
# Keys should have 0-based element indices (not chunk counts!)
# For 30×80 array with 3×2 chunks:
# Chunk at Julia (1,1) → key indices (0, 0, 0) in HDF5 order
# Chunk at Julia (1,3) → key indices (2, 0, 0) in HDF5 order
# Boundary key should be (80, 30, 0) - one past array dims
```

3. **Verify internal node keys**:

```julia
# Internal nodes: Key[i] describes minimum key in Child[i]
# Must read actual keys from child nodes, not use dummy values
# Boundary key is from last key of last child node
```

### V1 B-tree Specific Debugging

**Key format validation**:

- Chunk indices are **0-based element positions** of first element in chunk
- NOT chunk numbers (0, 1, 2, ...) but actual array element indices
- Example: 5×3 tiling has indices like (0,0,0), (0,5,0), (3,0,0), (3,5,0)
- Last index (datatype offset) must always be 0
- Boundary key = array dimensions in 0-based (one past last valid index)

**Node traversal validation**:

- Internal nodes have level > 0, children point to other B-tree nodes
- Leaf nodes have level = 0, children point to chunk data
- Each node must have valid TREE signature at its offset
- Child offsets must be valid RelOffset values, not confused with chunk_size

**Common bugs**:

- Reading `dimensionality + 1` indices when DataLayout dimensionality already includes element size
- Using chunk counts instead of element indices for keys
- Using dummy keys in internal nodes instead of reading from children
- Confusing chunk_size field with child offset during reading (check interleaved format)
- Running the full JLD2 test suite takes somewhere between 5 to 7 minutes.
- You CANNOT call `isdefined` on a header message object. This will always return false. Stop trying it. Trust the definitions in headermessages.jl.
- Use `h5ls -r -v --address <filename>` to very quickly access offsets of datasets or dataset chunks within hdf5 or jld2 files
- When developing JLD2, you will often find yourself waiting for JLD2 to precompile. If you are not concerned with runtime performance, use the `-O1` or possibly even `-O0` flag as in `julia -O1` to reduce the optimization and level and hopefully reducing turnaround time.
- When you write new functions: Do not make the type signatures more strict than needed for correctness. This leads to many bugs while developing. Example: `f(xs::Vector{Int64}) = ...` could likely also be written as `f(xs:AbstractVector{<:Integer})` if it only needs to be indexable and have some kind of interger as values.