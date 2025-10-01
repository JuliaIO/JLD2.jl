# CLAUDE.md - JLD2 Development Guide

This guide enables Claude instances to effectively develop JLD2, a Julia HDF5-compatible serialization package.

## Quick Start

```bash
# Install and test
julia --project -e 'using Pkg; Pkg.instantiate()'
julia -e 'using Pkg; Pkg.test("JLD2")'  # Full suite: 5-7 minutes

# Fast development iteration (reduce compilation)
julia -O1 --project test_script.jl  # Reduces precompilation time

# ❌ WRONG - There is no way to select specific tests to run in this way! This runs the full tests!
julia --project -e 'using Pkg; Pkg.test("JLD2"; test_args=["<some testset>"])

# Build docs
julia --project=docs docs/make.jl
```

## Architecture Essentials

### Core Structure

- `src/JLD2.jl` - Main module with JLDFile/Group types
- `src/data/` - Type-specific serialization
  - `custom_serialization.jl` - User hooks via `writeas`/`readas`
  - `writing_datatypes.jl` - HDF5 datatype generation
  - `reconstructing_datatypes.jl` - Deserialization logic
- `src/io/` - I/O backends (mmap default, buffered fallback)
- `src/headermessages.jl` - HDF5 message formats using @pseudostruct
- `src/fractal_heaps.jl`, `src/global_heaps.jl` - Heap management (reusable patterns)
- Filters in `src/Filters.jl` and `filterpkgs/`

### Key Types

```julia
JLDFile{T<:IO}  # Main file handle
Group{T}        # HDF5 groups (directories)
RelOffset       # HDF5 relative offset (add 512 for absolute)
```

### Module Load Order (Critical)

```julia
include("types.jl")           # JLDFile type first
include("links.jl")           # Core link hierarchy
include("path_resolution.jl") # Needs link types
include("external_files.jl")  # Needs JLDFile
include("groups.jl")          # Needs all above
```

## Critical Development Principles

### Code Quality Guidelines

- Comprehensive documentation for all public types and functions
- Consistent error handling with descriptive messages
- Unit tests covering edge cases and error conditions
- Validation functions separated from core constructors
- Use specific error types (InvalidDataException, UnsupportedFeatureException) with context
- Always use `isnothing(variable)` instead of `variable === nothing` and `!isnothing(variable)` instead of `variable !== nothing`
- Where it makes sense. In julia you can pass keyword arguments like this: `fun(; variable)` assuming that function `fun` takes a keyword named `variable` and that the calling scope has the value already stored in a var name `variable`. This makes for concise syntax. ( It requires the `;` !)
- **Avoid try-catch blocks**: Usage of try-catch environments is usually a sign of bad code design. Functions should return well-defined values for expected conditions rather than throwing exceptions for normal control flow

- When writing data transformations that involve branching and possibly nested for loopes, like computing chunk indices, wrap them into separate functions and document their desired behaviour. These functions can often be greatly simplified in further editing steps by using higher order functions. However, this is an optimization that can follow later. Creating a working solution has priority.

### Build Tools First Approach

When facing complex issues, invest in debugging infrastructure before attempting fixes. This saves 10-20x debugging time.

**Multi-Layer Debugging Architecture:**

1. **Raw Byte Analysis** - Hex dumps with structure recognition
2. **Structure Validation** - Specification compliance checking
3. **Comparative Analysis** - Reference implementation comparison

### Memory Layout: Julia vs HDF5

**Both use identical memory layout** - only dimension reporting differs:

```julia
# Julia array: 30×10 (30 fastest, 10 slowest)
# HDF5 reports: (10, 30) plus element size
# Memory: IDENTICAL - contiguous elements

hdf5_dims = reverse(julia_dims)  # Only for reporting
# NEVER use "row-major/column-major" terminology - creates confusion

# Chunk indices in HDF5:
# Julia chunk (1,1) → HDF5 indices (0, 0, 0)  # 0-based, reversed, +element dimension
```

- **Julia indexing**: `arr[i,j,k,l]` where `i` is the **fastest-changing** index, then `j`, then `k`, then `l`
- **HDF5 dimensionality**: Reports the same array as `{l_max, k_max, j_max, i_max, elementsize}` - **reversed order**
- **HDF5 chunk indices**: Use 0-based indices in HDF5 order: `{l0, k0, j0, i0, 0}`
  - The last (fastest) dimension in HDF5 is always 0 because you must read whole elements at a time

## Hex Dump Debugging (Critical Tool)

### Structure-Aware Hex Dumps

```julia
# Read raw bytes at specific offset
io = open("file.jld2", "r")
seek(io, 512 + rel_offset.offset)  # RelOffset to absolute
bytes = read(io, 64)

# Display with annotations
println("Offset $(position(io)):")
println("  Signature: $(bytes[1:4])  # Expected: TREE/OHDR/FADB")
println("  Type/Level: $(bytes[5:6])")
println("  Entries: $(reinterpret(UInt16, bytes[7:8])[1])")
println("Hex: $(join(string.(bytes, base=16, pad=2), " "))")
```

### Intelligent Pattern Recognition

```julia
# Auto-detect HDF5 signatures in hex dump
function analyze_hex(bytes)
    sig = reinterpret(UInt32, bytes[1:4])[1]
    if sig == htol(0x45455254)
        println("B-tree node (TREE)")
    elseif sig == 0x5244484f
        println("Object header (OHDR)")
    elseif sig == 0x42444146
        println("Fixed array data block (FADB)")
    end
end
```

### Side-by-Side Comparison

Note, this is only useful if you are, for example, comparing object headers where you have already correctly identified the correct offsets within each of the files.

```julia
# Compare working (h5py) vs broken (JLD2) files
function compare_at_offset(file1, file2, offset1, offset2, nbytes=64)
    io1 = open(file1, "r"); seek(io1, offset1)
    io2 = open(file2, "r"); seek(io2, offset2)

    bytes1 = read(io1, nbytes)
    bytes2 = read(io2, nbytes)

    for i in 1:nbytes
        if bytes1[i] != bytes2[i]
            println("Diff at +$i: $(bytes1[i]) vs $(bytes2[i])")
        end
    end
end
```

## @pseudostruct Field Patterns

The @pseudostruct macro generates four functions for each header message.

- `jlwrite(::IO, ::Val{<message name>}, hflags, hsize, kw)`
- `jlsizeof(::Val{<message name>}, hflags=0x00, size=0; kwargs...)`
- `Base.show(io::IO, hm::Hmessage)`
- `getproperty(::HmWrap{<message name>, IO}, <property name>)`

This code generation is a complex but well tested process.
Before testing the code generation you should check if it is a usage error or whether the message definition does not adhere to the format specification.

### Field Role Classification

```julia
# 1. SIZE PARAMETERS (always required)
data_size::@Int(2)        # Used in increments
name_len::UInt16          # Determines other field sizes

# 2. CONTENT FIELDS (can skip in compute_size)
data::@Blob(data_size)    # Pure data storage
name::@FixedLengthString(name_len)  # Content only

# 3. CONTROL FIELDS (required for conditions)
layout_class::LayoutClass # Used in if statements
flags::UInt8              # Determines conditional fields

# 4. FIELDS WITH DEFAULTS (use default if not provided)
shared_field::RelOffset = UNDEFINED_ADDRESS  # Has fallback
```

**WARNING**: Cannot use `isdefined()` on @pseudostruct message objects - always returns false. Trust field definitions.

### Conditional Field Patterns

```julia
# HmLinkMessage requires both size and data for link_type
if flags & 0x10 != 0  # Bit 3 set
    link_type::UInt8
    link_info_size::@Int(2)
    link_info::@Blob(link_info_size)
end
```

## Chunked Data Implementation

### DataLayout Dimensions Critical Rule

For chunked layouts:

- `DataLayout.dimensions` = **chunk dimensions** (NOT array dimensions)
- Array dimensions go in `HmDataspace` message
- Common bug: storing array dims causes chunk iteration failure

### Chunk Padding (Mandatory)

```julia
# All chunks must be same size, even partial edge chunks
if actual_chunk_size != chunks
    full_chunk = zeros(T, chunks)
    ranges = ntuple(i -> 1:actual_chunk_size[i], N)
    full_chunk[ranges...] = chunk_data_partial
    chunk_data = full_chunk
end
```

### Linear Index Calculation

Must exactly match between read/write:

```julia
# Precompute for HDF5 ordering
grid_dims_hdf5 = reverse(grid_dims)
down_chunks = zeros(Int, ndims_hdf5)
acc = 1
for i in ndims_hdf5:-1:1
    down_chunks[i] = acc
    acc *= grid_dims_hdf5[i]
end

# Convert Julia index to HDF5 linear index
hdf5_coords = reverse(Tuple(julia_idx) .- 1)  # 0-based, reversed
linear_idx = sum(down_chunks .* hdf5_coords)
```

## Variable-Size Field Patterns

### Pattern Recognition for Reuse

Many HDF5 structures share variable-size patterns. Check existing code:

```julia
# Fractal Heap pattern (reusable)
field_size = cld(Int(max_bits), 8)  # Bits to bytes
value = if field_size == 1
    UInt64(jlread(io, UInt8))
elseif field_size == 2
    UInt64(jlread(io, UInt16))
elseif field_size == 4
    UInt64(jlread(io, UInt32))
else
    jlread(io, UInt64)
end
```

### Address Type Variations

```julia
# Different v4 indexing types use different address formats
# Fixed Array (Type 3): RelOffset
base_address = fileoffset(f, layout.data_offset)

# Implicit Index (Type 2): Int64
base_address = Int64(layout.data_offset)

# Single Chunk (Type 1): RelOffset
chunk_address = fileoffset(f, layout.data_offset)
```

### Structure Reuse Patterns

Complex HDF5 structures often share patterns that can be reused:

**Identification Strategy:**
Example:

1. Search codebase for similar field size calculations
2. Look for `cld(bits, 8)` patterns
3. Check Fractal Heap implementation for heap structures
4. Review existing chunk indexing for shared patterns

## Testing Methodology

### Validation Pattern for New Features

**ALWAYS validate file format before testing functionality:**

1. Use debugging tool like `JLD2.print_header_messages(f, dataset_name)` to verify the expected header messages are present and correct
2. **Independent End-to-End Tests**: Create tests that truly exercise the complete file format, not just individual processing functions
3. **Verify Storage Path**: Confirm the intended storage mechanism was actually used (e.g., DataLayout version 3 for V1 B-tree, not regular storage)

```julia
# ✅ CORRECT - Test complete file format
JLD2.write_chunked_array(f, "test", data)
JLD2.print_header_messages(f, "test")  # Verify expected messages

# ❌ WRONG - Tests function but uses regular storage
process_with_feature(data)
f["test"] = data  # Bypasses feature, uses regular format!
```

### Progressive Testing Strategy

1. **Start Simple**: Even chunks, basic types
2. **Add Complexity**: Partial chunks, edge cases
3. **Threshold Analysis**: Find exact corruption boundaries
   ```julia
   # Test incrementally to find feature activation
   for size in [10, 100, 1000, 10000]
       test_with_size(size)
   end
   ```
4. **Element-Level Analysis**:
   ```julia
   data = reshape(Float32.(1:300), dims...)  # Predictable pattern
   @test data[1] == 1.0     # First element
   @test data[end] == 300.0  # Last element
   ```

### External Validation & Debugging

```bash
# HDF5 tools
h5dump -H file.jld2              # Structure without data
h5ls -r -v --address file.jld2   # Dataset offsets
h5debug file.jld2 offset          # Inspect specific offset
h5debug file.jld2 <btree_offset> <ndims> <dim1> <dim2> ...  # Debug B-tree node
h5ls -r -v --address file.jld2   # Quickly reveal dataset offsets

# h5py cross-validation
python3 -c "import h5py; f=h5py.File('test.h5'); print(f['dataset'][:].sum())"
```

```python
# h5py validation script
import h5py
f = h5py.File('test.h5', 'r')
print('Shape:', f['dataset'].shape)
print('Sum:', f['dataset'][:].sum())
```

### Test File Creation for Specific Features

```python
import h5py
# Implicit Index - requires early allocation
dcpl = h5py.h5p.create(h5py.h5p.DATASET_CREATE)
dcpl.set_alloc_time(h5py.h5d.ALLOC_TIME_EARLY)

# Extensible Array - requires maxshape
f.create_dataset('data', shape=(10,10), maxshape=(None,None))
```

### Adding test files to the test suite to protect against regressions

## Testing Patterns

- Each test file focuses on a specific aspect (groups, compression, etc.)
- `test/runtests.jl` orchestrates all tests and handles filter package setup
- Uses `@testitem` macros for package-level testing with TestItemRunner
- Tests include compatibility with external HDF5 tools and legacy formats
- New test files intended to representatively test newly implemented features must be added to the runtests.jl files

## Debugging Patterns

### Strategic Debug Output

```julia
# GOOD: Key decision points only
if chunk_idx == CartesianIndex(1, 1)
    println("DEBUG first chunk:")
    println("  address: $chunk_address (expected: 2048)")
    println("  key: $key_values")
end
# Remove before commit
```

### Common Bug Patterns

**Index Offset Confusion:**

```julia
# BUG: Mixing 1-based and 0-based
chunk_root = CartesianIndex(indices .* dims .+ 1)  # Julia 1-based
vidxs = chunk_ids .+ chunk_root  # Double offset!

# FIX: Zero-based offset for chunks
chunk_root = CartesianIndex(indices .* dims)  # 0-based offset
vidxs = chunk_ids .+ chunk_root
```

**Dimension Ordering:**

- Symptom: Data appears scrambled/transposed

```julia
# BUG: Forgot to reverse for HDF5
dimensions = size(data)  # Wrong order

# FIX: Always reverse for HDF5
dimensions = reverse(size(data))
```

Also: HDF5 often-times interprets the element size as an extra (fastest-changing) dimension

### Binary File Inspection

```julia
# Verify signatures
io = open("file.jld2", "r")
seek(io, 512 + rel_offset.offset)
sig = read(io, UInt32)

# Expected signatures:
# B-tree: htol(0x45455254) = "TREE"
# Object: 0x5244484f = "OHDR"
# Fixed Array Header: 0x44484146 = "FAHD"
# Fixed Array Data: 0x42444146 = "FADB"

# Verify data at chunk offset
seek(io, 512 + chunk_offset)
chunk_bytes = read(io, chunk_size_bytes)
chunk_data = reinterpret(Float32, chunk_bytes)
println("First values: ", chunk_data[1:min(5, end)])
```

## V1 B-tree Specifics

### Key Format Rules

- Keys are **0-based element positions**, NOT chunk numbers
- Format: `(element_indices..., 0)` where last is always 0
- Example for 30×80 array with 3×2 chunks:
  - Chunk at Julia (1,1) → key (0, 0, 0)
  - Chunk at Julia (1,3) → key (4, 0, 0) # Element position!
- Boundary key = array dimensions (one past last valid)

## Error Handling Best Practices

- **Avoid try-catch**: Indicates poor design. Return well-defined values
- Include context: file paths, operation type, offsets
- Use specific error types: `InvalidDataException`, `UnsupportedFeatureException`
- Test with `@test_throws Union{ErrorType1, ErrorType2}` for platform differences
- Preserve original error types while adding context

## Code Organization

### Multi-Structure Features

```
src/feature_name.jl
  - read_feature()        # Entry point
  - read_structure_a()    # Structure-specific
  - read_structure_b()    # Structure-specific
  - shared_utilities()    # Reusable helpers
```

**Benefits:**

- Clear navigation following structure hierarchy
- Easy independent testing
- Minimal coupling between features
- Functions map to specification sections

### Type Flexibility

```julia
# GOOD: Flexible type signatures
f(xs::AbstractVector{<:Integer}) = ...

# BAD: Overly strict
f(xs::Vector{Int64}) = ...
```

### Remark: Difference between good library code and good development code

- Good library code uses multiple dispatch to use meaningful function names for all implementations that serve a similar purpose.
- Good library code is structured into separate modules encapsulating specific features and all associated helper functions and only exposing relevant API functions to the top-level package.
- Good development code on the other hand must reduce risk unnecessary errors:
  - Use unique function and descriptive names for every new function to easily detect code paths while debugging
  - Do not use separate modules and name spaces to avoid scope issues while focusing on the actual features.

## Performance & Development Speed

- Target 32KB chunks for optimal I/O
- Use `julia -O1 <args>` during development (faster precompilation)
- Use `-O2` only for benchmarks
- Memory-mapped I/O default, buffered fallback
- Groups use OrderedDict (maintain insertion order)
- WeakRef caching for external files (max 32, LRU eviction)

## Link System & Security

- AbstractLink hierarchy: HardLink, SoftLink, ExternalLink
- Security: Validate paths, limit "../" traversal
- Task-local reference chain tracking for circular detection
- Exponential backoff for transient failures
- Use `lookup_link` for new code, `lookup_offset` for compatibility

## Development Workflow

1. **Phase-based approach**: Isolate functionality
2. **Minimal reproductions**:
   ```julia
   # test_edge_case.jl
   data = CustomType(1, 2.0)  # Trigger specific path
   jldopen("minimal.jld2", "w") do f
       f["test"] = data
   end
   ```
3. **External validation**: Always compare with h5py
4. **New feature testing**: Add concise and meaningful test to the test suite. Test them individually first
5. **Full test suite**: Run at the end of a developent effort to ensure everything still works
6. **Documentation**: End each phase with continuation prompt referencing DEVELOPMENT_INSIGHTS.md

## Critical Gotchas

- `count(x -> x == "..", components)` NOT `count("..", split(...))`
- RelOffset + 512 = absolute file position (superblock at 512)
- Chunk dimensions ≠ array dimensions in DataLayout
- Test suite auto-handles filter packages
- OrderedCollections import through JLD2 module
- Groups don't know their file path (architectural limitation)
- `jlsizeof(RelOffset)` = 8 bytes in JLD2

## When Debugging Complex Issues

1. **Systematic Approach**:

   - Check format with `h5dump -H`
   - Verify with `JLD2.print_header_messages()`
   - Compare hex dumps with working h5py files
   - Read raw bytes at suspicious offsets

2. **Data Corruption Pattern**:

   - Find location where chunk was supposed to be written and verify by accessing bytes directly
   - Check first/middle/last elements
   - Loading successful (no error) but data is partially wrong? Check index calculation
   - Use sequential test data
   - Isolate to specific pipeline stage

3. **Format Compliance**:
   - Remember: Functional correctness ≠ format compliance
   - Data can round-trip in JLD2 while failing h5dump
   - Always validate against external tools

**Key Insight**: Complex HDF5 structures often share patterns. Search existing code (Fractal Heaps, chunking) before implementing from scratch.

### Format Compliance vs Functional Correctness

**Key Insight:** Data can round-trip correctly within JLD2 while failing external tool compatibility.

**Common Compliance Issues:**

- Byte ordering and padding requirements
- Message header format with conditional fields
- Chunk addressing format matching specification
- Checksum calculations

**Testing Both Aspects:**

1. Functional: Verify data round-trips correctly
2. Compliance: Verify external tools can read files
3. Reference: Compare against known-good implementations

## Notes to interact with user:

- When you finish a step of phase of a larger development plan you should ALWAYS end with writing a prompt for a different claude instance to continue the work with concise but well-written instructions and referencing further markdown files that can provide additional information.
