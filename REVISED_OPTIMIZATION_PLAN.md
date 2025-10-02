# REVISED @pseudostruct Optimization Plan

## The Real Problem (Thanks to User Insight!)

**I was wrong about the main issue.** The critical problem is NOT caching fields across multiple `getproperty` calls (those branches are mutually exclusive!).

The **REAL** performance problem is:

### Reading Large Content Fields Unnecessarily

When accessing a field, we traverse through ALL previous fields to compute the offset, and currently we **READ THE CONTENT** of fields like `@FixedLengthString` and `@Blob` even when we only need to skip past them.

**Example:**
```julia
@pseudostruct Example begin
    version::UInt8
    name_len::UInt16
    name::@FixedLengthString(name_len)  # Could be 1000 bytes!
    data_size::UInt32
    data::@Blob(data_size)              # Could be MEGABYTES!
    next_field::UInt64
end
```

**Current code to access `next_field`:**
```julia
if s == :next_field
    offset = base_addr
    offset += sizeof(UInt8)    # skip version
    seek(io, offset)
    name_len = jlread(io, UInt16)  # Read length (needed)
    offset += sizeof(UInt16)
    seek(io, offset)
    name = String(jlread(io, UInt8, name_len))  # ❌ READ 1000 BYTES!
    offset += name_len
    seek(io, offset)
    data_size = jlread(io, UInt32)  # Read size (needed)
    offset += sizeof(UInt32)
    seek(io, offset)
    data = jlread(io, UInt8, data_size)  # ❌ READ MEGABYTES!
    offset += data_size
    seek(io, offset)
    next_field = jlread(io, UInt64)  # Finally!
    return next_field
end
```

**Problem**: We read MEGABYTES of data just to skip past it!

**Optimized code:**
```julia
if s == :next_field
    offset = base_addr
    offset += sizeof(UInt8)    # skip version
    seek(io, offset)
    name_len = jlread(io, UInt16)  # Read length (needed for offset calc)
    offset += sizeof(UInt16)
    offset += name_len         # ✅ SKIP without reading content!
    seek(io, offset)
    data_size = jlread(io, UInt32)  # Read size (needed for offset calc)
    offset += sizeof(UInt32)
    offset += data_size        # ✅ SKIP without reading content!
    seek(io, offset)
    next_field = jlread(io, UInt64)
    return next_field
end
```

**Savings**: Skip reading megabytes of data!

## Field Role Classification

1. **Size Parameter Fields**: Must read the VALUE
   - `name_len::UInt16` - needed for subsequent offset calculations
   - `data_size::UInt32` - needed for subsequent offset calculations

2. **Content Fields**: Only need SIZE, not content
   - `name::@FixedLengthString(name_len)` - skip the string bytes
   - `data::@Blob(data_size)` - skip the blob bytes

3. **Target Field**: Must read (it's what we're accessing!)
   - Whatever field `s` refers to

## Root Cause Analysis

Looking at `src/macros_utils.jl`, line 486-492:

```julia
important_names = Symbol[]

for (s, T, increment, io_assign, cond) in fields_list
    filter_symbols(important_names, increment)  # Accumulates across ALL fields
    filter_symbols(important_names, cond)
    filter_symbols(important_names, T)
```

**BUG**: `important_names` accumulates symbols from ALL fields, not just the current field's dependencies!

This causes ALL previous fields to be considered "important" and their content to be read.

**Fix**: Reset `important_names` for each field accessor.

## Implementation Plan

### Step 1: Fix Dependency Tracking (30 minutes)

**File**: `src/macros_utils.jl`, function `assemble_getprop` (lines 483-574)

**Change**: Move `important_names` initialization inside the loop

```julia
function assemble_getprop(fields_list)
    expr = Expr(:block)
    prep_statements = []
    # REMOVE: important_names = Symbol[]  # <-- Don't accumulate across all fields

    for (s, T, increment, io_assign, cond) in fields_list
        # ADD: Reset for each field
        important_names = Symbol[]  # <-- Fresh set for each field's dependencies

        # Extract dependencies for THIS field only
        filter_symbols(important_names, increment)
        filter_symbols(important_names, cond)
        filter_symbols(important_names, T)

        # ... rest of the loop ...
    end
end
```

This ensures each field accessor only reads the fields IT depends on, not all previous fields.

### Step 2: Test the Fix (30 minutes)

Create test case that demonstrates the optimization:

```julia
# test/offset_optimization_test.jl
using JLD2, Test

@testset "Content Field Skipping" begin
    mktempdir() do dir
        fn = joinpath(dir, "test.jld2")

        # Create file with large string
        jldsave(fn; large_data = repeat("x", 1000000))  # 1MB string

        # Track I/O operations
        f = jldopen(fn, "r")

        # Accessing a field AFTER large_data should NOT read all the data
        # This would be slow if it read the full 1MB
        @time begin
            # Access header metadata only
            # Should be fast (< 0.01s), not slow (> 0.1s from reading 1MB)
        end

        close(f)
    end
end
```

### Step 3: Verify Generated Code (30 minutes)

Use `@macroexpand` to verify the optimization works:

```julia
# Verify that content fields are skipped
expr = @macroexpand @pseudostruct TestStruct begin
    len::UInt16
    content::@FixedLengthString(len)
    after::UInt32
end

# Check that accessing 'after' doesn't include:
# content = String(jlread(...))
# Should only have:
# offset += len
```

## Expected Impact

### For Real-World Structures

**HmLinkMessage** with 100-byte link name:
- Current: Reads 100 bytes unnecessarily
- Optimized: Skips 100 bytes
- **Savings**: 100 bytes per property access

**HmDataLayout** with large compact data:
- Current: Could read megabytes unnecessarily
- Optimized: Skips all content
- **Savings**: Potentially gigabytes for large files

### Secondary Benefits

1. **Shorter generated code** (~20-30% reduction)
   - Don't generate read expressions for content fields
   - Simpler offset calculations
   - Faster compilation

2. **Better I/O cache locality**
   - Skip large regions → better buffer utilization
   - Fewer memory allocations

3. **Type stability improvements**
   - Fewer variables in scope
   - Simpler control flow

## Implementation Timeline

| Step | Duration | Description |
|------|----------|-------------|
| 1. Fix dependency tracking | 30 min | Move important_names inside loop |
| 2. Test the fix | 30 min | Create test case, verify correctness |
| 3. Verify code generation | 30 min | Check @macroexpand output |
| 4. Run full test suite | 30 min | Ensure no regressions |
| **Total** | **2 hours** | Complete implementation |

## Success Criteria

1. ✅ Content fields (@FixedLengthString, @Blob) not read when skipped
2. ✅ Size parameter fields still read (needed for offset calculation)
3. ✅ All JLD2 tests pass
4. ✅ No performance regression
5. ✅ Measurable speedup for structures with large content fields

## Risk Assessment

**Very Low Risk**:
- Small, localized change (one line moved)
- Existing filtering logic already handles this correctly
- Just fixing a bug where important_names was accumulating incorrectly
- No API changes, no binary format changes

## Bonus Optimizations (If Time Permits)

After the main fix, additional optimizations:

1. **Constant folding for fixed-size fields** (~1 hour)
   - Compute offsets for UInt8, UInt16, etc. at macro expansion time

2. **Type assertions** (~30 min)
   - Add `::T` to help compiler optimize

3. **Reduce seek() calls** (~1 hour)
   - Track IO position to eliminate redundant seeks

But these are OPTIONAL - the main fix (Step 1) is the critical one.
