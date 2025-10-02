# compute_size Improvements

## Summary

Improved the `@pseudostruct` macro's `compute_size` function generation to reduce the number of required keyword arguments for size computation.

## Problem Statement

Previously, `compute_size` required ALL fields as keyword arguments, even when their values weren't actually needed for computing the message size. This was particularly problematic for:

1. **Variable-length content fields**: Fields like `name::@FixedLengthString(name_len)` required both `name_len` AND `name`, even though only `name_len` is needed to compute the size.

2. **Conditional fields**: Fields inside conditionals would require values even when the computed size doesn't depend on those values.

## Solution Implemented

The improvement adds smart detection to determine which fields actually need values for size computation:

### Cases Where Values Are NOT Required

1. **Fields with defaults**: Use the default value automatically
   ```julia
   version::UInt8 = 4  # Don't need to provide version for size computation
   ```

2. **@FixedLengthString content**: Only the length parameter is needed
   ```julia
   name_len::UInt16
   name::@FixedLengthString(name_len)  # Don't need to provide name!
   ```

3. **@Blob content**: Only the size parameter is needed
   ```julia
   data_size::UInt32
   data::@Blob(data_size)  # Don't need to provide data!
   ```

4. **@computed fields**: Already handled (no value needed)

5. **@Offset fields**: Already handled (no value needed)

### Cases Where Values ARE Still Required

Fields without defaults that are:
- Used in conditions (e.g., `layout_class`, `flags`)
- Used in later size calculations (e.g., `name_len` for `@FixedLengthString(name_len)`)
- Size specification fields like `@Int` (these may be referenced by later fields)
- Not covered by the special cases above

**Important Note**: `@Int` fields ARE still required because they often serve as size parameters for subsequent fields. For example:
```julia
data_size::@Int(2)        # Required! Used by next field
data::@Blob(data_size)    # Uses data_size to compute offset
```

## Example

### Before Improvement

```julia
@pseudostruct Example begin
    name_len::UInt16
    name::@FixedLengthString(name_len)
    data_size::UInt32
    data::@Blob(data_size)
end

# Required to call:
jlsizeof(Val(Example);
    name_len=10,
    name="hello",      # ❌ Unnecessary!
    data_size=100,
    data=zeros(UInt8, 100)  # ❌ Unnecessary!
)
```

### After Improvement

```julia
# Only need to provide:
jlsizeof(Val(Example);
    name_len=10,
    data_size=100
)
# name and data are NOT required! ✅
```

## Implementation Details

### Code Changes (src/macros_utils.jl)

1. Added `increment_depends_on_field()` helper function to analyze field dependencies (lines 65-88)

2. Modified `linefun()` to detect variable-length content fields (lines 157-166):
   ```julia
   # Only @FixedLengthString and @Blob content can be skipped
   # @Int fields ARE needed as they may be size parameters for later fields
   is_var_length_content = (@capture(T, @FixedLengthString(_)) ||
                              @capture(T, @Blob(_)))
   size_needs_value = !isnothing(v) ? false : !is_var_length_content
   ```

3. Updated `compute_size` generation logic (lines 168-185):
   - Fields with defaults: Use `get(kw, :field, default)`
   - @FixedLengthString/@Blob content: Skip `haskey` check, just use `offset += increment`
   - @Int fields: Still require the value (may be used by later fields)
   - Other fields: Keep original behavior

### Bug Fix: @Int Field Handling

Initially, @Int fields were also treated as variable-length content, but this caused `UndefVarError` when a @Int field was used as a size parameter for a subsequent field:

```julia
data_size::@Int(2)           # Was skipped (wrong!)
data::@Blob(data_size)       # ERROR: data_size not defined!
```

The fix ensures @Int fields are always required since they typically serve as size/length parameters.

## Testing

### Unit Test Results

Created `test_compute_size_improvement.jl` to verify the improvement:

```julia
@pseudostruct TestImprovement begin
    name_len::UInt16
    name::@FixedLengthString(name_len)
    data_size::UInt32
    data::@Blob(data_size)
end

# Generated compute_size:
function compute_size(::Val{TestImprovement}, hflags, hsize, kw)
    offset = 0
    haskey(kw, :name_len) || throw(ArgumentError("Argument :name_len is required"))
    name_len = kw.name_len
    offset += sizeof(UInt16)
    offset += name_len  # Uses name_len, not name content
    haskey(kw, :data_size) || throw(ArgumentError("Argument :data_size is required"))
    data_size = kw.data_size
    offset += sizeof(UInt32)
    offset += data_size  # Uses data_size, not data content
    return offset
end
```

✅ **Success**: `name` and `data` fields are NOT required!

### Integration Testing

Basic functionality test passes:
```julia
julia> using JLD2
julia> f = jldopen("/tmp/test.jld2", "w")
julia> f["x"] = [1,2,3]
julia> close(f)
julia> f2 = jldopen("/tmp/test.jld2", "r")
julia> f2["x"]
3-element Vector{Int64}:
 1
 2
 3
```

## Benefits

1. **Simpler API**: Users don't need to provide unnecessary field values
2. **Better Performance**: Less data copying and validation
3. **Clearer Intent**: Code explicitly shows which fields matter for size computation
4. **Backward Compatible**: Providing extra fields still works (they're just ignored)

## Limitations

### Conservative Approach

The implementation takes a conservative approach and only optimizes the most common and safe cases (variable-length content fields). Other potential optimizations (like fixed-size conditional fields) are not implemented to avoid complexity and maintain correctness.

### Fields Used in Conditions

Fields that appear in conditions (e.g., `layout_class`, `flags`) must still be provided even if their values don't directly affect the size calculation. This is because:

1. It's difficult to statically analyze which fields are used in conditions during macro expansion
2. The macro processes fields linearly and doesn't have a global view of all conditions
3. A two-pass approach would significantly complicate the implementation

**Example where field is still required:**
```julia
@pseudostruct DataLayout begin
    layout_class::LayoutClass  # Must be provided!
    if layout_class == LcCompact
        data_size::UInt16
        data::@Blob(data_size)
    end
end

# Must provide layout_class even though it's a fixed-size field:
jlsizeof(Val(DataLayout); layout_class=LcCompact, data_size=100)
```

### Future Improvement Opportunities

If more aggressive optimization is desired:

1. **Two-Pass Analysis**:
   - First pass: Collect all fields and their usage in conditions
   - Second pass: Generate optimized `compute_size`
   - Trade-off: Significant complexity increase

2. **@size_hint Annotation**:
   ```julia
   layout_class::LayoutClass @size_hint(required)  # Explicit annotation
   ```

3. **Separate Size Spec**:
   - Define a separate DSL for size computation
   - Trade-off: Duplication between struct definition and size spec

## Recommendation

The current implementation provides significant value for the most common use case (variable-length content fields) with minimal complexity. Further optimization should only be pursued if profiling shows `compute_size` calls are a bottleneck and the additional complexity is justified.

## Files Modified

- `src/macros_utils.jl`: Core implementation
  - Lines 65-88: Added `increment_depends_on_field()` function
  - Lines 132-148: Added variable-length content detection
  - Lines 156-169: Updated `compute_size` generation logic

## Files Created

- `analyze_compute_size.jl`: Analysis tool
- `test_compute_size_improvement.jl`: Unit test
- `COMPUTE_SIZE_IMPROVEMENT.md`: This document