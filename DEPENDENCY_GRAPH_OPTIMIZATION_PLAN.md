# Dependency Graph-Based @pseudostruct Optimization Plan

## Executive Summary

This document outlines a comprehensive optimization strategy for the `@pseudostruct` macro based on full dependency graph analysis. The optimization reduces compilation time by generating shorter, more type-stable code while eliminating redundant I/O operations.

## Performance Targets

- **Code size reduction**: 30-43% fewer generated lines
- **Compilation time**: ~40-60% faster compilation
- **Runtime I/O**: 60-80% fewer redundant reads
- **Type stability**: Explicit type assertions for better inference

## Core Problem Analysis

### Current Implementation Issues

1. **Redundant Field Reads**: Each property access re-reads dependency fields
   - Example: `flags` field read 5+ times when accessing different properties
   - Each read includes: seek, jlread, offset increment

2. **Verbose Code Generation**: Large generated functions slow compilation
   - 8-10 lines per property accessor
   - Many redundant offset calculations
   - Repetitive seek operations

3. **Limited Constant Folding**: Constant offsets recalculated at runtime
   - Fixed-size fields like `version::UInt8` have compile-time known positions
   - Currently computed dynamically

4. **Weak Type Stability**: Compiler must track types through many lines
   - Type information flows through long code paths
   - Makes optimization harder for LLVM

### Example: HmLinkMessage Analysis

**Structure**:
```julia
@pseudostruct HmLinkMessage begin
    version::UInt8 = 1
    flags::UInt8 = 0x18
    isset(flags, 3) && link_type::UInt8
    isset(flags, 2) && creation_order::Int64
    isset(flags, 4) && link_name_charset::UInt8
    link_name_len::@Int(2^(flags%4))
    link_name::@FixedLengthString(link_name_len)
    # ... more fields ...
end
```

**Dependency Analysis**:
- `flags` is used by: link_type, creation_order, link_name_charset, link_name_len (4 fields)
- `link_name_len` is used by: link_name (1 field)
- Total: 5 property accessors would read `flags` redundantly

**Current Code Generation** (per property):
```julia
if s == :link_type
    offset = base_addr
    offset += sizeof(UInt8)           # Skip version
    seek(io, offset)
    flags = jlread(io, UInt8)         # READ FLAGS AGAIN
    offset += sizeof(UInt8)
    seek(io, offset)
    if isset(flags, 3)
        link_type = jlread(io, UInt8)
        return link_type
    end
end
# Similar blocks for other properties...
```

**Issues**:
- 8 lines per property × 5 properties = 40 lines
- `flags` read 5 times (4 redundant)
- Offset calculations repeated

## Optimization Strategy

### 1. Full Dependency Graph Construction

Build complete dependency graph at macro expansion time:

```julia
struct DependencyInfo
    field_name::Symbol
    dependencies::Set{Symbol}       # Direct dependencies
    transitive_deps::Set{Symbol}    # All transitive dependencies
    is_constant_size::Bool          # Size known at compile time
    constant_size::Int              # Size in bytes (if constant)
    usage_count::Int                # How many fields depend on this
end

function build_dependency_graph(fields_list) -> Dict{Symbol, DependencyInfo}
    # 1. Extract dependencies from increments and conditions
    # 2. Compute transitive closure
    # 3. Count usage of each field
    # 4. Identify constant-size fields
    # 5. Return complete graph
end
```

### 2. Identify Caching Candidates

Fields should be cached if:
1. Used as dependency by 2+ other fields
2. Has conditional dependent fields
3. Used in multiple property accessors

**For HmLinkMessage**:
- `flags`: used by 4+ fields → **MUST CACHE**
- `link_name_len`: used by 1 field → optional (but small cost)

### 3. Constant Folding at Code Generation

Compute offsets for constant-size prefix at macro expansion:

```julia
function compute_constant_prefix(fields_list) -> Vector{(Symbol, Int)}
    offset = 0
    prefix = []

    for (name, type, ...) in fields_list
        if !is_conditional && is_constant_size(type)
            push!(prefix, (name, offset))
            offset += sizeof(type)
        else
            break  # Prefix ends at first variable/conditional field
        end
    end

    return prefix
end
```

**For HmLinkMessage**:
- `version`: offset 0, size 1 byte
- `flags`: offset 1, size 1 byte
- Constant prefix: 2 bytes

### 4. Optimized Code Generation Pattern

```julia
function generate_optimized_getprop(fields, dep_graph)
    # Identify cache fields
    cache_fields = [f for f in fields if dep_graph[f].usage_count >= 2]

    # Generate cache variables
    cache_vars = Dict(f => Symbol("cached_", f) for f in cache_fields)

    # Generate property accessors
    for field in fields
        deps = dep_graph[field.name].dependencies
        cache_deps = filter(d -> d in cache_fields, deps)

        # Generate optimized accessor
        quote
            if s == $(QuoteNode(field.name))
                offset = base_addr + $(constant_offset(field))

                # Load cached dependencies
                $((quote
                    if !isassigned($(cache_vars[dep]))
                        seek(io, $(dep_offset))
                        $(cache_vars[dep])[] = $(read_dep_expr)::$(dep_type)
                    end
                    $(dep) = $(cache_vars[dep])[]
                end for dep in cache_deps)...)

                # Read target field
                seek(io, offset)
                $(read_field_expr)
                return $(field.name)
            end
        end
    end
end
```

### 5. Type Stability Improvements

Add explicit type assertions:

```julia
# Current:
flags = jlread(io, UInt8)

# Optimized:
flags = jlread(io, UInt8)::UInt8

# Cached:
cached_flags[] = jlread(io, UInt8)::UInt8
flags = cached_flags[]::UInt8  # Guaranteed type
```

## Implementation Plan

### Phase 1: Dependency Graph Infrastructure (2-3 hours)

**File**: `src/macros_utils.jl`

1. **Add dependency graph builder** (after line 461):
```julia
"""
    build_complete_dependency_graph(fields_list) -> Dict{Symbol, DependencyInfo}

Build complete dependency graph with transitive closure and usage counts.
"""
function build_complete_dependency_graph(fields_list)
    # Implementation here
end
```

2. **Add caching candidate identification**:
```julia
"""
    identify_cache_candidates(dep_graph) -> Set{Symbol}

Identify fields that should be cached based on usage patterns.
"""
function identify_cache_candidates(dep_graph)
    Set(name for (name, info) in dep_graph if info.usage_count >= 2)
end
```

3. **Add constant folding utilities**:
```julia
"""
    compute_constant_offsets(fields_list) -> Dict{Symbol, Int}

Compute compile-time known offsets for constant-size fields.
"""
function compute_constant_offsets(fields_list)
    # Implementation
end
```

### Phase 2: Optimized Code Generator (3-4 hours)

**File**: `src/macros_utils.jl`

1. **Replace `assemble_getprop`** (lines 483-574):

```julia
function assemble_getprop_optimized(fields_list)
    # Build dependency graph
    dep_graph = build_complete_dependency_graph(fields_list)
    cache_fields = identify_cache_candidates(dep_graph)
    const_offsets = compute_constant_offsets(fields_list)

    # Generate cache variable declarations
    cache_decls = [:($(Symbol("cached_", f)) = Ref{$(field_type(f))}())
                   for f in cache_fields]

    # Generate property accessors
    property_cases = []
    for (s, T, increment, io_assign, cond) in fields_list
        isnothing(s) && continue

        # Get dependencies for this field
        deps = dep_graph[s].dependencies
        cache_deps = filter(d -> d in cache_fields, deps)

        # Generate accessor with caching
        accessor = generate_cached_accessor(s, T, increment, io_assign, cond,
                                           cache_deps, const_offsets)
        push!(property_cases, accessor)
    end

    return quote
        base_addr = getfield(m, :address)
        $(cache_decls...)
        $(property_cases...)
    end
end
```

2. **Add cache accessor generator**:
```julia
function generate_cached_accessor(field_name, field_type, increment, io_assign,
                                  condition, cache_deps, const_offsets)
    # Generate optimized accessor with:
    # - Constant offset folding
    # - Cached dependency loading
    # - Type assertions
    # - Minimal seeks
end
```

### Phase 3: Integration and Testing (2-3 hours)

1. **Update `generate_getprop`** to use new system:
```julia
function generate_getprop(exprs, fields=[], precond=true)
    # ... existing parsing logic ...

    # NEW: Use optimized assembler
    return assemble_getprop_optimized(fields)
end
```

2. **Add feature flag** for gradual rollout:
```julia
const USE_OPTIMIZED_CODEGEN = Ref(true)

function generate_getprop(exprs, fields=[], precond=true)
    # ... parse fields ...

    if USE_OPTIMIZED_CODEGEN[]
        return assemble_getprop_optimized(fields)
    else
        return assemble_getprop(fields)  # Old implementation
    end
end
```

3. **Testing strategy**:
```julia
# Test with optimization OFF
JLD2.USE_OPTIMIZED_CODEGEN[] = false
@testset "Legacy codegen" begin
    # Run all JLD2 tests
end

# Test with optimization ON
JLD2.USE_OPTIMIZED_CODEGEN[] = true
@testset "Optimized codegen" begin
    # Run all JLD2 tests
end

# Benchmark compilation time
@time @eval module TestCompilation
    using JLD2
    # Force recompilation
end
```

## Expected Impact

### Code Size Reduction

| Structure | Current Lines | Optimized Lines | Reduction |
|-----------|--------------|-----------------|-----------|
| HmLinkMessage | ~48 | ~33 | 31% |
| HmDataLayout | ~56 | ~35 | 38% |
| HmFillValue | ~40 | ~25 | 38% |
| **Average** | - | - | **~35%** |

### Runtime Performance

**I/O Operations** (HmLinkMessage example):
- Current: 24 reads (with redundancy)
- Optimized: 9 reads (with caching)
- **Reduction: 62.5%**

**Compilation Time**:
- Fewer lines → ~40-60% faster type inference
- Better type stability → ~20-30% faster LLVM optimization
- **Total: ~50% compilation time reduction expected**

### Memory Impact

- Cache variables: ~1-3 Ref cells per structure (8-24 bytes)
- Reduced I/O buffer thrashing: Better cache locality
- **Net: Negligible memory increase, better performance**

## Risk Mitigation

### Backward Compatibility

- **Binary format**: No changes (same bytes written)
- **API**: No changes (same property access semantics)
- **Feature flag**: Can disable optimization if issues arise

### Testing Strategy

1. **Unit tests**: Test dependency graph construction
2. **Integration tests**: Full JLD2 test suite
3. **Performance tests**: Compilation time benchmarks
4. **Correctness tests**: Verify identical behavior

### Rollback Plan

If issues arise:
1. Set `USE_OPTIMIZED_CODEGEN[] = false`
2. All existing code continues to work
3. No code changes needed

## Implementation Timeline

| Phase | Duration | Deliverables |
|-------|----------|--------------|
| Phase 1: Infrastructure | 2-3 hours | Dependency graph utilities |
| Phase 2: Code Generator | 3-4 hours | Optimized assemble_getprop |
| Phase 3: Integration | 2-3 hours | Tests, benchmarks, validation |
| **Total** | **7-10 hours** | Fully optimized macro system |

## Success Metrics

1. ✅ All existing tests pass with optimization enabled
2. ✅ Compilation time reduced by ≥40%
3. ✅ Code size reduced by ≥30%
4. ✅ No runtime performance regression
5. ✅ Zero memory leaks or correctness issues

## Future Enhancements

Once base optimization is stable:

1. **Per-structure optimization tuning**
   - Some structures benefit more from caching than others
   - Could use profiling data to tune thresholds

2. **Lazy evaluation for complex conditionals**
   - Defer offset calculations for rarely-accessed properties
   - Generate property clusters based on access patterns

3. **SIMD-friendly layouts**
   - Pack small fields for vector operations
   - Align fields for better memory access

4. **Compilation cache**
   - Cache compiled property accessors across sessions
   - Share compiled code between similar structures

## References

- Current implementation: `src/macros_utils.jl` lines 259-574
- Analysis tools: `analyze_dependency_graph.jl`, `optimized_codegen_prototype.jl`
- Documentation: `DEVELOPMENT_INSIGHTS.md`, `PSEUDOSTRUCT_IMPROVEMENTS_SUMMARY.md`
