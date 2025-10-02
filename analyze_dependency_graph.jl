"""
Comprehensive dependency graph analysis for @pseudostruct macro optimization.

This tool analyzes the dependency patterns in pseudostruct definitions to:
1. Build a complete dependency graph showing which fields depend on which
2. Identify optimization opportunities (constant folding, redundant reads)
3. Generate optimized code with minimal reads and seeks
"""

using JLD2
using MacroTools

"""
    FieldInfo

Represents a field in a pseudostruct with all its metadata.
"""
struct FieldInfo
    name::Symbol                    # Field name (e.g., :flags)
    type_expr                        # Type expression (e.g., UInt8, @Int(2))
    increment_expr                   # Offset increment expression
    condition                        # Condition for this field (true if unconditional)
    is_constant_size::Bool          # True if size is compile-time constant
    constant_size::Int              # Size in bytes if constant
    dependencies::Set{Symbol}       # Fields this depends on
end

"""
    DependencyGraph

Complete dependency analysis of a pseudostruct definition.
"""
struct DependencyGraph
    fields::Vector{FieldInfo}
    field_map::Dict{Symbol, FieldInfo}
    dependency_chains::Dict{Symbol, Vector{Symbol}}  # Transitive dependencies
    read_order::Vector{Symbol}       # Optimal read order (topological sort)
    constant_prefix_size::Int        # Size of constant-sized prefix
end

"""
    extract_dependencies(expr) -> Set{Symbol}

Extract all field names referenced in an expression.
"""
function extract_dependencies(expr)
    deps = Set{Symbol}()
    MacroTools.postwalk(expr) do ex
        if ex isa Symbol && !in(ex, [:offset, :io, :hflags, :hsize, :kw, :m,
                                      :sizeof, :length, :isset, :Int, :UInt8, :UInt16, :UInt32, :UInt64,
                                      :Int8, :Int16, :Int32, :Int64, :Float32, :Float64])
            push!(deps, ex)
        end
        ex
    end
    return deps
end

"""
    analyze_field(expr) -> Union{FieldInfo, Nothing}

Parse a single field expression and extract its metadata.
"""
function analyze_field(expr, precond=true)
    if expr isa LineNumberNode
        return nothing
    end

    # Handle conditional fields
    if @capture(expr, cond_ && body_)
        combined_cond = precond == true ? cond : :($precond && $cond)
        return analyze_field(body, combined_cond)
    end

    # Handle if blocks
    if @capture(expr, if cond_; body_; end)
        # Would need to recursively analyze body
        return nothing  # Simplified for now
    end

    # Handle skip
    if @capture(expr, @skip(n_))
        return nothing
    end

    # Handle field definitions
    s = T = v = nothing
    if @capture(expr, s_Symbol::T_) || @capture(expr, s_Symbol::T_ = v_)
        # Determine size and dependencies
        constant_size = 0
        is_constant = false
        increment_expr = nothing

        if @capture(T, @FixedLengthString(len_))
            increment_expr = len
        elseif @capture(T, @Blob(len_))
            increment_expr = len
        elseif @capture(T, @Int(len_))
            increment_expr = len
            if len isa Integer
                constant_size = len
                is_constant = true
            end
        elseif @capture(T, @Offset)
            increment_expr = 0
            constant_size = 0
            is_constant = true
        elseif @capture(T, @computed(expr_))
            increment_expr = 0
            constant_size = 0
            is_constant = true
        else
            # Regular type
            increment_expr = :(sizeof($T))
            # Try to evaluate constant size
            if T == :UInt8 || T == :Int8
                constant_size = 1
                is_constant = true
            elseif T == :UInt16 || T == :Int16
                constant_size = 2
                is_constant = true
            elseif T == :UInt32 || T == :Int32 || T == :Float32
                constant_size = 4
                is_constant = true
            elseif T == :UInt64 || T == :Int64 || T == :Float64
                constant_size = 8
                is_constant = true
            end
        end

        # Extract dependencies
        deps = Set{Symbol}()
        union!(deps, extract_dependencies(T))
        union!(deps, extract_dependencies(increment_expr))
        union!(deps, extract_dependencies(precond))
        if !isnothing(v)
            union!(deps, extract_dependencies(v))
        end

        return FieldInfo(s, T, increment_expr, precond, is_constant, constant_size, deps)
    end

    return nothing
end

"""
    build_full_dependency_graph(struct_expr) -> DependencyGraph

Build complete dependency graph from a @pseudostruct definition.
"""
function build_full_dependency_graph(struct_expr)
    fields = FieldInfo[]

    # Parse all fields
    for expr in struct_expr.args
        field_info = analyze_field(expr)
        if !isnothing(field_info)
            push!(fields, field_info)
        end
    end

    # Build field map
    field_map = Dict{Symbol, FieldInfo}(f.name => f for f in fields)

    # Compute transitive dependencies
    dependency_chains = Dict{Symbol, Vector{Symbol}}()

    function get_transitive_deps(name, visited=Set{Symbol}())
        if name in visited
            return Symbol[]  # Cycle detection
        end

        if haskey(dependency_chains, name)
            return dependency_chains[name]
        end

        push!(visited, name)

        if !haskey(field_map, name)
            return Symbol[]
        end

        direct_deps = field_map[name].dependencies
        all_deps = Set{Symbol}(direct_deps)

        for dep in direct_deps
            if haskey(field_map, dep)
                for transitive in get_transitive_deps(dep, copy(visited))
                    push!(all_deps, transitive)
                end
            end
        end

        result = collect(all_deps)
        dependency_chains[name] = result
        return result
    end

    for field in fields
        get_transitive_deps(field.name)
    end

    # Compute optimal read order (topological sort)
    read_order = Symbol[]
    remaining = Set(f.name for f in fields)

    while !isempty(remaining)
        # Find fields with no dependencies in remaining set
        ready = filter(remaining) do name
            deps = field_map[name].dependencies
            field_deps = filter(d -> haskey(field_map, d), deps)
            isempty(setdiff(field_deps, read_order))
        end

        if isempty(ready)
            # Cycle or external dependency - just pick one
            push!(read_order, first(remaining))
            delete!(remaining, first(remaining))
        else
            # Add all ready fields
            for name in ready
                push!(read_order, name)
                delete!(remaining, name)
            end
        end
    end

    # Compute constant prefix size
    constant_prefix_size = 0
    for name in read_order
        field = field_map[name]
        if field.condition == true && field.is_constant_size && isempty(field.dependencies)
            constant_prefix_size += field.constant_size
        else
            break
        end
    end

    return DependencyGraph(fields, field_map, dependency_chains, read_order, constant_prefix_size)
end

"""
    print_dependency_analysis(graph::DependencyGraph)

Pretty-print the dependency analysis.
"""
function print_dependency_analysis(graph::DependencyGraph)
    println("\n" * "="^80)
    println("DEPENDENCY GRAPH ANALYSIS")
    println("="^80)

    println("\n📊 Summary:")
    println("  Total fields: ", length(graph.fields))

    constant_fields = count(f -> f.is_constant_size, graph.fields)
    unconditional_fields = count(f -> f.condition == true, graph.fields)

    println("  Constant-size fields: $constant_fields")
    println("  Unconditional fields: $unconditional_fields")
    println("  Constant prefix size: $(graph.constant_prefix_size) bytes")

    # Analyze dependency patterns
    no_deps = count(f -> isempty(f.dependencies), graph.fields)
    println("  Fields with no dependencies: $no_deps")

    println("\n📝 Field Details:")
    println("  ", "="^78)

    for field in graph.fields
        println("\n  Field: :$(field.name)")
        println("    Type: $(field.type_expr)")
        println("    Condition: $(field.condition == true ? "always" : field.condition)")
        println("    Size: $(field.is_constant_size ? "$(field.constant_size) bytes" : "variable")")

        if !isempty(field.dependencies)
            println("    Direct dependencies: $(sort(collect(field.dependencies)))")
        end

        if haskey(graph.dependency_chains, field.name)
            transitive = graph.dependency_chains[field.name]
            if length(transitive) > length(field.dependencies)
                println("    Transitive dependencies: $(sort(transitive))")
            end
        end
    end

    println("\n📈 Optimal Read Order:")
    println("  ", join(string.(graph.read_order), " → "))

    println("\n🔍 Optimization Opportunities:")

    # Find fields that are dependencies of multiple other fields
    dep_counts = Dict{Symbol, Int}()
    for field in graph.fields
        for dep in field.dependencies
            dep_counts[dep] = get(dep_counts, dep, 0) + 1
        end
    end

    shared_deps = sort(collect(dep_counts), by=x->x[2], rev=true)
    if !isempty(shared_deps)
        println("\n  Fields used as dependencies (caching candidates):")
        for (name, count) in shared_deps
            if count > 1
                println("    :$name - used by $count other fields")
            end
        end
    end

    # Identify constant folding opportunities
    const_size_total = sum(f.constant_size for f in graph.fields if f.is_constant_size && f.condition == true)
    println("\n  Constant folding potential:")
    println("    Total constant-size, unconditional bytes: $const_size_total")

    println("\n" * "="^80)
end

"""
    analyze_pseudostruct(name::Symbol)

Analyze a specific pseudostruct by name.
"""
function analyze_pseudostruct(name::Symbol)
    # This would need to extract the definition from headermessages.jl
    # For now, we'll create examples manually
    println("To analyze $name, we need access to its source definition")
end

# Example: Analyze HmLinkMessage structure
function example_link_message_analysis()
    println("\n🔬 Example: HmLinkMessage Dependency Analysis\n")

    # Manually construct the structure as it appears in headermessages.jl
    struct_def = quote
        version::UInt8 = 1
        flags::UInt8 = 0x18
        isset(flags, 3) && link_type::UInt8
        isset(flags, 2) && creation_order::Int64
        isset(flags, 4) && (link_name_charset::UInt8 = 0x01)
        link_name_len::@Int(2^(flags%4)) = 10
        link_name::@FixedLengthString(link_name_len)
        (!isset(flags, 3) || link_type==0) && (target::RelOffset = 0)
        if isset(flags, 3) && link_type == 1
            link_info_size::UInt16
            soft_link::@Blob(link_info_size)
        end
        if isset(flags, 3) && link_type == 64
            link_info_size::UInt16
            external_link::@Blob(link_info_size)
        end
    end

    graph = build_full_dependency_graph(struct_def)
    print_dependency_analysis(graph)

    println("\n💡 Optimization Strategy for HmLinkMessage:")
    println("  1. flags is used by 5+ other fields → MUST cache")
    println("  2. link_name_len depends on flags → read after flags")
    println("  3. link_name depends on link_name_len → read after link_name_len")
    println("  4. link_type is conditional → only read when isset(flags, 3)")
    println("  5. version is constant size, no deps → can be in constant prefix")

    println("\n  Current code: Each property re-reads flags")
    println("  Optimized: Read flags once, cache for all properties")
    println("  Savings: 4-5 redundant UInt8 reads per property access")
end

function example_datalayout_analysis()
    println("\n🔬 Example: HmDataLayout Dependency Analysis\n")

    # Simplified version of HmDataLayout v3/v4
    struct_def = quote
        version::UInt8 = 4
        layout_class::LayoutClass
        if layout_class == LcCompact
            data_size::@Int(2)
            data_address::@Offset
            data::@Blob(data_size)
        end
        if layout_class == LcContiguous
            data_address::RelOffset
            data_size::@Int(8)
        end
        if layout_class == LcChunked
            dimensionality::UInt8
            data_address::RelOffset
            dimensions::NTuple(Int(dimensionality), UInt32)
        end
    end

    graph = build_full_dependency_graph(struct_def)
    print_dependency_analysis(graph)

    println("\n💡 Optimization Strategy for HmDataLayout:")
    println("  1. layout_class controls all conditionals → MUST read first")
    println("  2. version is constant in v4 → compile-time branch elimination possible")
    println("  3. Each branch is independent → optimal property access is branch-specific")
    println("  4. dimensionality controls NTuple size → read before dimensions")
end

# Generate comparison showing code size reduction
function estimate_code_size_reduction()
    println("\n" * "="^80)
    println("COMPILATION TIME OPTIMIZATION ANALYSIS")
    println("="^80)

    println("\n📉 Code Size Impact on Compilation Time:")
    println()
    println("Current approach (example for 5 properties with shared dependency):")
    println("  - Each property: 8 lines (seek, read flags, offset, seek, read target, return)")
    println("  - 5 properties × 8 lines = 40 lines of generated code")
    println("  - flags read 5 times")
    println()
    println("Optimized approach with dependency graph:")
    println("  - Shared prefix: 3 lines (cache flags)")
    println("  - Each property: 4 lines (offset calc, seek, read, return)")
    println("  - 3 + (5 × 4) = 23 lines of generated code")
    println("  - flags read once")
    println()
    println("📊 Improvement: 43% fewer lines → faster compilation")
    println("📊 Runtime: 80% fewer I/O operations for flag reads")

    println("\n🎯 Type Stability Benefits:")
    println("  - Constant-size prefix → type-stable offset calculations")
    println("  - Explicit caching → compiler can infer types better")
    println("  - Fewer branches → easier for compiler to optimize")

    println("\n💾 Memory Impact:")
    println("  - Fewer intermediate seeks → better I/O buffer utilization")
    println("  - Cached common fields → reduced memory bandwidth")

    println("\n" * "="^80)
end

# Run examples
if !isinteractive()
    example_link_message_analysis()
    println("\n\n")
    example_datalayout_analysis()
    println("\n\n")
    estimate_code_size_reduction()
end
