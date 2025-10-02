# Advanced @pseudostruct code generation with field grouping
# This is an experimental implementation that groups fields sharing dependencies

"""
Analyze which fields each field depends on by examining important_names.
Returns a Dict mapping field_symbol => Set of dependency field symbols
"""
function analyze_dependencies(fields_list, field_to_value)
    dependencies = Dict{Symbol, Set{Symbol}}()

    for (s, T, increment, io_assign, cond) in fields_list
        isnothing(s) && continue

        deps = Set{Symbol}()

        # Helper to collect symbol references
        function collect_refs(ex)
            if ex isa Symbol && haskey(field_to_value, ex)
                push!(deps, ex)
            elseif ex isa Expr
                for arg in ex.args
                    collect_refs(arg)
                end
            end
        end

        collect_refs(increment)
        collect_refs(cond)
        collect_refs(T)

        dependencies[s] = deps
    end

    return dependencies
end

"""
Group fields that share the same dependency set.
Returns Vector of (dep_set, field_list) pairs.
"""
function group_fields_by_dependencies(fields_list, dependencies)
    # Create canonical dependency signature for each field
    dep_signatures = Dict{Symbol, Set{Symbol}}()

    for (s, _, _, _, _) in fields_list
        isnothing(s) && continue

        # Build transitive closure of dependencies
        deps = Set{Symbol}()
        to_process = collect(get(dependencies, s, Set{Symbol}()))

        while !isempty(to_process)
            d = pop!(to_process)
            if d ∉ deps
                push!(deps, d)
                for dd in get(dependencies, d, Set{Symbol}())
                    push!(to_process, dd)
                end
            end
        end

        dep_signatures[s] = deps
    end

    # Group fields with identical dependency signatures
    groups = Dict{Set{Symbol}, Vector{Symbol}}()
    field_order = Symbol[]

    for (s, _, _, _, _) in fields_list
        isnothing(s) && continue
        push!(field_order, s)

        sig = dep_signatures[s]
        if !haskey(groups, sig)
            groups[sig] = Symbol[]
        end
        push!(groups[sig], s)
    end

    # Return groups in field definition order
    result = Tuple{Set{Symbol}, Vector{Symbol}}[]
    seen_sigs = Set{Set{Symbol}}()

    for field_sym in field_order
        sig = dep_signatures[field_sym]
        if sig ∉ seen_sigs
            push!(seen_sigs, sig)
            push!(result, (sig, groups[sig]))
        end
    end

    return result, dep_signatures
end

"""
Advanced version of assemble_getprop that groups fields by dependencies.
"""
function assemble_getprop_advanced(fields_list)
    expr = Expr(:block)
    prep_statements = []
    important_names = Symbol[]

    # Work with bare symbols during AST construction
    s_sym = :s
    m_sym = :m
    io_sym = :io
    offset_sym = :offset

    # Build a mapping from field symbols to their value variable names
    field_to_value = Dict{Symbol, Symbol}()

    # Add special parameter/local mappings
    field_to_value[:hflags] = :hflags
    field_to_value[:hsize] = :hsize
    field_to_value[:io] = io_sym
    field_to_value[:m] = m_sym
    field_to_value[:offset] = offset_sym

    for (s, T, increment, io_assign, cond) in fields_list
        if !isnothing(s) && !isnothing(io_assign) && io_assign.head == :(=)
            value_var = io_assign.args[1]
            field_to_value[s] = value_var
        end
    end

    # Helper function to replace field references with value variables
    function replace_field_refs(ex, in_lhs=false)
        if ex isa Symbol
            if !in_lhs && haskey(field_to_value, ex)
                return field_to_value[ex]
            end
            return ex
        elseif ex isa Expr
            if ex.head == :(=) || ex.head == :(+=) || ex.head == :(-=) || ex.head == :(*=)
                return Expr(ex.head,
                    replace_field_refs(ex.args[1], true),
                    replace_field_refs(ex.args[2], false),
                    [replace_field_refs(arg, false) for arg in ex.args[3:end]]...)
            else
                return Expr(ex.head, [replace_field_refs(arg, in_lhs) for arg in ex.args]...)
            end
        else
            return ex
        end
    end

    # Analyze dependencies and create groups
    dependencies = analyze_dependencies(fields_list, field_to_value)
    groups, dep_signatures = group_fields_by_dependencies(fields_list, dependencies)

    # Build lookup from field symbol to its full info
    field_info = Dict{Symbol, Tuple}()
    for entry in fields_list
        s = entry[1]
        !isnothing(s) && (field_info[s] = entry)
    end

    # Generate grouped accessors
    for (dep_set, field_group) in groups
        # Skip single-field groups - use original logic
        if length(field_group) == 1
            s = field_group[1]
            (_, T, increment, io_assign, cond) = field_info[s]

            # Apply transformations
            increment = replace_field_refs(increment)
            cond = replace_field_refs(cond)
            io_assign = !isnothing(io_assign) ? replace_field_refs(io_assign) : nothing

            # Build prep statements (original logic)
            # ... (keeping original implementation for single fields)
            continue
        end

        # Multi-field group: generate shared prep section
        # Collect all dependencies needed by this group
        shared_deps = sort(collect(dep_set))

        # Build condition for group: s == :field1 || s == :field2 || ...
        group_condition = Expr(:call, :(||), [:($$s_sym == $(QuoteNode(f))) for f in field_group]...)

        # Generate shared prep that reads all dependencies
        group_block = Expr(:block)
        push!(group_block.args, :($offset_sym = getfield($m_sym, :address)))

        # Add reads for all shared dependencies
        for dep in shared_deps
            if haskey(field_info, dep)
                (_, _, dep_increment, dep_io_assign, dep_cond) = field_info[dep]
                dep_increment = replace_field_refs(dep_increment)
                dep_io_assign = !isnothing(dep_io_assign) ? replace_field_refs(dep_io_assign) : nothing

                if !isnothing(dep_io_assign)
                    push!(group_block.args, dep_io_assign)
                    push!(group_block.args, dep_increment)
                end
            end
        end

        # Now generate conditional logic for each field in the group
        for field_sym in field_group
            (_, T, increment, io_assign, cond) = field_info[field_sym]

            increment = replace_field_refs(increment)
            cond = replace_field_refs(cond)
            io_assign = !isnothing(io_assign) ? replace_field_refs(io_assign) : nothing

            return_var = if !isnothing(io_assign) && io_assign.head == :(=)
                io_assign.args[1]
            else
                field_sym
            end

            # Early return for dependency fields themselves
            push!(group_block.args, :(
                $$s_sym == $(QuoteNode(field_sym)) && return $return_var
            ))
        end

        # Now handle the actual field logic (not just returning dependencies)
        for field_sym in field_group
            if field_sym ∉ dep_set  # Not a pure dependency field
                (_, T, increment, io_assign, cond) = field_info[field_sym]

                increment = replace_field_refs(increment)
                cond = replace_field_refs(cond)
                io_assign = !isnothing(io_assign) ? replace_field_refs(io_assign) : nothing

                return_var = if !isnothing(io_assign) && io_assign.head == :(=)
                    io_assign.args[1]
                else
                    field_sym
                end

                condexp = [io_assign, :(return $return_var)]
                cond != true && (condexp = [:(if $(cond); $(condexp...) end)])

                push!(group_block.args, :(
                    if $$s_sym == $(QuoteNode(field_sym))
                        $(condexp...)
                    end
                ))
            end
        end

        push!(expr.args, :(if $group_condition; $group_block end))
    end

    return esc(expr)
end