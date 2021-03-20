function jldopen(f::Function, args...; kws...)
    jld = jldopen(args...; kws...)
    try
        return f(jld)
    finally
        close(jld)
    end
end

"""
    @save filename var1 [var2 ...]
    @save filename {compress=true} var1 name2=var2

Write one or more variables `var1,...` from the current scope to a JLD2 file
`filename`.

For interactive use you can save all variables in the current module's global
scope using `@save filename`. More permanent code should prefer the explicit
form to avoid saving unwanted variables.

# Example

To save the string `hello` and array `xs` to the JLD2 file example.jld2:

    hello = "world"
    xs = [1,2,3]
    @save "example.jld2" hello xs

For passing options to the saving command use {}

    @save "example.jld2" {compress=true} hello xs

For saving variables under a different name use regular assignment syntax

    @save "example.jld2" greeting=hello xarray = xs
"""
macro save(filename, vars...)
    fields = []
    options = []
    for var in vars
        # Capture options of the form {compress = true, mmaparrays=false}
        if @capture(var, {opts__})
            for opt in opts
                if @capture(opt, key_ = val_)
                    push!(options, :($key = $(esc(val))))
                else
                    return :(throw(ArgumentError("Invalid option syntax")))
                end
            end
        # Allow assignment syntax a = b
        elseif @capture(var, a_ = b_)
            push!(fields, :(write(f, $(string(a)), $(esc(b)), wsession)))
        # Allow single arg syntax a   → "a" = a
        elseif @capture(var, a_Symbol)
            push!(fields, :(write(f, $(string(a)), $(esc(a)), wsession)))
        else
            return :(throw(ArgumentError("Invalid field syntax")))
        end
    end
    if !isempty(fields)
        return quote
            let
                f = jldopen($(esc(filename)), "w"; $(Expr(:tuple,options...))...)
                wsession = JLDWriteSession()
                try
                    $(Expr(:block, fields...))
                catch e
                    rethrow(e)
                finally
                    close(f)
                end
            end
        end
    else
        # The next part is old code that handles saving the whole workspace
        quote
            let
                m = $(__module__)
                f = jldopen($(esc(filename)), "w"; $(Expr(:tuple,options...))...)
                wsession = JLDWriteSession()
                try
                    for vname in names(m; all=true)
                        s = string(vname)
                        if !occursin(r"^_+[0-9]*$", s) # skip IJulia history vars
                            v = getfield(m, vname)
                            if !isa(v, Module)
                                try
                                    write(f, s, v, wsession)
                                catch e
                                    if isa(e, PointerException)
                                        @warn("skipping $vname because it contains a pointer")
                                    else
                                        rethrow(e)
                                    end
                                end
                            end
                        end
                    end
                finally
                    close(f)
                end
            end
        end
    end
end

"""
    @load filename var1 [var2 ...]

Load one or more variables `var1,...` from JLD2 file `filename` into the
current scope and return a vector of the loaded variable names.

For interactive use, the form `@load "somefile.jld2"` will load all variables
from `"somefile.jld2"` into the current scope. This form only supports literal
file names and should be avoided in more permanent code so that it's clear
where the variables come from.

# Example

To load the variables `hello` and `foo` from the file example.jld2, use

    @load "example.jld2" hello foo
"""
macro load(filename, vars...)
    if isempty(vars)
        if isa(filename, Expr)
            throw(ArgumentError("filename argument must be a string literal unless variable names are specified"))
        end
        # Load all variables in the top level of the file
        readexprs = Expr[]
        vars = Symbol[]
        f = jldopen(filename)
        try
            for n in keys(f)
                if !isgroup(f, lookup_offset(f.root_group, n))
                    push!(vars, Symbol(n))
                end
            end
        finally
            close(f)
        end
    end
    return quote
        ($([esc(x) for x in vars]...),) = jldopen($(esc(filename))) do f
            ($([:(read(f, $(string(x)))) for x in vars]...),)
        end
        $(Symbol[v for v in vars]) # convert to Array
    end
end

function loadtodict!(d::Dict, g::Union{JLDFile,Group}, prefix::String="")
    for k in keys(g)
        v = g[k]
        if v isa Group
            loadtodict!(d, v, prefix*k*"/")
        else
            d[prefix*k] = v
        end
    end
    return d
end

"""
    save_object(filename, x)

Write one object `x` from the current scope to a JLD2 file `filename`. The
object is stored with a default name of "single_stored_object". If default name
needs to be overridden use [`@save`](@ref) instead. If the file exists, it
overwrites it.

# Example

To save the string `hello` to the JLD2 file example.jld2:

    hello = "world"
    save_object("example.jld2", hello)
"""
function save_object(filename, x)
  jldopen(filename, "w") do file
    file["single_stored_object"] = x
  end
  return
end

"""
    load_object(filename)

Load the only available object from the JLD2 file `filename`. Provides
functionality similar to `loadRDS`. If `filename` contains more than one object,
the function throws an `ArgumentError`. The object name is not loaded into namespace. For
loading more than one object or objects directly into namespace, use
[`@load`](@ref).

# Example

To load the only object from the JLD2 file example.jld2:

    hello = "world"
    save_object("example.jld2", hello)
    hello_loaded = load_object("example.jl2")
"""
function load_object(filename)
  jldopen(filename, "r") do file
    all_keys = keys(file)
    length(all_keys) == 0 && throw(ArgumentError("File $filename does not contain any object"))
    length(all_keys) > 1 && throw(ArgumentError("File $filename contains more than one object. Use `load` or `@load` instead"))
    file[all_keys[1]] #Uses HDF5 functionality of treating the file like a dict
  end
end
