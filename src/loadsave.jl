function jldopen(f::Function, args...; kws...)
    jld = jldopen(args...; kws...)
    try
        return f(jld)
    finally
        close(jld)
    end
end

function _extractmode(xs)
  mode = "w"
  vars = sizehint!(Vector{Symbol}(), length(xs))
  for x ∈ xs
      if x isa Expr && x.head == :(=) && length(x.args) == 2 && x.args[] == :mode
          mode = x.args[2]
          @assert(mode ∈ ("r+", "w", "w+", "a", "a+"), "unsupport mode: $mode")
      elseif x isa Symbol
          push!(vars)
      else
          throw(ArgumentError("unsupport expression: `$x`"))
      end
  end
  mode, vars
end

"""
    @save "/path/file.jld2" [mode = "w"] x...

Save one or more variables into `file.jld2`.
The argument `mode` is optional, please check `?jldopen` for available modes.
"""
macro save(filename, vars...)
    mode, vars = _extractmode(vars)
    if isempty(vars)
        # Save all variables in the current module
        quote
            let
                m = $(VERSION >= v"0.7.0-DEV.484" ? __module__ : current_module())
                f = jldopen($(esc(filename)), $mode)
                wsession = JLDWriteSession()
                try
                    for vname in names(m, true)
                        s = string(vname)
                        if !ismatch(r"^_+[0-9]*$", s) # skip IJulia history vars
                            v = getfield(m, vname)
                            if !isa(v, Module)
                                try
                                    write(f, s, v, wsession)
                                catch e
                                    if isa(e, PointerException)
                                        warn("skipping $vname because it contains a pointer")
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
    else
        writeexprs = Vector{Expr}()
        for x ∈ vars
            push!(writeexprs, :(write(f, $(string(x)), $(esc(x)), wsession)))
        end

        quote
            jldopen($(esc(filename)), $mode) do f
                wsession = JLDWriteSession()
                $(Expr(:block, writeexprs...))
            end
        end
    end
end

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

# Save all the key-value pairs in the dict as top-level variables of the JLD
function save(f::File{format"JLD2"}, dict::Associative; kwargs...)
    jldopen(FileIO.filename(f), "w"; kwargs...) do file
        wsession = JLDWriteSession()
        for (k,v) in dict
            if !isa(k, AbstractString)
                throw(ArgumentError("keys must be strings (the names of variables), got $k"))
            end
            write(file, String(k), v, wsession)
        end
    end
end

# Or the names and values may be specified as alternating pairs
function save(f::File{format"JLD2"}, name::AbstractString, value, pairs...; kwargs...)
    if isodd(length(pairs)) || !isa(pairs[1:2:end], Tuple{Vararg{AbstractString}})
        throw(ArgumentError("arguments must be in name-value pairs"))
    end
    jldopen(FileIO.filename(f), "w"; kwargs...) do file
        wsession = JLDWriteSession()
        write(file, String(name), value, wsession)
        for i = 1:2:length(pairs)
            write(file, String(pairs[i]), pairs[i+1], wsession)
        end
    end
end

save(f::File{format"JLD2"}, value...; kwargs...) = error("must supply a name for each variable")

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

# load with just a filename returns a dictionary containing all the variables
function load(f::File{format"JLD2"}; kwargs...)
    jldopen(FileIO.filename(f), "r"; kwargs...) do file
        loadtodict!(Dict{String,Any}(), file)
    end
end

# When called with explicitly requested variable names, return each one
function load(f::File{format"JLD2"}, varname::AbstractString; kwargs...)
    jldopen(FileIO.filename(f), "r"; kwargs...) do file
        read(file, varname)
    end
end

load(f::File{format"JLD2"}, varnames::AbstractString...; kwargs...) =
    load(f, varnames; kwargs...)

function load(f::File{format"JLD2"}, varnames::Tuple{Vararg{AbstractString}}; kwargs...)
    jldopen(FileIO.filename(f), "r"; kwargs...) do file
        map((var)->read(file, var), varnames)
    end
end
