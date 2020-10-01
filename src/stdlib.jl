# This code is based on https://github.com/JuliaLang/Pkg.jl/blob/53b3580707309d6170651cfc79ddb17b8d4ff8f9/src/Types.jl

using Pkg
using UUIDs

const STDLIB = Ref{Dict{UUID, String}}()
const STDLIB_LOCK = ReentrantLock()

function projectfile_path(env_path::String; strict=false)
    for name in Base.project_names
        maybe_file = joinpath(env_path, name)
        isfile(maybe_file) && return maybe_file
    end
    return strict ? nothing : joinpath(env_path, "Project.toml")
end

stdlib_dir() = normpath(joinpath(Sys.BINDIR, "..", "share", "julia", "stdlib", "v$(VERSION.major).$(VERSION.minor)"))

stdlib_path(stdlib::String) = joinpath(stdlib_dir(), stdlib)

function load_stdlib()
    stdlib = Dict{UUID,String}()
    for name in readdir(stdlib_dir())
        projfile = projectfile_path(stdlib_path(name); strict=true)
        nothing === projfile && continue
        project = Pkg.TOML.parsefile(projfile)
        uuid = get(project, "uuid", nothing)
        nothing === uuid && continue
        stdlib[UUID(uuid)] = name
    end
    return stdlib
end

function stdlib()
    lock(STDLIB_LOCK) do
        if !isassigned(STDLIB)
            STDLIB[] = load_stdlib()
        end
        stdlib = deepcopy(STDLIB[])
    end
end

function stdlibmodules(m::Module)::Vector{Module}
    result = Vector{Module}(undef, 0)
    for x in values(stdlib())
        s = Symbol(x)
        if isdefined(m, s)
            m_dot_s = getproperty(m, s)
            if m_dot_s isa Module
                push!(result, m_dot_s)
            end
        end
    end
    return result
end
