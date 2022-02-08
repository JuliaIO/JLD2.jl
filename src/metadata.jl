
function write_env(f::JLDFile)
    project = read(Base.active_project(), String)
    manifest = read(
        joinpath(dirname(Base.active_project()), "Manifest.toml"),
            String)

    f["_metadata/project"] = project
    f["_metadata/manifest"] = manifest
    return nothing
end

function has_env(f::JLDFile)
    haskey(f, "_metadata/project")
end

function read_env(f::JLDFile)
    project = f["_metadata/project"]
    manifest = f["_metadata/manifest"]
    return (Pkg.TOML.parse(project), Pkg.TOML.parse(manifest))
end

function Pkg.activate(f::JLDFile, path=mktempdir(); kwargs...)
    mkpath(path)
    write(joinpath(path, "Project.toml"), f["_metadata/project"])
    write(joinpath(path, "Manifest.toml"), f["_metadata/manifest"])
    Pkg.activate(path; kwargs...)
end


