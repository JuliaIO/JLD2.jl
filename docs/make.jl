using Documenter
using JLD2
using Documenter.Remotes: GitHub

makedocs(;
    modules = [JLD2],
    sitename = "Julia Data Format",
    build = "build",
    clean = true,
    doctest = true,
    checkdocs = :exports,
    #root = "./",
    #src = "src",
    repo = GitHub("JuliaIO/JLD2.jl"),
    pages = [
        "Home" => "index.md",
        "Basic Usage" => "basic_usage.md",
        "Custom Serialization" => "customserialization.md",
        "Compression" => "compression.md",
        "Internals & Design" => "internals.md",
        "HDF5 Compatibility" => "hdf5compat.md",
        "Advanced Usage" => "advanced.md",
        "Dataset Links" => "external_links.md"
        "Legacy" => "legacy.md",
        "Troubleshooting" => "troubleshooting.md"
    ],
    warnonly = [:cross_references],
)

deploydocs(
    repo = "github.com/JuliaIO/JLD2.jl.git",
    target = "build",
    push_preview = true,
)
