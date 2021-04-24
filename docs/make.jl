using Documenter
using JLD2

makedocs(;
    modules = [JLD2],
    sitename = "Julia Data Format",
    build = "build",
    clean = true,
    doctest = true,
    #root="./",
    #src="src",
    repo = "https://github.com/JuliaIO/JLD2.jl",
    pages = [
        "Basics" => "index.md",
	"Custom Serialization" => "customserialization.md",
    "Compression" => "compression.md",
	"Internals & Design" => "internals.md",
	"HDF5 Compatibility" => "hdf5compat.md",
    "Legacy" => "legacy.md",
    	],
    )

deploydocs(
    repo = "github.com/JuliaIO/JLD2.jl.git",
    target = "build",
    push_preview = true,
)
