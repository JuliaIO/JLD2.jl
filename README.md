# JLD2

[![](https://img.shields.io/badge/docs-online-blue.svg)](https://JuliaIO.github.io/JLD2.jl/dev)
[![CI](https://github.com/JuliaIO/JLD2.jl/actions/workflows/ci.yml/badge.svg)](https://github.com/JuliaIO/JLD2.jl/actions/workflows/ci.yml)
[![codecov.io](https://codecov.io/github/JuliaIO/JLD2.jl/coverage.svg?branch=master)](https://codecov.io/github/JuliaIO/JLD2.jl?branch=master)
[![JLD2 Downloads](https://img.shields.io/badge/dynamic/json?url=http%3A%2F%2Fjuliapkgstats.com%2Fapi%2Fv1%2Fmonthly_downloads%2FJLD2&query=total_requests&suffix=%2Fmonth&label=Downloads)](http://juliapkgstats.com/pkg/JLD2)
[![Aqua QA](https://raw.githubusercontent.com/JuliaTesting/Aqua.jl/master/badge.svg)](https://github.com/JuliaTesting/Aqua.jl)


JLD2 is a package for the [julia programming language](https://julialang.org/) for saving and loading data.
Highlights include:

- Simple API for basic usage: `jldsave(filename; data)` and `load(filename, "data")`
- JLD2 can serialize complex nested structures out of the box.
- JLD2 files adhere to the HDF5 format specification making it compatible with HDF5 tooling
and H5 libraries in other languages. (Can also read HDF5 files.)
- It is fast. JLD2 uses the julia compiler to generate efficient code for serializing complex structures.
- Users may provide custom serialization procedures to control how data gets stored.
- JLD2 provides *upgrade* mechanisms for data structures that need post-processing on load (for example when the julia types have changed) 

For details on usage see the [documentation](https://JuliaIO.github.io/JLD2.jl/dev).