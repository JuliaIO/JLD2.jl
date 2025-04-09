JLD2 is a package for the [julia programming language](https://julialang.org/) for saving and loading data.
Highlights include:

- Simple API for basic usage: `jldsave(filename; data)` and `load(filename, "data")`
- JLD2 can serialize complex nested structures out of the box.
- JLD2 files adhere to the HDF5 format specification making it compatible with HDF5 tooling
and H5 libraries in other languages. (Can also read HDF5 files.)
- It is fast. JLD2 uses the julia compiler to generate efficient code for serializing complex structures.
- Users may provide custom serialization procedures to control how data gets stored.
- JLD2 provides *upgrade* mechanisms for data structures that need post-processing on load (for example when the julia types have changed) 


## Overview

- Basic Usage
- Advanced Usage
- HDF5 compatibility
- Gotchas


## Gotchas

### Objects are cached during loading
JLD2 caches objects during loading. It may give you the same object twice.
This can lead to surprising results if you edit loaded arrays. Note, the underlying file is not being edited!
```julia-repl
julia> jldsave("demo.jld2", a=zeros(2))

julia> f = jldopen("demo.jld2")
JLDFile /home/isensee/demo.jld2 (read-only)
 └─🔢 a

julia> a = f["a"] # bind loaded array to name `a`
2-element Vector{Float64}:
 0.0
 0.0

julia> a[1] = 42; # editing the underlying array

julia> f["a"]
2-element Vector{Float64}:
 42.0
  0.0

julia> a = nothing # remove all references to the loaded array

julia> GC.gc(true) # call GC to remove the cache

julia> f["a"] # a new copy is loaded from the file
2-element Vector{Float64}:
 0.0
 0.0
```

### Cross-compatibility
JLD2 tries to write files in a way that allows you to load them on different operating systems and in particular both on 32bit and 64bit systems.
However, many julia structs may be inherently different on different architectures making this task impossible.
In particular, moving data from a 64bit system to a 32bit system is only guaranteed to work for basic datatypes.

### Security
Beware of opening JLD2 files from untrusted sources. A malicious file may execute code on your computer. See e.g. this project's [issue #117](https://github.com/JuliaIO/JLD2.jl/issues/117). To check a file, you can use [JLD2DebugTools.jl](https://github.com/JonasIsensee/JLD2DebugTools.jl) to view what kinds of objects are stored.
