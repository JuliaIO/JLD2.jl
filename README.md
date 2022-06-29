# JLD2

| **Documentation**   |  **Tests**     | **CodeCov**  |
|:--------:|:---------------:|:-------:|
|[![](https://img.shields.io/badge/docs-online-blue.svg)](https://JuliaIO.github.io/JLD2.jl/dev)| [![CI](https://github.com/juliaio/JLD2.jl/workflows/CI/badge.svg?branch=master)](https://github.com/JuliaIO/JLD2.jl/actions) | [![codecov.io](https://codecov.io/github/JuliaIO/JLD2.jl/coverage.svg?branch=master)](https://codecov.io/github/JuliaIO/JLD2.jl?branch=master) |

JLD2 saves and loads Julia data structures in a format comprising a subset of HDF5, without any dependency on the HDF5 C library. It typically outperforms [the JLD package](https://github.com/JuliaIO/JLD.jl) (sometimes by multiple orders of magnitude) and often outperforms Julia's built-in serializer. While other HDF5 implementations supporting HDF5 File Format Specification Version 3.0 (i.e. libhdf5 1.10 or later) should be able to read the files that JLD2 produces, JLD2 is likely to be incapable of reading files created or modified by other HDF5 implementations. JLD2 does not aim to be backwards or forwards compatible with the JLD package.

## Reading and writing data
### `save` and `load` functions

The `save` and `load` functions, provided by [FileIO](https://github.com/JuliaIO/FileIO.jl), provide a mechanism to read and write data from a JLD2 file. To use these functions, you may either write `using FileIO` or `using JLD2`. FileIO will determine the correct package automatically.

The `save` function accepts an `AbstractDict` yielding the key/value pairs, where the key is a string representing the name of the dataset and the value represents its contents:

```julia
using FileIO
save("example.jld2", Dict("hello" => "world", "foo" => :bar))
```

The `save` function can also accept the dataset names and contents as arguments:

```julia
save("example.jld2", "hello", "world", "foo", :bar)
```

When using the `save` function, the file extension must be `.jld2`, since the extension `.jld` currently belongs to the previous JLD package.

If called with a filename argument only, the `load` function loads all datasets from the given file into a Dict:

```julia
load("example.jld2") # -> Dict{String,Any}("hello" => "world", "foo" => :bar)
```

If called with a single dataset name, `load` returns the contents of that dataset from the file:

```julia
load("example.jld2", "hello") # -> "world"
```

If called with multiple dataset names, `load` returns the contents of the given datasets as a tuple:

```julia
load("example.jld2", "hello", "foo") # -> ("world", :bar)
```

### A new interface: jldsave

`jldsave` makes use of julia's keyword argument syntax to store files,
thus leveraging the parser and not having to rely on macros. The new interface can be imported with `using JLD2`. To use it, write

```julia
using JLD2

x = 1
y = 2
z = 42

# The simplest case:
jldsave("example.jld2"; x, y, z)
# it is equivalent to 
jldsave("example.jld2"; x=x, y=y, z=z)

# You can assign new names selectively
jldsave("example.jld2"; x, a=y, z)

# and if you want to confuse your future self and everyone else, do
jldsave("example.jld2"; z=x, x=y, y=z)
```

In the above examples, `;` after the filename is important. Compression and non-default IO types may be set via positional arguments.

### File interface

It is also possible to interact with JLD2 files using a file-like interface. The `jldopen` function accepts a file name and an argument specifying how the file should be opened:

```julia
using JLD2

f = jldopen("example.jld2", "r")  # open read-only (default)
f = jldopen("example.jld2", "r+") # open read/write, failing if no file exists
f = jldopen("example.jld2", "w")  # open read/write, overwriting existing file
f = jldopen("example.jld2", "a+") # open read/write, preserving contents of existing file or creating a new file
```

Data can be written to the file using `write(f, "name", data)` or `f["name"] = data`, or read from the file using `read(f, "name")` or `f["name"]`. When you are done with the file, remember to call `close(f)`.

Like `open`, `jldopen` also accepts a function as the first argument, permitting `do`-block syntax:

```julia
jldopen("example.jld2", "w") do file
    file["bigdata"] = randn(5)
end
```

### Groups

It is possible to construct groups within a JLD2 file, which may or may not be useful for organizing your data. You can create groups explicitly:

```julia
jldopen("example.jld2", "w") do file
    mygroup = JLD2.Group(file, "mygroup")
    mygroup["mystuff"] = 42
end
```

or implicitly, by saving a variable with a name containing slashes as path delimiters:

```julia
jldopen("example.jld2", "w") do file
    file["mygroup/mystuff"] = 42
end
# or save("example.jld2", "mygroup/mystuff", 42)
```

Both of these examples yield the same group structure, which you can see at the REPL:

```
julia> file = jldopen("example.jld2", "r")
JLDFile /Users/simon/example.jld2 (read-only)
 └─📂 mygroup
    └─🔢 mystuff
```

Similarly, you can access groups directly:

```julia
jldopen("example.jld2", "r") do file
    @assert file["mygroup"]["mystuff"] == 42
end
```

or using slashes as path delimiters:

```julia
@assert load("example.jld2", "mygroup/mystuff") == 42
```

### Custom Serialization

The API is simple enough, to enable custom serialization for your type `A` you define
a new type e.g. `ASerialization` that contains the fields you want to store and define
`JLD2.writeas(::Type{A}) = ASerialization`.
Internally JLD2 will call `Base.convert` when writing and loading, so you need to make sure to extend that for your type.

```julia
struct A
    x::Int
end

struct ASerialization
    x::Vector{Int}
end

JLD2.writeas(::Type{A}) = ASerialization
Base.convert(::Type{ASerialization}, a::A) = ASerialization([a.x])
Base.convert(::Type{A}, a::ASerialization) = A(only(a.x))
```

If you do not want to overload `Base.convert` then you can also define

```julia
JLD2.wconvert(::Type{ASerialization}, a::A) = ASerialization([a.x])
JLD2.rconvert(::Type{A}, a::ASerialization) = A(only(a.x))
```

instead. This may be particularly relevant when types are involved that are not your own.

```julia
struct B
    x::Float64
end

JLD2.writeas(::Type{B}) = Float64
JLD2.wconvert(::Type{Float64}, b::B) = b.x
JLD2.rconvert(::Type{B}, x::Float64) = B(x)

arr = [B(rand()) for i=1:10]

jldsave("test.jld2"; arr)
```

In this example JLD2 converts the array of `B` structs to a plain `Vector{Float64}` prior to 
storing to disk.
