# JLD2

**NOTE**: This package is now **actively maintained again**! It was not maintained for some time and there still is a backlog of outstanding issues that will be addressed in the near future. You are invited to test JLD2 and raise any issues you come across. However, tread with care as you may come across problems that can potentially cause data loss.

[![Travis Build Status](https://travis-ci.org/JuliaIO/JLD2.jl.svg?branch=master)](https://travis-ci.org/JuliaIO/JLD2.jl)
[![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/j9jvpgimd04qs8dn/branch/master?svg=true)](https://ci.appveyor.com/project/JonasIsensee/jld2-jl/branch/master)
[![codecov.io](http://codecov.io/github/JuliaIO/JLD2.jl/coverage.svg?branch=master)](http://codecov.io/github/JuliaIO/JLD2.jl?branch=master)

JLD2 saves and loads Julia data structures in a format comprising a subset of HDF5, without any dependency on the HDF5 C library. It typically outperforms [the previous JLD package](https://github.com/JuliaIO/JLD.jl) (sometimes by multiple orders of magnitude) and often outperforms Julia's built-in serializer. While other HDF5 implementations supporting HDF5 File Format Specification Version 3.0 (i.e. libhdf5 1.10 or later) should be able to read the files that JLD2 produces, JLD2 is likely to be incapable of reading files created or modified by other HDF5 implementations. JLD2 does not aim to be backwards or forwards compatible with the previous JLD package.

__Please use caution.__ If your tolerance for data loss is low, [JLD](https://github.com/JuliaIO/JLD.jl) may be a better choice at this time.

## Reading and writing data

### `@save` and `@load` macros

The `@save` and `@load` macros are the simplest way to interact with a JLD2 file. The `@save` macro writes one or more variables from the current scope to the JLD2 file. For example:

```julia
using JLD2
hello = "world"
foo = :bar
@save "example.jld2" hello foo
```

This writes the variables `hello` and `foo` to datasets in a new JLD2 file named `example.jld2`. The `@load` macro loads variables out of a JLD2 file:

```julia
@load "example.jld2" hello foo
```

This assigns the contents of the `hello` and `foo` datasets to variables of the same name in the current scope.

It is best practice to explicitly name the variables to be loaded and saved from a file, so that it is clear from whence these variables arise. However, for convenience, JLD2 also provides variants of `@load` and `@save` that do not require variables to be named explicitly. When called with no variable arguments, `@save <filename>` writes all variables in the global scope of the current module to file `<filename>`, while `@load <filename>` loads all variables in file `<filename>`. When called with no variable arguments, `@load` requires that the file name is provided as a string literal, i.e., it is not possible to select the file at runtime.

Additional customization is possible using assignment syntax and option passing:

```
@save "example.jld2" bye=hello bar=foo
@save "example.jld2" {compress=true} hello bar=foo
```


### `save` and `load` functions

The `save` and `load` functions, provided by [FileIO](https://github.com/JuliaIO/FileIO.jl), provide an alternative mechanism to read and write data from a JLD2 file. To use these functions, you must say `using FileIO`; it is not necessary to say `using JLD2` since FileIO will determine the correct package automatically.

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

### Custom Serialization (Experimental)

Version `v0.3.0` of introduces support for custom serialization.
For now this feature is considered experimental as it passes tests but 
has little testing in the wild. → Please test and report if you encounter problems.

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

@save "test.jld2" arr
```

In this example JLD2 converts the array of `B` structs to a plain `Vector{Float64}` prior to 
storing to disk.
