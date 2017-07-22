# JLD2

[![Travis Build Status](https://travis-ci.org/simonster/JLD2.jl.svg?branch=master)](https://travis-ci.org/simonster/JLD2.jl)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/9wk39naux5dhwhen?svg=true)](https://ci.appveyor.com/project/simonster/jld2-jl)
[![codecov.io](http://codecov.io/github/simonster/JLD2.jl/coverage.svg?branch=master)](http://codecov.io/github/simonster/JLD2.jl?branch=master)

JLD2 saves and loads Julia data structures in a format comprising a subset of HDF5, without any dependency on the HDF5 C library. It typically outperforms [the previous JLD package](https://github.com/JuliaIO/JLD.jl) (sometimes by multiple orders of magnitude) and often outperforms Julia's built-in serializer. While other HDF5 implementations supporting HDF5 File Format Specification Version 3.0 (i.e. libhdf5 1.10 or later) should be able to read the files that JLD2 produces, JLD2 is likely to be incapable of reading files created or modified by other HDF5 implementations. JLD2 does not aim to be backwards or forwards compatible with the previous JLD package.

The code here should work on Julia 0.6. It has extensive unit tests, but it has received little testing in the wild. __Please use caution.__ If your tolerance for data loss is low, [JLD](https://github.com/JuliaIO/JLD.jl) may be a better choice at this time.

## Reading and writing data

### `@save` and `@load` macros

The `@save` and `@load` macros are the simplest way to interact with a JLD2 file. The `@save` macro writes one or more variables from the current scope to the JLD file. For example:

```julia
using JLD2, FileIO
hello = "world"
foo = :bar
@save "example.jld2" hello foo
```

This writes the variables `hello` and `foo` to datasets in a new JLD2 file named `example.jld2`. The `@load` macro loads variables out of a JLD2 file:

```
@load "example.jld2" hello foo
```

This assigns the contents of the `hello` and `foo` datasets to variables of the same name in the current scope.

It is best practice to explicitly name the variables to be loaded and saved from a file, so that it is clear from whence these variables arise. However, for convenience, JLD2 also provides variants of `@load` and `@save` that do not require variables to be named explicitly. When called with no variable arguments, `@save <filename>` writes all variables in the global scope of the current module to file `<filename>`, while `@load <filename>` loads all variables in file `<filename>`. When called with no variable arguments, `@load` requires that the file name is provided as a string literal, i.e., it is not possible to select the file at runtime.

### `save` and `load` functions

The `save` and `load` functions, provided by [FileIO](https://github.com/JuliaIO/FileIO.jl), provide an alternative mechanism to read and write data from a JLD2 file. To use these functions, you must say `using FileIO`; it is not necessary to say `using JLD2` since FileIO will determine the correct package automatically.

The `save` function accepts an `Associative` yielding the key/value pairs, where the key is a string representing the name of the dataset and the value represents its contents:

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
load("example.jld2", "hello", "foo") # -> ("hello", :bar)
```

### File interface

It is also possible to interact with JLD2 files using a file-like interface. The `jldopen` function accepts a file name and an argument specifying how the file should be opened:

```julia
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
