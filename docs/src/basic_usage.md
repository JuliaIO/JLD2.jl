## Reading and writing data
JLD2 provides a few different options to save and load data:

 - [FileIO interface](@ref)
 - [Single object storage](@ref)
 - [File handles](@ref)
 - [UnPack Extension](@ref)

### FileIO interface
The `save` and `load` functions, provided by [FileIO](https://github.com/JuliaIO/FileIO.jl), are one of the simplest ways to use `JLD2`.
The `save` function accepts an `AbstractDict` yielding the key/value pairs, where the key is a string representing the name of the dataset and the value represents its contents:

```@repl
save("example.jld2", Dict("hello" => "world", "foo" => :bar))
```

The `save` function can also accept the dataset names and contents as arguments:

```@repl
save("example.jld2", "hello", "world", "foo", :bar)
```

For `save` and `load` to automatically detect that you want to save a JLD2 file use the file suffix `".jld2"`.

If called with a filename argument only, the `load` function loads all datasets from the given file into a `Dict`:

```@repl
load("example.jld2")
```

When called with a single dataset name, `load` returns the contents of that dataset from the file:

```@repl
load("example.jld2", "hello")
```

When called with multiple dataset names, `load` returns the contents of the given datasets as a tuple:

```@repl
load("example.jld2", "foo", "hello")
```

### jldsave

`jldsave` makes use of julia's keyword argument syntax to store files.
This can be useful, when your data variables already have the correct name, e.g. use
`jldsave(file; variablename)` instead of `save(file, "variablename", variablename)

```@docs
jldsave
```

### Single object storage
If only a single object needs to stored and loaded from a file, one can use
`save_object` and `load_object` functions.

```@docs
save_object
load_object
```

### File handles

It is also possible to interact with JLD2 files using a file-like interface. The `jldopen` function accepts a file name and an argument specifying how the file should be opened:

```@docs
jldopen
```

Data can be written to the file using `write(f, "name", data)` or `f["name"] = data`, or read from the file using `read(f, "name")` or `f["name"]`. When you are done with the file, remember to call `close(f)`.

Like `open`, `jldopen` also accepts a function as the first argument, permitting `do`-block syntax:

```@example
jldopen("example.jld2", "w") do f
    write(f, "variant1", 1.0)
    f["variant2"] = (rand(5), rand(Bool, 3))
end

f = jldopen("example.jld2")
v1 = read(f, "variant1")
v2 = f["variant2"]
close(f)
v1, v2
```

#### Groups
JLD2 files allow for nesting datasets into groups which may be useful for organizing your data.
You may construct groups explicitly:
```@example
jldopen("example.jld2", "w") do file
    mygroup = JLD2.Group(file, "mygroup")
    mygroup["mystuff"] = 42
    display(file)
end
```

or implicitly, by saving a variable with a name containing slashes as path delimiters:
```@example
save("example.jld2", "mygroup/mystuff", 42)
```

Similarly, you can access groups directly:

```@example
jldopen("example.jld2") do file
    file["mygroup"]["mystuff"]
end
```

or using slashes as path delimiters:

```@example
load("example.jld2", "mygroup/mystuff")
```

When loading files with nested groups these will be unrolled into paths by default but
yield nested dictionaries but with the `nested` keyword argument.
```@repl
load("example.jld2")
load("example.jld2"; nested=true)
```

### UnPack Extension

When additionally loading the [UnPack.jl](https://github.com/mauro3/UnPack.jl) package, its `@unpack` and `@pack!` macros can be used to quickly save and load data from the file-like interface. Example:

```@example
using UnPack
file = jldopen("example.jld2", "w")
x, y = rand(2)

@pack! file = x, y # equivalent to file["x"] = x; file["y"] = y
@unpack x, y = file # equivalent to x = file["x"]; y = file["y"]
close(file)
```