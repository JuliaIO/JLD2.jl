# Julia Data Format - JLD2

JLD2 saves and loads Julia data structures in a format comprising a subset of HDF5, without any dependency on the HDF5 C library. 
JLD2 is able to read most HDF5 files created by other HDF5 implementations supporting HDF5 File Format Specification Version 3.0 (i.e. libhdf5 1.10 or later) and similarly those should be able to read the files that JLD2 produces. JLD2 provides read-only support for files created with the JLD package.

## Reading and writing data

### A new interface: jldsave

`jldsave` makes use of julia's keyword argument syntax to store files,
thus leveraging the parser and not having to rely on macros. To use it, write

```julia
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

Compression and non-default IO types may be set via positional arguments.
### `save_object` and `load_object` functions

If only a single object needs to stored and loaded from a file, one can use
`save_object` and `load_object` functions.

```@docs
save_object
load_object
```

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

## File interface

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

## Groups

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

```julia-repl
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

When loading files with nested groups these will be unrolled into paths by default but
yield nested dictionaries but with the `nested` keyword argument.
```julia
load("example.jld2") # -> Dict("mygroup/mystuff" => 42)
load("example.jld2"; nested=true) # -> Dict("mygroup" => Dict("mystuff" => 42))
```

### UnPack.jl API

When additionally loading the [UnPack.jl](https://github.com/mauro3/UnPack.jl) package, its `@unpack` and `@pack!` macros can be used to quickly save and load data from the file-like interface. Example:

```julia
using UnPack
file = jldopen("example.jld2", "w")
x, y = rand(2)

@pack! file = x, y # equivalent to file["x"] = x; file["y"] = y
@unpack x, y = file # equivalent to x = file["x"]; y = file["y"]
```

The group `file_group = Group(file, "mygroup")` can be accessed with the same file-like interface as the "full" struct.

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
Beware of opening JLD2 files from untrusted sources. A malicious file may execute code on your computer. See e.g. [here](https://github.com/JuliaIO/JLD2.jl/issues/117). To check a file, you can use [JLD2DebugTools.jl](https://github.com/JonasIsensee/JLD2DebugTools.jl) to view what kinds of objects are stored.