# Advanced Usage

## Explicit Type Remapping

Sometimes you store data using `struct`s that you defined yourself or are
shipped with some package and weeks later, when you want to 
load the data, the structs have changed.

```julia
using JLD2
struct A
    x::Int
end
jldsave("example.jld2"; a = A(42))
```

This results in warnings and sometimes even errors when trying to load the
file as demonstrated here.
```julia
julia> using JLD2
julia> struct A{T}
            x::T
       end
julia> load("example.jld2")
┌ Warning: read type A is not a leaf type in workspace; reconstructing
└ @ JLD2 ~/.julia/dev/JLD2/src/data/reconstructing_datatypes.jl:273
Dict{String, Any} with 1 entry:
  "a" => var"##A#257"(42)
```

As of JLD2 version `v0.4.21` there is a fix. The `JLDFile` struct contains a `typemap` dictionary that allows for explicit type remapping. 
Now you can define a struct
that matches the old definition and load your data.

```julia
julia> struct A_old
            x::Int
        end
julia> f = jldopen("example.jld2","r"; typemap=Dict("Main.A" => A_old))
JLDFile /home/jonas/.julia/dev/JLD2/example.jld2 (read-only)
 └─🔢 a
julia> f["a"]
A_old(42)
```

## Upgrading old structures on load
The section above explains how you can make JLD2 load old structs with a different DataType name as target.
A different method for loading old data is described here:

```julia
# This is the old version of the struct stored in the file
struct OldStructVersion
    x::Int
    y::Float64
end
orig = OldStructVersion(1,2.0)
jldsave("test.jld2"; data=orig)

### new session

# This is the new version of your struct
struct UpdatedStruct
    x::Float64 # no longer int
    y::Float64
    z::Float64 # = x*y
end

# When upgrading a struct, JLD2 will load the fields of the old struct into a `NamedTuple`
# and call `rconvert` on it. Here we implement a conversion method that returns an `UpdatedStruct`
JLD2.rconvert(::Type{UpdatedStruct}, nt::NamedTuple) = UpdatedStruct(Float64(nt.x), nt.y, nt.x*nt.y)

# Here we provide the `typemap` keyword argument. It is a dictionary mapping the stored struct name
# to an `Upgrade` instance with the new struct.
load("test.jld2", "data"; typemap=Dict("Main.OldStructVersion" => JLD2.Upgrade(UpdatedStruct)))
```

## Groups - Appending to files


Group objects can be constructed with two optional keyword arguments:
```julia
g = Group(file;
          est_num_entries=4
          est_link_name_len=8)
```

These determine how much (additional) empty space should be allocated for the group description. (list of entries)
This can be useful for performance when one expects to append many additional datasets after first writing the file.

## JLD2DebugTools

There is an experimental repository [JLD2DebugTools.jl](https://github.com/JonasIsensee/JLD2DebugTools.jl) that may help with debugging files.


## Fallback Behaviour
By default JLD2 will attempt to open files using the `MmapIO` backend. If that fails, it retries using `IOStream`.