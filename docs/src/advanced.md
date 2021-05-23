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

As of JLD2 version `v0.4.8` there is a fix. The `JLDFile` struct contains a `typemap` dictionary that allows for explicit type remapping. 
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