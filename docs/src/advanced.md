```@meta
    ShareDefaultModule=false
```

# Advanced Usage

## Loading plain types

```@repl
using JLD2 #hide
jldsave("test.jld2"; z= 1.0 + im * 2.0)
load("test.jld2", "z")
load("test.jld2", "z"; plain=true)
@__MODULE__
```


## Explicit Type Remapping

Sometimes you store data using `struct`s that you defined yourself or are
shipped with some package and weeks later, when you want to
load the data, the structs have changed.

```@example
using JLD2 #hide
struct A
    x::Int
end
jldsave("example.jld2"; a = A(42))
```

This results in warnings and sometimes even errors when trying to load the
file as demonstrated here.
```@repl
using JLD2 #hide
struct A{T}
    x::T
end
load("example.jld2")
```

The `JLDFile` struct contains a `typemap` field that allows for explicit type remapping.
You can define a struct that matches the old definition and load your data.

```@repl
using JLD2 #hide
struct A_old
    x::Int
end
f = jldopen("example.jld2","r"; typemap=Dict("Main.A" => A_old))
f["a"]
close(f)
```

## Upgrading old structures on load
The section above explains how you can make JLD2 load old structs with a different Datatype name as target.
A different method for loading old data is described here:

```@example
using JLD2 #hide
# This is the old version of the struct stored in the file
struct OldStructVersion
    x::Int
    y::Float64
end
orig = OldStructVersion(1,2.0)
jldsave("test.jld2"; data=orig)
```
```@example
using JLD2 #hide
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

## Full control over type reconstruction
The recommended and more powerful option is to take full control over type mapping by
providing a custom mapping function that gets full access to all stored information
including the type parameters.
Example like above:

```julia
struct OldStruct{T}
    x::T
end

old_int = OldStruct(42)
old_float = OldStruct(3.14)
jldsave("test.jld2"; old_int, old_float, inttype=OldStruct{Int}, floattype=OldStruct{Float64}, )

struct NormalStruct{T}
    x::T
end

struct SquaredStruct{T}
    xsquared::T
end

JLD2.rconvert(::Type{SquaredStruct{T}}, nt) where T = SquaredStruct{T}(nt.x^2)

typemap = function(f::JLD2.JLDFile, typepath::String, params::Vector)
    if typepath == "Main.OldStruct"
        if params[1] == Int
            @info "Mapping an OldStruct{Int} to SquaredStruct{Int} with conversion"
            # If the type param is Int, map to squared struct
            # and wrap in `Upgrade` to trigger custom conversion with `rconvert`
            return JLD2.Upgrade(SquaredStruct{Int})
        else
            @info "Mapping an OldStruct{T} to NormalStruct{T} without conversion"
            # All other OldStructs just get updated to NormalStruct
            return NormalStruct{params...}
        end
    end
    # This typemap functino is called for every single type that is decoded.
    # All types that do not need special handling should be forwarded to the default
    # implementation.
    @info "Forwarding $typepath with parameters $params to default type mapping"
    return JLD2.default_typemap(f, typepath, params)
end

load("test.jld2"; typemap)
```

```
[ Info: Forwarding Core.Int64 with parameters Any[] to default type mapping
[ Info: Mapping an OldStruct{Int} to SquaredStruct{Int} with conversion
[ Info: Forwarding Core.Float64 with parameters Any[] to default type mapping
[ Info: Mapping an OldStruct{T} to NormalStruct{T} without conversion
[ Info: Forwarding Core.Int64 with parameters Any[] to default type mapping
[ Info: Mapping an OldStruct{Int} to SquaredStruct{Int} with conversion
[ Info: Forwarding Core.Float64 with parameters Any[] to default type mapping
[ Info: Mapping an OldStruct{T} to NormalStruct{T} without conversion
Dict{String, Any} with 4 entries:
  "inttype"   => SquaredStruct{Int64}
  "old_float" => NormalStruct{Float64}(3.14)
  "old_int"   => SquaredStruct{Int64}(1764)
  "floattype" => NormalStruct{Float64}
```


Note that the dictionary approach and the mapping function are mutually exclusive.



## Groups - Appending to files


Group objects can be constructed with two optional keyword arguments:
```julia
g = Group(file;
          est_num_entries=4
          est_link_name_len=8)
```

These determine how much (additional) empty space should be allocated for the group description. (list of entries)
This can be useful for performance when one expects to append many additional datasets after first writing the file.


## Fallback Behaviour
By default JLD2 will attempt to open files using the `MmapIO` backend. If that fails, it retries using `IOStream`.

## Virtual Datasets

Virtual datasets (VDS) allow you to create datasets that reference data from multiple source files without copying the data. This is useful for combining large distributed datasets efficiently.

### Basic Usage

Create a virtual dataset mapping entire source files:

```julia
using JLD2

# Create source files
jldsave("data1.jld2"; x = fill(1.0, 3))
jldsave("data2.jld2"; x = fill(2.0, 3))

# Create virtual dataset
jldopen("virtual.jld2", "w") do f
    mappings = [
        JLD2.VirtualMapping("./data1.jld2", "x"),
        JLD2.VirtualMapping("./data2.jld2", "x")
    ]
    JLD2.create_virtual_dataset(f, "combined", (3, 2), Float64, mappings)
end

# Read back
data = jldopen("virtual.jld2", "r") do f
    f["combined"]  # Returns [1.0 2.0; 1.0 2.0; 1.0 2.0]
end
```

### Selection Methods

Virtual mappings support three ways to specify regions:

**1. Julia index ranges (recommended)**
```julia
mapping = JLD2.VirtualMapping("./data.jld2", "measurements";
    vds_indices=(1:1, 1:5))  # Place in first row, columns 1-5

mapping = JLD2.VirtualMapping("./data.jld2", "measurements";
    src_indices=(1:10, 5:15),   # Take rows 1-10, cols 5-15 from source
    vds_indices=(1:10, 1:11))   # Place at rows 1-10, cols 1-11 in VDS
```

**2. Root index + shape (most intuitive)**
```julia
mapping = JLD2.VirtualMapping("./data.jld2", "measurements";
    vds_root=(2, 1),      # Start at row 2, column 1
    vds_shape=(1, 5))     # Block is 1 row × 5 columns

mapping = JLD2.VirtualMapping("./data.jld2", "measurements";
    src_root=(5, 10), src_shape=(3, 4),  # Take 3×4 block from source
    vds_root=(1, 1),  vds_shape=(3, 4))  # Place at top-left of VDS
```

**3. Direct HyperslabSelection (advanced)**
```julia
vds_sel = JLD2.HyperslabSelection([0x0, 0x0], [0x1, 0x1], [0x1, 0x1], [0x5, 0x1])
mapping = JLD2.VirtualMapping("./data.jld2", "measurements"; vds_selection=vds_sel)
```

### Strided Selections

Select non-contiguous regions using strided ranges:

```julia
# Every other row
mapping = JLD2.VirtualMapping("./data.jld2", "measurements";
    vds_indices=(1:2:10, 1:5))  # Rows 1, 3, 5, 7, 9 in VDS
```

### Automatic Inference

Automatically infer dimensions and types from source files:

```julia
jldopen("virtual.jld2", "w") do f
    source_files = ["./data1.jld2", "./data2.jld2", "./data3.jld2"]

    # Automatically determines dimensions and element type
    JLD2.create_virtual_dataset(f, "combined", source_files, "measurements")
end
```

### Pattern-based File Names

Use `%b` for sequential file patterns:

```julia
# Expands to sub-0.jld2, sub-1.jld2, etc.
mapping = JLD2.VirtualMapping("./sub-%b.jld2", "dataset")
```


