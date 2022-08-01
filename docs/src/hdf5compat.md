# HDF5 Compatibility

JLD2 is built upon the [HDF5 Format Specification](https://support.hdfgroup.org/HDF5/doc/H5.format.html) and produces files that are compatible with the official [HDF5](https://www.hdfgroup.org/solutions/hdf5) C library.

This has the advantage that other libraries that use HDF5 such as the Julia
wrapper [`HDF5.jl`](https://github.com/JuliaIO/HDF5.jl) or 
even with [`h5py`](https://www.h5py.org/) using Python. In addition to that, adhering
to the HDF5 standards allows you to use the file introspection tools 
such as `h5dump` and `h5debug` provided by the HDF5 group.

!!! warning

    General compatibility only holds for a list of basic types:
        - Numbers `FloatXX`, `IntXX` and `UIntXX`
        - Strings
        - Arrays of those types
    Other structures can in principle also be decoded but may involve work. 
    See below for more information
    

## Understanding how Julia `struct`s are encoded

The HDF5 standard supports so-called `compound datatypes` that comprise of a set of 
already known datatypes. This is very similar to julia's `struct`s. 
When a user wants to write a non-default type to disk then JLD2
will create the corresponding compound datatypes and _commit_ them to the
file. All custom type definitions in a JLD2 file will be stored 
in a `_types/` group.
This way, the type definitions only needs to be written to the file
once and all instances of that `struct` reference it.

#### Example

```julia
julia> using JLD2

julia> struct MyCustomStruct
       x::Int64
       y::Float64
       end

julia> @save "test.jld2" a=MyCustomStruct(42, π)
```
Let's see what JLD2 makes out of my simple `MyCustomStruct`. To do that
we view the output of `h5dump`

```bash
$> h5dump test.jld2
HDF5 "test.jld2" {
GROUP "/" {
   GROUP "_types" {
      DATATYPE "00000001" H5T_COMPOUND {
         H5T_STRING {
            STRSIZE H5T_VARIABLE;
            STRPAD H5T_STR_NULLPAD;
            CSET H5T_CSET_UTF8;
            CTYPE H5T_C_S1;
         } "name";
         H5T_VLEN { H5T_REFERENCE { H5T_STD_REF_OBJECT }} "parameters";
      }
         ATTRIBUTE "julia_type" {
            DATATYPE  "/_types/00000001"
            DATASPACE  SCALAR
            DATA {
            (0): {
                  "Core.DataType",
                  ()
               }
            }
         }
      DATATYPE "00000002" H5T_COMPOUND {
         H5T_STD_I64LE "x";
         H5T_IEEE_F64LE "y";
      }
         ATTRIBUTE "julia_type" {
            DATATYPE  "/_types/00000001"
            DATASPACE  SCALAR
            DATA {
            (0): {
                  "Main.MyCustomStruct",
                  ()
               }
            }
         }
   }
   DATASET "a" {
      DATATYPE  "/_types/00000002"
      DATASPACE  SCALAR
      DATA {
      (0): {
            42,
            3.14159
         }
      }
   }
}
}
```

We can see that the file contains two things at top-level. There is a dataset `"a"`
(that is what we wanted to store) and there is a group `_types` which is where
all the necessary type information is stored.

You can see that JLD2 _committed_ two compound datatypes. The first one is `Core.Datatype`
which at first seems rather unintuitive. It is needed to tell HDF5 what a serialized 
julia datatype looks like (a name and a list of parameters).

Below that is the definition of `MyCustomStruct` with two fields 
`H5T_STD_I64LE "x"` and `H5T_IEEE_F64LE "y"` defining the integer field `x` and
the float field `y`.

## A note on pointers

In the julia programming language pointers `Ptr` are not needed very often. However,
when binary dependencies come into play and memory is passed back and forth,
pointers do become relevant. Pointers are addresses to locations in memory and
thus lose their meaning after a program has terminated.

In principle, there is little point in storing a pointer to a file but
in order to allow for a more seamless experience JLD2 will, similar to `Base.Serialization`
silently accept pointers. This is useful when storing large structures such as
a `DifferentialEquations.jl` solution object that might contain a pointer somewhere.
Upon deserialization any pointer fields are instantiated as null pointers.

This is done with just three lines of code utilizing the custom serialization logic and 
it is shown here as it serves as a good example for usage of that feature.

```julia
   writeas(::Type{<:Ptr}) = Nothing
   rconvert(::Type{Ptr{T}}, ::Nothing) where {T} = Ptr{T}()
```

Usually one would also have to define a method for `wconvert`. However, in this 
case JLD2 figures out that no explicit conversion is needed to construct `nothing`.
