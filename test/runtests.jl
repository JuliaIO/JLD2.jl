using JLD2, FileIO, Compat
using Compat.Test

include("lookup3.jl")
include("internal.jl")
include("rw.jl")
include("append.jl")
include("groups.jl")
include("consistency.jl")
include("loadsave.jl")

# Only run the reconstruction tests on versions where `workspace` is a thing
if VERSION < v"0.7.0-DEV.2917"
    include("recon.jl")
end

#include("customserialization.jl") currently broken due to #265
