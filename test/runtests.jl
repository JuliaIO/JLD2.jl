using JLD2
using Base.Test

include("lookup3.jl")
include("internal.jl")
include("rw.jl")
include("append.jl")
include("groups.jl")
include("finalization.jl")
include("recon.jl")
#include("customserialization.jl") currently broken due to #265
