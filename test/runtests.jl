using JLD2, FileIO
using Base.Test

include("lookup3.jl")
include("internal.jl")
include("rw.jl")
include("append.jl")
include("groups.jl")
include("consistency.jl")
include("loadsave.jl")
include("recon.jl")
#include("customserialization.jl") currently broken due to #265
