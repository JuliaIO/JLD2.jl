using JLD2, FileIO
using Test

include("bufferedreader.jl")
include("lookup3.jl")
include("internal.jl")
include("rw.jl")
include("append.jl")
include("groups.jl")
include("consistency.jl")
include("loadsave.jl")
include("modules.jl")
include("modules-nested.jl")
include("isreconstructed.jl")
include("backwards_compatibility.jl")
include("inlineunion.jl")
include("customserialization.jl")
include("compression.jl")