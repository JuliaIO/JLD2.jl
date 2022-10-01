using JLD2, FileIO
using Test

function better_success(cmd)
    fn1, _ = mktemp()
    fn2, _ = mktemp()
    try
       run(pipeline(cmd, stdout=fn1, stderr=fn2))
    catch
        println(String(read(fn1)))
        println(String(read(fn2)))
        return false
    end
    return true
end

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
include("test_files.jl")