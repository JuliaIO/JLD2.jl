using JLD2, FileIO
using Test
using CodecZlib, CodecBzip2, CodecZstd, CodecLz4

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
include("unpack_test.jl")
include("dataset_api.jl")
include("mmap_test.jl")
include("wrapped_io.jl")

using TestItemRunner

@run_package_tests

@testitem "Aqua Testing" begin
    using Aqua
    Aqua.test_all(JLD2;
        deps_compat = (;
            ignore = [:Mmap,],
        ),
        ambiguities = (;
            # There is are ambiguity that can never be hit
            exclude = [JLD2.write_data, JLD2.WriteDataspace],
        )
    )
end
