using JLD2, FileIO
using Test

using Pkg
filterpath = joinpath(pkgdir(JLD2), "filterpkgs")
Pkg.develop([
    PackageSpec(name="JLD2Bzip2", path=joinpath(filterpath, "JLD2Bzip2")),
    PackageSpec(name="JLD2Lz4", path=joinpath(filterpath, "JLD2Lz4")),
])
using JLD2Bzip2, JLD2Lz4

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
include("test_dataset_show.jl")
include("mmap_test.jl")
include("generic_io_tests.jl")
include("links.jl")

using TestItemRunner

@run_package_tests

@testitem "Aqua Testing" begin
    using Aqua
    Aqua.test_all(JLD2;
        deps_compat = (;
            ignore = [:Mmap,],
        ),
        # There are a whole bunch of pseudo-ambiguities which can never be hit.
        ambiguities = false,
    )
end
