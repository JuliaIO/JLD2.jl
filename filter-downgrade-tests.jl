ENV["JULIA_PKG_PRECOMPILE_AUTO"]=0

old_jld2_version = v"0.6.5"
filter_adds = [
    (; name="JLD2Bzip2", version=v"0.1.2")
    (; name="JLD2Lz4",   version=v"0.1.2")
]

function run_out_tree_tests(;devs=nothing, adds=nothing)
    test_dir = cp(joinpath(@__DIR__, "test"), joinpath(mktempdir(), "test"))
    setup_code = """
    using Pkg
    isnothing($(devs)) || Pkg.develop($(devs))
    isnothing($(adds)) || Pkg.add($(adds))
    Pkg.instantiate()
    Pkg.precompile()
    Pkg.status(;mode=Pkg.PKGMODE_MANIFEST)
    """
    run(`$(Base.julia_cmd()) --project=$(test_dir) -e $setup_code`)
    run(`$(Base.julia_cmd()) --project=$(test_dir) $(joinpath(test_dir, "runtests.jl"))`)
end

# Run the old JLD2's own test suite with the current filter packages swapped
# into its source tree.
function run_old_jld2_tests(old_version)
    jld2_dir = joinpath(mktempdir(), "JLD2")
    copy_code = """
    using Pkg
    Pkg.activate(; temp=true)
    Pkg.add(name="JLD2", version=$(repr(old_version)))
    using JLD2
    cp(pkgdir(JLD2), $(repr(jld2_dir)))
    """
    run(`$(Base.julia_cmd()) -e $copy_code`)
    # Files copied out of the julia package store are read-only
    chmod(jld2_dir, 0o755; recursive=true)
    # Swap in the current filter packages
    cp(joinpath(@__DIR__, "filterpkgs"), joinpath(jld2_dir, "filterpkgs"); force=true)
    test_code = """
    using Pkg
    Pkg.activate($(repr(jld2_dir)))
    Pkg.test()
    """
    run(`$(Base.julia_cmd()) -e $test_code`)
end

main_devs = [(;path=@__DIR__,)]

# current JLD2, old filters
run_out_tree_tests(devs=main_devs, adds=filter_adds)
# old JLD2, current filters
run_old_jld2_tests(old_jld2_version)
