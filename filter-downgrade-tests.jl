ENV["JULIA_PKG_PRECOMPILE_AUTO"]=0

pkgs_under_test = [
    (; name="JLD2",      old_version=v"0.6.4", localpath=".")
    (; name="JLD2Bzip2", old_version=v"0.1.2", localpath="filterpkgs/JLD2Bzip2")
    (; name="JLD2Lz4",   old_version=v"0.1.1", localpath="filterpkgs/JLD2Lz4")
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

# First copy out the different packages under test to break the workspace
pkg_temp_dirs = map(pkgs_under_test) do (;name, old_version, localpath)
    cp(joinpath(@__DIR__, localpath), joinpath(mktempdir(), name))
end

filter_devs = map(2:length(pkgs_under_test)) do i
    (;path=pkg_temp_dirs[i],)
end
filter_adds = map(2:length(pkgs_under_test)) do i
    (;name=pkgs_under_test[i].name, version=pkgs_under_test[i].old_version,)
end
main_devs = [(;path=pkg_temp_dirs[1],)]
main_adds = [(;name=pkgs_under_test[1].name, version=pkgs_under_test[1].old_version,)]

# current JLD2, old filters
run_out_tree_tests(devs=main_devs, adds=filter_adds)
# old JLD2, current filters
run_out_tree_tests(devs=filter_devs, adds=main_adds)
