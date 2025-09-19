using Test, JLD2
using LazyArtifacts
#=
When adding test files to the JLD2Testfiles repo tag a new
release and adapt Artifacts.toml and the line below accordingly.
    git clone git@github.com:JonasIsensee/JLD2TestFiles.git
add file & commit
    git tag v0.1.X
    git push
    git push --tags
go to github and create a new release with name "v0.1.x"
update Artifacts.toml:
1) navigate to testdir
2) launch repl and run
    using Pkg
    Pkg.activate(; temp=true)
    Pkg.add("ArtifactUtils")
    using ArtifactUtils
    add_artifact!(
        "Artifacts.toml",
        "testfiles",
        "https://github.com/JonasIsensee/JLD2TestFiles/archive/refs/tags/v0.1.x.tar.gz", # replace version here
        force=true,
        lazy=true)
update artifact string below to reflect new version number
=#
# NOTE: Keep this artifact version in sync with test_dataset_show.jl
testfiles = artifact"testfiles/JLD2TestFiles-0.1.9/artifacts"

@testset "HDF5 compat test files" begin
    # These are test files copied from the HDF5.jl test suite

    fn = joinpath(testfiles,"compound.h5")
    jldopen(fn) do f
        data = f["data"]
        @test data[1] == data[2]
        nt = data[1]
        @test nt.wgt == 1.0
        @test nt.xyz == [-2.4559041161056125, 0.43236207188504794, -0.5088338908493437]
        @test nt.uvw == [-0.44966656055677057, 0.6453930541533174, 0.6174688574881305]
        @test nt.E == 1.1915731810042547
    end

    # Should return some enum type and load names correctly
    fn = joinpath(testfiles,"h5ex_t_enum.h5")
    jldopen(fn) do f
        @test size(f["DS1"]) == (7,4)
    end

    fn = joinpath(testfiles,"h5ex_t_array.h5")
    jldopen(fn) do f
        @test f["DS1"][1] == (0:-1:-4) .* [0,1,2]'
        @test f["DS1"][2] == hcat(collect(0:4), ones(Int,5), collect(2:-1:-2))
    end

    fn = joinpath(testfiles,"h5ex_t_float.h5")
    jldopen(fn) do f
        @test size(f["DS1"]) == (7,4)
        @test f["DS1"][9] ≈ 5/3
    end

    # Big Endian Integers are not implemented
    fn = joinpath(testfiles,"h5ex_t_int.h5")
    jldopen(fn) do f
        @test f["DS1"] == [0:-1:-6 zeros(Int,7) 0:6 0:2:12]
    end

    fn = joinpath(testfiles,"h5ex_t_objref.h5")
    jldopen(fn) do f
        @test f["DS1"][1] === f["G1"]
        @test f["DS1"][2] === f["DS2"]
    end

    fn = joinpath(testfiles,"h5ex_t_opaque.h5")
    jldopen(fn) do f
        @test f["DS1"][1].data == [0x4f, 0x50, 0x41, 0x51, 0x55, 0x45, 0x30]
        @test f["DS1"][2].data == [0x4f, 0x50, 0x41, 0x51, 0x55, 0x45, 0x31]
        @test f["DS1"][3].data == [0x4f, 0x50, 0x41, 0x51, 0x55, 0x45, 0x32]
        @test f["DS1"][4].data == [0x4f, 0x50, 0x41, 0x51, 0x55, 0x45, 0x33]
    end

    fn = joinpath(testfiles,"h5ex_t_string.h5")
    jldopen(fn) do f
        @test f["DS1"] == ["Parting", "is such", "sweet", "sorrow."]
    end

    fn = joinpath(testfiles,"h5ex_t_vlen.h5")
    jldopen(fn) do f
        @test f["DS1"][1] == [3, 2, 1]
        @test f["DS1"][2] == [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]
    end

    fn = joinpath(testfiles,"h5ex_t_vlstring.h5")
    jldopen(fn) do f
        @test f["DS1"] == ["Parting", "is such", "sweet", "sorrow."]
    end

    fn = joinpath(testfiles,"nullterm_ascii.h5")
    jldopen(fn) do f
        @test f["test"] == "Hello World"
    end

    fn = joinpath(testfiles,"large_fractal_heap.h5")
    jldopen(fn) do f
        @test length(keys(f)) == 200000
    end

    fn = joinpath(testfiles,"netcdf.nc")
    jldopen(fn) do f
        @test f["hello"] == ones(5)
        #@test_broken f["x"]
        #@test_broken f["z"]
        @test f["grouped/data"] == 0:9
        #@test_broken f["grouped/y"]
    end

    fn = joinpath(testfiles,"simple.nc")
    jldopen(fn) do f
        @test f["dim1"] == [2, 4, 6]
        @test f["dim2"] == ["a", "b", "c", "d"]
        @test f["mydata"] == Matrix(reshape(1:12, 4, 3))
        JLD2.load_attributes(f, "dim1") # not sure what to test for. just not erroring so far
        JLD2.load_attributes(f, "dim2")
        JLD2.load_attributes(f, "mydata")
        JLD2.load_attributes(f.root_group)
    end

    # julia> using JLD
    # julia> struct A; x::Int; y::Float64; z::String; end
    # julia> save("jldstruct.jld", "a", A(1,2.0,"3"))
    fn = joinpath(testfiles,"jldstruct.jld")
    jldopen(fn) do f
        a = f["a"]
        @test a.x == 1
        @test a.y == 2.0
        @test a.z == "3"
    end

    fn = joinpath(testfiles,"chunking1.h5")
    jldopen(fn) do f
        @test f["uncompressed_chunks"] == reshape(1:1000., 25, 40)
        @test f["compressed_chunks"] == reshape(1:1000., 25, 40)
        @test f["shuffle_compressed_chunks"] == reshape(1:1000, 25, 40)
        @test size(f["incomplete_allocation"]) == (50,50,10)
        @test f["incomplete_allocation"][1:50,1:50, 2] == reshape(1:2500, 50,50)
        #f["incomplete_allocation"][1,1,1] == 0
    end
end

@testset "Opening corrupted files" begin
    mktemp() do path, io
        close(io)
        @test_throws EOFError jldopen(path)
        # We want to make sure that this didn't accidentally add itself to the open
        # files cache
        @test_throws EOFError jldopen(path)
        # Opening for overwriting should succeed
        close(jldopen(path, "w+"))
    end
end

@testset "Mutable Struct Reconstruction Issue #506" begin
    fn = joinpath(testfiles,"struct_reconstruction.jld2")
    data = load(fn)
    @test data["dms"] isa JLD2.SerializedDict
    @test JLD2.isreconstructed(data["dms"].kvvec[1].second)
    @test data["ds"] isa JLD2.SerializedDict
    @test JLD2.isreconstructed(data["ds"].kvvec[1].second)
    @test JLD2.isreconstructed(data["tms"][2])
end

@testset "Issue #538 Reconstruction of UnionAll type parameter" begin
    fn = joinpath(testfiles, "unionall_typeparam.jld2")
    data = load(fn, "test")
    @test propertynames(data) == (:val,)
    @test data.val == 4
end

module RecoverableChangesInStructs
    using JLD2, Test
    fn = joinpath(Main.testfiles, "recoverable_changes_in structs.jld2")
    # for saving:
    # struct A; x::Int; end
    # struct B; a::A; end

    # struct C; x::Int; end
    # struct D; c::C; end
    # jldsave("$(my_object_filename)"; b=B(A(42)), d=D(C(42)))


    struct A; x::Float64; end
    struct B; a::A; end

    struct C; x::Tuple{Int,Int}; end
    struct D; c::C; end
    @testset "Recoverable changes in structs, Issue #354" begin
        b = load(fn, "b")
        @test b == B(A(42.0))

        d = load(fn, "d")
        # Reconstructed type has correct value
        @test d.c.x == 42
        # This is evil
        #Base.convert(::Type{Tuple{Int,Int}}, x::Int) = (x, 2x)
        #d = load(fn, "d")
        #@test d.c.x == (42, 84)
    end
end



module Readas
    using JLD2
    struct UndefinedFunction <:Function
        fun
    end
    (f::UndefinedFunction)(args...; kwargs...) = error("The function \$(f.fun) is not defined")

    # Case when readas is defined
    # If `F` doesn't exist when reading, should be read as `Foo{UndefinedFunction}`
    struct Foo{F<:Function}
        fun::F
    end
    struct FooSerialization
        fun
    end
    JLD2.writeas(::Type{<:Foo}) = FooSerialization
    Base.convert(::Type{<:FooSerialization}, f::Foo) = FooSerialization(f.fun)

    JLD2.readas(::Type{<:FooSerialization}) = Foo
    function Base.convert(::Type{<:Foo}, f::FooSerialization)
        isa(f.fun, Function) && return Foo(f.fun)
        return Foo(UndefinedFunction(f.fun))
    end

    # Case when readas is not defined (N)
    # If `F` doesn't exist when reading, should be read as `FooNSerialization`
    struct FooN{F<:Function}
        fun::F
    end
    struct FooNSerialization
        fun
    end
    JLD2.writeas(::Type{<:FooN}) = FooNSerialization
    Base.convert(::Type{<:FooNSerialization}, f::FooN) = FooNSerialization(f.fun)
    Base.convert(::Type{<:FooN}, f::FooNSerialization) = FooN(f.fun)
end

# jldsave("readas_foo_sin.jld2"; foo=Readas.Foo(sin))
# fun(x) = x^2
# jldsave("readas_foo_a.jld2"; foo=Readas.Foo(fun))
# jldsave("readas_foo_n_sin.jld2"; foo=Readas.FooN(sin))
# jldsave("readas_foo_n_a.jld2"; foo=Readas.FooN(fun))

@testset "readas api for struct upgrades" begin
    getfoo(file) = jldopen(joinpath(testfiles,file)) do io; io["foo"]; end
    @test getfoo("readas_foo_sin.jld2") isa Readas.Foo{typeof(sin)}
    @test getfoo("readas_foo_n_sin.jld2") isa Readas.FooN{typeof(sin)}
    @test getfoo("readas_foo_a.jld2") isa Readas.Foo{Readas.UndefinedFunction}
    @test getfoo("readas_foo_n_a.jld2") isa Readas.FooNSerialization
end


@testset "plain reconstruction" begin
    fn = joinpath(testfiles,"struct_reconstruction.jld2")
    data = load(fn; plain=true)
    # This is somewhat broken: Tuples are committed with field names "1", "2",...
    # these are valid names but break most of the common API incl. the @NamedTuple macro
    #@test data["tms"] == @NamedTuple{1::Int64, 2}((1, (a = 1,)))
    @test getproperty(data["tms"], Symbol(1)) == 1
    @test data["s"]   == (a = 1,)
    @test data["ds"].kvvec[1]  == (; first = "a", second = (a = 1,))
end

@testset "Reconstruct struct with singleton fields" begin
    fn = joinpath(testfiles,"singleton_struct_fields.jld2")
    if VERSION ≥ v"1.7"
        @test_warn "type Main.MissingStruct does not exist in workspace; reconstructing" ms = load(fn, "ms")
    else # @test_warn works differently on 1.6
        ms = load(fn, "ms")
    end
    @test ms.a == 1
    @test ms.b === nothing
    @test ms.c === missing
    @test ms.d == 2.0
    @test typeof(ms).parameters[1] == :MissingStruct

    @test_nowarn ms = load(fn, "ms"; typemap=Dict("Main.MissingStruct" => JLD2.Upgrade(NamedTuple)))
    @test ms.a == 1
    @test ms.b === nothing
    @test ms.c === missing
    @test ms.d == 2.0

    ms = load(fn, "ms"; plain=true)
    @test ms == (; a=1, d=2.0)
end

@testset "Mmap with misaligned data" begin
    fn = joinpath(testfiles, "mmapfile.h5")
    f = jldopen(fn)
    ds = JLD2.get_dataset(f, "arr")
    arr = JLD2.readmmap(ds)
    @test arr isa Base.ReinterpretArray
    @test arr == 1:100
    close(f)
end

@testset "data created with JLD2@0.5 filters" begin
    fn = joinpath(testfiles, "jld2-0.5-filters.jld2")
    f = jldopen(fn)
    @testset "$(cname)" for (cname) in ["bzip2", "zlib", "lz4f", "zstd"]
        @test f["$(cname)-1"] == zeros(10000)
        @test f["$(cname)-2"] == ones(10000)
        @test f["$(cname)-3"] == zeros(1)
        @test f["$(cname)-4"] == zeros(UInt8, 1)
        @test f["$(cname)-5"] == f["random_numbers_uncompressed"]
    end
    close(f)
end
