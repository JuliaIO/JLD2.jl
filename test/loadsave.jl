using JLD2, FileIO, Test

fn = joinpath(mktempdir(), "test.jld2")

# test iotype fallback
#  no fallback specified → throw method error
@test_throws MethodError JLD2.openfile(ArgumentError, fn, true, true, false, nothing)
#  fallback specified → switch to fallback
fh = JLD2.openfile(ArgumentError, fn, true, true, false, JLD2.MmapIO)
@test fh isa JLD2.MmapIO
# To avoid an mmap error on mac, have to write something to the stream before closing
JLD2.ensureroom(fh, 8)
write(fh, 42)
JLD2.truncate_and_close(fh, 8)

# test file path checking
Sys.isunix() && @test_throws ArgumentError jldopen("/dev/null", "r")
@test_throws ArgumentError jldopen(dirname(fn), "r")

# Test load macros
jldopen(fn, "w") do f
    write(f, "loadmacrotestvar1", ['a', 'b', 'c'])
    write(f, "loadmacrotestvar2", 1)
end

@eval begin # wrapped in eval since @load with no args needs file at compile time
function func1()
    @load $fn
    @test loadmacrotestvar1 == ['a', 'b', 'c']
    @test loadmacrotestvar2 == 1
end
end

func1()

function func2()
    @load fn loadmacrotestvar1 loadmacrotestvar2
    @test loadmacrotestvar1 == ['a', 'b', 'c']
    @test loadmacrotestvar2 == 1
end

func2()

@test !isdefined(@__MODULE__, :loadmacrotestvar1) # should not be in global scope
@test (@eval @load $fn) == [:loadmacrotestvar1, :loadmacrotestvar2]
@test loadmacrotestvar1 == ['a', 'b', 'c']
@test loadmacrotestvar2 == 1

#Test load_object/save_object function Issue#210

@testset "load_object / save_object" begin
  @test_throws ArgumentError load_object(fn) #fn already has two objects
  #Create a file with no objects
  jldopen(fn, "w") do f
  end
  @test_throws ArgumentError load_object(fn) #fn no objects

  save_object(fn, ['a', 'b', 'c']) #rewrite fn to have one object
  l1 = jldopen(fn, "r") do f
    @test length(keys(f)) == 1
    f[JLD2.SINGLE_OBJECT_NAME]
  end
  @test l1 == ['a', 'b', 'c']

  #Test with default name
  l1 = load_object(fn)
  @test l1 == ['a', 'b', 'c']

  #Test with non-default name
  jldopen(fn, "w") do f
    write(f, "loadobjecttestvar2", 1)
  end
  l2 = load_object(fn)
  @test l2 == 1
end

# Test save macros
hello = "world"
@save fn hello
jldopen(fn, "r") do f
    @test read(f, "hello") == "world"
end

cmd = """
using JLD2
hello = "there"
@save $(repr(fn))
"""
run(`$(Base.julia_cmd()) -e $cmd`)
jldopen(fn, "r") do f
    @test read(f, "hello") == "there"
end

# Test Dict save/load
save(format"JLD2", fn, Dict("the"=>"quick", "brown"=>"fox", "stuff"=>reshape(1:4, (2, 2))))
@test load(fn) == Dict("the"=>"quick", "brown"=>"fox", "stuff"=>reshape(1:4, (2, 2)))

# Test Dict/save load with symbol keys
save(format"JLD2", fn, Dict(:the=>"quick", :brown=>"fox", :stuff=>reshape(1:4, (2, 2))))
@test load(fn) == Dict("the"=>"quick", "brown"=>"fox", "stuff"=>reshape(1:4, (2, 2)))

# Test load/save with pairs
save(format"JLD2", fn, "jumps", "over", "the", "lazy", "dog", reshape(1:4, (2, 2)))
@test load(fn, "jumps", "the", "dog") == ("over", "lazy", reshape(1:4, (2, 2)))
@test load(fn, "jumps") == "over"

jldopen(fn, "r+") do f
    @test !isempty(f)
    @test haskey(f, "dog")
    @test !haskey(f, "notdog")
    @test keys(f) == ["jumps", "the", "dog"]
    x1 = rand(UInt8, 1024^2+1024)
    x2 = rand(UInt8, 1024)
    f["x1"] = x1
    f["x2"] = x2
    @test f["x1"] == x1
    @test f["x2"] == x2
end

# Issue #19
save(fn, Dict("a"=>[1,2,3]))
io = open(fn)
@info("The next error message (involving \"loading nothing\") is a sign of normal operation")
@test_throws FileIO.CapturedException load(io)
close(io)

# Issue #33
d = Dict("params/p1" => 1,
         "params/p2" => 2.,
         "params/p3/p1" => 94,
         "data" => [[1,2,3], [4.,5.,6]])
save(fn, d)
@test load(fn) == d

# Issue #106
mutable struct MyMutableTest
    a::Int
    b::Vector{Int}
end
Base.getproperty(df::MyMutableTest, s::Symbol) =
    throw(ArgumentError("should not be called"))
Base.setproperty!(df::MyMutableTest, s::Symbol, x::Int) =
    throw(ArgumentError("should not be called"))
Base.isequal(x::MyMutableTest, y::MyMutableTest) =
    isequal(getfield(x, :a), getfield(y, :a)) && isequal(getfield(x, :b), getfield(y, :b))
mmtd = Dict("A" => MyMutableTest(1, [10]))
save(fn, mmtd)
@test isequal(load(fn), mmtd)

# Issue #125
len = 2^16
longstring = prod(fill("*",len));
lsd = Dict("longstring" => longstring)
save(fn, lsd)
@test isequal(load(fn), lsd)


# Testing Save macro
hello = "world"
num = 1.5

@save fn hello num
@test load(fn) == Dict("hello"=>"world", "num"=>1.5)

@save fn {compress=true} hello num
@test load(fn) == Dict("hello"=>"world", "num"=>1.5)

@save fn {compress=true, mmaparrays=false} hello num
@test load(fn) == Dict("hello"=>"world", "num"=>1.5)

@save fn {iotype=IOStream} hello num
@test load(fn) == Dict("hello"=>"world", "num"=>1.5)

@save fn bye = hello num
@test load(fn) == Dict("bye"=>"world", "num"=>1.5)

@save fn bye = hello num = 10
@test load(fn) == Dict("bye"=>"world", "num"=>10)

@test_throws ArgumentError @save fn {compress} hello

@test_throws ArgumentError @save fn hello=>"error"


# Issue # 189
struct RecursiveStruct
    x::RecursiveStruct
    RecursiveStruct() = new()
    RecursiveStruct(x) = new(x)
end


@testset "Recursive Immutable Types" begin
    x = RecursiveStruct()
    y = RecursiveStruct(x)

    @save fn x y
    JLD2.jldopen(fn, "r") do f
        @test f["x"] == x
        @test f["y"] == y
    end
end

# Issue #131
# write/read a Union{T,Missing}
len = 10_000
vect = Vector{Union{Bool,Missing}}(undef,len)
vect .= true
jldopen(fn,"w") do f
  f["vect"] = vect
end
vect_read = jldopen(fn,"r") do f
  f["vect"]
end
@test !any(ismissing.(vect_read))

# Also related to issue #131, but more types
len = 10_000
vect = Vector{Union{Missing,Float32,Float64,Int32}}(missing,len)
vect[vcat(1:10,33,44,55)] .= Int32(21)
vect[vcat(11:20,66,77,88)] .= 33.
vect[vcat(21:30,99)] .= Float32(123.)
jldopen(fn,"w") do f
  f["vect"] = vect
end
vect_read = jldopen(fn,"r") do f
  f["vect"]
end
@test all(findall(ismissing,vect) == findall(ismissing,vect_read))
@test all( skipmissing(vect) .=== skipmissing(vect_read))

# Issue #183
jfn, _ = mktemp()
@test_throws SystemError jldopen(jfn, "r", fallback = nothing)

# PR #206 Allow serialization of UnionAll in Union
struct UA1{T}; x::T; end
struct UA2{T}; y::T; end
@testset "UnionAll in Union" begin
    fn = joinpath(mktempdir(), "test.jld2")

    U1 = Union{Float64, Int}
    U2 = Union{Int, Vector}
    U3 = Union{UA1, UA2, Int}

    # Test types
    jldopen(fn, "w") do f
        f["u1"] = U1
        f["u2"] = U2
        f["u3"] = U3
    end

    u1, u2, u3 = jldopen(fn, "r") do f
        f["u1"], f["u2"], f["u3"]
    end
    @test u1 === U1
    @test u2 === U2
    @test u3 === U3
    # Test Vector with that eltype
    jldopen(fn, "w") do f
        f["u1"] = U1[1.0, 2, 3.0]
        f["u2"] = U2[1, [2.0], 3, ["4"]]
        f["u3"] = U3[UA1(1), UA2(2.0), 3, UA1("4")]
    end

    u1, u2, u3 = jldopen(fn, "r") do f
        f["u1"], f["u2"], f["u3"]
    end

    @test u1 == U1[1.0, 2, 3.0]
    @test u2 == U2[1, [2.0], 3, ["4"]]
    @test u3 == U3[UA1(1), UA2(2.0), 3, UA1("4")]
end



# Test for Issue #247
@testset "Tuple of Empty Structs" begin
    fn = joinpath(mktempdir(), "test.jld2")
    @save fn tup=(EmptyImmutable(), EmptyImmutable())
    @load fn tup

    @test tup == (EmptyImmutable(), EmptyImmutable())

    # Test for Recursively Empty struct
    @save fn tup=(EmptyII(EmptyImmutable()), EmptyImmutable())
    @load fn tup

    @test tup == (EmptyII(EmptyImmutable()), EmptyImmutable())
end


# Test for storing pointers
@testset "Pointer Serialization" begin
    fn = joinpath(mktempdir(), "test.jld2")
    @save fn ptr=pointer(zeros(5))
    @load fn ptr

    @test ptr == Ptr{Float64}(0)

    # Test for pointer inside structure
    @save fn tup=(; ptr = pointer(zeros(5)))
    @load fn tup

    @test tup == (; ptr = Ptr{Float64}(0))
end


# Test jldsave
@testset "jldsave API" begin
    fn = joinpath(mktempdir(), "test.jld2")

    jldsave(fn; a=1, b=2)
    jldopen(fn, "r") do f
        @test f["a"] == 1
        @test f["b"] == 2
    end

    jldsave(fn, IOStream; a=1, b=2)
    jldopen(fn, "r") do f
        @test f["a"] == 1
        @test f["b"] == 2
    end
end

# Test for object deletion
@testset "Object Deletion" begin
    fn = joinpath(mktempdir(), "test.jld2")
    # To hit all code paths, need to write sufficiently many entries
    jldopen(fn, "w") do f
        write(f, "a", 1)
        write(f, "b", 2)
        write(f, "g/a", 3)
        write(f, "g/b", 4)
    end
    jldopen(fn, "a") do f
        write(f, "c", 5)
        write(f, "d", 6)
        write(f, "e", 7)
        write(f, "f", 9)
    end
    jldopen(fn, "a") do f
        write(f, "h", 6)
        write(f, "i", 7)
        write(f, "j", 9)
    end


    data = FileIO.load(fn)
    @test all(["a", "b", "g/a", "g/b"] .∈ Ref(collect(keys(data))))

    # test read-only
    jldopen(fn, "r") do f
        @test_throws ArgumentError delete!(f, "a")
    end

    # test delete and write again
    jldopen(fn, "a") do f
        @test haskey(f, "g")
        delete!(f, "g")
        @test !haskey(f, "g")
    end
    jldopen(fn, "a") do f
        @test !haskey(f, "g")
        write(f, "g", 10)
        @test haskey(f, "g")
    end

    # test delete of group
    jldopen(fn, "a") do f
        delete!(f, "g")
    end
    jldopen(fn, "r") do f
        @test !haskey(f, "g")
    end
end

## Test for Issue #329

struct SingleUnionallField
    x::IdDict
end

@testset "Issue #329" begin
    fn = joinpath(mktempdir(), "test.jld2")
    jldsave(fn; a = SingleUnionallField(IdDict()))
    # Shouldn't throw
    a = load(fn, "a")
    @test a isa SingleUnionallField
    @test a.x isa IdDict{Any,Any}
end

@testset "Issue #327" begin
    path = tempname()
    x = (1,2)
    JLD2.@save path x
    @test load(path, "x") === (1,2)

    D = Dict("a"=>"Hazel")
    JLD2.@save path D
    @test load(path, "D") == D
end


@testset "Recoverable changes in structs, Issue #354" begin
    tmpdir = mktempdir()
    atexit(() -> rm(tmpdir; force = true, recursive = true))

    my_object_filename = joinpath(tmpdir, "my_object.jld2")
    saving_filename = joinpath(tmpdir, "saving.jl")
    loading_filename = joinpath(tmpdir, "loading.jl")

    saving_contents = """
        append!(Base.LOAD_PATH, $(Base.LOAD_PATH))
        unique!(Base.LOAD_PATH)
        using JLD2
        struct A; x::Int; end
        struct B; a::A; end

        struct C; x::Int; end
        struct D; c::C; end
        jldsave("$(my_object_filename)"; b=B(A(42)), d=D(C(42)))
    """

    loading_contents = """
        append!(Base.LOAD_PATH, $(Base.LOAD_PATH))
        unique!(Base.LOAD_PATH)
        using JLD2, Test
        struct A; x::Float64; end
        struct B; a::A; end
        b = load("$(my_object_filename)", "b")
        @test b == B(A(42.0))

        struct C; x::Tuple{Int,Int}; end
        struct D; c::C; end
        d = load("$(my_object_filename)", "d")
        # Reconstructed type has correct value
        @test d.c.x == 42
        Base.convert(::Type{Tuple{Int,Int}}, x::Int) = (x, 2x)

        d = load("$(my_object_filename)", "d")
        @test d.c.x == (42, 84)
    """

    rm(my_object_filename; force = true, recursive = true)
    rm(saving_filename; force = true, recursive = true)
    rm(loading_filename; force = true, recursive = true)

    if Sys.iswindows()
        saving_contents = replace(saving_contents, '\\' => "\\\\")
        loading_contents = replace(loading_contents, '\\' => "\\\\")
    end

    write(saving_filename, saving_contents)
    write(loading_filename, loading_contents)

    saving_cmd = `$(Base.julia_cmd()) $(saving_filename)`
    loading_cmd = `$(Base.julia_cmd()) $(loading_filename)`

    rm(my_object_filename; force = true, recursive = true)

    @test better_success(saving_cmd)
    @test better_success(loading_cmd)

    rm(tmpdir; force = true, recursive = true)
end

# Test for saving long NTuples
@testset "Long NTuples" begin
    cd(mktempdir()) do
        tup = ntuple(i->i^2, 5000)
        jldsave("test.jld2"; tup)
        @test tup == load_object("test.jld2")
    end
end


# Test for explicit type remapping
struct A1
    x::Int
end

struct A2
    x::Int
end

@testset "Explicit Type remapping" begin
    cd(mktempdir()) do
        jldsave("test.jld2", a=A1(42))
        @test A1(42) == load("test.jld2", "a")
        @test A2(42) == load("test.jld2", "a"; typemap=Dict("Main.A1" => A2))

        jldsave("test.jld2", a=(A1(42),))
        @test Tuple{A1} == typeof(load("test.jld2", "a"))
        @test Tuple{A2} == typeof(load("test.jld2", "a"; typemap=Dict("Main.A1" => A2)))
    end
end


# Not fully initialized mutable types
mutable struct FirstUninitialized
    x::Any
    y::Int
    FirstUninitialized(y) = (fu=new(); fu.y=y; fu)
end

@testset "Load incomplete mutable types" begin
    mktempdir() do folder
        fu = FirstUninitialized(42)
        jldsave(joinpath(folder,"test.jld2"); fu)
        fu_loaded = load(joinpath(folder,"test.jld2"), "fu")
        @test fu.y == fu_loaded.y
        @test_throws UndefRefError fu.x

    end
end

if VERSION ≥ v"1.8"

    struct SingletonStruct end

    # This is a workaround since for old julia versions this syntax is not allowed and the parser
    # is eager and attempts to parse this piece of code anyway
    eval(Meta.parse("""mutable struct WrappedSingleton
                  x::Int
                  const y::Vector{Int}
                  const z::SingletonStruct
              end
              """))

    @testset "Constant fields in mutable structs (>v1.8) - Issue #410" begin
        mktempdir() do folder
            # The real test here is that it doesn't fail in v1.8
            b = WrappedSingleton(1, [2,3], SingletonStruct())
            save("test.jld2", "b", b)
            bloaded = load("test.jld2", "b")
            @test b.x == bloaded.x
            @test b.y == bloaded.y
            @test b.z == bloaded.z
        end
    end

end