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

## Issue #431 Identity-Preservation of nested structs with CustomSerialization 

abstract type AT end
Base.@kwdef mutable struct T1 <: AT
    f
end
Base.@kwdef mutable struct T2 <: AT
    t1::T1
end
Base.@kwdef mutable struct T3 <: AT
    t1::T1
    t2::T2
end

# Custom Serialization is not transitive, so using a custom serialization via a type
# that is custom serialized itself will not work
mutable struct PseudoDict{K,V}
    kv::Vector{Pair{K,V}}
end

DSA=PseudoDict{Symbol, Any}
JLD2.writeas(::Type{T}) where {T <: AT} = DSA
JLD2.wconvert(::Type{DSA}, t::AT) = DSA([f => getproperty(t, f) for f in fieldnames(typeof(t))])
JLD2.rconvert(::Type{T}, dsa::DSA) where {T <: AT} = T(; dsa.kv...)

@testset "Issue #431 Identity-Preservation" begin
    cd(mktempdir()) do
        t1 = T1(1)
        t2 = T2(t1)
        t3 = T3(t1, t2)
        save_object("kk.jld2", t3)

        t3 = load_object("kk.jld2")
        @test t3.t1 === t3.t2.t1
    end
end

## Issue #433 circular references with custom serialization


mutable struct CR
    r::CR
    CR() = new()
    CR(x) = new(x)
end

mutable struct CRSerialized
    r::CR
end

JLD2.writeas(::Type{CR}) = CRSerialized
JLD2.wconvert(::Type{CRSerialized}, t::CR) = CRSerialized(t.r)
JLD2.rconvert(::Type{CR}, dsa::CRSerialized) = CR(dsa.r)
@testset "Issue #433 circular references with custom serialization" begin
    cd(mktempdir()) do
        cr = CR()
        cr.r = cr
        save_object("kk.jld2", cr)

        t1 = load_object("kk.jld2")
        @test t1 === t1.r
    end
end

# Test jldsave
@testset "Multi-threaded read" begin
    fn = joinpath(mktempdir(), "test.jld2")

    jldsave(fn; a=1, b=2)

    #########################
    # Valid access patterns #
    #########################

    #Normal read
    jldopen(fn, "r"; parallel_read = true) do f
        @test f["a"] == 1
        @test f["b"] == 2
        @test fn ∉ keys(JLD2.OPEN_FILES)
    end

    # Can read in parallel and serial (read-only)
    f1 = jldopen(fn)
    f2 = jldopen(fn; parallel_read = true)
    @test JLD2.OPEN_FILES[realpath(fn)] == f1
    @test f1 != f2
    close(f1); close(f2)

    f1 = jldopen(fn, "a")
    @test_throws ArgumentError jldopen(fn; parallel_read = true)
    close(f1)

    ###########################
    # Invalid access patterns #
    ###########################

    # Open for non-read in parallel context
    @test_throws ArgumentError jldopen(fn, "w"; parallel_read = true) do f end 
    @test_throws ArgumentError jldopen(fn, "w+"; parallel_read = true) do f end
    @test_throws ArgumentError jldopen(fn, "r+"; parallel_read = true) do f end 
    @test_throws ArgumentError jldopen(fn, "a+"; parallel_read = true) do f end
    @test_throws ArgumentError jldopen(fn, "a"; parallel_read = true) do f end


    rm(fn; force = true, recursive = true)
end


###################################################################################################
##             `Upgrade` Tests
###################################################################################################

struct OldStructVersion
    x::Int
    y::Float64
end

struct UpdatedStruct
    x::Float64 # no longer int
    y::Float64
    z::Float64 # x*y
end
JLD2.rconvert(::Type{UpdatedStruct}, nt::NamedTuple) = UpdatedStruct(Float64(nt.x), nt.y, nt.x*nt.y)
# Dummy Upgrade, keeps struct but changes values
JLD2.rconvert(::Type{OldStructVersion}, nt::NamedTuple) = OldStructVersion(nt.x, 2*nt.y)


@testset "Explicit `Upgrade`ing" begin
    cd(mktempdir()) do
        orig = OldStructVersion(1,2.0)
        newver = UpdatedStruct(1.0, 2.0, 2.0)
        save_object("test.jld2", orig)
        @test orig == load_object("test.jld2")
        @test newver == JLD2.load("test.jld2", "single_stored_object"; typemap=Dict("Main.OldStructVersion" => JLD2.Upgrade(UpdatedStruct)))

        # Test for container with vector elements being upgraded
        wrapped_orig = ([orig,],)
        wrapped_newver = ([OldStructVersion(1,4.0),],)
        save_object("test.jld2", wrapped_orig)
        @test wrapped_orig == load_object("test.jld2")
        @test wrapped_newver == JLD2.load("test.jld2", "single_stored_object"; typemap=Dict("Main.OldStructVersion" => JLD2.Upgrade(OldStructVersion)))
    end
end

@testset "Issue #484 round-trip Tuple{Type{Int}}" begin
    cd(mktempdir()) do 
        T = Tuple{Type{Int}}
	jldsave("test.jld2"; T)
	@test T == load("test.jld2", "T")
    end
end


@testset "Issue #510 uninitialized Dicts" begin
    cd(mktempdir()) do
        save_object("test.jld2", Vector{Dict{String,Float64}}(undef, 10))
        o = load_object("test.jld2")
        @test !any(isassigned.(Ref(o), eachindex(o)))
    end
end

@testset "Issue 486 store NTuple type with indeterminate length" begin
    cd(mktempdir()) do
        s_type = Tuple{Int, Tuple{Vararg{Int, T}} where T}
        a = Dict{s_type, Int}()
        a[(0, (1, 2, 3))] = 4
        @test a == (save_object("test.jld2", a); load_object("test.jld2"))
    end
end 

@testset "FileIO.load of (nested) groups" begin
    cd(mktempdir()) do
        # create a file
        fn = "test.jld2"
        f = jldopen(fn, "w")
        f["a"] = 1
        f["b/c"] = 2
        close(f)
        @test load(fn) == Dict("a" => 1, "b/c" => 2)
        @test load(fn; nested=true) == Dict("a" => 1, "b" => Dict("c" => 2))

        f = jldopen(fn, "a")
        f["b/d/e"] = 3
        f["b/d/f/g"] = 4
        close(f)
        @test load(fn, "b/d/f") == Dict("g" => 4)
        @test load(fn, "b/d/f", "a") == (Dict("g" => 4), 1)
        @test load(fn, "b/d") == Dict("e" => 3, "f" => Dict("g"=> 4))
    end
end

@testset "Union{} in type signature Issue #532" begin
    cd(mktempdir()) do 
        op = (1, pairs((;)))
        jldsave("testopempty.jld2"; op)
        @test op == load("testopempty.jld2", "op")
    end
end

@testset "Issue #536 reading directly after writing" begin
    cd(mktempdir()) do 
        x = (1, Dict(2=>3))
        jldopen("test.jld2", "w") do f
            f["x"] = x
            @test x == f["x"]
        end
    end
end

@testset "Issue #558 a struct with 256 fields" begin
    fields = [Symbol("field",i) for i in 1:256]
    @eval struct Structwithmanyfields
        $(fields...)
    end
    # Fill the struct
    obj = Structwithmanyfields(ntuple(i -> i, 256)...)

    cd(mktempdir()) do 
        save_object("myStruct.jld2", obj)
        loaded = load_object("myStruct.jld2")
        @test loaded isa Structwithmanyfields
    end
end

# @testset "Issue #466 non-normalized unicode string identifiers" begin
#     ahat_unnormalized = String([0x61, 0xcc, 0x82])
#     ahat_normalized = String([0xc3, 0xa2])
#     dummy_data = 42
#     fn = "test.jld2"
#     cd(mktempdir()) do 
#         jldopen(fn, "w") do f
#             f[ahat_unnormalized] = dummy_data
#             @test haskey(f, ahat_normalized)
#             @test haskey(f, ahat_unnormalized)
#             @test f[ahat_normalized] == dummy_data
#             @test f[ahat_unnormalized] == dummy_data
#         end
#         jldopen(fn) do f
#             @test haskey(f, ahat_normalized)
#             @test haskey(f, ahat_unnormalized)
#             @test f[ahat_normalized] == dummy_data
#             @test f[ahat_unnormalized] == dummy_data
#         end
#     end
# end

module DummyModule
    using ..JLD2
    struct AA end
    struct BB end
    Base.@kwdef struct CC
        x::Any = 1
    end
    JLD2.rconvert(::Type{CC}, nt::NamedTuple) = CC()
    JLD2.rconvert(::Type{BB}, nt::NamedTuple) = BB()
    mutable struct DD end
    JLD2.rconvert(::Type{DD}, nt::NamedTuple) = DD()
end
@testset "Upgrading a struct that was formerly singleton" begin
    cd(mktempdir()) do     
        jldsave("testing.jld2"; a = DummyModule.AA())
        @test DummyModule.BB() == load("testing.jld2", "a"; typemap = Dict("Main.DummyModule.AA" => JLD2.Upgrade(DummyModule.BB)))
        @test DummyModule.BB() == load("testing.jld2", "a"; typemap = Dict("Main.DummyModule.AA" => DummyModule.BB))
        @test DummyModule.CC() == load("testing.jld2", "a"; typemap = Dict("Main.DummyModule.AA" => JLD2.Upgrade(DummyModule.CC)))
    end
end

@testset "Issue #628 Upgrading structs that contains unions" begin
    cd(mktempdir()) do
        @testset "immutable" begin
            a1 = Dict("a" => [nothing DummyModule.AA()])
            save("a.jld2", a1)
            a2 = load("a.jld2"; typemap = Dict("Main.DummyModule.AA" => JLD2.Upgrade(DummyModule.AA)))
            @test only(keys(a1)) == "a"
            @test keys(a1) == keys(a2)
            @test a1["a"] == a2["a"]
        end
        @testset "mutable" begin
            a1 = Dict("a" => [nothing DummyModule.DD()])
            save("a.jld2", a1)
            a2 = load("a.jld2"; typemap = Dict("Main.DummyModule.DD" => JLD2.Upgrade(DummyModule.DD)))
            @test only(keys(a1)) == "a"
            @test keys(a1) == keys(a2)
            @test typeof(a1["a"]) == typeof(a2["a"])
        end
    end
end

@testset "Issue #571 Loading Vararg NTuples" begin
    tst = [("a",), ("a", "b")]
    cd(mktempdir()) do
        save_object("tst.jld2", tst)
        @test tst == load_object("tst.jld2")
    end
end

@testset "Disable committing datatypes" begin
    fn = joinpath(mktempdir(), "disable_committing_datatypes.jld2")
    jldopen(fn, "w") do f
        f.disable_commit = true

        @test_throws ArgumentError f["1"] = Dict(1=>2)
        @test_throws ArgumentError f["2"] = Vector{Float64}
        @test_throws ArgumentError f["3"] = (1,2,3)
        @test_throws ArgumentError f["4"] = :asymbol
        # No throw
        f["5"] = "a string"
        # this could eventually be allowed
        @test_throws ArgumentError f["4"] = (; a=1, b=2)
    end
end


@testset "Missing Types in Tuples" begin
    fn = joinpath(mktempdir(), "missing_types_in_tuple.jld2")
    eval(:(module ModuleWithFunction
        fun(x) = x+1
    end))
    eval(:(save_object($fn, (1, ModuleWithFunction.fun, 2))))
    obj = load_object(fn)
    @test length(obj) == 3
    @test obj[1] == 1
    @test obj[2] == eval(:(ModuleWithFunction.fun))
    @test obj[3] == 2

    eval(:(module ModuleWithFunction end))
    obj = load_object(fn)
    @test length(obj) == 3
    @test obj[1] == 1
    @test JLD2.isreconstructed(obj[2])
    @test obj[3] == 2
    
end

if VERSION ≥ v"1.7"
    @testset "Storing an anonymous function" begin
        fn = joinpath(mktempdir(), "storing_anon_function.jld2")
        f = x -> x+1
        @test_warn contains("Attempting to store") save_object(fn, f)
    end
end

struct Foo601 <: Function
    x::Int
end

@testset "Issue #601: Storing <: Function objects" begin
    foo = Foo601(2)
    tempfile = tempname()
    jldsave(tempfile; foo)
    loaded_foo = load(tempfile, "foo")
    @test loaded_foo isa Foo601
end


@testset "Issue #603 - reused objectids" begin
    fn = joinpath(mktempdir(), "issue603_reused_objectids.jld2")

    function create_large_dict(levels::Int, items_per_level::Int, item_size::Int)
        # Create a nested dictionary with the specified number of levels
        function create_nested_dict(current_level, max_level)
            if current_level > max_level
                return Dict("x" => rand(Int, item_size))
            else
                return Dict(
                    string(i) => create_nested_dict(current_level + 1, max_level)
                    for i in 1:items_per_level
                )
            end
        end
        # Create the top-level dictionary
        return create_nested_dict(1, levels)
    end

    obj = create_large_dict(3, 30, 1);
    jldsave(fn; obj)
    res = load(fn, "obj")
    function recursive_test(obj, res)
        @test length(keys(obj)) == length(keys(res))
        for (k, v) in obj
            @test haskey(res, k)
            if isa(v, Dict)
                recursive_test(v, res[k])
            else
                @test v == res[k]
            end
        end
    end
    recursive_test(obj, res)
end

@testset "Issue #611 - non-initialized Symbol field" begin
    fn = joinpath(mktempdir(), "non-init-symbolfield.jld2")
    data = Vector{Symbol}(undef, 2)
    data[1] = :a
    jldsave(fn; data)
    loaded_data = load(fn, "data")
    @test !isassigned(loaded_data, 2)
    @test loaded_data[1] == :a
end

@testset "Issue #619 - Number type turnaround" begin
    fn = joinpath(mktempdir(), "number_type_turnaround.jld2")
    types = [UInt8, Int8, UInt16, Int16, UInt32, Int32, UInt64, Int64, UInt128, Int128, Float16, Float32, Float64]
    jldopen(fn, "w") do f
        for t in types
            f[string(t)] = typemax(t)
        end
    end
    jldopen(fn) do f
        for t in types
            v = f[string(t)]
            @test v isa t
            @test v == typemax(t)
        end
    end
end

@static if VERSION >= v"1.11"
    @testset "Support for Memory" begin
        fn = joinpath(mktempdir(), "memorytype.jld2")
        m = Memory{Int}(undef, 10)
        mr = memoryref(m, 5)
        jldsave(fn; m, mr)
        d = load(fn)
        @test d["m"] isa Memory
        @test d["m"] == m
        @test d["mr"][] == mr[]
        @test d["mr"].mem === d["m"]

        m = Memory{String}(undef, 2)
        m[1] = "1"
        m[2] = "2"
        mr = memoryref(m, 2)
        jldsave(fn; m, mr)
        d = load(fn)
        @test d["m"] isa Memory
        @test d["m"] == m
        @test d["mr"][] == mr[]
        @test d["mr"].mem === d["m"]
    end
end


@testset "Advanced typemap" begin
    fn = joinpath(mktempdir(), "advancedtypemap.jld2")
    z = 1 + 2im # Complex{Int}
    zf = 1f0 + 2f0*im # Complex{Float32}
    jldsave(fn; z, type = Complex{Int}, zf)

    typemap = function(f, typepath, params)
        @info "typepath: $typepath, params: $params"
        if typepath == "Base.Complex" && params == [Int]
            return JLD2.Upgrade(Complex{Float64})
        end
        JLD2.default_typemap(f, typepath, params)
    end

    JLD2.rconvert(::Type{Complex{Float64}}, nt::NamedTuple) =
        Complex{Float64}(nt.re + 1, nt.im)

    d = load(fn; typemap)
    @test d["z"] isa Complex{Float64}
    @test d["z"] == 2.0 + 2.0im
    @test d["type"] == Complex{Float64}
    @test d["zf"] isa Complex{Float32}
    @test d["zf"] == 1.0 + 2.0im
end
