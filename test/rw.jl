using JLD2, Test, LinearAlgebra, Random

data = Dict{String, Any}()
# Seed random so that we get the same values every time
Random.seed!(1337)

# Define variables of different types
data["x"] = 3.7
data["A"] = A = reshape(1:15, 3, 5)
data["A3"] = reshape(1:30, 3, 5, 2)
data["A4"] = reshape(1:120, 2, 3, 4, 5)
data["Aarray"] = Vector{Float64}[[1.2,1.3],[2.2,2.3,2.4]]
data["basic_types"] = Any[UInt8(42), UInt16(42), UInt32(42), UInt64(42), UInt128(42),
                  Int8(42), Int16(42), Int32(42), Int64(42), Int128(42),
                  Float16(42), Float32(42), Float64(42)]
data["str"] = "Hello"
data["str_unicode"] = "pandapanda🐼panda"
data["str_embedded_null"] = "there is a null\0 in the middle of this string"
data["strings"] = String["It", "was", "a", "dark", "and", "stormy", "night"]
data["empty_string"] = ""
data["empty_string_array"] = String[]
data["empty_array_of_strings"] = String[""]
data["tf"] = true
data["TF"] = A .> 10
data["B"] = B = [-1.5 sqrt(2) NaN 6;
     0.0  Inf eps() -Inf]
data["AB"] = Any[A, B]
data["t"] = (3, "cat")
data["c"] = Complex{Float32}(3,7)
data["cint"] = 1+im  # issue 108
data["C"] = reshape(reinterpret(Complex{Float64}, B), (4,))
data["emptyA"] = zeros(0,2)
data["emptyB"] = zeros(2,0)
try
    global MyStruct
    mutable struct MyStruct
        len::Int
        data::Array{Float64}
        MyStruct(len::Int) = new(len)
        MyStruct(len::Int, data::Array{Float64}) = new(len, data)
    end
catch
end
data["ms"] = MyStruct(2, [3.2, -1.7])
data["msempty"] = MyStruct(5, Float64[])
data["sym"] = :TestSymbol
data["syms"] = syms = [:a, :b]
data["d"] = Dict([(syms[1],"aardvark"), (syms[2], "banana")])
data["oidd"] = IdDict([(syms[1],"aardvark"), (syms[2], "banana")])
# Incremental construction due to lacking constructor method in v1.0
data["imdd"] = Base.ImmutableDict(Base.ImmutableDict(syms[1]=>"aardvark"), syms[2]=>"banana")
data["ex"] = quote
    function incrementby1(x::Int)
        x+1
    end
end
data["T"] = UInt8
data["Tarr"] = DataType[UInt8, UInt16, UInt32, UInt64, UInt128]
data["char"] = 'x'
data["unicode_char"] = '\U10ffff'
data["α"] = 22
data["β"] = Any[[1, 2], [3, 4]]  # issue #93
data["vv"] = Vector{Int}[[1,2,3]]  # issue #123
data["typevar"] = Array{Int}[[1]]
data["typevar_lb"] = (Vector{U} where U<:Integer)[[1]]
data["typevar_ub"] = (Vector{U} where U>:Int)[[1]]
data["typevar_lb_ub"] = (Vector{U} where Int<:U<:Real)[[1]]
data["arr_undef"] = Vector{Any}(undef, 1)
data["arr_undefs"] = Matrix{Any}(undef, 2, 2)
data["ms_undef"] = MyStruct(0)
# Unexported type:
data["version_info"] = Base.GIT_VERSION_INFO
# Immutable type:
data["rng"] = 1:5
# Custom BitsType (#99)
primitive type MyBT 64 end
data["bt"] = bt = reinterpret(MyBT, Int64(55))
data["btarray"] = fill(bt, 5, 7)
# Symbol arrays (#100)
data["sa_asc"] = [:a, :b]
data["sa_utf8"] = [:α, :β]
# SubArray (to test tuple type params)
data["subarray"] = view([1:5;], 1:5)
# Array of empty tuples (to test tuple type params)
data["arr_empty_tuple"] = Tuple{}[]
struct EmptyImmutable end
data["emptyimmutable"] = emptyimmutable = EmptyImmutable()
data["arr_emptyimmutable"] = [emptyimmutable]
data["empty_arr_emptyimmutable"] = EmptyImmutable[]
mutable struct EmptyType end
data["emptytype"] = EmptyType()
data["arr_emptytype"] = [EmptyType()]
data["empty_arr_emptytype"] = EmptyImmutable[]
data["uninitialized_arr_emptytype"] = Vector{EmptyType}(undef, 1)
struct EmptyII
    x::EmptyImmutable
end
data["emptyii"] = EmptyII(EmptyImmutable())
struct EmptyIT
    x::EmptyType
end
data["emptyit"] = EmptyIT(EmptyType())
mutable struct EmptyTI
    x::EmptyImmutable
end
data["emptyti"] = EmptyTI(EmptyImmutable())
mutable struct EmptyTT
    x::EmptyType
end
data["emptytt"] = EmptyTT(EmptyType())
struct EmptyIIOtherField
    x::EmptyImmutable
    y::Float64
end
data["emptyiiotherfield"] = EmptyIIOtherField(EmptyImmutable(), 5.0)
struct EmptyIIType
    x::Type{Float64}
    y::EmptyImmutable
end
data["emptyiitype"] = EmptyIIType(Float64, EmptyImmutable())

# Unicode type field names (#118)
mutable struct MyUnicodeStruct☺{τ}
    α::τ
    ∂ₓα::τ
    MyUnicodeStruct☺{τ}(α::τ, ∂ₓα::τ) where τ = new(α, ∂ₓα)
end
data["unicodestruct☺"] = MyUnicodeStruct☺{Float64}(1.0, -1.0)
# Arrays of matrices (#131)
data["array_of_matrices"] = Matrix{Int}[[1 2; 3 4], [5 6; 7 8]]
# Tuple of arrays and bitstype
data["tup"] = (1, 2, [1, 2], [1 2; 3 4], bt)
# Empty tuple
data["empty_tup"] = ()
# Non-pointer-free struct
struct MyImmutable{T}
    x::Int
    y::Vector{T}
    z::Bool
end
data["nonpointerfree_immutable_1"] = MyImmutable(1, [1., 2., 3.], false)
data["nonpointerfree_immutable_2"] = MyImmutable(2, Any[3., 4., 5.], true)
struct MyImmutable2
    x::Vector{Int}
    MyImmutable2() = new()
end
data["nonpointerfree_immutable_3"] = MyImmutable2()
struct Vague
    name::String
end
data["vague"] = Vague("foo")
# Immutable with a union of BitsTypes
struct BitsUnion
    x::Union{Int64,Float64}
end
data["bitsunion"] = BitsUnion(5.0)
# Immutable with a union of Types
struct TypeUnionField
    x::Type{T} where T<:Union{Int64,Float64}
end
data["typeunionfield"] = TypeUnionField(Int64)
# Generic union type field
struct GenericUnionField
    x::Union{Vector{Int},Int}
end
data["genericunionfield"] = GenericUnionField(1)
# Array references
data["arr_contained"] = arr_contained = [1, 2, 3]
data["arr_ref"] = arr_ref = typeof(arr_contained)[]
push!(arr_ref, arr_contained, arr_contained)
# Object references
mutable struct ObjRefType
    x::ObjRefType
    y::ObjRefType
    ObjRefType() = new()
    ObjRefType(x, y) = new(x, y)
end
ref1 =ObjRefType()
data["obj_ref"] = ObjRefType(ObjRefType(ref1, ref1), ObjRefType(ref1, ref1))
# Immutable that requires padding between elements in array
struct PaddingTest
    x::Int64
    y::Int8
end
data["padding_test"] = PaddingTest[PaddingTest(i, i) for i = 1:8]
# Empty arrays of various types and sizes
data["empty_arr_1"] = Int[]
data["empty_arr_2"] = Matrix{Int}(undef, 56, 0)
data["empty_arr_3"] = Any[]
data["empty_arr_4"] = Matrix{Any}(undef, 0, 97)
# Moderately big dataset (which will be mmapped)
data["bigdata"] = [1:1000000;]
# BigFloats and BigInts
data["bigint"] = big(3)
data["bigfloat"] = big(3.2)
data["bigints"] = big(3).^(1:100)
data["bigfloats"] = big(3.2).^(1:100)
struct BigFloatIntObject
    bigfloat::BigFloat
    bigint::BigInt
end
data["bigfloatintobj"] = BigFloatIntObject(big(pi), big(typemax(UInt128))+1)
# None
data["none"] = Union{}
data["nonearr"] = Vector{Union{}}(undef, 5)
# nothing/Nothing
data["scalar_nothing"] = nothing
data["vector_nothing"] = Union{Int,Nothing}[1,nothing]

# some data big enough to ensure that compression is used:
data["Abig"] = kron(Matrix{Float64}(I, 10, 10), rand(20, 20))
data["Bbig"] = Any[i for i=1:3000]
data["Sbig"] = "A test string "^1000

# Bitstype type parameters
mutable struct BitsParams{x}; end
data["bitsparamfloat"]  = BitsParams{1.0}()
data["bitsparambool"]   = BitsParams{true}()
data["bitsparamsymbol"] = BitsParams{:x}()
data["bitsparamint"]    = BitsParams{1}()
data["bitsparamuint"]   = BitsParams{0x01}()
data["bitsparamint16"]  = BitsParams{Int16(1)}()

# Tuple of tuples
data["tuple_of_tuples"] = (1, 2, (3, 4, [5, 6]), [7, 8])

# Zero-dimensional arrays
data["zerod"] = zerod = Array{Int}(undef)
zerod[] = 1
data["zerod_any"] = zerod_any = Array{Any}(undef)
zerod_any[] = 1.0+1.0im

# Cyclic object
mutable struct CyclicObject
    x::CyclicObject

    CyclicObject() = new()
    CyclicObject(x) = new(x)
end
data["cyclicobject"] = CyclicObject()
data["cyclicobject"].x = data["cyclicobject"]

# SimpleVector
data["simplevec"] = Core.svec(1, 2, Int64, "foo")
iseq(x::Core.SimpleVector, y::Core.SimpleVector) = collect(x) == collect(y)

# JLD issue #243
# Type that overloads != so that it is not boolean
mutable struct NALikeType; end
Base.:!=(::NALikeType, ::NALikeType) = NALikeType()
Base.:!=(::NALikeType, ::Nothing) = NALikeType()
Base.:!=(::Nothing, ::NALikeType) = NALikeType()
data["natyperef"] = Any[NALikeType(), NALikeType()]

# JLD2 issue #31 (lots of strings)
data["lotsastrings"] = fill("a", 100000)

iseq(x,y) = isequal(x,y)
function iseq(x::Array{EmptyType}, y::Array{EmptyType})
    size(x) != size(y) && return false
    for i = 1:length(x)
        def = isassigned(x, i)
        def != isassigned(y, i) && return false
        if def
            iseq(x[i], y[i]) || return false
        end
    end
    return true
end
iseq(x::MyStruct, y::MyStruct) = (x.len == y.len && x.data == y.data)
iseq(x::MyImmutable, y::MyImmutable) = (isequal(x.x, y.x) && isequal(x.y, y.y) && isequal(x.z, y.z))
iseq(x::Union{EmptyTI,EmptyTT,EmptyIT}, y::Union{EmptyTI,EmptyTT,EmptyIT}) = iseq(x.x, y.x)
iseq(c1::Array, c2::Array) = length(c1) == length(c2) && all(p->iseq(p...), zip(c1, c2))
function iseq(c1::Base.Sys.CPUinfo, c2::Base.Sys.CPUinfo)
    for n in fieldnames(Base.Sys.CPUinfo)
        if getfield(c1, n) != getfield(c2, n)
            return false
        end
    end
    true
end
iseq(x::MyUnicodeStruct☺, y::MyUnicodeStruct☺) = (x.α == y.α && x.∂ₓα == y.∂ₓα)
iseq(x::Array{Union{}}, y::Array{Union{}}) = size(x) == size(y)
iseq(x::BigFloatIntObject, y::BigFloatIntObject) = (x.bigfloat == y.bigfloat && x.bigint == y.bigint)
iseq(x::T, y::T) where {T<:Union{EmptyType,EmptyImmutable,NALikeType}} = true
iseq(x::BitsParams{T}, y::BitsParams{S}) where {T,S} = (T == S)


# Test for equality of expressions, skipping line numbers
checkexpr(a, b) = @assert a == b
function checkexpr(a::Expr, b::Expr)
    @assert a.head == b.head
    i = 1
    j = 1
    while i <= length(a.args) && j <= length(b.args)
        if isa(a.args[i], Expr) && a.args[i].head == :line
            i += 1
            continue
        end
        if isa(b.args[j], Expr) && b.args[j].head == :line
            j += 1
            continue
        end
        checkexpr(a.args[i], b.args[j])
        i += 1
        j += 1
    end
    @assert i >= length(a.args) && j >= length(b.args)
end

fn = joinpath(mktempdir(), "test.jld")
openfuns = [
    (writef=() -> jldopen(fn, "w"; iotype=JLD2.MmapIO, compress=false),
     readf=() -> jldopen(fn, "r"; iotype=JLD2.MmapIO),
     inf="Mmap, uncompressed"),
    (writef=() -> jldopen(fn, "w"; iotype=IOStream, compress=false),
     readf=() -> jldopen(fn, "r"; iotype=IOStream),
     info="IOStream, uncompressed"),
     (writef=() -> jldopen(fn, "w"; iotype=JLD2.MmapIO, compress=true),
     readf=() -> jldopen(fn, "r"; iotype=JLD2.MmapIO),
     inf="Mmap, compressed"),
    (writef=() -> jldopen(fn, "w"; iotype=IOStream, compress=true),
     readf=() -> jldopen(fn, "r"; iotype=IOStream),
     inf="IOStream, compressed"),
    (writef=() -> (global io_buffer=IOBuffer(); jldopen(io_buffer, "w"; compress=false)),
     readf=() -> (seekstart(io_buffer); jldopen(io_buffer)),
     inf="IOBuffer, uncompressed"),
    (writef=() -> (global io_buffer=IOBuffer(); jldopen(io_buffer, "w"; compress=true)),
     readf=() -> (seekstart(io_buffer); jldopen(io_buffer)),
     inf="IOBuffer, compressed"),
]

function write_all(openfun, data)
    f = openfun()
    try
        for key in keys(data)
            write(f, key, data[key])
        end
    finally
        close(f)
    end
    nothing
end 

function check_all(openfun, data)
    f = openfun()
    try
        for key in keys(data)
            value = data[key]
            tmp = read(f, key)

            # Special cases for reading undefs
            if key=="arr_undef"
                !isa(tmp, Array{Any, 1}) || length(tmp) != 1 || isassigned(tmp, 1) &&
                error("For arr_undef, read value does not agree with written value")
            elseif key=="arr_undefs"
                !isa(tmp, Array{Any, 2}) || length(tmp) != 4 || any(map(i->isassigned(tmp, i), 1:4)) &&
                error("For arr_undefs, read value does not agree with written value")
            elseif key=="ms_undef"
                !isa(tmp, MyStruct) || tmp.len != 0 || isdefined(tmp, :data) &&
                error("For ms_undef, read value does not agree with written value")
            elseif key=="ex"
                checkexpr(value, tmp)
            elseif key=="vague"
                @test typeof(tmp) == typeof(value) && tmp.name == value.name
            elseif key=="arr_ref"
                @test tmp == arr_ref
                @test tmp[1] === tmp[2]
            elseif key=="obj_ref"
                @test tmp.x.x === tmp.x.y == tmp.y.x === tmp.y.y
                @test tmp.x !== tmp.y
            elseif key=="cyclicobject"
                @test tmp.x === tmp
            else
                written_type = typeof(value)
                if typeof(tmp) != written_type
                    error("For $key, read type $(typeof(tmp)) does not agree with written type $(written_type)")
                end
                try
                    @test iseq(tmp, value)
                catch
                    println("For $key, read value $tmp does not agree with written value $value")
                end
            end
        end
    finally
        close(f)
    end
    nothing
end


for (writef, readf, inf) in openfuns
    @info(inf)
    @info("  Write time:")
    @time write_all(writef, data)
    @info("  Read time:")
    @time check_all(readf, data)
end