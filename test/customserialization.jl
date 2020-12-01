using JLD2, Test

struct SingleFieldWrapper{T}
    x::T
end
Base.:(==)(a::SingleFieldWrapper, b::SingleFieldWrapper) = a.x == b.x

struct MultiFieldWrapper{T}
    x::T
    y::Int
end
Base.:(==)(a::MultiFieldWrapper, b::MultiFieldWrapper) = (a.x == b.x && a.y == b.y)

struct UntypedWrapper
    x
end
Base.:(==)(a::UntypedWrapper, b::UntypedWrapper) = a.x == b.x

# Mutable field wrapper
mutable struct MutableFieldWrapper{T}
    x::T
end
Base.:(==)(a::MutableFieldWrapper, b::MutableFieldWrapper) = a.x == b.x


# This is a type with a custom serialization, where the original type has data
# but the custom serialization is empty
struct CSA
    x::Ptr{Cvoid}
end
a = CSA(Ptr{Cvoid}(0))

struct CSASerialization end
JLD2.writeas(::Type{CSA}) = CSASerialization
function JLD2.wconvert(::Type{CSASerialization}, x::CSA)
    global converted = true
    CSASerialization()
end
function JLD2.rconvert(::Type{CSA}, x::CSASerialization)
    global converted = true
    CSA(Ptr{Cvoid}(0))
end

# This is a type with a custom serialization, where the original type has no
# data but the custom serialization does
struct CSB end
b = CSB()

struct CSBSerialization
    x::Int64
end
JLD2.writeas(::Type{CSB}) = CSBSerialization
function JLD2.wconvert(::Type{CSBSerialization}, x::CSB)
    global converted = true
    CSBSerialization(9018620829326368991)
end
function JLD2.rconvert(::Type{CSB}, x::CSBSerialization)
    global converted = true
    x.x == 9018620829326368991 ? CSB() : error("invalid deserialized data")
end

# This is a type where the custom serialized data can be stored inline when it
# is a field of another type, but the original data could not
mutable struct CSC
    x::Vector{Int}
end
Base.:(==)(a::CSC, b::CSC) = a.x == b.x
c = CSC(rand(Int, 2))

struct CSCSerialization
    a::Int
    b::Int
end
JLD2.writeas(::Type{CSC}) = CSCSerialization
function JLD2.wconvert(::Type{CSCSerialization}, x::CSC)
    global converted = true
    CSCSerialization(x.x[1], x.x[2])
end
function JLD2.rconvert(::Type{CSC}, x::CSCSerialization)
    global converted = true
    CSC([x.a, x.b])
end

# This is a type where the original data could be stored inline when it is a
# field of another type, but the custom serialized data cannot
struct CSD
    a::Int
    b::Int
end
d = CSD(rand(Int), rand(Int))

struct CSDSerialization
    x::Vector{Int}
end
JLD2.writeas(::Type{CSD}) = CSDSerialization
function JLD2.wconvert(::Type{CSDSerialization}, x::CSD)
    global converted = true
    CSDSerialization([x.a, x.b])
end
function JLD2.rconvert(::Type{CSD}, x::CSDSerialization)
    global converted = true
    CSD(x.x[1], x.x[2])
end

# This is a type that gets written as an array
struct CSE
    a::Int
    b::Int
    c::Int
end
e = CSE(rand(Int), rand(Int), rand(Int))

JLD2.writeas(::Type{CSE}) = Vector{Int}
function JLD2.wconvert(::Type{Vector{Int}}, x::CSE)
    global converted = true
    [x.a, x.b, x.c]
end
function JLD2.rconvert(::Type{CSE}, x::Vector{Int})
    global converted = true
    CSE(x[1], x[2], x[3])
end

# This is a type that is custom-serialized as an Int
struct CSF
    x::Int
end
f = CSF(rand(Int))

JLD2.writeas(::Type{CSF}) = Int
function Base.convert(::Type{Int}, x::CSF)
    global converted = true
    x.x
end
function Base.convert(::Type{CSF}, x::Int)
    global converted = true
    CSF(x)
end

# This is a type that is custom-serialized as a String
struct CSG
    x::Int
end
g = CSG(rand(Int))

JLD2.writeas(::Type{CSG}) = String
function Base.convert(::Type{String}, x::CSG)
    global converted = true
    convert(String, string(x.x))
end
function Base.convert(::Type{CSG}, x::String)
    global converted = true
    CSG(parse(Int, x))
end

# This is a type that is custom-serialized as a DataType
struct CSH
    T::DataType
    N::Int
end
h = CSH(Int, 2)

JLD2.writeas(::Type{CSH}) = DataType
function Base.convert(::Type{DataType}, x::CSH)
    global converted = true
    Array{x.T, x.N}
end
function Base.convert(::Type{CSH}, x::Type{Array{T,N}}) where {T,N}
    global converted = true
    CSH(T,N)
end

# This is a type that is custom-serialized as a Union
struct CSK
    T::DataType
    S::DataType
end
k = CSK(Int, Float64)
Base.:(==)(x::CSK, y::CSK) = (x.T == y.T && x.S == y.S) ||
                             (x.T == y.S && x.S == y.T)

JLD2.writeas(::Type{CSK}) = Union
function Base.convert(::Type{Union}, x::CSK)
    global converted = true
    Union{x.T,x.S}
end
function Base.convert(::Type{CSK}, x::Union)
    global converted = true
    CSK(Base.uniontypes(x)...)
end

function write_tests(file, prefix, obj)
    write(file, prefix, obj)
    write(file, "$(prefix)_singlefieldwrapper", SingleFieldWrapper(obj))
    write(file, "$(prefix)_multifieldwrapper", MultiFieldWrapper(obj, 2147483645))
    write(file, "$(prefix)_untypedwrapper", UntypedWrapper(obj))
    write(file, "$(prefix)_arr", [obj])
    write(file, "$(prefix)_empty_arr", typeof(obj)[])
    write(file, "$(prefix)_tuple", (obj,))
    write(file, "$(prefix)_mutablefieldwrapper", MutableFieldWrapper(obj))
end

function read_tests(file, prefix, obj)
    global converted = false
    @test read(file, prefix) == obj
    @test converted
    @test read(file, "$(prefix)_singlefieldwrapper") == SingleFieldWrapper(obj)
    @test read(file, "$(prefix)_multifieldwrapper") == MultiFieldWrapper(obj, 2147483645)
    @test read(file, "$(prefix)_untypedwrapper") == UntypedWrapper(obj)
    arr = read(file, "$(prefix)_arr")
    @test typeof(arr) == Vector{typeof(obj)} && length(arr) == 1 && arr[1] == obj
    empty_arr = read(file, "$(prefix)_empty_arr")
    @test typeof(empty_arr) == Vector{typeof(obj)} && length(empty_arr) == 0
    @test read(file, "$(prefix)_tuple") == (obj,) 
    @test read(file, "$(prefix)_mutablefieldwrapper") == MutableFieldWrapper(obj)
end

@testset "Custom Serialization" begin
    fn = joinpath(mktempdir(),"test.jld")
    file = jldopen(fn, "w")
    write_tests(file, "a", a)
    write_tests(file, "b", b)
    write_tests(file, "c", c)
    write_tests(file, "d", d)
    write_tests(file, "e", e)
    write_tests(file, "f", f)
    write_tests(file, "g", g)
    write_tests(file, "h", h)
    write_tests(file, "k", k)
    close(file)


    file = jldopen(fn, "r")
    read_tests(file, "a", a)
    read_tests(file, "b", b)
    read_tests(file, "c", c)
    read_tests(file, "d", d)
    read_tests(file, "e", e)
    read_tests(file, "f", f)
    read_tests(file, "g", g)
    read_tests(file, "h", h)
    read_tests(file, "k", k)
    close(file)
end


# Function Reconstruction (does not strictly belong to custom serialization but was
# broken by it)
struct S1
    a
    f
end

struct S2{F}
    a
    f::F
end

struct S3{F}
    f::F
end

λ() = 42

function round_trip(x)
    mktempdir() do dir
        fn = joinpath(dir, "test.jld2")
        @save fn grp=x
        @load fn grp
        return grp
    end
end

@testset "round trip Function values" begin
    @test 42 == round_trip(λ)()
    @test 42 == first(round_trip((λ,)))()
    @test 42 == first(round_trip((λ, λ)))()
    @test 42 == first(round_trip((λ, 42)))()
    @test 42 == first(round_trip([λ]))()
    @test 42 == first(round_trip([λ, λ]))()
    @test 42 == first(round_trip([λ, 42]))()
    @test 42 == round_trip(S1(42, λ)).f()
    @test 42 == round_trip(S2(42, λ)).f()
    @test 42 == round_trip(S3(λ)).f()
end