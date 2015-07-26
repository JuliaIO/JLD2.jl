using JLD2, Compat, Base.Test

macro read(fid, sym)
    if !isa(sym, Symbol)
        error("Second input to @read must be a symbol (i.e., a variable)")
    end
    esc(:($sym = read($fid, $(string(sym)))))
end
macro write(fid, sym)
    if !isa(sym, Symbol)
        error("Second input to @write must be a symbol (i.e., a variable)")
    end
    esc(:(write($fid, $(string(sym)), $sym)))
end

# Define variables of different types
x = 3.7
A = reshape(1:15, 3, 5)
Aarray = Vector{Float64}[[1.2,1.3],[2.2,2.3,2.4]]
basic_types = Any[UInt8(42), UInt16(42), UInt32(42), UInt64(42), UInt128(42),
                  Int8(42), Int16(42), Int32(42), Int64(42), Int128(42),
                  Float16(42), Float32(42), Float64(42)]
str = "Hello"
str_unicode = "pandapanda🐼panda"
str_embedded_null = "there is a null\0 in the middle of this string"
stringsA = ASCIIString["It", "was", "a", "dark", "and", "stormy", "night"]
stringsU = UTF8String["It", "was", "a", "dark", "and", "stormy", "night"]
string16 = utf16("it was a dark and stormy night")
strings16 = convert(Array{UTF16String}, stringsA)
strings16_2d = reshape(strings16[1:6], (2,3))
empty_string = ""
empty_string_array = ASCIIString[]
empty_array_of_strings = ASCIIString[""]
tf = true
TF = A .> 10
B = [-1.5 sqrt(2) NaN 6;
     0.0  Inf eps() -Inf]
AB = Any[A, B]
t = (3, "cat")
c = Complex64(3,7)
cint = 1+im  # issue 108
C = reinterpret(Complex128, B, (4,))
emptyA = zeros(0,2)
emptyB = zeros(2,0)
try
    global MyStruct
    type MyStruct
        len::Int
        data::Array{Float64}
        MyStruct(len::Int) = new(len)
        MyStruct(len::Int, data::Array{Float64}) = new(len, data)
    end
catch
end
ms = MyStruct(2, [3.2, -1.7])
msempty = MyStruct(5, Float64[])
sym = :TestSymbol
syms = [:a, :b]
d = Dict([(syms[1],"aardvark"), (syms[2], "banana")])
ex = quote
    function incrementby1(x::Int)
        x+1
    end
end
T = UInt8
Tarr = DataType[UInt8, UInt16, UInt32, UInt64, UInt128]
char = 'x'
unicode_char = '\U10ffff'
α = 22
β = Any[[1, 2], [3, 4]]  # issue #93
vv = Vector{Int}[[1,2,3]]  # issue #123
typevar = Array{Int}[[1]]
typevar_lb = Vector{TypeVar(:U, Integer)}[[1]]
typevar_ub = Vector{TypeVar(:U, Int, Any)}[[1]]
typevar_lb_ub = Vector{TypeVar(:U, Int, Real)}[[1]]
undef = cell(1)
undefs = cell(2, 2)
ms_undef = MyStruct(0)
# Unexported type:
cpus = Base.Sys.cpu_info()
# Immutable type:
rng = 1:5
# Type with a pointer field (#84)
immutable ObjWithPointer
    a::Ptr{Void}
end
objwithpointer = ObjWithPointer(0)
# Custom BitsType (#99)
bitstype 64 MyBT
bt = reinterpret(MyBT, @compat Int64(55))
btarray = fill(bt, 5, 7)
# Symbol arrays (#100)
sa_asc = [:a, :b]
sa_utf8 = [:α, :β]
# SubArray (to test tuple type params)
subarray = sub([1:5;], 1:5)
# Array of empty tuples (to test tuple type params)
arr_empty_tuple = (@compat Tuple{})[]
immutable EmptyImmutable end
emptyimmutable = EmptyImmutable()
arr_emptyimmutable = [emptyimmutable]
type EmptyType end
emptytype = EmptyType()
arr_emptytype = [emptytype]
immutable EmptyII
    x::EmptyImmutable
end
emptyii = EmptyII(EmptyImmutable())
immutable EmptyIT
    x::EmptyType
end
emptyit = EmptyIT(EmptyType())
type EmptyTI
    x::EmptyImmutable
end
emptyti = EmptyTI(EmptyImmutable())
type EmptyTT
    x::EmptyType
end
emptytt = EmptyTT(EmptyType())
immutable EmptyIIOtherField
    x::EmptyImmutable
    y::Float64
end
emptyiiotherfield = EmptyIIOtherField(EmptyImmutable(), 5.0)
immutable EmptyIIType
    x::Type{Float64}
    y::EmptyImmutable
end
emptyiitype = EmptyIIType(Float64, EmptyImmutable())

# Unicode type field names (#118)
type MyUnicodeStruct☺{τ}
    α::τ
    ∂ₓα::τ
    MyUnicodeStruct☺(α::τ, ∂ₓα::τ) = new(α, ∂ₓα)
end
unicodestruct☺ = MyUnicodeStruct☺{Float64}(1.0, -1.0)
# Arrays of matrices (#131)
array_of_matrices = Matrix{Int}[[1 2; 3 4], [5 6; 7 8]]
# Tuple of arrays and bitstype
tup = (1, 2, [1, 2], [1 2; 3 4], bt)
# Empty tuple
empty_tup = ()
# Non-pointer-free immutable
immutable MyImmutable{T}
    x::Int
    y::Vector{T}
    z::Bool
end
nonpointerfree_immutable_1 = MyImmutable(1, [1., 2., 3.], false)
nonpointerfree_immutable_2 = MyImmutable(2, Any[3., 4., 5.], true)
immutable MyImmutable2
    x::Vector{Int}
    MyImmutable2() = new()
end
nonpointerfree_immutable_3 = MyImmutable2()
# Immutable with a non-concrete datatype (issue #143)
immutable Vague
    name::ByteString
end
vague = Vague("foo")
# Immutable with a union of BitsTypes
immutable BitsUnion
    x::Union(Int64, Float64)
end
bitsunion = BitsUnion(5.0)
# Immutable with a union of Types
immutable TypeUnionField
    x::Union(Type{Int64}, Type{Float64})
end
typeunionfield = TypeUnionField(Int64)
# Generic union type field
immutable GenericUnionField
    x::Union(Vector{Int},Int)
end
genericunionfield = GenericUnionField(1)
# Array references
arr_contained = [1, 2, 3]
arr_ref = typeof(arr_contained)[]
push!(arr_ref, arr_contained, arr_contained)
# Object references
type ObjRefType
    x::ObjRefType
    y::ObjRefType
    ObjRefType() = new()
    ObjRefType(x, y) = new(x, y)
end
ref1 = ObjRefType()
obj_ref = ObjRefType(ObjRefType(ref1, ref1), ObjRefType(ref1, ref1))
# Immutable that requires padding between elements in array
immutable PaddingTest
    x::Int64
    y::Int8
end
padding_test = PaddingTest[PaddingTest(i, i) for i = 1:8]
# Empty arrays of various types and sizes
empty_arr_1 = Int[]
empty_arr_2 = Array(Int, 56, 0)
empty_arr_3 = Any[]
empty_arr_4 = cell(0, 97)
# Moderately big dataset (which will be mmapped)
bigdata = [1:10000;]
# BigFloats and BigInts
bigint = big(3)
bigfloat = big(3.2)
bigints = big(3).^(1:100)
bigfloats = big(3.2).^(1:100)
# None
none = Union()
nonearr = Array(Union(), 5)
# nothing/Void
scalar_nothing = nothing
vector_nothing = Union(Int,Nothing)[1,nothing]

# some data big enough to ensure that compression is used:
Abig = kron(eye(10), rand(20,20))
Bbig = Any[i for i=1:3000]
Sbig = "A test string "^1000

# Bitstype type parameters
type BitsParams{x}; end
bitsparamfloat  = BitsParams{1.0}()
bitsparambool   = BitsParams{true}()
bitsparamsymbol = BitsParams{:x}()
bitsparamint    = BitsParams{1}()
bitsparamuint   = BitsParams{0x01}()
bitsparamint16  = BitsParams{@compat Int16(1)}()

# Tuple of tuples
tuple_of_tuples = (1, 2, (3, 4, [5, 6]), [7, 8])

# SimpleVector
if VERSION >= v"0.4.0-dev+4319"
    simplevec = Base.svec(1, 2, Int64, "foo")
    iseq(x::SimpleVector, y::SimpleVector) = collect(x) == collect(y)
end

# Issue #243
# Type that overloads != so that it is not boolean
type NALikeType; end
Base.(:(!=))(::NALikeType, ::NALikeType) = NALikeType()
Base.(:(!=))(::NALikeType, ::Nothing) = NALikeType()
Base.(:(!=))(::Nothing, ::NALikeType) = NALikeType()
natyperef = Any[NALikeType(), NALikeType()]

iseq(x,y) = isequal(x,y)
iseq(x::MyStruct, y::MyStruct) = (x.len == y.len && x.data == y.data)
iseq(x::MyImmutable, y::MyImmutable) = (isequal(x.x, y.x) && isequal(x.y, y.y) && isequal(x.z, y.z))
iseq(x::Union(EmptyTI, EmptyTT), y::Union(EmptyTI, EmptyTT)) = isequal(x.x, y.x)
iseq(c1::Array{Base.Sys.CPUinfo}, c2::Array{Base.Sys.CPUinfo}) = length(c1) == length(c2) && all([iseq(c1[i], c2[i]) for i = 1:length(c1)])
function iseq(c1::Base.Sys.CPUinfo, c2::Base.Sys.CPUinfo)
    for n in fieldnames(Base.Sys.CPUinfo)
        if getfield(c1, n) != getfield(c2, n)
            return false
        end
    end
    true
end
iseq(x::MyUnicodeStruct☺, y::MyUnicodeStruct☺) = (x.α == y.α && x.∂ₓα == y.∂ₓα)
iseq(x::Array{None}, y::Array{None}) = size(x) == size(y)
macro check(fid, sym)
    ex = quote
        let tmp
            try
                tmp = read($fid, $(string(sym)))
            catch e
                warn("Error reading ", $(string(sym)))
                rethrow(e)
            end
            if !iseq(tmp, $sym)
                written = $sym
                error("For ", $(string(sym)), ", read value $tmp does not agree with written value $written")
            end
            written_type = typeof($sym)
            if typeof(tmp) != written_type
                error("For ", $(string(sym)), ", read type $(typeof(tmp)) does not agree with written type $(written_type)")
            end
        end
    end
    esc(ex)
end

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

fn = joinpath(tempdir(),"test.jld")

# Issue #106
module Mod106
bitstype 64 Typ{T}
typ{T}(x::Int64, ::Type{T}) = Base.box(Typ{T}, Base.unbox(Int64,x))
abstract UnexportedT
end
 
fid = jldopen(fn, "w")
println(fn)
@write fid x
@write fid A
@write fid Aarray
@write fid basic_types
@write fid str
@write fid str_unicode
@write fid str_embedded_null
@write fid stringsA
@write fid stringsU
@write fid string16
@write fid strings16
@write fid strings16_2d
@write fid empty_string
@write fid empty_string_array
@write fid empty_array_of_strings
@write fid tf
@write fid TF
@write fid AB
@write fid t
@write fid c
@write fid cint
@write fid C
@write fid emptyA
@write fid emptyB
@write fid ms
@write fid msempty
@write fid sym
@write fid syms
@write fid d
@write fid ex
@write fid T
@write fid Tarr
@write fid char
@write fid unicode_char
@write fid α
@write fid β
@write fid vv
@write fid cpus
@write fid rng
@write fid typevar
@write fid typevar_lb
@write fid typevar_ub
@write fid typevar_lb_ub
@write fid undef
@write fid undefs
@write fid ms_undef
@test_throws JLD2.PointerException @write fid objwithpointer
@write fid bt
@write fid btarray
@write fid sa_asc
@write fid sa_utf8
@write fid subarray
@write fid arr_empty_tuple
@write fid emptyimmutable
@write fid emptytype
@write fid arr_emptyimmutable
@write fid arr_emptytype
@write fid emptyii
@write fid emptyit
@write fid emptyti
@write fid emptytt
@write fid emptyiiotherfield
@write fid emptyiitype
@write fid unicodestruct☺
@write fid array_of_matrices
@write fid tup
@write fid empty_tup
@write fid nonpointerfree_immutable_1
@write fid nonpointerfree_immutable_2
@write fid nonpointerfree_immutable_3
@write fid vague
@write fid bitsunion
@write fid typeunionfield
@write fid genericunionfield
@write fid arr_ref
@write fid obj_ref
@write fid padding_test
@write fid empty_arr_1
@write fid empty_arr_2
@write fid empty_arr_3
@write fid empty_arr_4
@write fid bigdata
@write fid bigfloat
@write fid bigint
@write fid bigfloats
@write fid bigints
@write fid none
@write fid nonearr
@write fid scalar_nothing
@write fid vector_nothing
@write fid Abig
@write fid Bbig
@write fid Sbig
@write fid bitsparamint16
@write fid bitsparamfloat
@write fid bitsparambool
@write fid bitsparamsymbol
@write fid bitsparamint
@write fid bitsparamuint
@write fid tuple_of_tuples
VERSION >= v"0.4.0-dev+4319" && @write fid simplevec
@write fid natyperef

close(fid)

fidr = jldopen(fn, "r")
@check fidr x
@check fidr A
@check fidr Aarray
@check fidr basic_types
@check fidr str
@check fidr str_unicode
@check fidr str_embedded_null
@check fidr stringsA
@check fidr stringsU
@check fidr string16
@check fidr strings16
@check fidr strings16_2d
@check fidr empty_string
@check fidr empty_string_array
@check fidr empty_array_of_strings
@check fidr tf
@check fidr TF
@check fidr AB
@check fidr t
@check fidr c
@check fidr cint
@check fidr C
@check fidr emptyA
@check fidr emptyB
@check fidr ms
@check fidr msempty
@check fidr sym
@check fidr syms
@check fidr d
exr = read(fidr, "ex")   # line numbers are stripped, don't expect equality
checkexpr(ex, exr)
@check fidr T
@check fidr Tarr
@check fidr char
@check fidr unicode_char
@check fidr α
@check fidr β
@check fidr vv
@check fidr cpus
@check fidr rng
@check fidr typevar
@check fidr typevar_lb
@check fidr typevar_ub
@check fidr typevar_lb_ub

# Special cases for reading undefs
undef = read(fidr, "undef")
if !isa(undef, Array{Any, 1}) || length(undef) != 1 || isdefined(undef, 1)
    error("For undef, read value does not agree with written value")
end
undefs = read(fidr, "undefs")
if !isa(undefs, Array{Any, 2}) || length(undefs) != 4 || any(map(i->isdefined(undefs, i), 1:4))
    error("For undefs, read value does not agree with written value")
end
ms_undef = read(fidr, "ms_undef")
if !isa(ms_undef, MyStruct) || ms_undef.len != 0 || isdefined(ms_undef, :data)
    error("For ms_undef, read value does not agree with written value")
end

@check fidr bt
@check fidr btarray
@check fidr sa_asc
@check fidr sa_utf8
@check fidr subarray
@check fidr arr_empty_tuple
@check fidr emptyimmutable
@check fidr emptytype
@check fidr arr_emptyimmutable
@check fidr arr_emptytype
@check fidr emptyii
@check fidr emptyit
@check fidr emptyti
@check fidr emptytt
@check fidr emptyiiotherfield
@check fidr emptyiitype
@check fidr unicodestruct☺
@check fidr array_of_matrices
@check fidr tup
@check fidr empty_tup
@check fidr nonpointerfree_immutable_1
@check fidr nonpointerfree_immutable_2
@check fidr nonpointerfree_immutable_3
vaguer = read(fidr, "vague")
@test typeof(vaguer) == typeof(vague) && vaguer.name == vague.name
@check fidr bitsunion
@check fidr typeunionfield
@check fidr genericunionfield

arr = read(fidr, "arr_ref")
@test arr == arr_ref
@test arr[1] === arr[2]

obj = read(fidr, "obj_ref")
@test obj.x.x === obj.x.y == obj.y.x === obj.y.y
@test obj.x !== obj.y

@check fidr padding_test
@check fidr empty_arr_1
@check fidr empty_arr_2
@check fidr empty_arr_3
@check fidr empty_arr_4
@check fidr bigdata
@check fidr bigfloat
@check fidr bigint
@check fidr bigfloats
@check fidr bigints
@check fidr none
@check fidr nonearr
@check fidr scalar_nothing
@check fidr vector_nothing
@check fidr Abig
@check fidr Bbig
@check fidr Sbig
@check fidr bitsparamfloat
@check fidr bitsparambool
@check fidr bitsparamsymbol
@check fidr bitsparamint
@check fidr bitsparamuint
@check fidr tuple_of_tuples
VERSION >= v"0.4.0-dev+4319" && @check fidr simplevec
@check fidr natyperef

close(fidr)
