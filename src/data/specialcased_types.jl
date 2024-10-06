## Opaque Data
struct OpaqueData{N}
    data::Vector{UInt8}
    OpaqueData(data) = new{length(data)}(data)
end

function jlconvert(rr::ReadRepresentation{OpaqueData{N}, NTuple{N,UInt8}}, ::JLDFile, ptr::Ptr, ::RelOffset) where N
    data = Vector{UInt8}(undef, N)
    unsafe_copyto!(pointer(data), pconvert(Ptr{UInt8}, ptr), N)
    OpaqueData(data)
end


## Strings

const H5TYPE_VLEN_UTF8 = VariableLengthDatatype(DT_VARIABLE_LENGTH, 0x11, 0x01, 0x00,
                                                odr_sizeof(Vlen{UInt8}),
                                                FixedPointDatatype(1, false))

h5fieldtype(::JLDFile, ::Type{String}, ::Type{String}, ::Initialized) =
    H5TYPE_VLEN_UTF8
function h5fieldtype(f::JLDFile, writeas::Type{String},
                     readas::Type, init::Initialized)
    @lookup_committed f readas
    commit(f, h5fieldtype(f, writeas, writeas, init), writeas, readas)
end

fieldodr(::Type{String}, ::Bool) = Vlen{String}

# Stored as variable-length strings
odr_sizeof(x::FixedLengthString{String}) = x.length

h5type(f::JLDFile, writeas::Type{String}, x::String) =
    StringDatatype(typeof(x), jlsizeof(x))
h5type(f::JLDFile, writeas::Type{String}, x) =
    h5fieldtype(f, writeas, typeof(x), Val{true})
odr(::Type{String}) = fieldodr(String, true)
objodr(x::String) = FixedLengthString{String}(jlsizeof(x))

struct NullTerminated end
struct SpacePadded end
struct AsciiString{TERM}
    length::Int
end


struct FixedLengthAsciiString{TERM, N} end

function jltype(f::JLDFile, dt::BasicDatatype)
    class = dt.class
    if class >> 4 == 1
        if class%16 == DT_REFERENCE
            return ReadRepresentation{Any,RelOffset}()
        elseif class%16 == DT_STRING
            if dt.bitfield1 == 0x00 && dt.bitfield2 == 0x00 && dt.bitfield3 == 0x00
                #return AsciiString{NullTerminated}(dt.size)
                return ReadRepresentation{String, FixedLengthAsciiString{NullTerminated, dt.size}}()
            elseif dt.bitfield1 == 0x10 && dt.bitfield2 == 0x00 && dt.bitfield3 == 0x00
                return FixedLengthString{String}(dt.size)
            elseif dt.bitfield1 == 0x02 && dt.bitfield2 == 0x00 && dt.bitfield3 == 0x00
                return ReadRepresentation{String, FixedLengthAsciiString{SpacePadded, dt.size}}()
            else
                throw(UnsupportedFeatureException("Encountered an unsupported string type. $dt"))
            end
        elseif class%16 == DT_OPAQUE
            return ReadRepresentation{OpaqueData{Int(dt.size)},NTuple{Int(dt.size),UInt8}}()

        else
            throw(UnsupportedFeatureException("Encountered an unsupported type."))
        end
    end
    if class%16 == DT_STRING
        if (dt.bitfield1 == 0x01 || dt.bitfield1 == 0x11) && dt.bitfield2 == 0x00 && dt.bitfield3 == 0x00
            return FixedLengthString{String}(dt.size)
        elseif dt.bitfield1 == 0x10 && dt.bitfield2 == 0x00 && dt.bitfield3 == 0x00
            return FixedLengthString{String}(dt.size)
        else
            throw(UnsupportedFeatureException("Encountered an unsupported string type."))
        end
    elseif class%16 == DT_OPAQUE
        return ReadRepresentation{OpaqueData{Int(dt.size)},NTuple{Int(dt.size),UInt8}}()
    elseif class%16 == DT_REFERENCE
        return ReadRepresentation{Any,RelOffset}()
    else
        throw(UnsupportedFeatureException())
    end
end

function jltype(f::JLDFile, dt::VariableLengthDatatype)
    if dt == H5TYPE_VLEN_UTF8
        # this is the fully supported JLD2 string
        return ReadRepresentation{String,Vlen{String}}()
    elseif dt.bitfield1 & 0x1 == 0x1
        # it's some kind of string. Let's try
        return ReadRepresentation{String,Vlen{String}}()
    else#if dt.bitfield1 & 0x1 == 0x0 # it's a sequence
        rr = jltype(f, dt.basetype)
        T = typeof(rr).parameters[1]
        odr = typeof(rr).parameters[2]
        return ReadRepresentation{Vector{T}, Vlen{odr}}()
    end
end

jlconvert(::ReadRepresentation{Vector{T},Vlen{ODR}}, f::JLDFile, ptr::Ptr, ::RelOffset) where {T, ODR} =
    jlconvert(ReadRepresentation{T,Vlen{ODR}}(), f, ptr, UNDEFINED_ADDRESS)


function h5convert!(out::Pointers, fls::FixedLengthString, f::JLDFile, x, ::JLDWriteSession)
    fls.length == jlsizeof(x) || throw(InvalidDataException())
    (unsafe_copyto!(pconvert(Ptr{UInt8}, out), pointer(x), fls.length); nothing)
end
h5convert!(out::Pointers, ::Type{Vlen{String}}, f::JLDFile, x, wsession::JLDWriteSession) =
    store_vlen!(out, UInt8, f, unsafe_wrap(Vector{UInt8}, x), wsession)

jlconvert(::ReadRepresentation{String,Vlen{String}}, f::JLDFile, ptr::Ptr, ::RelOffset) =
    String(jlconvert(ReadRepresentation{UInt8,Vlen{UInt8}}(), f, ptr, NULL_REFERENCE))
function jlconvert(rr::FixedLengthString{String}, ::JLDFile, ptr::Ptr, ::RelOffset)
    data = Vector{UInt8}(undef, rr.length)
    unsafe_copyto!(pointer(data), pconvert(Ptr{UInt8}, ptr), rr.length)
    String(data)
end

# Ascii String
function jlconvert(rr::AsciiString{NullTerminated}, ::JLDFile, ptr::Ptr, ::RelOffset)
    data = Vector{UInt8}(undef, rr.length)
    unsafe_copyto!(pointer(data), pconvert(Ptr{UInt8}, ptr), rr.length)
    String(data[1:end-1])
end
function jlconvert(rr::ReadRepresentation{String, FixedLengthAsciiString{NullTerminated,N}}, ::JLDFile, ptr::Ptr, ::RelOffset) where {N}
    data = Vector{UInt8}(undef, N)
    unsafe_copyto!(pointer(data), pconvert(Ptr{UInt8}, ptr), N)
    String(data)
end

function jlconvert(rr::ReadRepresentation{String, FixedLengthAsciiString{SpacePadded,N}}, ::JLDFile, ptr::Ptr, ::RelOffset) where {N}
    data = Vector{UInt8}(undef, N)
    unsafe_copyto!(pointer(data), pconvert(Ptr{UInt8}, ptr), N)
    rstrip(String(data))
end
odr_sizeof(x::AsciiString) = Int(x.length)
odr_sizeof(x::Type{FixedLengthAsciiString{TERM, N}}) where {TERM, N} = Int(N)::Int





# Used only for custom serialization
constructrr(::JLDFile, ::Type{String}, dt::VariableLengthDatatype{FixedPointDatatype}, ::Vector{ReadAttribute}) =
    dt == H5TYPE_VLEN_UTF8 ?
        (ReadRepresentation{String,Vlen{String}}(), true) :
        throw(UnsupportedFeatureException())

## Symbols

function h5fieldtype(f::JLDFile, ::Type{Symbol}, readas::Type, ::Initialized)
    @lookup_committed f readas
    commit(f, H5TYPE_VLEN_UTF8, Symbol, readas)
end
fieldodr(::Type{Symbol}, ::Bool) = Vlen{String}

h5type(f::JLDFile, ::Type{Symbol}, x) = h5fieldtype(f, Symbol, typeof(x), Val{true})
odr(::Type{Symbol}) = Vlen{String}

function h5convert!(out::Pointers, ::Type{Vlen{String}}, f::JLDFile, x::Symbol, ::JLDWriteSession)
    s = String(x)
    store_vlen!(out, UInt8, f, unsafe_wrap(Vector{UInt8}, s), f.datatype_wsession)
end

constructrr(::JLDFile, ::Type{Symbol}, dt::VariableLengthDatatype{FixedPointDatatype}, ::Vector{ReadAttribute}) =
    dt == H5TYPE_VLEN_UTF8 ? (ReadRepresentation{Symbol,Vlen{String}}(), true) :
                             throw(UnsupportedFeatureException())

jlconvert(::ReadRepresentation{Symbol,Vlen{String}}, f::JLDFile, ptr::Ptr, ::RelOffset) =
    Symbol(jlconvert(ReadRepresentation{UInt8,Vlen{UInt8}}(), f, ptr, NULL_REFERENCE))

## BigInts and BigFloats

writeas(::Union{Type{BigInt},Type{BigFloat}}) = String
wconvert(::Type{String}, x::BigInt) = string(x, base = 62)
wconvert(::Type{String}, x::BigFloat) = string(x)
rconvert(::Type{BigInt}, x::String) = parse(BigInt, x, base = 62)
rconvert(::Type{BigFloat}, x::String) = parse(BigFloat, x)

# BigInts and BigFloats are defined as mutable structs but should be reconstructed like an
# immutable one. This overrides the default behavior.
function jlconvert(::ReadRepresentation{BN,CustomSerialization{String,Vlen{String}}},
        f::JLDFile, ptr::Ptr, header_offset::RelOffset) where BN <: Union{BigInt, BigFloat}
    rconvert(BN, jlconvert(ReadRepresentation{String, Vlen{String}}(), f, ptr, header_offset))
end

## Pointers

# Previously it was disallowed to serialize pointers.
# Due to popular demand and in particular to not error on serializing complex structures
# that contain non-essential pointers this has been changed to instead
# return null pointers.
writeas(::Type{Ptr{T}}) where {T} = Nothing
rconvert(::Type{Ptr{T}}, ::Nothing) where {T} = Ptr{T}(0)

## Arrays

h5fieldtype(::JLDFile, ::Type{T}, ::Type{T}, ::Initialized) where {T<:Array} =
    ReferenceDatatype()
fieldodr(::Type{T}, ::Bool) where {T<:Array} = RelOffset

function odr(A::Type{<:Array})
    T = eltype(A)
    writtenas = writeas(T)
    CustomSerialization(writtenas, T, fieldodr(writtenas, false))
end

# This is all so that when you define a writeas method to write something as an
# array, it writes a reference to the actual array where the datatype is
# committed and has a written_type attribute.
function h5fieldtype(f::JLDFile, ::Type{T}, readas::DataType,
                     ::Initialized) where T<:Array
    @lookup_committed f readas
    commit(f, ReferenceDatatype(), T, readas)
end
h5type(f::JLDFile, ::Type{T}, x) where {T<:Array} =
    h5fieldtype(f, T, typeof(x), Val{true})

function h5type(f::JLDFile, ::Type{T}, ::T) where T<:Array
    if T <: Array{Union{}}
        return ReferenceDatatype()
    end
    ty = T.parameters[1]
    writtenas = writeas(ty)
    if !hasfielddata(writtenas)
        # This is a hacky way to generate an instance of ty
        # the instance isn't actually needed for anything except that inside
        # h5type ty is determined via typeof(x)
        # annoyingly for some types h5type needs the instance
        h5type(f, writtenas, rconvert(ty, newstruct(writtenas)))
    else
        h5fieldtype(f, writtenas, ty, Val{false})
    end
end

_odr(writtenas::Type{T}, readas::Type{T}, odr) where {T<:Array} = odr
_odr(writtenas::Type{T}, readas::DataType, odr) where {T<:Array} =
    CustomSerialization{writtenas,RelOffset}

function constructrr(::JLDFile, ::Type{T}, dt::BasicDatatype,
                     attrs::Vector{ReadAttribute}) where T<:Array
    dt.class == DT_REFERENCE || throw(UnsupportedFeatureException())
    (ReadRepresentation{Array, RelOffset}(), true)
end

## SimpleVectors

writeas(::Type{Core.SimpleVector}) = Vector{Any}
wconvert(::Type{Vector{Any}}, x::Core.SimpleVector) = collect(Any, x)
rconvert(::Type{Core.SimpleVector}, x::Vector{Any}) = Core.svec(x...)

## Standard Dictionary Types
# Custom dictionaries may need to store additional fields
mutable struct SerializedDict
    kvvec # Vector{Pair{K,V}}
end

writeas(::Type{Dict{K,V}}) where {K,V} = SerializedDict
writeas(::Type{IdDict{K,V}}) where {K,V} = SerializedDict
writeas(::Type{Base.ImmutableDict{K,V}}) where {K,V} = SerializedDict

wconvert(::Type{SerializedDict}, x::AbstractDict) = SerializedDict(collect(x))
rconvert(::Type{T}, x::SerializedDict) where {T<:Union{Dict,IdDict,Base.ImmutableDict}} = rconvert(T,x.kvvec)

# These are kept around for legacy
rconvert(::Type{T}, x::Vector{ <: Pair}) where {T<:Union{Dict,IdDict}} = T(x)
function rconvert(::Type{<:Base.ImmutableDict}, x::Vector{Pair{K,V}}) where {K,V}
    @assert !isempty(x)
    d = Base.ImmutableDict(x[1])
    for p in (@view x[2:end])
        d = Base.ImmutableDict(d, p)
    end
    d
end

## NTuples
# Immutable objects are stored as HDF5 structs and inlined into
# parent structures. HDF5 only allows typemax(Unt16) bytes
# for struct description. NTuples are the most common offender for
# exploding struct size. (e.g. in the form of large StaticArrays)
# The definitions below prevent inlining of large NTuples and
# convert to an array instead.
const NTUPLE_INLINE_THRESHOLD = 10

function writeas(NT::Type{NTuple{N,T}}) where {N,T}
    if N > NTUPLE_INLINE_THRESHOLD
        return Vector{T}
    else
        return NT
    end
end

wconvert(::Type{Vector{T}}, x::NTuple{N,T}) where {N,T} = collect(x)
rconvert(::Type{NTuple{N,T}}, x::Vector{T}) where {N,T} = NTuple{N,T}(x)

## Modules
# Modules are stored by name
writeas(::Type{Module}) = String
wconvert(::Type{String}, x::Module) = string(x)
function rconvert(::Type{Module}, x::String)
    pkg = Symbol(x)
    # Try to find the module
    # Start with the method used to find compression libraries
    m =_findmod(pkg)
    isnothing(m) || return Base.loaded_modules[m]
    @info "Encountered reference to module $x, but it is not currently loaded."
    return try
        topimport(pkg)
        Base.loaded_modules[_findmod(pkg)]
    catch
       @warn "Could not load module $x. Returning a dummy module"
       Module(Symbol(x*"_dummy"))
    end
end

function jlconvert(::ReadRepresentation{Module,CustomSerialization{String,Vlen{String}}},
    f::JLDFile, ptr::Ptr, header_offset::RelOffset)
rconvert(Module, jlconvert(ReadRepresentation{String, Vlen{String}}(), f, ptr, header_offset))
end
