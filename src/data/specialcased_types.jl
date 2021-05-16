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

function jltype(f::JLDFile, dt::BasicDatatype)
    if dt.class == DT_STRING
        if (dt.bitfield1 == 0x01 || dt.bitfield1 == 0x11) && dt.bitfield2 == 0x00 && dt.bitfield3 == 0x00
            return FixedLengthString{String}(dt.size)
        else
            throw(UnsupportedFeatureException())
        end
    elseif dt.class == DT_OPAQUE
        error("attempted to read a bare (non-committed) opaque datatype")
    elseif dt.class == DT_REFERENCE
        return ReadRepresentation{Any,RelOffset}()
    else
        throw(UnsupportedFeatureException())
    end
end

function jltype(f::JLDFile, dt::VariableLengthDatatype)
    if dt == H5TYPE_VLEN_UTF8
        return ReadRepresentation{String,Vlen{String}}()
    else
        throw(UnsupportedFeatureException())
    end
end

function h5convert!(out::Pointers, fls::FixedLengthString, f::JLDFile, x, ::JLDWriteSession)
    fls.length == jlsizeof(x) || throw(InvalidDataException())
    (unsafe_copyto!(convert(Ptr{UInt8}, out), pointer(x), fls.length); nothing)
end
h5convert!(out::Pointers, ::Type{Vlen{String}}, f::JLDFile, x, wsession::JLDWriteSession) =
    store_vlen!(out, UInt8, f, unsafe_wrap(Vector{UInt8}, x), wsession)

jlconvert(::ReadRepresentation{String,Vlen{String}}, f::JLDFile, ptr::Ptr, ::RelOffset) =
    String(jlconvert(ReadRepresentation{UInt8,Vlen{UInt8}}(), f, ptr, NULL_REFERENCE))
function jlconvert(rr::FixedLengthString{String}, ::JLDFile, ptr::Ptr, ::RelOffset)
    data = Vector{UInt8}(undef, rr.length)
    unsafe_copyto!(pointer(data), convert(Ptr{UInt8}, ptr), rr.length)
    String(data)
end

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

@inline function odr(::Type{Array{T,N}}) where {T,N}
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

## Dicts

writeas(::Type{Dict{K,V}}) where {K,V} = Vector{Pair{K,V}}
writeas(::Type{IdDict}) = Vector{Pair{Any,Any}}
wconvert(::Type{Vector{Pair{K,V}}}, x::AbstractDict{K,V}) where {K,V} = collect(x)
function rconvert(::Type{T}, x::Vector{Pair{K,V}}) where {T<:AbstractDict,K,V}
    d = T()
    isa(d, Dict) && sizehint!(d::Dict, length(x))
    for (k,v) in x
        d[k] = v
    end
    d
end

## Module
writeas(::Type{Module}) = String
wconvert(::Type{String}, m::Module) = string(m)
rconvert(::Type{Module}, m::String) = eval(Meta.parse(m))

## Methods

import Core: TypeName

mutable struct SerializedMethod
    name::Symbol
    mod::String # Module
    file::Symbol
    line::Int32
    sig::Type #::DataType # Tuple
    slot_syms::String
    nargs::Int32
    isva::Bool
    source::Core.CodeInfo
    nospecialize::Bool
    pure::Bool
end

JLD2.writeas(::Type{Method}) = SerializedMethod

function JLD2.wconvert(::Type{SerializedMethod}, m::Method)
    SerializedMethod(
        m.name,
        wconvert(String, m.module),
        m.file,
        m.line,
        m.sig,
        m.slot_syms,
        m.nargs,
        m.isva,
        Base._uncompressed_ast(m, m.source),
        m.nospecialize,
        m.pure
    )
end

recover_module(mod) = eval(Symbol(mod)) #eval(Meta.parse(mod))

function JLD2.rconvert(::Type{Method}, sm::SerializedMethod)
    m = ccall(:jl_new_method_uninit, Ref{Method}, (Any,), Main)
    m.name = sm.name
    m.module = recover_module(sm.mod)
    m.file = sm.file
    m.line = sm.line
    m.sig = sm.sig
    m.slot_syms = sm.slot_syms
    m.nospecialize = sm.nospecialize
    m.nargs = sm.nargs
    m.isva = sm.isva
    m.source = sm.source
    m.pure = sm.pure
    return m
end


"""
    isanon(t::DataType)::Bool

Test whether a `DataType` belongs to an anonymous function.
Code borrowed from `BSON` which itself is modelled on `Base.Serialize`.
"""
function isanon(t::DataType)
    tn = t.name
    if isdefined(tn, :mt)
      name = tn.mt.name
      mod = tn.module
      return t.super === Function &&
      unsafe_load(Base.unsafe_convert(Ptr{UInt8}, tn.name)) == UInt8('#') &&
      (!isdefined(mod, name) || t != typeof(getfield(mod, name)))
    end
    return false
end



# Contains information about TypeName (for functions)
mutable struct SerializedTypeName
    jlversion::String
    name::Symbol
    names # Core.SimpleVector
    super::DataType
    parameters # Core.SimpleVector
    types   # Core.SimpleVector
    has_instance::Bool
    abstract::Bool
    mutable::Bool
    ninitialized::Bool
end

function SerializedTypeName(t::TypeName)
    primary = Base.unwrap_unionall(t.wrapper)
    SerializedTypeName(
        Base.string(VERSION), t.name, t.names, primary.super, primary.parameters,
        primary.types, isdefined(primary, :instance), primary.abstract,
        primary.mutable, primary.ninitialized)
end

# The method table is typically part of the TypeName. Here, to allow reconstruction
# of anonymous functions without problems, first load `SerializedTypeName` to construct
# a new TypeName + DataType and then load the methods.
mutable struct SerializedMethodTable
    name::Symbol
    methodlist
    max_args::Int
    kwsorter
end

function SerializedMethodTable(mt)
    SerializedMethodTable(mt.name, collect(Base.MethodList(mt)), mt.max_args,
    isdefined(mt, :kwsorter) ? mt.kwsorter : nothing)
end

"""
    new_typename(::Type{TypeName}, st::SerializedTypeName)

Construct a new TypeName given information in form of a `SerializedTypeName`.
"""
function new_typename(::Type{TypeName}, st::SerializedTypeName)
    # TODO: In the future, test for correct julia version
    # Currently not needed if only v1.6 is supported


    tn = ccall(:jl_new_typename_in, Ref{Core.TypeName}, (Any, Any),
                st.name, ReconstructedTypes)

    tn.names = st.names
    ndt = ccall(:jl_new_datatype, Any, (Any, Any, Any, Any, Any, Any, Cint, Cint, Cint),
                tn, tn.module, st.super, st.parameters, st.names, st.types,
                st.abstract, st.mutable, st.ninitialized)
    ty = tn.wrapper = ndt.name.wrapper

    ccall(:jl_set_const, Cvoid, (Any, Any, Any), tn.module, tn.name, ty)
    if has_instance && !isdefined(ty, :instance)
        # use setfield! directly to avoid `fieldtype` lowering expecting to see a Singleton object already on ty
        Core.setfield!(ty, :instance, ccall(:jl_new_struct, Any, (Any, Any...), ty))
    end
    return tn
end

"""
    add_method_table(tn::TypeName, smt::SerializedMethodTable) 

Construct and add new method table to given `TypeName`.
"""
function add_method_table(tn::TypeName, smt::SerializedMethodTable)
    ndt = tn.wrapper
    tn.mt = ccall(:jl_new_method_table, Any, (Any, Any), tn.name, tn.module)
    tn.mt.name = smt.name 
    tn.mt.max_args = smt.max_args
    for def in smt.methodlist
        isdefined(def, :sig) || continue
        # update function name
        def.name = tn.name
        #type is already correctly replaced on instantiate
        #def.sig = Tuple{ndt, def.sig.types[2:end]...}
        ccall(:jl_method_table_insert, Cvoid, (Any, Any, Ptr{Cvoid}), tn.mt, def, C_NULL)
    end
end