# Controls whether tuples and non-pointerfree immutables, which Julia
# stores as references, are stored inline in compound types when
# possible. Currently this is problematic because Julia fields of these
# types may be undefined.
const INLINE_TUPLE = false

const EMPTY_TUPLE_TYPE = Tuple{}
typealias TypesType SimpleVector
typealias TupleType{T<:Tuple} Type{T}
typealias CommitParam Union(Type{Val{false}}, Type{Val{true}})
typetuple(types) = Tuple{types...}

## Helper functions

# Determines whether a specific field type should be saved in the file
hasdata(::Type{Union{}}) = false
hasdata{T}(::Type{Type{T}}) = true
@generated function hasdata{T}(::Type{T})
    if !isleaftype(T)
        true
    elseif sizeof(T) == 0
        false
    elseif isempty(T.types)
        true
    else
        for x in T.types
            x == T || hasdata(x) && return true
        end
        false
    end
end

@generated function sizeof{Offsets,Types}(::OnDiskRepresentation{Offsets,Types})
    Offsets[end]+sizeof(Types.parameters[end])
end

@generated function haspadding{T}(::Type{T})
    isempty(T.types) && return false
    fo = fieldoffsets(T)
    offset = 0
    for i = 1:length(T.types)
        offset != fo[i] && return true
        ty = T.types[i]
        haspadding(ty) && return true
        offset += sizeof(ty)
    end
    return offset != sizeof(T)
end

fieldnames{T<:Tuple}(x::Type{T}) = 1:length(x.types)
fieldnames(x::ANY) = Base.fieldnames(x)

# fieldodr gives the on-disk representation of a field of a given type,
# which is either always initialized (initialized=true) or potentially
# uninitialized (initialized=false)
fieldodr(::Type{Union{}}, ::Bool) = nothing
@generated function fieldodr{T}(::Type{T}, initialized::Bool)
    if isleaftype(T)
        if !hasdata(T)
            # A ghost type or pointer singleton, so no need to store at all
            return nothing
        elseif isa(T, DataType)
            if isbits(T)
                return :(odr(T))
            elseif !T.mutable
                return :(initialized ? odr(T) : Reference)
            end
        end
    end
    Reference
end

# h5fieldtype is fieldodr's HDF5 companion. It should give the HDF5
# datatype reflecting the on-disk representation.
#
# Performance note: we don't care about type inference here at all
# because these functions are only called when constructing type
# representations
h5fieldtype(f::JLDFile, ::Type{Union()}, ::Bool) = nothing
function h5fieldtype(f::JLDFile, T::DataType, initialized::Bool)
    if isleaftype(T)
        if !hasdata(T)
            return nothing
        elseif isbits(T) || (initialized && !T.mutable)
            haskey(f.jlh5type, T) && return f.jlh5type[T]
            if isempty(T.types)
                # bitstype
                return commit(f, OpaqueDatatype(max(1, sizeof(T))), T)
            else
                # Compound type
                return commit_compound(f, fieldnames(T), T)
            end
        end
    end
    ReferenceDatatype()
end
h5fieldtype(f::JLDFile, ::ANY, ::Bool) = ReferenceDatatype()

# odr gives the on-disk representation of a given type
@generated function odr{T}(::Type{T})
    if !hasdata(T)
        # A singleton type, so no need to store at all
        return nothing
    elseif isbits(T) && !haspadding(T)
        # Has a specialized convert method or is an unpadded type
        return T
    end

    offsets = []
    types = []
    offset = 0
    for i = 1:length(T.types)
        fodr = fieldodr(T.types[i], i <= T.ninitialized)
        push!(types, fodr)
        push!(offsets, offset)
        offset += sizeof(fodr)
    end

    OnDiskRepresentation{tuple(offsets...),Tuple{types...}}()
end

# objodr gives the on-disk representation of a given object. This is
# almost always the on-disk representation of the type. The only
# exception is strings, where the length is encoded in the datatype in
# HDF5, but in the object in Julia.
objodr(x) = odr(typeof(x))

# h5type is objodr's HDF5 companion. It should give the HDF5 datatype
# reflecting the on-disk representation
#
# Performance note: this should be inferrable.
function h5type{T}(f::JLDFile, x::T)
    haskey(f.jlh5type, T) && return f.jlh5type[T]
    if !hasdata(T)
        commit(f, OpaqueDatatype(1), T)
    elseif isempty(T.types) # bitstype
        commit(f, OpaqueDatatype(sizeof(T)), T)
    else
        commit_compound(f, fieldnames(T), T)
    end
end

# Make a compound datatype from a set of names and types
function commit_compound(f::JLDFile, names::AbstractVector, T::DataType)
    types = T.types
    h5names = ByteString[]
    offsets = Int[]
    members = H5Datatype[]
    fieldtypes = Reference[]
    hasfieldtype = false

    offset = 0
    for i = 1:length(types)
        !hasdata(types[i]) && continue
        dtype = h5fieldtype(f, types[i], i <= T.ninitialized)
        dtype == nothing && continue
        push!(h5names, string(names[i]))
        if isa(dtype, CommittedDatatype)
            # HDF5 cannot store relationships among committed
            # datatypes. We store these separately in an attribute.
            push!(fieldtypes, Reference(dtype.header_offset))
            dtype = f.datatypes[dtype.index]
            hasfieldtype = true
        else
            push!(fieldtypes, Reference(0))
        end
        push!(members, dtype)
        push!(offsets, offset)
        offset += dtype.size::UInt32
    end
    if offset == 0
        commit(f, OpaqueDatatype(1), T)::CommittedDatatype
    elseif hasfieldtype
        fieldtypeattr = Attribute(:field_datatypes, Dataspace(fieldtypes), ReferenceDatatype(),
                                  Reference, fieldtypes)
        commit(f, CompoundDatatype(offset, h5names, offsets, members), T, fieldtypeattr)::CommittedDatatype
    else
        commit(f, CompoundDatatype(offset, h5names, offsets, members), T)::CommittedDatatype
    end
end

# Write an HDF5 datatype to the file
function commit(f::JLDFile, dtype::H5Datatype, T::Type, attributes::Attribute...)
    io = f.io
    dtdt = h5type(f, isa(T, DataType) ? DataType : isa(T, Union) ? Union{} : throw(ArgumentError("unknown type $T")))
    offset = f.end_of_data

    seek(io, offset)
    id = length(f.datatypes)+1
    cdt = CommittedDatatype(offset, id)
    f.datatype_locations[offset] = cdt
    f.jlh5type[T] = cdt
    f.h5jltype[cdt] = T
    push!(f.datatypes, dtype)

    typeattr = Attribute(:julia_type, Dataspace(T), dtdt, odr(typeof(T)), T)
    commit(f, dtype, tuple(typeattr, attributes...))

    cdt::CommittedDatatype
end

## Serialization of datatypes to JLD
##
## h5fieldtype - gets the H5Datatype corresponding to a given
## Julia type, when the Julia type is stored as an element of an HDF5
## compound type or array. This is the only function that can operate
## on non-leaf types.
##
## h5type - gets the H5Datatype corresponding to an object of the
## given Julia type. For pointerfree types, this is usually the same as
## the h5fieldtype.
##
## h5convert! - converts data from Julia to HDF5 in a buffer. Most
## methods are dynamically generated by gen_h5convert, but methods for
## special built-in types are predefined.
##
## jlconvert - converts data from HDF5 to a Julia object.
##
## jlconvert! - converts data from HDF5 to Julia in a buffer. This is
## only applicable in cases where fields of that type may not be stored
## as references (e.g., not plain types).

## Special types
##
## To create a special serialization of a datatype, one should:
##
## - Define a method of h5fieldtype that dispatches to h5type
## - Define a method of h5type that constructs the type
## - Define h5convert! and jlconvert
## - If the type is an immutable, define jlconvert!

ReadRepresentation{T}(::Type{T}, S) = ReadRepresentation{T,S}()
ReadRepresentation{T}(::Type{T}) = ReadRepresentation{T,T}()

# This construction prevents these methods from getting called on type unions
typealias PrimitiveTypeTypes Union(Type{Int8}, Type{Int16}, Type{Int32}, Type{Int64}, Type{Int128},
                                   Type{UInt8}, Type{UInt16}, Type{UInt32}, Type{UInt64}, Type{UInt128},
                                   Type{Float16}, Type{Float32}, Type{Float64})
typealias PrimitiveTypes     Union(Int8, Int16, Int32, Int64, Int128, UInt8, UInt16, UInt32,
                                   UInt64, UInt128, Float16, Float32, Float64)
h5fieldtype(::JLDFile, T::Union(Type{Int8}, Type{Int16}, Type{Int32}, Type{Int64}, Type{Int128}), ::Bool) =
    FixedPointDatatype(sizeof(T), true)
h5fieldtype(::JLDFile, T::Union(Type{UInt8}, Type{UInt16}, Type{UInt32}, Type{UInt64}, Type{UInt128}), ::Bool) =
    FixedPointDatatype(sizeof(T), false)

function jltype(f::JLDFile, dt::FixedPointDatatype)
    signed = dt.bitfield1 == 0x08 ? true : dt.bitfield1 == 0x00 ? false : throw(UnsupportedFeatureException())
    ((dt.bitfield2 == 0x00) & (dt.bitfield3 == 0x00) & (dt.bitoffset == 0) & (dt.bitprecision == dt.size*8)) || 
        throw(UnsupportedFeatureException())
    if dt.size == 8
        return signed ? ReadRepresentation(Int64) : ReadRepresentation(UInt64)
    elseif dt.size == 4
        return signed ? ReadRepresentation(Int32) : ReadRepresentation(UInt32)
    elseif dt.size == 2
        return signed ? ReadRepresentation(Int8) : ReadRepresentation(UInt32)
    elseif dt.size == 1
        return signed ? ReadRepresentation(Int16) : ReadRepresentation(UInt16)
    elseif dt.size == 16
        return signed ? ReadRepresentation(Int128) : ReadRepresentation(UInt128)
    else
        throw(UnsupportedFeatureException())
    end
end

h5fieldtype(::JLDFile, ::Type{Float16}, ::Bool) =
    FloatingPointDatatype(DT_FLOATING_POINT, 0x20, 0x0f, 0x00, 2, 0, 16, 10, 5, 0, 10, 0x0000000f)
h5fieldtype(::JLDFile, ::Type{Float32}, ::Bool) =
    FloatingPointDatatype(DT_FLOATING_POINT, 0x20, 0x1f, 0x00, 4, 0, 32, 23, 8, 0, 23, 0x0000007f)
h5fieldtype(::JLDFile, ::Type{Float64}, ::Bool) =
    FloatingPointDatatype(DT_FLOATING_POINT, 0x20, 0x3f, 0x00, 8, 0, 64, 52, 11, 0, 52, 0x000003ff)

function jltype(f::JLDFile, dt::FloatingPointDatatype)
    if dt == h5type(f, Float64)
        return ReadRepresentation(Float64)
    elseif dt == h5type(f, Float32)
        return ReadRepresentation(Float32)
    elseif dt == h5type(f, Float16)
        return ReadRepresentation(Float16)
    else
        throw(UnsupportedFeatureException())
    end
end

h5type(f::JLDFile, x::PrimitiveTypes) = h5fieldtype(f, typeof(x), true)

## Routines for references

# A hack to prevent us from needing to box the output pointer
# XXX not thread-safe!
const BOXED_PTR = Ref{Ptr{Void}}()
function h5convert!(out::Ptr, odr::Type{Reference}, f::JLDFile, x::Reference, wsession::JLDWriteSession)
    (unsafe_store!(convert(Ptr{Reference}, out), x); nothing)
end
@inline function h5convert!(out::Ptr, odr::Type{Reference}, f::JLDFile, x::ANY, wsession::JLDWriteSession)
    BOXED_PTR[] = out
    h5convert_with_boxed_ptr!(f, x, wsession)
end
@noinline function h5convert_with_boxed_ptr!(f::JLDFile, x, wsession::JLDWriteSession)
    unsafe_store!(convert(Ptr{Reference}, BOXED_PTR[]), write_ref(f, wsession, x)); nothing
end
h5convert_uninitialized!(out::Ptr, odr::Type{Reference}) =
    (unsafe_store!(convert(Ptr{Reference}, out), Reference(0)); nothing)

## Routines for variable-length datatypes

immutable Vlen{T}
    size::UInt32
    id::GlobalHeapID
end
sizeof{T<:Vlen}(::Type{T}) = 4 + sizeof(GlobalHeapID)

# Write variable-length data and store the offset and length to out pointer
function writevlen!(out::Ptr, odr, f::JLDFile, x::AbstractVector, wsession::JLDWriteSession)
    unsafe_store!(convert(Ptr{UInt32}, out), length(x))
    unsafe_store!(convert(Ptr{GlobalHeapID}, out)+4, write_heap_object(f, odr, x, wsession))
    nothing
end
h5convert!{T}(out::Ptr, ::Type{Vlen{T}}, f::JLDFile, x, wsession::JLDWriteSession) =
    writevlen!(out, T, f, x, wsession)

# Read variable-length data given offset and length in ptr
readvlen{T}(f::JLDFile, ptr::Ptr, ::Type{T}) =
    read_heap_object(f, unsafe_load(convert(Ptr{GlobalHeapID}, ptr+4)), T)
jlconvert{T,S}(::ReadRepresentation{T,Vlen{S}}, f::JLDFile, ptr::Ptr) =
    convert(T, readvlen(f, ptr, S))

h5convert_uninitialized!{T<:Vlen}(out::Ptr, odr::Type{T}) =
    (unsafe_store!(convert(Ptr{Int128}, out), 0); nothing)

## ByteStrings

const VLEN_ASCII = VariableLengthDatatype(DT_VARIABLE_LENGTH, 0x11, 0x00, 0x00,
                                          sizeof(Vlen{UInt8}),
                                          FixedPointDatatype(1, false))
const VLEN_UTF8 = VariableLengthDatatype(DT_VARIABLE_LENGTH, 0x11, 0x01, 0x00,
                                         sizeof(Vlen{UInt8}),
                                         FixedPointDatatype(1, false))

h5fieldtype(::JLDFile, ::Type{ASCIIString}, ::Bool) = VLEN_ASCII
h5fieldtype(::JLDFile, ::Type{UTF8String}, ::Bool) = VLEN_UTF8
h5fieldtype(::JLDFile, ::Type{ByteString}, ::Bool) = VLEN_UTF8
fieldodr(::Type{ASCIIString}, ::Bool) = Vlen{ASCIIString}
fieldodr(::Union(Type{UTF8String}, Type{ByteString}), ::Bool) = Vlen{UTF8String}

# Stored as variable-length strings
immutable FixedLengthString{T<:ByteString}
    length::Int
end
sizeof(x::FixedLengthString) = x.length

h5type(f::JLDFile, x::ByteString) = StringDatatype(typeof(x), length(x))
odr{T<:ByteString}(::Type{T}) = error("cannot call odr on a ByteString type")
objodr(x::ByteString) = FixedLengthString{typeof(x)}(length(x))

function jltype(f::JLDFile, dt::VariableLengthDatatype)
    if dt == VLEN_ASCII
        return ReadRepresentation(ASCIIString, Vlen{ASCIIString})
    elseif dt == VLEN_UTF8
        return ReadRepresentation(UTF8String, Vlen{UTF8String})
    else
        throw(UnsupportedFeatureException())
    end
end

h5convert!(out::Ptr, ::FixedLengthString, f::JLDFile, x, ::JLDWriteSession) =
    (unsafe_copy!(convert(Ptr{UInt8}, out), pointer(x.data), length(x.data)); nothing)
h5convert!{T<:ByteString}(out::Ptr, ::Type{Vlen{T}}, f::JLDFile, x, wsession::JLDWriteSession) =
    writevlen!(out, UInt8, f, x.data, wsession)
jlconvert{T,S<:ByteString}(::Union(ReadRepresentation{T,S}, ReadRepresentation{T,S}), f::JLDFile, ptr::Ptr) =
    convert(T, readvlen(f, ptr, UInt8))
jlconvert(T::ReadRepresentation{ByteString,Vlen{UTF8String}}, file::JLDFile, ptr::Ptr) =
    bytestring(readvlen(file, ptr, UInt8))

## UTF16Strings

h5fieldtype(f::JLDFile, ::Type{UTF16String}, ::Bool) = 
    haskey(f.jlh5type, UTF16String) ? f.jlh5type[UTF16String] :
                                      commit(f, VariableLengthDatatype(h5fieldtype(f, UInt16, true)), UTF16String)
fieldodr(::Type{UTF16String}, ::Bool) = Vlen{UInt16}

h5type(f::JLDFile, ::UTF16String) = h5fieldtype(f, UTF16String, true)
odr(::Type{UTF16String}) = Vlen{UInt16}

h5convert!(out::Ptr, ::Type{Vlen{UInt16}}, f::JLDFile, x::UTF16String, wsession::JLDWriteSession) =
    writevlen!(out, UInt16, f, x.data, wsession)

## Symbols

hasdata(::Type{Symbol}) = true

h5fieldtype(f::JLDFile, ::Type{Symbol}, ::Bool) = 
    haskey(f.jlh5type, Symbol) ? f.jlh5type[Symbol] : commit(f, h5fieldtype(f, UTF8String, true), Symbol)
fieldodr(::Type{Symbol}, ::Bool) = Vlen{UTF8String}

# Stored as variable-length
h5type(f::JLDFile, ::Symbol) = h5fieldtype(f, Symbol, true)
odr(::Type{Symbol}) = Vlen{UTF8String}

h5convert!(out::Ptr, ::Type{Vlen{UTF8String}}, f::JLDFile, x::Symbol, ::JLDWriteSession) =
    writevlen!(out, UInt8, f, string(x).data, f.datatype_wsession)
jlconvert(::ReadRepresentation{Symbol,Vlen{UTF8String}}, f::JLDFile, ptr::Ptr) = symbol(readvlen(f, ptr, UInt8))

## BigInts and BigFloats

h5fieldtype(f::JLDFile, T::Union(Type{BigInt}, Type{BigFloat}), ::Bool) =
    haskey(f.jlh5type, T) ? f.jlh5type[T] : commit(f, h5fieldtype(f, ASCIIString, true), T)
fieldodr(::Union(Type{BigInt}, Type{BigFloat}), ::Bool) = Vlen{ASCIIString}

# Stored as a variable-length string
h5type(f::JLDFile, x::Union(BigInt, BigFloat)) = h5fieldtype(f, typeof(x), true)
odr(::Union(Type{BigInt}, Type{BigFloat})) = Vlen{ASCIIString}

h5convert!(out::Ptr, ::Type{Vlen{ASCIIString}}, f::JLDFile, x::BigInt, wsession::JLDWriteSession) =
    writevlen!(out, UInt8, f, base(62, x).data, wsession)
h5convert!(out::Ptr, ::Type{Vlen{ASCIIString}}, f::JLDFile, x::BigFloat, wsession::JLDWriteSession) =
    writevlen!(out, UInt8, f, string(x).data, wsession)

jlconvert(::ReadRepresentation{BigInt,Vlen{ASCIIString}}, f::JLDFile, ptr::Ptr) =
    parse(BigInt, ASCIIString(readvlen(f, ptr, UInt8)), 62)
jlconvert(::ReadRepresentation{BigFloat,Vlen{ASCIIString}}, f::JLDFile, ptr::Ptr) =
    parse(BigFloat, ASCIIString(readvlen(f, ptr, UInt8)))

## DataTypes

const H5TYPE_DATATYPE = CompoundDatatype(
    sizeof(Vlen{UTF8String})+sizeof(Vlen{Reference}),
    ["name", "parameters"],
    [0, sizeof(Vlen{UTF8String})],
    [VLEN_UTF8, VariableLengthDatatype(ReferenceDatatype())]
)
typealias DataTypeODR OnDiskRepresentation{(0,sizeof(Vlen{UTF8String})),Tuple{Vlen{UTF8String},Vlen{Reference}}}

function h5fieldtype(f::JLDFile, ::Type{DataType}, ::Bool)
    haskey(f.jlh5type, DataType) && return f.jlh5type[DataType]
    io = f.io
    offset = f.end_of_data

    seek(io, offset)
    id = length(f.datatypes)+1
    cdt = CommittedDatatype(offset, id)
    f.datatype_locations[offset] = cdt
    f.jlh5type[DataType] = cdt
    f.h5jltype[cdt] = DataType
    push!(f.datatypes, cdt)

    commit(f, H5TYPE_DATATYPE, (Attribute(:julia_type, Dataspace(DataType), cdt, odr(DataType), DataType),))

    cdt
end
fieldodr{T<:DataType}(::Type{T}, ::Bool) = DataTypeODR()

h5type(f::JLDFile, ::DataType) = h5fieldtype(f, DataType, true)
odr{T<:DataType}(::Type{T}) = DataTypeODR()

function h5convert!(out::Ptr, ::DataTypeODR, f::JLDFile, T::DataType, wsession::JLDWriteSession)
    tn = Symbol[]
    m = T.name.module
    while m != module_parent(m)
        push!(tn, module_name(m))
        m = module_parent(m)
    end
    reverse!(tn)
    push!(tn, T.name.name)
    writevlen!(out, UInt8, f, join(tn, ".").data, f.datatype_wsession)
    if !isempty(T.parameters)
        writevlen!(out+sizeof(Vlen{UInt8}), Reference, f, Reference[write_ref(f, wsession, x) for x in T.parameters], f.datatype_wsession)
    end
    nothing
end
jlconvert{T<:Type}(::Type{T}, file::JLDFile, ptr::Ptr) =
    julia_type(UTF8String(readvlen(f, ptr, UInt8)))

## Union Types

function h5fieldtype(f::JLDFile, ::Type{Union}, ::Bool)
    haskey(f.jlh5type, Union) && return f.jlh5type[Union]
    commit(f, VariableLengthDatatype(H5TYPE_DATATYPE), Union)
end
fieldodr{T<:Union}(::Type{T}, ::Bool) = Vlen{DataTypeODR()}

h5type(f::JLDFile, ::Union) = h5fieldtype(f, Union, true)
odr{T<:Union}(::Type{T}) = fieldodr(Union, true)

h5convert!(out::Ptr, ::Type{Vlen{DataTypeODR()}}, f::JLDFile, x::Union, wsession::JLDWriteSession) =
    writevlen!(out, DataTypeODR(), f, DataType[x for x in x.types], wsession)

## Pointers

immutable PointerException <: Exception; end
show(io::IO, ::PointerException) = print(io, "cannot write a pointer to JLD file")
h5fieldtype{T<:Ptr}(::JLDFile, ::Type{T}, ::Bool) = throw(PointerException())
h5type(::JLDFile, ::Ptr) = throw(PointerException())

## Arrays

# These show up as having T.size == 0, hence the need for
# specialization.
hasdata{T<:Array}(::Type{T}) = true

h5fieldtype{T<:Array}(::JLDFile, ::Type{T}, ::Bool) = ReferenceDatatype()
fieldodr{T<:Array}(::Type{T}, ::Bool) = Reference

@generated function h5type{T}(f::JLDFile, ::Array{T})
    if !hasdata(T)
        quote
            haskey(f.jlh5type, T) && return f.jlh5type[T]
            return commit(f, OpaqueDatatype(1), T)
        end
    else
        :(h5fieldtype(f, T, false))
    end
end
odr{T,N}(::Type{Array{T,N}}) = fieldodr(T, false)

## User-defined types
##
## Similar to special types, but h5convert!/jl_convert are dynamically
## generated.

## Tuples
@generated function jlconvert(T::TupleType, file::JLDFile, ptr::Ptr)
    ex = Expr(:block)
    args = ex.args
    tup = Expr(:tuple)
    tupargs = tup.args
    types = T.types
    for i = 1:length(types)
        h5offset = dtype.offsets[i]
        field = symbol(string("field", i))

        if dtype.members[i] == ReferenceDatatype()
            push!(args, :($field = read_ref(file, unsafe_load(convert(Ptr{Reference}, ptr)+$h5offset))))
        else
            push!(args, :($field = jlconvert($(types[i]), file, ptr+$h5offset)))
        end
        push!(tupargs, field)
    end

    :($ex; $tup)
end

## All other objects

# For cases not defined above: If the type is mutable and non-empty,
# this is a reference. If the type is immutable, this is a type itself.
@generated function jlconvert{T}(::Type{T}, file::JLDFile, ptr::Ptr)
    if isempty(fieldnames(T))
        # Bitstypes
        if T.size == 0
            :($T())
        else
            :(_jlconvert_bits($T, ptr))
        end
    elseif T.size == 0
        # Empty types/immutables
        :(ccall(:jl_new_struct_uninit, Any, (Any,), $T)::$T)
    else
        dtype = make_compound(parent, 1:length(T.types), T)
        ex = Expr(:block)
        args = ex.args
        if T.mutable
            # Types
            fn = fieldnames(T)
            for i = 1:length(dtype.offsets)
                h5offset = dtype.offsets[i]

                if dtype.members[i] == ReferenceDatatype()
                    push!(args, quote
                        ref = unsafe_load(convert(Ptr{Reference}, ptr)+$h5offset)
                        if ref != Reference(0)
                            out.$(fn[i]) = convert($(T.types[i]), read_ref(file, ref))
                        end
                    end)
                else
                    push!(args, :(out.$(fn[i]) = jlconvert($(T.types[i]), file, ptr+$h5offset)))
                end
            end
            quote
                out = ccall(:jl_new_struct_uninit, Any, (Any,), $T)::$T
                $ex
                out
            end
        else
            # Immutables
            if T.pointerfree
                quote
                    out = Array($T, 1)
                    jlconvert!(pointer(out), $T, file, ptr)
                    out[1]
                end
            else
                for i = 1:length(dtype.offsets)
                    h5offset = typeinfo.offsets[i]
                    obj = gensym("obj")
                    if dtype.members[i] == ReferenceDatatype()
                        push!(args, quote
                            ref = unsafe_load(convert(Ptr{Reference}, ptr)+$h5offset)
                            if ref != Reference(0)
                                ccall(:jl_set_nth_field, Void, (Any, Csize_t, Any), out, $(i-1), convert($(T.types[i]), read_ref(file, ref)))
                            end
                        end)
                    else
                        push!(args, :(ccall(:jl_set_nth_field, Void, (Any, Csize_t, Any), out, $(i-1), jlconvert($(T.types[i]), file, ptr+$h5offset))))
                    end
                end
                @eval function jlconvert(::Type{$T}, file::JLDFile, ptr::Ptr)
                    out = ccall(:jl_new_struct_uninit, Any, (Any,), $T)::$T
                    $ex
                    out
                end
            end
        end
    end
end

@generated function jlconvert!{T}(::Type{T}, file::JLDFile, ptr::Ptr)
    if isempty(fieldnames(T))
        if T.size == 0
            !T.mutable ? nothing :
                :(unsafe_store!(convert(Ptr{Ptr{Void}}, out), pointer_from_objref($T())))
        else
            :(_jlconvert_bits!(out, $T, ptr))
        end
    elseif T.size == 0
        nothing
    elseif !isbits(T)
        error("attempted to call jlconvert! on non-isbits type $T")
    else
        dtype = make_compound(parent, 1:length(T.types), T)
        ex = Expr(:block)
        args = ex.args
        jloffsets = fieldoffsets(T)
        for i = 1:length(dtype.offsets)
            h5offset = dtype.offsets[i]
            jloffset = jloffsets[i]
            push!(args, :(jlconvert!(out+$jloffset, $(T.types[i]), file, ptr+$h5offset)))
        end
        push!(args, nothing)
        ex
    end
end

jlconvert{T}(::ReadRepresentation{T,T}, ::JLDFile, ptr::Ptr) = unsafe_load(convert(Ptr{T}, ptr))
jlconvert!{T}(out::Ptr, ::ReadRepresentation{T,T}, ::JLDFile, ptr::Ptr) =
    (unsafe_store!(convert(Ptr{T}, out), unsafe_load(convert(Ptr{T}, ptr))); nothing)

## Common functions for all non-special types (including h5convert!)

unknown_type_err(T) =
    error("""$T is not of a type supported by JLD
             Please report this error at https://github.com/timholy/HDF5.jl""")

@generated function h5convert!(out::Ptr, odr::OnDiskRepresentation, file::JLDFile, x, wsession::JLDWriteSession)
    T = x
    offsets, members = odr.parameters

    getindex_fn = isa(T, TupleType) ? (:getindex) : (:getfield)
    ex = Expr(:block)
    args = ex.args
    for i = 1:length(offsets)
        offset = offsets[i]
        member = members.parameters[i]
        if member == Reference && i > T.ninitialized
            push!(args, :(h5convert_uninitialized!(out+$offset, $(member))))
        else
            push!(args, :(h5convert!(out+$offset, $(member), file, $getindex_fn(x, $i), wsession)))
        end
    end
    push!(args, nothing)
    ex
end

# In some cases, we save a datatype but don't store any data (because
# it's a pointer singleton or ghost)
h5convert!(::Ptr, ::Type{Union{}}, ::JLDFile, ::ANY, ::JLDWriteSession) = nothing

# All remaining are just unsafe_store! calls
h5convert!{T}(out::Ptr, ::Type{T}, ::JLDFile, x::T, ::JLDWriteSession) =
    (unsafe_store!(convert(Ptr{typeof(x)}, out), x); nothing)

## Find the corresponding Julia type for a given HDF5 type

# Type mapping function. Given an HDF5Datatype, find (or construct) the
# corresponding Julia type.
function jldatatype(parent::JLDFile, dtype::H5Datatype)
    class_id = HDF5.h5t_get_class(dtype.id)
    if class_id == HDF5.H5T_STRING
        cset = HDF5.h5t_get_cset(dtype.id)
        if cset == HDF5.H5T_CSET_ASCII
            return ASCIIString
        elseif cset == HDF5.H5T_CSET_UTF8
            return UTF8String
        else
            error("character set ", cset, " not recognized")
        end
    elseif class_id == HDF5.H5T_INTEGER || class_id == HDF5.H5T_FLOAT
        # This can be a performance hotspot
        HDF5.h5t_equal(dtype.id, HDF5.H5T_NATIVE_DOUBLE) > 0 && return Float64
        HDF5.h5t_equal(dtype.id, HDF5.H5T_NATIVE_INT64) > 0 && return Int64
        HDF5.h5t_equal(dtype.id, HDF5.H5T_NATIVE_FLOAT) > 0 && return Float32
        HDF5.h5t_equal(dtype.id, HDF5.H5T_NATIVE_INT32) > 0 && return Int32
        HDF5.h5t_equal(dtype.id, HDF5.H5T_NATIVE_UINT8) > 0 && return UInt8
        HDF5.h5t_equal(dtype.id, HDF5.H5T_NATIVE_UINT64) > 0 && return UInt64
        HDF5.h5t_equal(dtype.id, HDF5.H5T_NATIVE_UINT32) > 0 && return UInt32
        HDF5.h5t_equal(dtype.id, HDF5.H5T_NATIVE_INT8) > 0 && return Int8
        HDF5.h5t_equal(dtype.id, HDF5.H5T_NATIVE_INT16) > 0 && return Int16
        HDF5.h5t_equal(dtype.id, HDF5.H5T_NATIVE_UINT16) > 0 && return UInt16
        error("unrecognized integer or float type")
    elseif class_id == HDF5.H5T_COMPOUND || class_id == HDF5.H5T_OPAQUE
        addr = HDF5.objinfo(dtype).addr
        haskey(parent.h5jltype, addr) && return parent.h5jltype[addr]

        typename = a_read(dtype, name_type_attr)
        T = julia_type(typename)
        if T == UnsupportedType
            warn("type $typename not present in workspace; reconstructing")
            T = reconstruct_type(parent, dtype, typename)
        end

        if !(T in BUILTIN_TYPES)
            # Call jldatatype on dependent types to validate them and
            # define jlconvert
            if class_id == HDF5.H5T_COMPOUND
                for i = 0:HDF5.h5t_get_nmembers(dtype.id)-1
                    member_name = HDF5.h5t_get_member_name(dtype.id, i)
                    idx = rsearchindex(member_name, "_")
                    if idx != sizeof(member_name)
                        member_dtype = HDF5.t_open(parent.plain, string(pathtypes, '/', lpad(member_name[idx+1:end], 8, '0')))
                        jldatatype(parent, member_dtype)
                    end
                end
            end

            gen_jlconvert(JldTypeInfo(parent, T, false), T)
        end

        # Verify that types match
        newtype = h5type(parent, T, false).dtype
        dtype == newtype || throw(TypeMismatchException(typename))

        # Store type in type index
        index = typeindex(parent, addr)
        parent.jlh5type[T] = H5Datatype(dtype, index)
        parent.h5jltype[addr] = T
        T
    else
        error("unrecognized HDF5 datatype class ", class_id)
    end
end

# Create a Julia type based on the HDF5Datatype from the file. Used
# when the type is no longer available.
function reconstruct_type(parent::JLDFile, dtype::H5Datatype, savedname::AbstractString)
    name = gensym(savedname)
    class_id = HDF5.h5t_get_class(dtype.id)
    if class_id == HDF5.H5T_OPAQUE
        if exists(dtype, "empty")
            @eval (immutable $name; end; $name)
        else
            sz = Int(HDF5.h5t_get_size(dtype.id))*8
            @eval (bitstype $sz $name; $name)
        end
    else
        # Figure out field names and types
        nfields = HDF5.h5t_get_nmembers(dtype.id)
        fieldnames = Array(Symbol, nfields)
        fieldtypes = Array(Type, nfields)
        for i = 1:nfields
            membername = HDF5.h5t_get_member_name(dtype.id, i-1)
            idx = rsearchindex(membername, "_")
            fieldname = fieldnames[i] = symbol(membername[1:idx-1])

            if idx != sizeof(membername)
                # There is something past the underscore in the HDF5 field
                # name, so the type is stored in file
                memberdtype = HDF5.t_open(parent.plain, string(pathtypes, '/', lpad(membername[idx+1:end], 8, '0')))
                fieldtypes[i] = jldatatype(parent, memberdtype)
            else
                memberclass = HDF5.h5t_get_member_class(dtype.id, i-1)
                if memberclass == HDF5.H5T_REFERENCE
                    # Field is a reference, so use Any
                    fieldtypes[i] = Any
                else
                    # Type is built-in
                    memberdtype = HDF5Datatype(HDF5.h5t_get_member_type(dtype.id, i-1), parent.plain)
                    fieldtypes[i] = jldatatype(parent, memberdtype)
                end
            end
        end

        if startswith(savedname, "(") || startswith(savedname, "Core.Tuple{")
            # We're reconstructing a tuple
            typetuple(fieldtypes)
        else
            # We're reconstructing some other type
            @eval begin
                immutable $name
                    $([:($(fieldnames[i])::$(fieldtypes[i])) for i = 1:nfields]...)
                end
                $name
            end
        end
    end
end
