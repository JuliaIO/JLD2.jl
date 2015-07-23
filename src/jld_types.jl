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

## Generic machinery

# Carries the type and on-disk representation of data to be read from
# the disk
ReadRepresentation(T, S) = ReadRepresentation{T,S}()
ReadRepresentation(T) = ReadRepresentation{T,T}()
sizeof{T,S}(::ReadRepresentation{T,S}) = sizeof(S)

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
            (x <: T && T <: x) || hasdata(x) && return true
        end
        false
    end
end

# Gets the size of an on-disk representation
@generated function sizeof{Offsets,Types,ODRs}(::OnDiskRepresentation{Offsets,Types,ODRs})
    Offsets[end]+sizeof(ODRs.parameters[end])
end

# Determines whether a type has padding and thus needs special handling
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
                return commit(f, OpaqueDatatype(sizeof(T)), T)
            else
                # Compound type
                return commit_compound(f, fieldnames(T), T)
            end
        end
    end
    ReferenceDatatype()
end
h5fieldtype(f::JLDFile, ::ANY, ::Bool) = ReferenceDatatype()

# odr gives the on-disk representation of a given type, similar to
# fieldodr, but actually encoding the data for things that odr stores
# as references
@generated function odr{T}(::Type{T})
    if !hasdata(T)
        # A singleton type, so no need to store at all
        return nothing
    elseif isbits(T) && !haspadding(T)
        # Has a specialized convert method or is an unpadded type
        return T
    end

    offsets = zeros(Int, length(T.types))
    odrs = Array(Any, length(T.types))
    offset = 0
    for i = 1:length(T.types)
        fodr = fieldodr(T.types[i], i <= T.ninitialized)
        offsets[i] = offset
        odrs[i] = fodr
        offset += sizeof(fodr)
    end

    OnDiskRepresentation{tuple(offsets...),Tuple{T.types...},Tuple{odrs...}}()
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

    @assert offset != 0
    if hasfieldtype
        fieldtypeattr = WrittenAttribute(:field_datatypes, Dataspace(f, fieldtypes, DataType), ReferenceDatatype(),
                                         Reference, fieldtypes)
        commit(f, CompoundDatatype(offset, h5names, offsets, members), T, fieldtypeattr)::CommittedDatatype
    else
        commit(f, CompoundDatatype(offset, h5names, offsets, members), T)::CommittedDatatype
    end
end

# Write an HDF5 datatype to the file
function commit(f::JLDFile, dtype::H5Datatype, T::DataType, attributes::WrittenAttribute...)
    io = f.io

    # This needs to be written this way or type inference gets unhappy...
    # Also needs to happen here so that we write the DataType type
    # before we try to find where this type will be written
    typeattr = WrittenAttribute(:julia_type, Dataspace(f, DataType, odr(DataType)), h5type(f, DataType), odr(DataType), T)

    offset = f.end_of_data

    seek(io, offset)
    id = length(f.datatypes)+1
    cdt = CommittedDatatype(offset, id)
    f.datatype_locations[offset] = cdt
    f.jlh5type[T] = cdt
    f.h5jltype[cdt] = ReadRepresentation(T, odr(T))
    push!(f.datatypes, dtype)

    commit(f, dtype, tuple(typeattr, attributes...))

    cdt::CommittedDatatype
end

# jltype is the inverse of h5type, providing a ReadRepresentation for an
# H5Datatype. We handle committed datatypes here, and other datatypes below.
function jltype(f::JLDFile, cdt::CommittedDatatype)
    haskey(f.h5jltype, cdt) && return f.h5jltype[cdt]
    dt, attrs = read_committed_datatype(f, cdt)

    julia_type_attr = nothing
    field_datatypes_attr = nothing
    for attr in attrs
        if attr.name == :julia_type
            julia_type_attr = attr
        elseif attr.name == :field_datatypes
            field_datatypes_attr = attr
        end
    end

    if isa(julia_type_attr, Void)
        throw(InvalidDataException())
    end
    julia_type_attr = julia_type_attr::ReadAttribute

    # If type of datatype is this datatype, then this is the committed
    # datatype that describes a datatype
    if julia_type_attr.datatype_offset == cdt.header_offset
        # Verify that the datatype matches our expectations
        if dt != H5TYPE_DATATYPE
            error("""The HDF5 datatype representing a Julia datatype does not match
                     the expectations of this version of JLD.

                     You may need to update JLD to read this file.""")
        end
        f.jlh5type[DataType] = cdt
        f.datatypes[cdt.index] = dt
        return (f.h5jltype[cdt] = ReadRepresentation(DataType, DataTypeODR()))
    end

    # datatype = read_data(f, julia_type_attr, H5TYPE_DATATYPE, ReadRepresentation(DataType, DataTypeODR()))
    datatype = read_data(f, julia_type_attr)
    rr, canonical = constructrr(f, datatype, dt, field_datatypes_attr)
    rr = rr::ReadRepresentation

    canonical && (f.jlh5type[datatype] = cdt)
    f.datatypes[cdt.index] = dt
    f.h5jltype[cdt] = rr
end

# Constructs a ReadRepresentation for a given opaque (bitstype) type
function constructrr(::JLDFile, T::DataType, dt::BasicDatatype, ::Void)
    dt.class == DT_OPAQUE || throw(UnsupportedFeatureException())
    if sizeof(T) == dt.size
        (ReadRepresentation(T), true)
    elseif !hasdata(T) && dt.size == 1
        (ReadRepresentation(T, nothing), true)
    else
        warn("bitstype $T has size $(sizeof(T)), but written type has size $(dt.size); reconstructing")
        (ReadRepresentation(reconstruct_bitstype(T.name.name, dt.size)), true)
    end
end

# Constructs a ReadRepresentation for a given compound type
function constructrr(f::JLDFile, T::DataType, dt::CompoundDatatype, field_datatypes_attr::Union{ReadAttribute,Void})
    # Map names in dt to their indices
    dtnames = Dict{ByteString,Int}()
    for i = 1:length(dt.names)
        dtnames[dt.names[i]] = i
    end
    mapped = falses(length(dt.names))

    # Read field_datatypes_attr if it exists
    if !isa(field_datatypes_attr, Void)
        refs = read_data(f, field_datatypes_attr, ReferenceDatatype(), ReadRepresentation(Reference))
    end

    offsets = Array(Int, length(T.types))
    types = Array(Any, length(T.types))
    odrs = Array(Any, length(T.types))
    fn = fieldnames(T)
    for i = 1:length(T.types)
        wstype = T.types[i]
        if !hasdata(wstype)
            offsets[i] = i == 1 ? 0 : offsets[i-1] + sizeof(types[i-1])
            types[i] = nothing
        else
            stringfield = string(fn[i])
            if !haskey(dtnames, stringfield)
                warn("saved type $T is missing field $stringfield in workspace type; reconstructing")
                return reconstruct_compound(T.name.name, dt, field_datatypes_attr)
            end

            dtindex = dtnames[stringfield]
            if !isa(field_datatypes_attr, Void) && (ref = refs[dtindex]) != Reference(0)
                dtrr = jltype(f, f.datatype_locations[ref.offset])
            else
                dtrr = jltype(f, dt.members[i])
            end

            readtype, odr = typeof(dtrr).parameters

            if typeintersect(readtype, wstype) === Union{} &&
               !method_exists(convert, Tuple{Type{wstype}, readtype})
                # Saved type does not match type in workspace and no
                # convert method exists, so we definitely need to
                # reconstruct
                warn("saved type $T has field $(stringfield)::$(readtype), but workspace type has field $(stringfield)::$(wstype), and no applicable convert method exists; reconstructing""")
                return reconstruct_compound(T.name.name, dt, field_datatypes_attr)
            end

            types[i] = readtype
            odrs[i] = odr
            offsets[i] = dt.offsets[dtindex]

            mapped[dtindex] = true
        end
    end

    if !all(mapped)
        warn("""the following fields are present in type $T in the file but not present in the type the workspace:

                $(join(dt.names[!mapped], "\n"))

                Data in these fields will not be accessible""")
    end
    (ReadRepresentation(T, OnDiskRepresentation{tuple(offsets...),Tuple{types...},Tuple{odrs...}}()), false)
end

## Type reconstruction

module ReconstructedTypes end

function reconstruct_bitstype(name, size)
    sym = gensym(name)
    eval(ReconstructedTypes, :(bitstype $(size*8) $(sym)))
    getfield(ReconstructedTypes, sym)
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
    elseif dt.size == 1
        return signed ? ReadRepresentation(Int8) : ReadRepresentation(UInt8)
    elseif dt.size == 4
        return signed ? ReadRepresentation(Int32) : ReadRepresentation(UInt32)
    elseif dt.size == 2
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
    if dt == h5fieldtype(f, Float64, true)
        return ReadRepresentation(Float64)
    elseif dt == h5fieldtype(f, Float32, true)
        return ReadRepresentation(Float32)
    elseif dt == h5fieldtype(f, Float16, true)
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
@inline function h5convert!(out::Ptr, odr::Type{Reference}, f::JLDFile, x::ANY, wsession::JLDWriteSession)
    BOXED_PTR[] = out
    h5convert_with_boxed_ptr!(f, x, wsession)
end
function h5convert_with_boxed_ptr!(f::JLDFile, x::Reference, wsession::JLDWriteSession)
    unsafe_store!(convert(Ptr{Reference}, BOXED_PTR[]), x)
    nothing
end
function h5convert_with_boxed_ptr!(f::JLDFile, x, wsession::JLDWriteSession)
    ptr = BOXED_PTR[]
    unsafe_store!(convert(Ptr{Reference}, ptr), write_ref(f, x, wsession))
    nothing
end
h5convert_uninitialized!(out::Ptr, odr::Type{Reference}) =
    (unsafe_store!(convert(Ptr{Reference}, out), Reference(0)); nothing)


# Reading references as references
jlconvert(::ReadRepresentation{Reference,Reference}, f::JLDFile, ptr::Ptr) =
    unsafe_load(convert(Ptr{Reference}, ptr))
jlconvert_isinitialized(::ReadRepresentation{Reference,Reference}, ptr::Ptr) = true
# jlconvert!(outptr::Ptr, ::ReadRepresentation{Reference,Reference}, f::JLDFile, ptr::Ptr) =
#     unsafe_store!(convert(Ptr{Reference, outptr}), unsafe_load(convert(Ptr{Reference}, ptr)))

# Reading references as other types
jlconvert{T}(::ReadRepresentation{T,Reference}, f::JLDFile, ptr::Ptr) =
    convert(T, read_dataset(f, unsafe_load(convert(Ptr{Offset}, ptr))))::T
jlconvert_isinitialized{T}(::ReadRepresentation{T,Reference}, ptr::Ptr) =
    unsafe_load(convert(Ptr{Reference}, ptr)) != Reference(0)

## Routines for variable-length datatypes

immutable Vlen{T}
    size::UInt32
    id::GlobalHeapID
end
sizeof{T<:Vlen}(::Type{T}) = 4 + sizeof(GlobalHeapID)

# Write variable-length data and store the offset and length to out pointer
@inline function store_vlen!(out::Ptr, odr, f::JLDFile, x::AbstractVector, wsession::JLDWriteSession)
    unsafe_store!(convert(Ptr{UInt32}, out), length(x))
    unsafe_store!(convert(Ptr{GlobalHeapID}, out)+4, write_heap_object(f, odr, x, wsession))
    nothing
end
h5convert!{T}(out::Ptr, ::Type{Vlen{T}}, f::JLDFile, x, wsession::JLDWriteSession) =
    store_vlen!(out, T, f, x, wsession)

h5convert_uninitialized!{T<:Vlen}(out::Ptr, odr::Type{T}) =
    (unsafe_store!(convert(Ptr{Int128}, out), 0); nothing)

# Read variable-length data given offset and length in ptr
load_vlen{T}(f::JLDFile, ptr::Ptr, ::Type{T}) =
    read_heap_object(f, unsafe_load(convert(Ptr{GlobalHeapID}, ptr+4)), T)
jlconvert{T,S}(::ReadRepresentation{T,Vlen{S}}, f::JLDFile, ptr::Ptr) =
    convert(T, load_vlen(f, ptr, S))::T
jlconvert_isinitialized{T,S}(::ReadRepresentation{T,Vlen{S}}, ptr::Ptr) =
    unsafe_load(convert(Ptr{GlobalHeapID}, ptr+4)) != GlobalHeapID(0, 0)

## ByteStrings

const H5TYPE_VLEN_ASCII = VariableLengthDatatype(DT_VARIABLE_LENGTH, 0x11, 0x00, 0x00,
                                                 sizeof(Vlen{UInt8}),
                                                 FixedPointDatatype(1, false))
const H5TYPE_VLEN_UTF8 = VariableLengthDatatype(DT_VARIABLE_LENGTH, 0x11, 0x01, 0x00,
                                               sizeof(Vlen{UInt8}),
                                               FixedPointDatatype(1, false))

h5fieldtype(::JLDFile, ::Type{ASCIIString}, ::Bool) = H5TYPE_VLEN_ASCII
h5fieldtype(::JLDFile, ::Type{UTF8String}, ::Bool) = H5TYPE_VLEN_UTF8
h5fieldtype(::JLDFile, ::Type{ByteString}, ::Bool) = H5TYPE_VLEN_UTF8
fieldodr(::Type{ASCIIString}, ::Bool) = Vlen{ASCIIString}
fieldodr(::Union(Type{UTF8String}, Type{ByteString}), ::Bool) = Vlen{UTF8String}

# Stored as variable-length strings
immutable FixedLengthString{T<:AbstractString}
    length::Int
end
sizeof{T<:ByteString}(x::FixedLengthString{T}) = x.length

h5type(f::JLDFile, x::ByteString) = StringDatatype(typeof(x), sizeof(x))
odr{T<:ByteString}(::Type{T}) = fieldodr(T, true)
objodr(x::ByteString) = FixedLengthString{typeof(x)}(sizeof(x))

function jltype(f::JLDFile, dt::BasicDatatype)
    if dt.class == DT_STRING
        if dt.bitfield1 == 0x01 && dt.bitfield2 == 0x00 && dt.bitfield3 == 0x00
            return FixedLengthString{ASCIIString}(dt.size)
        elseif dt.bitfield1 == 0x11 && dt.bitfield2 == 0x00 && dt.bitfield3 == 0x00
            return FixedLengthString{UTF8String}(dt.size)
        else
            throw(UnsupportedFeatureException())
        end
    elseif dt.class == DT_OPAQUE
        error("attempted to read a bare (non-committed) opaque datatype")
    elseif dt.class == DT_REFERENCE
        return ReadRepresentation(Any, Reference)
    else
        throw(UnsupportedFeatureException())
    end
end

function jltype(f::JLDFile, dt::VariableLengthDatatype)
    if dt == H5TYPE_VLEN_ASCII
        return ReadRepresentation(ASCIIString, Vlen{ASCIIString})
    elseif dt == H5TYPE_VLEN_UTF8
        return ReadRepresentation(UTF8String, Vlen{UTF8String})
    else
        throw(UnsupportedFeatureException())
    end
end

h5convert!(out::Ptr, ::FixedLengthString, f::JLDFile, x, ::JLDWriteSession) =
    (unsafe_copy!(convert(Ptr{UInt8}, out), pointer(x.data), length(x.data)); nothing)
h5convert!{T<:ByteString}(out::Ptr, ::Type{Vlen{T}}, f::JLDFile, x, wsession::JLDWriteSession) =
    store_vlen!(out, UInt8, f, x.data, wsession)
jlconvert{T,S<:ByteString}(::ReadRepresentation{T,Vlen{S}}, f::JLDFile, ptr::Ptr) =
    convert(T, S(load_vlen(f, ptr, UInt8)))::T
jlconvert(T::ReadRepresentation{ByteString,Vlen{UTF8String}}, f::JLDFile, ptr::Ptr) =
    bytestring(load_vlen(f, ptr, UInt8))
function jlconvert{S<:ByteString}(rr::FixedLengthString{S}, ::JLDFile, ptr::Ptr)
    data = Array(UInt8, rr.length)
    unsafe_copy!(pointer(data), convert(Ptr{UInt8}, ptr), rr.length)
    S(data)
end

## UTF16Strings

const H5TYPE_VLEN_UINT16 = VariableLengthDatatype(FixedPointDatatype(2, true))
h5fieldtype(f::JLDFile, ::Type{UTF16String}, ::Bool) = 
    haskey(f.jlh5type, UTF16String) ? f.jlh5type[UTF16String] :
                                      commit(f, H5TYPE_VLEN_UINT16, UTF16String)
fieldodr(::Type{UTF16String}, ::Bool) = Vlen{UInt16}

h5type(f::JLDFile, ::UTF16String) = h5fieldtype(f, UTF16String, true)
odr(::Type{UTF16String}) = Vlen{UInt16}

# The data field of a UTF16String has an embedded null, but we don't
# want to write that, so this is a bit more complicated.
sizeof(x::FixedLengthString{UTF16String}) = 2 * x.length
function h5convert!(out::Ptr, ::Type{Vlen{UInt16}}, f::JLDFile, x::UTF16String, wsession::JLDWriteSession)
    n = length(x.data)-1
    unsafe_store!(convert(Ptr{UInt32}, out), n)
    unsafe_store!(convert(Ptr{GlobalHeapID}, out)+4, write_heap_object(f, FixedLengthString{UTF16String}(n), x, wsession))
    nothing
end
h5convert!(out::Ptr, fl::FixedLengthString{UTF16String}, f::JLDFile, x::UTF16String, wsession::JLDWriteSession) =
    (unsafe_copy!(convert(Ptr{UInt16}, out), pointer(x.data), fl.length); nothing)

# Add the embedded null back in when reading
function jlconvert(T::ReadRepresentation{UTF16String,Vlen{UInt16}}, f::JLDFile, ptr::Ptr)
    vl = load_vlen(f, ptr, UInt16)
    push!(vl, 0)
    UTF16String(vl)
end

constructrr(::JLDFile, ::Type{UTF16String}, dt::VariableLengthDatatype{FixedPointDatatype}, ::Void) =
    dt == H5TYPE_VLEN_UINT16 ? (ReadRepresentation(UTF16String, Vlen{UInt16}), true) :
                               throw(UnsupportedFeatureException())

## Symbols

hasdata(::Type{Symbol}) = true

h5fieldtype(f::JLDFile, ::Type{Symbol}, ::Bool) = 
    haskey(f.jlh5type, Symbol) ? f.jlh5type[Symbol] : commit(f, H5TYPE_VLEN_UTF8, Symbol)
fieldodr(::Type{Symbol}, ::Bool) = Vlen{UTF8String}

h5type(f::JLDFile, ::Symbol) = h5fieldtype(f, Symbol, true)
odr(::Type{Symbol}) = Vlen{UTF8String}

h5convert!(out::Ptr, ::Type{Vlen{UTF8String}}, f::JLDFile, x::Symbol, ::JLDWriteSession) =
    store_vlen!(out, UInt8, f, string(x).data, f.datatype_wsession)

constructrr(::JLDFile, ::Type{Symbol}, dt::VariableLengthDatatype{FixedPointDatatype}, ::Void) =
    dt == H5TYPE_VLEN_UTF8 ? (ReadRepresentation(Symbol, Vlen{UTF8String}), true) :
                             throw(UnsupportedFeatureException())

jlconvert(::ReadRepresentation{Symbol,Vlen{UTF8String}}, f::JLDFile, ptr::Ptr) = symbol(load_vlen(f, ptr, UInt8))

## BigInts and BigFloats

h5fieldtype(f::JLDFile, T::Union{Type{BigInt},Type{BigFloat}}, ::Bool) =
    haskey(f.jlh5type, T) ? f.jlh5type[T] : commit(f, H5TYPE_VLEN_ASCII, T)
fieldodr(::Union(Type{BigInt}, Type{BigFloat}), ::Bool) = Vlen{ASCIIString}

# Stored as a variable-length string
h5type(f::JLDFile, x::Union{BigInt,BigFloat}) = h5fieldtype(f, typeof(x), true)
odr(::Union(Type{BigInt}, Type{BigFloat})) = Vlen{ASCIIString}

h5convert!(out::Ptr, ::Type{Vlen{ASCIIString}}, f::JLDFile, x::BigInt, wsession::JLDWriteSession) =
    store_vlen!(out, UInt8, f, base(62, x).data, wsession)
h5convert!(out::Ptr, ::Type{Vlen{ASCIIString}}, f::JLDFile, x::BigFloat, wsession::JLDWriteSession) =
    store_vlen!(out, UInt8, f, string(x).data, wsession)

constructrr(::JLDFile, T::Union{Type{BigInt},Type{BigFloat}}, dt::VariableLengthDatatype, ::Void) =
    dt == H5TYPE_VLEN_ASCII ? (ReadRepresentation(T, Vlen{ASCIIString}), true) :
                              throw(UnsupportedFeatureException())

jlconvert(::ReadRepresentation{BigInt,Vlen{ASCIIString}}, f::JLDFile, ptr::Ptr) =
    parse(BigInt, ASCIIString(load_vlen(f, ptr, UInt8)), 62)
jlconvert(::ReadRepresentation{BigFloat,Vlen{ASCIIString}}, f::JLDFile, ptr::Ptr) =
    parse(BigFloat, ASCIIString(load_vlen(f, ptr, UInt8)))

## DataTypes

const H5TYPE_DATATYPE = CompoundDatatype(
    sizeof(Vlen{UTF8String})+sizeof(Vlen{Reference}),
    ["name", "parameters"],
    [0, sizeof(Vlen{UTF8String})],
    [H5TYPE_VLEN_UTF8, VariableLengthDatatype(ReferenceDatatype())]
)
typealias DataTypeODR OnDiskRepresentation{(0,sizeof(Vlen{UTF8String})),Tuple{UTF8String,Vector{Any}},Tuple{Vlen{UTF8String},Vlen{Reference}}}

function h5fieldtype(f::JLDFile, ::Type{DataType}, ::Bool)
    haskey(f.jlh5type, DataType) && return f.jlh5type[DataType]
    io = f.io
    offset = f.end_of_data

    seek(io, offset)
    id = length(f.datatypes)+1
    cdt = CommittedDatatype(offset, id)
    f.datatype_locations[offset] = cdt
    f.jlh5type[DataType] = cdt
    f.h5jltype[cdt] = ReadRepresentation(DataType, DataTypeODR())
    push!(f.datatypes, cdt)

    commit(f, H5TYPE_DATATYPE, (WrittenAttribute(:julia_type, Dataspace(f, DataType, odr(DataType)), cdt, odr(DataType), DataType),))

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
    store_vlen!(out, UInt8, f, join(tn, ".").data, f.datatype_wsession)
    if !isempty(T.parameters)
        store_vlen!(out+sizeof(Vlen{UInt8}), Reference, f, Reference[write_ref(f, x, wsession) for x in T.parameters], f.datatype_wsession)
    end
    nothing
end

immutable UnknownType{Path} end
function jlconvert{T}(::ReadRepresentation{T,DataTypeODR()}, f::JLDFile, ptr::Ptr)
    path = bytestring(load_vlen(f, ptr, UInt8))
    parts = split(path, '.')
    m = Main
    for part in parts
        sym = symbol(part)
        !isdefined(m, sym) && return UnknownType{symbol(path)}
        m = getfield(m, sym)
    end
    isa(m, DataType) || return UnknownType{symbol(path)}
    m = m::DataType
    
    hasparameters = unsafe_load(convert(Ptr{UInt32}, ptr+sizeof(Vlen{UInt8}))) != 0
    if hasparameters
        refs = load_vlen(f, ptr+sizeof(Vlen{UInt8}), Reference)
        params = [read_dataset(f, x.offset) for x in refs]
        try
            return m{params...}::DataType
        catch e
            return UnknownType{symbol(path)}
        end
    elseif m === Tuple
        # Need to instantiate with no parameters, since Tuple is really
        # Tuple{Vararg{Any}}
        return Tuple{}::DataType
    end
    m
end

## Union Types

const H5TYPE_UNION = VariableLengthDatatype(H5TYPE_DATATYPE)

h5fieldtype(f::JLDFile, ::Type{Union}, ::Bool) =
    haskey(f.jlh5type, Union) ? f.jlh5type[Union] : commit(f, H5TYPE_UNION, Union)
fieldodr{T<:Union}(::Type{T}, ::Bool) = Vlen{DataTypeODR()}

h5type(f::JLDFile, ::Union) = h5fieldtype(f, Union, true)
odr{T<:Union}(::Type{T}) = fieldodr(Union, true)

h5convert!(out::Ptr, ::Type{Vlen{DataTypeODR()}}, f::JLDFile, x::Union, wsession::JLDWriteSession) =
    store_vlen!(out, DataTypeODR(), f, DataType[x for x in x.types], wsession)

function constructrr(::JLDFile, ::Type{Union}, dt::VariableLengthDatatype, ::Void)
    dt == H5TYPE_UNION ? (ReadRepresentation(Union, Vlen{DataTypeODR()}), true) :
                         throw(UnsupportedFeatureException())
end

function jlconvert(::ReadRepresentation{Union, Vlen{DataTypeODR()}}, f::JLDFile, ptr::Ptr)
    refs = load_vlen(f, ptr, Reference)
    params = DataType[read_dataset(f, x.offset) for x in refs]
    Union{params...}
end

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
    !hasdata(T) ? ReferenceDatatype() : :(h5fieldtype(f, T, false))
end
odr{T,N}(::Type{Array{T,N}}) = fieldodr(T, false)

## User-defined types
##
## Similar to special types, but h5convert!/jl_convert are dynamically
## generated.

# jlconvert for types that represented the same way in memory as in the file
jlconvert{T}(::ReadRepresentation{T,T}, ::JLDFile, ptr::Ptr) = unsafe_load(convert(Ptr{T}, ptr))
# jlconvert!{T}(out::Ptr, ::ReadRepresentation{T,T}, ::JLDFile, ptr::Ptr) =
#     (unsafe_store!(convert(Ptr{T}, out), unsafe_load(convert(Ptr{T}, ptr))); nothing)

# jlconvert!{T,S}(out::Ptr, ::ReadRepresentation{T,S}, ::JLDFile, ptr::Ptr) =
#     (unsafe_store!(convert(Ptr{T}, out), convert(T, unsafe_load(convert(Ptr{S}, ptr)))); nothing)

# jlconvert for other types
immutable UndefinedFieldException
    ty::DataType
    fieldname::Symbol
end

show(io::IO, x::UndefinedFieldException) =
    print(io, """field $fieldname of type $ty must be defined in the current workspace,
                 but was undefined in the file""")

jlconvert_isinitialized(::Any, ::Ptr) = true
@generated function jlconvert{T,S}(::ReadRepresentation{T,S}, f::JLDFile, ptr::Ptr)
    isa(S, DataType) && return :(convert(T, unsafe_load(convert(Ptr{S}, ptr))))
    S === nothing && return T.instance
    @assert isa(S, OnDiskRepresentation)

    offsets, types, odrs = typeof(S).parameters
    types = types.types
    odrs = odrs.types

    blk = Expr(:block)
    args = blk.args
    fsyms = []
    fn = fieldnames(T)
    offset = 0
    for i = 1:length(T.types)
        ttype = T.types[i]
        offset = offsets[i]
        rtype = types[i]
        odr = odrs[i]

        fsym = symbol(string("field_", fn[i]))
        push!(fsyms, fsym)

        rr = :(ReadRepresentation{$rtype,$odr}())

        if odr === nothing
            # Type is not stored or single instance
            push!(:($fsym = $(ttype.instance)))
        elseif i <= T.ninitialized || isbits(ttype)
            # Reference must always be initialized
            push!(args, quote
                jlconvert_isinitialized($rr, ptr+$offset) || throw(UndefinedFieldException(T,$(QuoteNode(fn[i]))))
                $fsym = convert($ttype, jlconvert($rr, f, ptr+$offset)::$rtype)::$ttype
            end)
        else
            # Reference may not be initialized
            push!(args, quote
                jlconvert_isinitialized($rr, ptr+$offset) || return $(Expr(:new, T, fsyms[1:i-1]...))
                $fsym = convert($ttype, jlconvert($rr, f, ptr+$offset)::$rtype)::$ttype
            end)
        end
    end
    push!(args, Expr(:new, T, fsyms...))

    blk
end

# @generated function jlconvert!{T}(::Type{T}, file::JLDFile, ptr::Ptr)
#     if isempty(fieldnames(T))
#         if T.size == 0
#             !T.mutable ? nothing :
#                 :(unsafe_store!(convert(Ptr{Ptr{Void}}, out), pointer_from_objref($T())))
#         else
#             :(_jlconvert_bits!(out, $T, ptr))
#         end
#     elseif T.size == 0
#         nothing
#     elseif !isbits(T)
#         error("attempted to call jlconvert! on non-isbits type $T")
#     else
#         dtype = make_compound(parent, 1:length(T.types), T)
#         ex = Expr(:block)
#         args = ex.args
#         jloffsets = fieldoffsets(T)
#         for i = 1:length(dtype.offsets)
#             h5offset = dtype.offsets[i]
#             jloffset = jloffsets[i]
#             push!(args, :(jlconvert!(out+$jloffset, $(T.types[i]), file, ptr+$h5offset)))
#         end
#         push!(args, nothing)
#         ex
#     end
# end

## Common functions for all non-special types (including h5convert!)

unknown_type_err(T) =
    error("""$T is not of a type supported by JLD
             Please report this error at https://github.com/timholy/HDF5.jl""")

@generated function h5convert!(out::Ptr, odr::OnDiskRepresentation, file::JLDFile, x, wsession::JLDWriteSession)
    T = x
    offsets, types, members = odr.parameters
    types = types.types
    members = members.types

    getindex_fn = isa(T, TupleType) ? (:getindex) : (:getfield)
    ex = Expr(:block)
    args = ex.args
    for i = 1:length(offsets)
        offset = offsets[i]
        member = members[i]
        conv = :(h5convert!(out+$offset, $(member), file, convert($(types[i]), $getindex_fn(x, $i)), wsession))
        if i > T.ninitialized && !isbits(x.types[i])
            push!(args, quote
                if !isdefined(x, $i)
                    h5convert_uninitialized!(out+$offset, $(member))
                else
                    $conv
                end
            end)
        else
            push!(args, conv)
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

# Create a Julia type based on the HDF5Datatype from the file. Used
# when the type is no longer available.
# function reconstruct_type(parent::JLDFile, dtype::H5Datatype, savedname::AbstractString)
#     name = gensym(savedname)
#     class_id = HDF5.h5t_get_class(dtype.id)
#     if class_id == HDF5.H5T_OPAQUE
#         if exists(dtype, "empty")
#             @eval (immutable $name; end; $name)
#         else
#             sz = Int(HDF5.h5t_get_size(dtype.id))*8
#             @eval (bitstype $sz $name; $name)
#         end
#     else
#         # Figure out field names and types
#         nfields = HDF5.h5t_get_nmembers(dtype.id)
#         fieldnames = Array(Symbol, nfields)
#         fieldtypes = Array(Type, nfields)
#         for i = 1:nfields
#             membername = HDF5.h5t_get_member_name(dtype.id, i-1)
#             idx = rsearchindex(membername, "_")
#             fieldname = fieldnames[i] = symbol(membername[1:idx-1])

#             if idx != sizeof(membername)
#                 # There is something past the underscore in the HDF5 field
#                 # name, so the type is stored in file
#                 memberdtype = HDF5.t_open(parent.plain, string(pathtypes, '/', lpad(membername[idx+1:end], 8, '0')))
#                 fieldtypes[i] = jldatatype(parent, memberdtype)
#             else
#                 memberclass = HDF5.h5t_get_member_class(dtype.id, i-1)
#                 if memberclass == HDF5.H5T_REFERENCE
#                     # Field is a reference, so use Any
#                     fieldtypes[i] = Any
#                 else
#                     # Type is built-in
#                     memberdtype = HDF5Datatype(HDF5.h5t_get_member_type(dtype.id, i-1), parent.plain)
#                     fieldtypes[i] = jldatatype(parent, memberdtype)
#                 end
#             end
#         end

#         if startswith(savedname, "(") || startswith(savedname, "Core.Tuple{")
#             # We're reconstructing a tuple
#             typetuple(fieldtypes)
#         else
#             # We're reconstructing some other type
#             @eval begin
#                 immutable $name
#                     $([:($(fieldnames[i])::$(fieldtypes[i])) for i = 1:nfields]...)
#                 end
#                 $name
#             end
#         end
#     end
# end
