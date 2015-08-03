typealias TupleType{T<:Tuple} Type{T}
typealias Initialized Union(Type{Val{true}}, Type{Val{false}})

immutable UnknownType{T}
    name::T
    parameters::Vector{Any}

    UnknownType(name) = new(name)
    UnknownType(name, parameters) = new(name, parameters)
end
UnknownType(name) = UnknownType{typeof(name)}(name)
UnknownType(name, parameters) = UnknownType{typeof(name)}(name, parameters)

## Generic machinery

# Carries the type and on-disk representation of data to be read from
# the disk
ReadRepresentation(T, S) = ReadRepresentation{T,S}()
ReadRepresentation(T) = ReadRepresentation{T,T}()
sizeof{T,S}(::ReadRepresentation{T,S}) = sizeof(S)

# Determines whether a specific field type should be saved in the file
hasfielddata(::Type{Union{}}) = false
hasfielddata{T}(::Type{Type{T}}) = true
hasfielddata{T}(::Type{T}) = !isleaftype(T) || !isbits(T) || sizeof(T) != 0

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

fieldnames{T<:Tuple}(x::Type{T}) = [symbol(x) for x = 1:length(x.types)]
fieldnames(x::ANY) = Base.fieldnames(x)

# fieldodr gives the on-disk representation of a field of a given type,
# which is either always initialized (initialized=true) or potentially
# uninitialized (initialized=false)
@generated function fieldodr{T}(::Type{T}, initialized::Bool)
    if isleaftype(T)
        if !hasfielddata(T)
            # A ghost type, so no need to store at all
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
h5fieldtype(f::JLDFile, ::Type{Union()}, ::Initialized) = nothing
@generated function h5fieldtype{T}(f::JLDFile, ::Type{T}, initialized::Initialized)
    if isleaftype(T)
        if !hasfielddata(T)
            return nothing
        elseif isbits(T) || (isa(initialized, Type{Type{Val{true}}}) && !T.mutable)
            return quote
                haskey(f.jlh5type, T) && return f.jlh5type[T]
                $(if isempty(T.types)
                    # bitstype
                    :(return commit(f, OpaqueDatatype(sizeof(T)), T))
                else
                    # Compound type
                    :(return commit_compound(f, fieldnames(T), T))
                end)
            end
        end
    end
    ReferenceDatatype()
end

# odr gives the on-disk representation of a given type, similar to
# fieldodr, but actually encoding the data for things that odr stores
# as references
@generated function odr{T}(::Type{T})
    if sizeof(T) == 0
        # A pointer singleton or ghost, so no need to store at all
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
    if sizeof(T) == 0
        commit(f, OpaqueDatatype(1), T, WrittenAttribute(f, :empty, UInt8(1)))
    elseif isempty(T.types) # bitstype
        commit(f, OpaqueDatatype(sizeof(T)), T)
    else
        commit_compound(f, fieldnames(T), T)
    end
end

# Make a compound datatype from a set of names and types
function commit_compound(f::JLDFile, names::AbstractVector{Symbol}, T::DataType)
    types = T.types
    offsets = Int[]
    h5names = Symbol[]
    members = H5Datatype[]
    fieldtypes = Reference[]
    hasfieldtype = false

    offset = 0
    for i = 1:length(types)
        !hasfielddata(types[i]) && continue
        dtype = h5fieldtype(f, types[i], Val{i <= T.ninitialized})
        dtype == nothing && continue
        push!(h5names, names[i])
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
        fieldtypeattr = WrittenAttribute(:field_datatypes, WriteDataspace(f, fieldtypes, DataType), ReferenceDatatype(),
                                         fieldtypes)
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
    typeattr = WrittenAttribute(:julia_type, WriteDataspace(f, DataType, odr(DataType)), h5type(f, DataType), T)

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
    haskey(f.h5jltype, cdt) && return f.h5jltype[cdt]::ReadRepresentation
    dt, attrs = read_committed_datatype(f, cdt)

    julia_type_attr = nothing
    field_datatypes_attr = nothing
    empty_attr = false
    for attr in attrs
        if attr.name == :julia_type
            julia_type_attr = attr
        elseif attr.name == :field_datatypes
            field_datatypes_attr = attr
        elseif attr.name == :empty
            empty_attr = true
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
    datatype = read_attr_data(f, julia_type_attr)
    if isa(field_datatypes_attr, Void)
        rr, canonical = constructrr(f, datatype, dt, nothing, empty_attr)
    else
        refs = read_attr_data(f, field_datatypes_attr::ReadAttribute,
                              ReferenceDatatype(), ReadRepresentation(Reference))
        rr, canonical = constructrr(f, datatype, dt, refs, empty_attr)
    end
    rr = rr::ReadRepresentation

    canonical && (f.jlh5type[datatype] = cdt)
    f.datatypes[cdt.index] = dt
    f.h5jltype[cdt] = rr
end

# Constructs a ReadRepresentation for a given opaque (bitstype) type
function constructrr(::JLDFile, T::DataType, dt::BasicDatatype, ::Void, empty::Bool)
    dt.class == DT_OPAQUE || throw(UnsupportedFeatureException())
    if sizeof(T) == dt.size
        (ReadRepresentation(T), true)
    elseif sizeof(T) == 0 && empty
        (ReadRepresentation(T, nothing), true)
    else
        warn("bitstype $T has size $(sizeof(T)), but written type has size $(dt.size); reconstructing")
        reconstruct_bitstype(T.name.name, dt.size, empty)
    end
end

# Constructs a ReadRepresentation for a given compound type. If hard_failure is
# true, then throw a TypeMappingException instead of attempting reconstruction.
immutable TypeMappingException <: Exception; end
function constructrr(f::JLDFile, T::DataType, dt::CompoundDatatype,
                     field_datatypes::Union{Vector{Reference},Void}, ::Bool,
                     hard_failure::Bool=false)
    # Map names in dt to their indices
    dtnames = Dict{Symbol,Int}()
    for i = 1:length(dt.names)
        dtnames[dt.names[i]] = i
    end
    mapped = falses(length(dt.names))

    offsets = Array(Int, length(T.types))
    types = Array(Any, length(T.types))
    odrs = Array(Any, length(T.types))
    fn = fieldnames(T)
    fo = fieldoffsets(T)
    samelayout = isbits(T) && sizeof(T) == dt.size
    dtindex = 0
    for i = 1:length(T.types)
        wstype = T.types[i]
        if !hasfielddata(wstype)
            types[i] = wstype
            odrs[i] = nothing
            offsets[i] = dtindex == 0 ? 0 : (dt.offsets[dtindex] + dt.members[dtindex].size::UInt32)
        else
            if !haskey(dtnames, fn[i])
                hard_failure && throw(TypeMappingException())
                warn("saved type ", T, " is missing field ", fn[i], " in workspace type; reconstructing")
                return reconstruct_compound(f, string(T), dt, field_datatypes_attr)
            end

            dtindex = dtnames[fn[i]]
            if !isa(field_datatypes, Void) && (ref = field_datatypes[dtindex]) != Reference(0)
                dtrr = jltype(f, f.datatype_locations[ref.offset])
            else
                dtrr = jltype(f, dt.members[dtindex])
            end

            readtype, odr = typeof(dtrr).parameters

            if typeintersect(readtype, wstype) === Union{} &&
               !method_exists(convert, Tuple{Type{wstype}, readtype})
                # Saved type does not match type in workspace and no
                # convert method exists, so we definitely need to reconstruct.
                hard_failure && throw(TypeMappingException())
                warn("saved type ", T, " has field ", stringfield, "::", readtype,
                     ", but workspace type has field ", stringfield, "::", wstype,
                     ", and no applicable convert method exists; reconstructing")
                return reconstruct_compound(f, string(T), dt, field_datatypes)
            end

            types[i] = readtype
            odrs[i] = odr
            offsets[i] = dt.offsets[dtindex]
            samelayout = samelayout && offsets[i] == fo[i] && types[i] === wstype

            mapped[dtindex] = true
        end
    end

    if !all(mapped)
        warn("the following fields are present in type ", T,
             " saved in the file but not present in the type the workspace:\n\n",
             join(dt.names[!mapped], "\n"),
             "\n\nData in these fields will not be accessible")
    end

    if samelayout
        (ReadRepresentation(T), true)
    else
        rodr = OnDiskRepresentation{tuple(offsets...),Tuple{types...},Tuple{odrs...}}()
        (ReadRepresentation(T, rodr), rodr == odr(T))
    end
end

## Type reconstruction

module ReconstructedTypes end

function reconstruct_bitstype(name::Union(Symbol,ByteString), size::Integer, empty::Bool)
    sym = gensym(name)
    eval(ReconstructedTypes, empty ? :(immutable $(sym) end) : :(bitstype $(size*8) $(sym)))
    T = getfield(ReconstructedTypes, sym)
    (ReadRepresentation(T, empty ? nothing : T), false)
end

function constructrr(f::JLDFile, unk::UnknownType, dt::BasicDatatype, ::Void, empty::Bool)
    warn("type ", typestring(unk), " does not exist in workspace; reconstructing")
    reconstruct_bitstype(typestring(unk), dt.size, empty)
end

# Convert an ordinary type or an UnknownType to a corresponding string. This is
# only used to create gensymmed names for reconstructed types.
function typestring(T::UnknownType)
    tn = IOBuffer()
    print(tn, T.name)
    if isdefined(T, :parameters)
        write(tn, '{')
        for x in T.parameters
            if isa(x, UnknownType)
                write(tn, typestring(x))
            else
                print(tn, x)
            end
        end
        write(tn, '}')
    end
    takebuf_string(tn)
end

# Reconstruct an UnknownType for which we were able to resolve the name, but not
# some of the parameters, by attempting to replace unknown parameters with the
# UB on the type.
function constructrr(f::JLDFile, unk::UnknownType{DataType}, dt::CompoundDatatype,
                     field_datatypes::Union{Vector{Reference},Void}, empty::Bool)
    if unk.name === Tuple
        # For a tuple with unknown fields, we should reconstruct the fields
        rodr = reconstruct_odr(f, dt, field_datatypes)
        # This is a "pseudo-RR" since the tuple is not fully parametrized, but
        # the parameters must depend on the types actually encoded in the file
        (ReadRepresentation(Tuple, rodr), false)
    else
        params = copy(unk.parameters)
        for i = 1:length(params)
            if isa(params[i], UnknownType)
                params[i] = unk.name.parameters[i]
            end
        end

        # Try to construct the rr for the relaxed type. On failure, fall back to
        # reconstruct_compound.
        try
            T = unk.name{params...}
            rr = constructrr(f, T, dt, field_datatypes, empty)
            warn("some parameters could not be resolved for type ", typestring(unk), "; reading as $T")
        catch err
            !isa(err, TypeMappingException) && rethrow(err)
            return reconstruct_compound(f, typestring(unk), dt, field_datatypes)
        end
    end
end

# An UnknownType for which we were not able to resolve the name.
function constructrr(f::JLDFile, unk::UnknownType, dt::CompoundDatatype,
                     field_datatypes::Union{Vector{Reference},Void}, ::Bool)
    ts = typestring(unk)
    warn("type ", ts, " does not exist in workspace; reconstructing")
    reconstruct_compound(f, ts, dt, field_datatypes)
end

# Reconstruct the ODR of a type from the CompoundDatatype and field_datatypes
# attribute
function reconstruct_odr(f::JLDFile, dt::CompoundDatatype,
                         field_datatypes::Union{Vector{Reference},Void})
    # Get the type and ODR information for each field
    types = Array(Any, length(dt.names))
    odrs = Array(Any, length(dt.names))
    for i = 1:length(dt.names)
        if !isa(field_datatypes, Void) && (ref = field_datatypes[i]) != Reference(0)
            dtrr = jltype(f, f.datatype_locations[ref.offset])
        else
            dtrr = jltype(f, dt.members[i])
        end
    types[i], odrs[i] = typeof(dtrr).parameters
    end
    OnDiskRepresentation{tuple(dt.offsets...),Tuple{types...},Tuple{odrs...}}()
end

# Reconstruct type that is a "lost cause": either we were not able to resolve
# the name, or the workspace type has additional fields, or cannot convert
# fields to workspace types
function reconstruct_compound(f::JLDFile, T::ByteString, dt::H5Datatype,
                              field_datatypes::Union{Vector{Reference},Void})
    rodr = reconstruct_odr(f, dt, field_datatypes)

    # Now reconstruct the type
    reconname = gensym(T)
    eval(ReconstructedTypes, Expr(:composite_type, reconname, Base.svec(),
                                  Base.svec(dt.names...), Any,
                                  typeof(rodr).parameters[2].types, false, 0))
    T = getfield(ReconstructedTypes, reconname)

    (ReadRepresentation(T, rodr), false)
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

## Special types
##
## To create a special serialization of a datatype, one should:
##
## - Pick the field's on-disk representation and HDF5
## - Define a method of h5type that constructs the type
## - Define h5convert! and jlconvert
## - If the type is an immutable, define jlconvert!

# This construction prevents these methods from getting called on type unions
typealias PrimitiveTypeTypes Union(Type{Int8}, Type{Int16}, Type{Int32}, Type{Int64}, Type{Int128},
                                   Type{UInt8}, Type{UInt16}, Type{UInt32}, Type{UInt64}, Type{UInt128},
                                   Type{Float16}, Type{Float32}, Type{Float64})
typealias PrimitiveTypes     Union(Int8, Int16, Int32, Int64, Int128, UInt8, UInt16, UInt32,
                                   UInt64, UInt128, Float16, Float32, Float64)
h5fieldtype(::JLDFile, T::Union(Type{Int8}, Type{Int16}, Type{Int32}, Type{Int64}, Type{Int128}), ::Initialized) =
    FixedPointDatatype(sizeof(T), true)
h5fieldtype(::JLDFile, T::Union(Type{UInt8}, Type{UInt16}, Type{UInt32}, Type{UInt64}, Type{UInt128}), ::Initialized) =
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

h5fieldtype(::JLDFile, ::Type{Float16}, ::Initialized) =
    FloatingPointDatatype(DT_FLOATING_POINT, 0x20, 0x0f, 0x00, 2, 0, 16, 10, 5, 0, 10, 0x0000000f)
h5fieldtype(::JLDFile, ::Type{Float32}, ::Initialized) =
    FloatingPointDatatype(DT_FLOATING_POINT, 0x20, 0x1f, 0x00, 4, 0, 32, 23, 8, 0, 23, 0x0000007f)
h5fieldtype(::JLDFile, ::Type{Float64}, ::Initialized) =
    FloatingPointDatatype(DT_FLOATING_POINT, 0x20, 0x3f, 0x00, 8, 0, 64, 52, 11, 0, 52, 0x000003ff)

function jltype(f::JLDFile, dt::FloatingPointDatatype)
    if dt == h5fieldtype(f, Float64, Val{true})
        return ReadRepresentation(Float64)
    elseif dt == h5fieldtype(f, Float32, Val{true})
        return ReadRepresentation(Float32)
    elseif dt == h5fieldtype(f, Float16, Val{true})
        return ReadRepresentation(Float16)
    else
        throw(UnsupportedFeatureException())
    end
end

h5type(f::JLDFile, x::PrimitiveTypes) = h5fieldtype(f, typeof(x), Val{true})

## Routines for references

# A hack to prevent us from needing to box the output pointer
# XXX not thread-safe!
h5type(::JLDFile, ::Reference) = ReferenceDatatype()
odr(::Type{Reference}) = Reference

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
jlconvert_canbeuninitialized(::ReadRepresentation{Reference,Reference}) = false
# jlconvert!(outptr::Ptr, ::ReadRepresentation{Reference,Reference}, f::JLDFile, ptr::Ptr) =
#     unsafe_store!(convert(Ptr{Reference, outptr}), unsafe_load(convert(Ptr{Reference}, ptr)))

# Reading references as other types
@inline function jlconvert{T}(::ReadRepresentation{T,Reference}, f::JLDFile, ptr::Ptr)
    x = read_dataset(f, unsafe_load(convert(Ptr{Offset}, ptr)))
    isa(x, T) && return x::T
    convert(T, x)::T
end
jlconvert_isinitialized{T}(::ReadRepresentation{T,Reference}, ptr::Ptr) =
    unsafe_load(convert(Ptr{Reference}, ptr)) != Reference(0)
jlconvert_canbeuninitialized{T}(::ReadRepresentation{T,Reference}) = true

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
jlconvert{T,S}(::ReadRepresentation{T,Vlen{S}}, f::JLDFile, ptr::Ptr) =
    read_heap_object(f, unsafe_load(convert(Ptr{GlobalHeapID}, ptr+4)), ReadRepresentation(T, S))
jlconvert_isinitialized{T,S}(::ReadRepresentation{T,Vlen{S}}, ptr::Ptr) =
    unsafe_load(convert(Ptr{GlobalHeapID}, ptr+4)) != GlobalHeapID(0, 0)
jlconvert_canbeuninitialized{T,S}(::ReadRepresentation{T,Vlen{S}}) = true

## ByteStrings

const H5TYPE_VLEN_ASCII = VariableLengthDatatype(DT_VARIABLE_LENGTH, 0x11, 0x00, 0x00,
                                                 sizeof(Vlen{UInt8}),
                                                 FixedPointDatatype(1, false))
const H5TYPE_VLEN_UTF8 = VariableLengthDatatype(DT_VARIABLE_LENGTH, 0x11, 0x01, 0x00,
                                               sizeof(Vlen{UInt8}),
                                               FixedPointDatatype(1, false))

h5fieldtype(::JLDFile, ::Type{ASCIIString}, ::Initialized) = H5TYPE_VLEN_ASCII
h5fieldtype(::JLDFile, ::Type{UTF8String}, ::Initialized) = H5TYPE_VLEN_UTF8
h5fieldtype(::JLDFile, ::Type{ByteString}, ::Initialized) = H5TYPE_VLEN_UTF8
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
    convert(T, S(jlconvert(ReadRepresentation(UInt8, Vlen{UInt8}), f, ptr)))::T
function jlconvert{S<:ByteString}(rr::FixedLengthString{S}, ::JLDFile, ptr::Ptr)
    data = Array(UInt8, rr.length)
    unsafe_copy!(pointer(data), convert(Ptr{UInt8}, ptr), rr.length)
    S(data)
end

## UTF16Strings

const H5TYPE_VLEN_UINT16 = VariableLengthDatatype(FixedPointDatatype(2, true))
h5fieldtype(f::JLDFile, ::Type{UTF16String}, ::Initialized) =
    haskey(f.jlh5type, UTF16String) ? f.jlh5type[UTF16String] :
                                      commit(f, H5TYPE_VLEN_UINT16, UTF16String)
fieldodr(::Type{UTF16String}, ::Bool) = Vlen{UInt16}

h5type(f::JLDFile, ::UTF16String) = h5fieldtype(f, UTF16String, Val{true})
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
    vl = jlconvert(ReadRepresentation(UInt16, Vlen{UInt16}), f, ptr)
    push!(vl, 0)
    UTF16String(vl)
end

constructrr(::JLDFile, ::Type{UTF16String}, dt::VariableLengthDatatype{FixedPointDatatype}, ::Void, ::Bool) =
    dt == H5TYPE_VLEN_UINT16 ? (ReadRepresentation(UTF16String, Vlen{UInt16}), true) :
                               throw(UnsupportedFeatureException())

## Symbols

h5fieldtype(f::JLDFile, ::Type{Symbol}, ::Initialized) =
    haskey(f.jlh5type, Symbol) ? f.jlh5type[Symbol] : commit(f, H5TYPE_VLEN_UTF8, Symbol)
fieldodr(::Type{Symbol}, ::Bool) = Vlen{UTF8String}

h5type(f::JLDFile, ::Symbol) = h5fieldtype(f, Symbol, Val{true})
odr(::Type{Symbol}) = Vlen{UTF8String}

h5convert!(out::Ptr, ::Type{Vlen{UTF8String}}, f::JLDFile, x::Symbol, ::JLDWriteSession) =
    store_vlen!(out, UInt8, f, string(x).data, f.datatype_wsession)

constructrr(::JLDFile, ::Type{Symbol}, dt::VariableLengthDatatype{FixedPointDatatype}, ::Void, ::Bool) =
    dt == H5TYPE_VLEN_UTF8 ? (ReadRepresentation(Symbol, Vlen{UTF8String}), true) :
                             throw(UnsupportedFeatureException())

jlconvert(::ReadRepresentation{Symbol,Vlen{UTF8String}}, f::JLDFile, ptr::Ptr) =
    symbol(jlconvert(ReadRepresentation(UInt8, Vlen{UInt8}), f, ptr))

## BigInts and BigFloats

h5fieldtype(f::JLDFile, T::Union{Type{BigInt},Type{BigFloat}}, ::Initialized) =
    haskey(f.jlh5type, T) ? f.jlh5type[T] : commit(f, H5TYPE_VLEN_ASCII, T)
fieldodr(::Union(Type{BigInt}, Type{BigFloat}), ::Bool) = Vlen{ASCIIString}

# Stored as a variable-length string
h5type(f::JLDFile, x::Union{BigInt,BigFloat}) = h5fieldtype(f, typeof(x), Val{true})
odr(::Union(Type{BigInt}, Type{BigFloat})) = Vlen{ASCIIString}

h5convert!(out::Ptr, ::Type{Vlen{ASCIIString}}, f::JLDFile, x::BigInt, wsession::JLDWriteSession) =
    store_vlen!(out, UInt8, f, base(62, x).data, wsession)
h5convert!(out::Ptr, ::Type{Vlen{ASCIIString}}, f::JLDFile, x::BigFloat, wsession::JLDWriteSession) =
    store_vlen!(out, UInt8, f, string(x).data, wsession)

constructrr(::JLDFile, T::Union{Type{BigInt},Type{BigFloat}}, dt::VariableLengthDatatype, ::Void, ::Bool) =
    dt == H5TYPE_VLEN_ASCII ? (ReadRepresentation(T, Vlen{ASCIIString}), true) :
                              throw(UnsupportedFeatureException())

jlconvert(::ReadRepresentation{BigInt,Vlen{ASCIIString}}, f::JLDFile, ptr::Ptr) =
    parse(BigInt, ASCIIString(jlconvert(ReadRepresentation(UInt8, Vlen{UInt8}), f, ptr)), 62)
jlconvert(::ReadRepresentation{BigFloat,Vlen{ASCIIString}}, f::JLDFile, ptr::Ptr) =
    parse(BigFloat, ASCIIString(jlconvert(ReadRepresentation(UInt8, Vlen{UInt8}), f, ptr)))

## DataTypes

const H5TYPE_DATATYPE = CompoundDatatype(
    sizeof(Vlen{UTF8String})+sizeof(Vlen{Reference}),
    ["name", "parameters"],
    [0, sizeof(Vlen{UTF8String})],
    [H5TYPE_VLEN_UTF8, VariableLengthDatatype(ReferenceDatatype())]
)
typealias DataTypeODR OnDiskRepresentation{(0,sizeof(Vlen{UTF8String})),Tuple{UTF8String,Vector{Any}},Tuple{Vlen{UTF8String},Vlen{Reference}}}

function h5fieldtype{T<:DataType}(f::JLDFile, ::Type{T}, ::Initialized)
    haskey(f.jlh5type, DataType) && return f.jlh5type[DataType]
    io = f.io
    offset = f.end_of_data

    seek(io, offset)
    id = length(f.datatypes)+1
    cdt = CommittedDatatype(offset, id)
    f.datatype_locations[offset] = cdt
    f.jlh5type[DataType] = cdt
    f.h5jltype[cdt] = ReadRepresentation(DataType, DataTypeODR())
    push!(f.datatypes, H5TYPE_DATATYPE)

    commit(f, H5TYPE_DATATYPE, (WrittenAttribute(:julia_type, WriteDataspace(f, DataType, odr(DataType)), cdt, DataType),))

    cdt
end
fieldodr{T<:DataType}(::Type{T}, ::Bool) = DataTypeODR()

h5type(f::JLDFile, ::DataType) = h5fieldtype(f, DataType, Val{true})
odr{T<:DataType}(::Type{T}) = DataTypeODR()

function typename(T::DataType)
    tn = Symbol[]
    m = T.name.module
    while m != module_parent(m)
        push!(tn, module_name(m))
        m = module_parent(m)
    end
    reverse!(tn)
    push!(tn, T.name.name)
    join(tn, ".")
end

function h5convert!(out::Ptr, ::DataTypeODR, f::JLDFile, T::DataType, wsession::JLDWriteSession)
    store_vlen!(out, UInt8, f, typename(T).data, f.datatype_wsession)
    if !isempty(T.parameters)
        refs = Reference[begin
            if isa(x, DataType)
                # The heuristic here is that, if the field type is a committed
                # data type, then we commit the datatype and write it as a
                # reference to the committed datatype. Otherwise we write it
                # as a name. This ensures that type parameters that affect the
                # structure of a type are written to the file, so that we can
                # reconstruct the type when the layout depends on the
                # parameters.
                dt = h5fieldtype(f, x, Val{true})
                if isa(dt, CommittedDatatype)
                    Reference((dt::CommittedDatatype).header_offset)
                else
                    write_ref(f, x, wsession)
                end
            else
                write_ref(f, x, wsession)
            end
        end for x in T.parameters]
        store_vlen!(out+sizeof(Vlen{UInt8}), Reference, f, refs, f.datatype_wsession)
    end
    nothing
end

# Read a type. Returns an instance of UnknownType if the type or parameters
# could not be resolved.
function jlconvert{T}(::ReadRepresentation{T,DataTypeODR()}, f::JLDFile, ptr::Ptr)
    hasparams = unsafe_load(convert(Ptr{UInt32}, ptr+sizeof(Vlen{UInt8}))) != 0
    unknown_params = false
    if hasparams
        paramrefs = jlconvert(ReadRepresentation(Reference, Vlen{Reference}), f, ptr+sizeof(Vlen{UInt8}))
        params = Any[begin
            # If the reference is to a committed datatype, read the datatype
            nulldt = CommittedDatatype(0, 0)
            cdt = get(f.datatype_locations, ref.offset, nulldt)
            res = cdt !== nulldt ? (typeof(jltype(f, cdt)::ReadRepresentation)::DataType).parameters[1] : read_dataset(f, ref.offset)
            unknown_params = unknown_params || isa(res, UnknownType)
            res
        end for ref in paramrefs]
    end

    path = bytestring(jlconvert(ReadRepresentation(UInt8, Vlen{UInt8}), f, ptr))
    parts = split(path, '.')
    m = Main
    for part in parts
        sym = symbol(part)
        if !isdefined(m, sym)
            return hasparams ? UnknownType(path, params) : UnknownType(path)
        end
        m = getfield(m, sym)
    end
    if !isa(m, DataType)
        return hasparams ? UnknownType(path, params) : UnknownType(path)
    end

    if hasparams
        unknown_params && return UnknownType(m, params)
        try
            return (m::DataType){params...}
        catch e
            return UnknownType(m, params)
        end
    elseif m === Tuple
        # Need to instantiate with no parameters, since Tuple is really
        # Tuple{Vararg{Any}}
        return Tuple{}
    end
    m
end

## Union Types

const H5TYPE_UNION = VariableLengthDatatype(H5TYPE_DATATYPE)

h5fieldtype{T<:Union}(f::JLDFile, ::Type{T}, ::Initialized) =
    haskey(f.jlh5type, Union) ? f.jlh5type[Union] : commit(f, H5TYPE_UNION, Union)
fieldodr{T<:Union}(::Type{T}, ::Bool) = Vlen{DataTypeODR()}
h5fieldtype(f::JLDFile, ::Type{Union{}}, ::Initialized) = nothing
fieldodr(::Type{Union{}}, initialized::Bool) = nothing

h5type(f::JLDFile, ::Union) = h5fieldtype(f, Union, Val{true})
odr(::Type{Union}) = fieldodr(Union, true)

h5convert!(out::Ptr, ::Type{Vlen{DataTypeODR()}}, f::JLDFile, x::Union, wsession::JLDWriteSession) =
    store_vlen!(out, DataTypeODR(), f, DataType[x for x in x.types], wsession)

function constructrr(::JLDFile, ::Type{Union}, dt::VariableLengthDatatype, ::Void, ::Bool)
    dt == H5TYPE_UNION ? (ReadRepresentation(Union, Vlen{DataTypeODR()}), true) :
                         throw(UnsupportedFeatureException())
end

jlconvert(::ReadRepresentation{Union, Vlen{DataTypeODR()}}, f::JLDFile, ptr::Ptr) =
    Union{jlconvert(ReadRepresentation(DataType, Vlen{DataTypeODR()}), f, ptr)...}

## Pointers

immutable PointerException <: Exception; end
show(io::IO, ::PointerException) = print(io, "cannot write a pointer to JLD file")
h5fieldtype{T<:Ptr}(::JLDFile, ::Type{T}, ::Initialized) = throw(PointerException())
h5type(::JLDFile, ::Ptr) = throw(PointerException())

## Arrays

# These show up as having T.size == 0, hence the need for
# specialization.
h5fieldtype{T<:Array}(::JLDFile, ::Type{T}, ::Initialized) = ReferenceDatatype()
fieldodr{T<:Array}(::Type{T}, ::Bool) = Reference

# Union{} is not a datatype, so write it as a reference
h5type(f::JLDFile, ::Array{Union{}}) = ReferenceDatatype()
@generated function h5type{T}(f::JLDFile, ::Array{T})
    !hasfielddata(T) ? :(h5type(f, $(Expr(:new, T)))) : :(h5fieldtype(f, T, Val{false}))
end
odr{T,N}(::Type{Array{T,N}}) = fieldodr(T, false)

## SimpleVectors

const H5TYPE_SIMPLEVECTOR = VariableLengthDatatype(ReferenceDatatype())

h5type(f::JLDFile, ::SimpleVector) =
    haskey(f.jlh5type, SimpleVector) ? f.jlh5type[SimpleVector] :
                                       commit(f, H5TYPE_SIMPLEVECTOR, SimpleVector)
odr(::Type{SimpleVector}) = Vlen{Reference}

h5convert!(out::Ptr, ::Type{Vlen{Reference}}, f::JLDFile, x::SimpleVector, wsession::JLDWriteSession) =
    store_vlen!(out, Reference, f, collect(x), wsession)


function constructrr(::JLDFile, ::Type{SimpleVector}, dt::VariableLengthDatatype, ::Void, ::Bool)
    dt == H5TYPE_SIMPLEVECTOR ? (ReadRepresentation(SimpleVector, Vlen{Reference}), true) :
                                throw(UnsupportedFeatureException())
end

jlconvert(::ReadRepresentation{SimpleVector,Vlen{Reference}}, f::JLDFile, ptr::Ptr) =
    Base.svec(jlconvert(ReadRepresentation(Any, Vlen{Reference}), f, ptr)...)

## User-defined types
##
## Similar to special types, but h5convert!/jl_convert are dynamically
## generated.

# jlconvert for types that represented the same way in memory as in the file
jlconvert{T}(::ReadRepresentation{T,T}, ::JLDFile, ptr::Ptr) = unsafe_load(convert(Ptr{T}, ptr))

# jlconvert for other types
immutable UndefinedFieldException
    ty::DataType
    fieldname::Symbol
end

show(io::IO, x::UndefinedFieldException) =
    print(io, """field $fieldname of type $ty must be defined in the current workspace,
                 but was undefined in the file""")

jlconvert_isinitialized(::Any, ::Ptr) = true
jlconvert_canbeuninitialized(::Any) = false
@generated function jlconvert{T,S}(::ReadRepresentation{T,S}, f::JLDFile, ptr::Ptr)
    isa(S, DataType) && return :(convert(T, unsafe_load(convert(Ptr{S}, ptr))))
    S === nothing && return Expr(:new, T)
    @assert isa(S, OnDiskRepresentation)

    offsets, types, odrs = typeof(S).parameters
    types = types.types
    odrs = odrs.types

    blk = Expr(:block)
    args = blk.args
    if isbits(T)
        # For bits types, we should always inline, because otherwise we'll just
        # pass a lot of crap around in registers
        push!(args, Expr(:meta, :inline))
    end
    fsyms = []
    fn = T === Tuple ? [symbol(i) for i = 1:length(types)] : fieldnames(T)
    offset = 0
    for i = 1:length(types)
        offset = offsets[i]
        rtype = types[i]
        odr = odrs[i]

        fsym = symbol(string("field_", fn[i]))
        push!(fsyms, fsym)

        rr = :(ReadRepresentation{$rtype,$odr}())

        if odr === nothing
            # Type is not stored or single instance
            push!(args, :($fsym = $(Expr(:new, T.types[i]))))
        else
            if jlconvert_canbeuninitialized(ReadRepresentation{rtype,odr}())
                push!(args, quote
                    if !jlconvert_isinitialized($rr, ptr+$offset)
                        $(if T <: Tuple || i <= T.ninitialized
                            # Reference must always be initialized
                            :(throw(UndefinedFieldException(T,$(QuoteNode(fn[i])))))
                        else
                            # Reference could be uninitialized
                            :(return $(Expr(:new, T, fsyms[1:i-1]...)))
                        end)
                    end
                end)
            end

            if T === Tuple
                # Special case for reconstructed tuples, where we don't know the
                # field types in advance
                push!(args, :($fsym = jlconvert($rr, f, ptr+$offset)::$rtype))
            else
                ttype = T.types[i]
                push!(args, :($fsym = convert($ttype, jlconvert($rr, f, ptr+$offset)::$rtype)::$ttype))
            end
        end
    end

    push!(args, T <: Tuple ? Expr(:tuple, fsyms...) : Expr(:new, T, fsyms...))

    blk
end

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
h5convert!(::Ptr, ::Void, ::JLDFile, ::ANY, ::JLDWriteSession) = nothing

# All remaining are just unsafe_store! calls
h5convert!{T}(out::Ptr, ::Type{T}, ::JLDFile, x::T, ::JLDWriteSession) =
    (unsafe_store!(convert(Ptr{typeof(x)}, out), x); nothing)
