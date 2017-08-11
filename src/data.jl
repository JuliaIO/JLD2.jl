const Initialized = Union{Type{Val{true}}, Type{Val{false}}}

const Pointers = Union{Ptr{Void}, IndirectPointer}

struct OnDiskRepresentation{Offsets,JLTypes,H5Types} end
odr_sizeof(::Void) = 0
@Base.pure odr_sizeof(x::DataType) = Int(x.size)

struct UnknownType{T}
    name::T
    parameters::Vector{Any}

    UnknownType{T}(name) where T = new(name)
    UnknownType{T}(name, parameters) where T = new(name, parameters)
end
UnknownType(name) = UnknownType{typeof(name)}(name)
UnknownType(name, parameters) = UnknownType{typeof(name)}(name, parameters)

struct Vlen{T}
    size::UInt32
    id::GlobalHeapID
end
odr_sizeof{T<:Vlen}(::Type{T}) = 4 + sizeof(GlobalHeapID)

# Initial ODR for DataType
const DataTypeODR = OnDiskRepresentation{(0, odr_sizeof(Vlen{String})),Tuple{String,Vector{Any}},Tuple{Vlen{String},Vlen{RelOffset}}}

const NULL_COMMITTED_DATATYPE = CommittedDatatype(RelOffset(0), 0)
# Look up the corresponding committed datatype for a given type
macro lookup_committed(f, T)
    quote
        cdt = get($(esc(f)).jlh5type, $(esc(T)), nothing)
        cdt !== nothing && return cdt::CommittedDatatype
    end
end

function track_weakref!(f::JLDFile, header_offset::RelOffset, v::ANY)
    header_offset !== NULL_REFERENCE && (f.jloffset[header_offset] = WeakRef(v))
    nothing
end

writeas(T::Type) = T

## Generic machinery

# Carries the type and on-disk representation of data to be read from
# the disk
odr_sizeof{T,S}(::ReadRepresentation{T,S}) = odr_sizeof(S)

# Determines whether a specific field type should be saved in the file
@noinline function hasfielddata(T::ANY)
    T === Union{} && return false
    !isleaftype(T) && return true
    T = T::DataType
    (T.mutable || T <: Type) && return true
    hasdata(T)
end

# Determines whether a specific type has fields that should be saved in the file
function hasdata(T::DataType)
    isempty(T.types) && T.size != 0 && return true
    for ty in T.types
        hasfielddata(writeas(ty)) && return true
    end
    false
end

# Gets the size of an on-disk representation
Base.@pure function odr_sizeof{Offsets,JLTypes,H5Types}(::OnDiskRepresentation{Offsets,JLTypes,H5Types})
    Offsets[end]+odr_sizeof(H5Types.parameters[end])
end

# Determines whether a type will have the same layout on disk as in memory
function samelayout(T::DataType)
    isempty(T.types) && return true
    offset = 0
    for i = 1:length(T.types)
        offset != fieldoffset(T, i) && return false
        ty = T.types[i]
        ty !== writeas(ty) && return false
        !samelayout(ty) && return false
        offset += ty.size
    end
    return offset == T.size
end

fieldnames(x::Type{T}) where {T<:Tuple} = [Symbol(x) for x = 1:length(x.types)]
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
                return :(initialized ? odr(T) : RelOffset)
            end
        end
    end
    RelOffset
end

# h5fieldtype is fieldodr's HDF5 companion. It should give the HDF5
# datatype reflecting the on-disk representation.
@generated function h5fieldtype(f::JLDFile, writeas::Type{T}, readas::Type,
                                initialized::Initialized) where T
    if isleaftype(T)
        if !hasfielddata(T)
            return nothing
        elseif isbits(T) || (isa(initialized, Type{Type{Val{true}}}) && !T.mutable)
            return quote
                @lookup_committed f T
                $(if isempty(T.types)
                    # Opaque datatype
                    :(return commit(f, OpaqueDatatype(T.size), T, readas))
                else
                    # Compound type
                    :(return commit_compound(f, fieldnames(T), T, readas))
                end)
            end
        end
    end
    ReferenceDatatype()
end

# Select an ODR, incorporating custom serialization only if the types do not
# match
CustomSerialization{WrittenAs}(::Type{WrittenAs}, ::Type{WrittenAs}, odr) = odr
CustomSerialization{WrittenAs,ReadAs}(::Type{WrittenAs}, ::Type{ReadAs}, odr) =
    CustomSerialization{WrittenAs,odr}

# objodr gives the on-disk representation of a given object. This is
# almost always the on-disk representation of the type. The only
# exception is strings, where the length is encoded in the datatype in
# HDF5, but in the object in Julia.
@inline function objodr(x)
    writtenas = writeas(typeof(x))
    _odr(writtenas, typeof(x), odr(writtenas))
end
_odr(writtenas::DataType, readas::DataType, odr) =
    CustomSerialization(writtenas, readas, odr)

# h5type is objodr's HDF5 companion. It should give the HDF5 datatype
# reflecting the on-disk representation
#
# Performance note: this should be inferrable.
function h5type(f::JLDFile, writtenas, x)
    check_writtenas_type(writtenas)
    T = typeof(x)
    @lookup_committed f T
    if !hasdata(writtenas)
        commit(f, OpaqueDatatype(1), writtenas, T, WrittenAttribute(f, :empty, UInt8(1)))
    elseif isempty(writtenas.types) # bitstype
        commit(f, OpaqueDatatype(writtenas.size), writtenas, T)
    else
        commit_compound(f, fieldnames(writtenas), writtenas, T)
    end
end
check_writtenas_type(::DataType) = nothing
check_writtenas_type(::Any) = throw(ArgumentError("writeas(leaftype) must return a leaf type"))
h5type(f::JLDFile, x) = h5type(f, writeas(typeof(x)), x)

# Make a compound datatype from a set of names and types
function commit_compound(f::JLDFile, names::AbstractVector{Symbol},
                         writtenas::DataType, readas::Type)
    types = writtenas.types
    offsets = Int[]
    h5names = Symbol[]
    members = H5Datatype[]
    fieldtypes = RelOffset[]
    hasfieldtype = false

    offset = 0
    for i = 1:length(types)
        fieldty = types[i]
        fieldwrittenas = writeas(fieldty)
        !hasfielddata(fieldwrittenas) && continue
        dtype = h5fieldtype(f, fieldwrittenas, fieldty, Val{i <= writtenas.ninitialized})
        dtype === nothing && continue
        push!(h5names, names[i])
        if isa(dtype, CommittedDatatype)
            # HDF5 cannot store relationships among committed
            # datatypes. We store these separately in an attribute.
            push!(fieldtypes, dtype.header_offset)
            dtype = f.datatypes[dtype.index]
            hasfieldtype = true
        else
            push!(fieldtypes, NULL_REFERENCE)
        end
        push!(members, dtype)
        push!(offsets, offset)
        offset += dtype.size::UInt32
    end

    @assert offset != 0
    compound = CompoundDatatype(offset, h5names, offsets, members)
    if hasfieldtype
        fieldtypeattr = WrittenAttribute(:field_datatypes,
                                         WriteDataspace(f, fieldtypes, DataType),
                                         ReferenceDatatype(),
                                         fieldtypes)
        commit(f, compound, writtenas, readas, fieldtypeattr)::CommittedDatatype
    else
        commit(f, compound, writtenas, readas)::CommittedDatatype
    end
end

# Write an HDF5 datatype to the file
function commit(f::JLDFile, dtype::H5Datatype, writeas::DataType, readas::DataType,
                attributes::WrittenAttribute...)
    io = f.io

    # This needs to be written this way or type inference gets unhappy...
    # Also needs to happen here so that we write the DataType type
    # before we try to find where this type will be written
    typeattr = WrittenAttribute(
        :julia_type, WriteDataspace(f, DataType, odr(DataType)), h5type(f, DataType, DataType), readas)

    offset = f.end_of_data

    seek(io, offset)
    id = length(f.datatypes)+1
    h5o = h5offset(f, offset)
    cdt = CommittedDatatype(h5o, id)
    f.datatype_locations[h5o] = cdt
    f.jlh5type[readas] = cdt
    push!(f.datatypes, dtype)
    f.types_group[@sprintf("%08d", id)] = h5o

    if writeas !== readas
        wrtypeattr = WrittenAttribute(:written_type,
                                      WriteDataspace(f, DataType, odr(DataType)),
                                      h5type(f, DataType, DataType), writeas)
        f.h5jltype[cdt] = ReadRepresentation{readas,CustomSerialization{writeas, odr(writeas)}}()
        commit(f, dtype, tuple(typeattr, wrtypeattr, attributes...))
    else
        f.h5jltype[cdt] = ReadRepresentation{writeas,odr(writeas)}()
        commit(f, dtype, tuple(typeattr, attributes...))
    end

    cdt::CommittedDatatype
end

function read_field_datatypes(f::JLDFile, attrs::Vector{ReadAttribute})
    for attr in attrs
        if attr.name == :field_datatypes
            return read_attr_data(f, attr, ReferenceDatatype(),
                                  ReadRepresentation{RelOffset,RelOffset}())
        end
    end
    RelOffset[]
end

function check_empty(attrs::Vector{ReadAttribute})
    for attr in attrs
        if attr.name == :empty
            return true
        end
    end
    false
end

# jltype is the inverse of h5type, providing a ReadRepresentation for an
# H5Datatype. We handle committed datatypes here, and other datatypes below.
function jltype(f::JLDFile, cdt::CommittedDatatype)
    haskey(f.h5jltype, cdt) && return f.h5jltype[cdt]::ReadRepresentation
    dt, attrs = read_committed_datatype(f, cdt)

    julia_type_attr = nothing
    written_type_attr = nothing
    for attr in attrs
        if attr.name == :julia_type
            julia_type_attr = attr
        elseif attr.name == :written_type
            written_type_attr = attr
        end
    end

    if isa(julia_type_attr, Void)
        throw(InvalidDataException())
    end
    julia_type_attr = julia_type_attr::ReadAttribute

    # If type of datatype is this datatype, then this is the committed
    # datatype that describes a datatype
    if h5offset(f, julia_type_attr.datatype_offset) == cdt.header_offset
        # Verify that the datatype matches our expectations
        if dt != H5TYPE_DATATYPE
            error("""The HDF5 datatype representing a Julia datatype does not match
                     the expectations of this version of JLD.

                     You may need to update JLD to read this file.""")
        end
        f.jlh5type[DataType] = cdt
        f.datatypes[cdt.index] = dt
        return (f.h5jltype[cdt] = ReadRepresentation{DataType, DataTypeODR()}())
    end

    datatype = read_attr_data(f, julia_type_attr)
    if written_type_attr !== nothing
        # Custom serialization
        readas = datatype
        datatype = read_attr_data(f, written_type_attr)
        if isa(readas, UnknownType)
            warn("custom serialization of ", typestring(readas),
                 " encountered, but the type does not exist in the workspace; the data will be read unconverted")
            rr = (constructrr(f, datatype, dt, attrs)::Tuple{ReadRepresentation,Bool})[1]
            canonical = false
        else
            rr, canonical = constructrr(f, datatype, dt, attrs)::Tuple{ReadRepresentation,Bool}
            rrty = typeof(rr)
            rr = ReadRepresentation{readas, CustomSerialization{rrty.parameters[1], rrty.parameters[2]}}()
            canonical = canonical && writeas(readas) === datatype
        end
    else
        rr, canonical = constructrr(f, datatype, dt, attrs)::Tuple{ReadRepresentation,Bool}
    end

    canonical && (f.jlh5type[datatype] = cdt)
    f.datatypes[cdt.index] = dt
    f.h5jltype[cdt] = rr
end

# Constructs a ReadRepresentation for a given opaque (bitstype) type
function constructrr(::JLDFile, T::DataType, dt::BasicDatatype, attrs::Vector{ReadAttribute})
    dt.class == DT_OPAQUE || throw(UnsupportedFeatureException())
    if T.size == dt.size && isempty(T.types)
        (ReadRepresentation{T,T}(), true)
    else
        empty = check_empty(attrs)
        if empty
            if !hasdata(T)
                (ReadRepresentation{T,nothing}(), true)
            else
                warn("$T has $(T.size*8) bytes, but written type was empty; reconstructing")
                reconstruct_bitstype(T.name.name, dt.size, empty)
            end
        else
            if isempty(T.types)
                warn("primitive type $T has $(T.size*8) bits, but written type has $(dt.size*8) bits; reconstructing")
            else
                warn("$T is a non-primitive type, but written type is a primitive type with $(dt.size*8) bits; reconstructing")
            end
            reconstruct_bitstype(T.name.name, dt.size, empty)
        end
    end
end

"""
constructrr(f::JLDFile, T::DataType, dt::CompoundType, attrs::Vector{ReadAttribute},
            hard_failure::Bool=false)

Constructs a ReadRepresentation for a given type. This is the generic method for all
types not specially handled below.

If hard_failure is true, then throw a TypeMappingException instead of attempting
reconstruction. This helps in cases where we can't know if reconstructed parametric types
will have a matching memory layout without first inspecting the memory layout.
"""
struct TypeMappingException <: Exception; end
function constructrr(f::JLDFile, T::DataType, dt::CompoundDatatype,
                     attrs::Vector{ReadAttribute},
                     hard_failure::Bool=false)
    field_datatypes = read_field_datatypes(f, attrs)

    # If read type is not a leaf type, reconstruct
    if !isleaftype(T)
        warn("read type $T is not a leaf type in workspace; reconstructing")
        return reconstruct_compound(f, string(T), dt, field_datatypes)
    end

    # Map names in dt to their indices
    dtnames = Dict{Symbol,Int}()
    for i = 1:length(dt.names)
        dtnames[dt.names[i]] = i
    end
    mapped = falses(length(dt.names))

    offsets = Vector{Int}(length(T.types))
    types = Vector{Any}(length(T.types))
    odrs = Vector{Any}(length(T.types))
    fn = fieldnames(T)
    samelayout = isbits(T) && T.size == dt.size
    dtindex = 0
    for i = 1:length(T.types)
        wstype = T.types[i]
        writtenas = writeas(wstype)
        if !hasfielddata(writtenas)
            types[i] = wstype
            odrs[i] = wstype === writtenas ? nothing : CustomSerialization{writtenas,nothing}
            offsets[i] = dtindex == 0 ? 0 : (dt.offsets[dtindex] + dt.members[dtindex].size::UInt32)
        else
            if !haskey(dtnames, fn[i])
                hard_failure && throw(TypeMappingException())
                warn("saved type ", T, " is missing field ", fn[i], " in workspace type; reconstructing")
                return reconstruct_compound(f, string(T), dt, field_datatypes)
            end

            dtindex = dtnames[fn[i]]
            if !isempty(field_datatypes) && (ref = field_datatypes[dtindex]) != NULL_REFERENCE
                dtrr = jltype(f, f.datatype_locations[ref])
            else
                dtrr = jltype(f, dt.members[dtindex])
            end

            readtype, odrtype = typeof(dtrr).parameters

            if typeintersect(readtype, wstype) === Union{} &&
               !method_exists(convert, Tuple{Type{wstype}, readtype})
                # Saved type does not match type in workspace and no
                # convert method exists, so we definitely need to reconstruct.
                hard_failure && throw(TypeMappingException())
                warn("saved type ", T, " has field ", fn[i], "::", readtype,
                     ", but workspace type has field ", fn[i], "::", wstype,
                     ", and no applicable convert method exists; reconstructing")
                return reconstruct_compound(f, string(T), dt, field_datatypes)
            end

            types[i] = readtype
            odrs[i] = odrtype
            offsets[i] = dt.offsets[dtindex]
            samelayout = samelayout && offsets[i] == fieldoffset(T, i) && types[i] === wstype

            mapped[dtindex] = true
        end
    end

    if !all(mapped)
        warn("the following fields are present in type ", T,
             " saved in the file but not present in the type the workspace:\n\n",
             join(dt.names[.!mapped], "\n"),
             "\n\nData in these fields will not be accessible")
    end

    if samelayout
        (ReadRepresentation{T,T}(), true)
    else
        wodr = odr(T)
        # This should theoretically be moved inside the if statement, but then it returns
        # the wrong result due to a bug in type inference on 0.6
        typeof_wodr = typeof(wodr)
        offsets = (offsets...)
        if wodr isa OnDiskRepresentation
            odr_offsets = typeof_wodr.parameters[1]
            odr_types = typeof_wodr.parameters[2].parameters
            odr_h5types = typeof_wodr.parameters[3].parameters
            tequal = length(odr_types) == length(types)
            if tequal
                for i = 1:length(types)
                    if !(odr_types[i] <: types[i]) || odr_h5types[i] != odrs[i]
                        tequal = false
                        break
                    end
                end
                if tequal && odr_offsets == offsets
                    # This should not be necessary, but type inference mistakenly changes
                    # the value of wodr here
                    wodr = typeof_wodr()
                    return (ReadRepresentation{T,wodr}(), true)
                end
            end
        end
        return (ReadRepresentation{T,OnDiskRepresentation{offsets, Tuple{types...}, Tuple{odrs...}}()}(), false)
    end
end

function constructrr(f::JLDFile, T::UnionAll, dt::CompoundDatatype,
                     attrs::Vector{ReadAttribute},
                     hard_failure::Bool=false)
    warn("read type $T is not a leaf type in workspace; reconstructing")
    return reconstruct_compound(f, string(T), dt, read_field_datatypes(f, attrs))
end

# h5convert! stores the HDF5 representation of Julia data to a pointer. This
# method handles types with no padding or references where this is just a simple
# store
h5convert!(out::Pointers, ::Type{T}, ::JLDFile, x, ::JLDWriteSession) where {T} =
    (unsafe_store!(convert(Ptr{T}, out), x); nothing)

# We pack types that have padding using a staged h5convert! method
@generated function h5convert!(out::Pointers,
       ::OnDiskRepresentation{Offsets,Types,H5Types},
       file::JLDFile, x, wsession::JLDWriteSession) where {Offsets,Types,H5Types}
    T = x
    types = Types.parameters
    members = H5Types.parameters

    getindex_fn = isa(T, Type{T} where T<:Tuple) ? (:getindex) : (:getfield)
    ex = Expr(:block)
    args = ex.args
    for i = 1:length(Offsets)
        member = members[i]
        isa(member, Void) && continue

        offset = Offsets[i]
        conv = :(h5convert!(out+$offset, $(member), file, convert($(types[i]), $getindex_fn(x, $i)), wsession))
        if i > T.ninitialized && (!isleaftype(x.types[i]) || !isbits(x.types[i]))
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

# jl_canbeuninitialized specifies whether a given ODR could be uninitialized,
# which determines whether or not we'll try to call jlconvert_isinitialized to
# determine whether or not it is actually defined when reconstructing types.
jlconvert_canbeuninitialized(::Any) = false

# jlconvert converts data from a pointer into a Julia object. This method
# handles types where this is just a simple load
@inline jlconvert{T}(::ReadRepresentation{T,T}, ::JLDFile, ptr::Ptr,
                     ::RelOffset) =
    unsafe_load(convert(Ptr{T}, ptr))

# When fields are undefined in the file but can't be in the workspace, we need
# to throw exceptions to prevent errors on null pointer loads
struct UndefinedFieldException
    ty::DataType
    fieldname::Symbol
end
Base.showerror(io::IO, x::UndefinedFieldException) =
    print(io, "field \"", x.fieldname, "\" of type ", x.ty,
          " must be defined in the current workspace, but was undefined in the file")

## Custom serialization

# wconvert and rconvert do type conversion before reading and writing,
# respectively. These fall back to convert.
wconvert(T, x) = convert(T, x)
rconvert(T, x) = convert(T, x)

odr_sizeof{T,ODR}(::Type{CustomSerialization{T,ODR}}) = odr_sizeof(ODR)

# Usually we want to convert the object and then write it.
@inline h5convert!(out::Pointers, ::Type{CustomSerialization{T,ODR}}, f::JLDFile,
                   x, wsession::JLDWriteSession) where {T,ODR} =
    h5convert!(out, ODR, f, wconvert(T, x)::T, wsession)

# When writing as a reference, we don't want to convert the object first. That
# should happen automatically after write_dataset is called so that the written
# object gets the right written_type attribute.
@inline h5convert!(out::Pointers, odr::Type{CustomSerialization{T,RelOffset}},
                   f::JLDFile, x, wsession::JLDWriteSession) where {T} =
    h5convert!(out, RelOffset, f, x, wsession)

# When writing as a reference to something that's being custom-serialized as an
# array, we have to convert the object first.
@inline h5convert!(out::Pointers, odr::Type{CustomSerialization{T,RelOffset}},
            f::JLDFile, x, wsession::JLDWriteSession) where {T<:Array} =
    h5convert!(out, RelOffset, f, wconvert(T, x)::T, wsession)

h5convert_uninitialized!(out::Pointers, odr::Type{CustomSerialization{T,ODR}}) where {T,ODR} =
    h5convert_uninitialized!(out, ODR)

jlconvert_canbeuninitialized{T,S,ODR}(::ReadRepresentation{T,CustomSerialization{S,ODR}}) =
    jlconvert_canbeuninitialized(ODR)
jlconvert_isinitialized{T,S,ODR}(::ReadRepresentation{T,CustomSerialization{S,ODR}}, ptr::Ptr) =
    jlconvert_isinitialized(ReadRepresentation{S,ODR}(), ptr)
jlconvert{T,S,ODR}(::ReadRepresentation{T,CustomSerialization{S,ODR}},
                   f::JLDFile, ptr::Ptr, header_offset::RelOffset) =
    rconvert(T, jlconvert(ReadRepresentation{S,ODR}(), f, ptr, header_offset))::T

## Primitive datatypes
# These get special handling only in that they have different HDF5 type
# representations than ordinary opaque types

# This construction prevents these methods from getting called on type unions
const SignedTypes        = Union{Type{Int8}, Type{Int16}, Type{Int32}, Type{Int64}, Type{Int128}}
const UnsignedTypes      = Union{Type{UInt8}, Type{UInt16}, Type{UInt32}, Type{UInt64}, Type{UInt128}}
const FloatTypes         = Union{Type{Float16}, Type{Float32}, Type{Float64}}
const PrimitiveTypeTypes = Union{SignedTypes, UnsignedTypes, FloatTypes}
const PrimitiveTypes     = Union{Int8, Int16, Int32, Int64, Int128, UInt8, UInt16, UInt32,
                                 UInt64, UInt128, Float16, Float32, Float64}

for T in Base.uniontypes(SignedTypes)
    @eval h5fieldtype(::JLDFile, ::$T, ::$T, ::Initialized) =
        FixedPointDatatype($(T.parameters[1].size), true)
end
for T in Base.uniontypes(UnsignedTypes)
    @eval h5fieldtype(::JLDFile, ::$T, ::$T, ::Initialized) =
        FixedPointDatatype($(T.parameters[1].size), false)
end

function jltype(f::JLDFile, dt::FixedPointDatatype)
    signed = dt.bitfield1 == 0x08 ? true : dt.bitfield1 == 0x00 ? false : throw(UnsupportedFeatureException())
    ((dt.bitfield2 == 0x00) & (dt.bitfield3 == 0x00) & (dt.bitoffset == 0) & (dt.bitprecision == dt.size*8)) ||
        throw(UnsupportedFeatureException())
    if dt.size == 8
        return signed ? ReadRepresentation{Int64,Int64}() : ReadRepresentation{UInt64,UInt64}()
    elseif dt.size == 1
        return signed ? ReadRepresentation{Int8,Int8}() : ReadRepresentation{UInt8,UInt8}()
    elseif dt.size == 4
        return signed ? ReadRepresentation{Int32,Int32}() : ReadRepresentation{UInt32,UInt32}()
    elseif dt.size == 2
        return signed ? ReadRepresentation{Int16,Int16}() : ReadRepresentation{UInt16,UInt16}()
    elseif dt.size == 16
        return signed ? ReadRepresentation{Int128,Int128}() : ReadRepresentation{UInt128,UInt128}()
    else
        throw(UnsupportedFeatureException())
    end
end

h5fieldtype(::JLDFile, ::Type{Float16}, ::Type{Float16}, ::Initialized) =
    FloatingPointDatatype(DT_FLOATING_POINT, 0x20, 0x0f, 0x00, 2, 0, 16, 10, 5, 0, 10, 0x0000000f)
h5fieldtype(::JLDFile, ::Type{Float32}, ::Type{Float32}, ::Initialized) =
    FloatingPointDatatype(DT_FLOATING_POINT, 0x20, 0x1f, 0x00, 4, 0, 32, 23, 8, 0, 23, 0x0000007f)
h5fieldtype(::JLDFile, ::Type{Float64}, ::Type{Float64}, ::Initialized) =
    FloatingPointDatatype(DT_FLOATING_POINT, 0x20, 0x3f, 0x00, 8, 0, 64, 52, 11, 0, 52, 0x000003ff)

function jltype(f::JLDFile, dt::FloatingPointDatatype)
    if dt == h5fieldtype(f, Float64, Float64, Val{true})
        return ReadRepresentation{Float64,Float64}()
    elseif dt == h5fieldtype(f, Float32, Float32, Val{true})
        return ReadRepresentation{Float32,Float32}()
    elseif dt == h5fieldtype(f, Float16, Float16, Val{true})
        return ReadRepresentation{Float16,Float16}()
    else
        throw(UnsupportedFeatureException())
    end
end

function h5fieldtype(f::JLDFile, writeas::PrimitiveTypeTypes, readas::Type, init::Initialized)
    @lookup_committed f readas
    commit(f, h5fieldtype(f, writeas, writeas, init), writeas, readas)
end
h5type(f::JLDFile, writeas::PrimitiveTypeTypes, x) =
    h5fieldtype(f, writeas, typeof(x), Val{true})

# Used only for custom serialization
constructrr(f::JLDFile, T::PrimitiveTypeTypes, dt::Union{FixedPointDatatype,FloatingPointDatatype},
            ::Vector{ReadAttribute}) =
    dt == h5fieldtype(f, T, T, Val{true}) ? (ReadRepresentation{T,T}(), true) :
                                            throw(UnsupportedFeatureException())

## References

h5type(::JLDFile, ::Type{RelOffset}, ::RelOffset) = ReferenceDatatype()
odr(::Type{RelOffset}) = RelOffset

@inline function h5convert!(out::Pointers, odr::Type{RelOffset}, f::JLDFile, x::Any,
                            wsession::JLDWriteSession)
    ref = write_ref(f, x, wsession)
    unsafe_store!(convert(Ptr{RelOffset}, out), ref)
    nothing
end
h5convert_uninitialized!(out::Pointers, odr::Type{RelOffset}) =
    (unsafe_store!(convert(Ptr{RelOffset}, out), NULL_REFERENCE); nothing)

# Reading references as references
jlconvert(::ReadRepresentation{RelOffset,RelOffset}, f::JLDFile, ptr::Ptr,
          ::RelOffset) =
    unsafe_load(convert(Ptr{RelOffset}, ptr))
jlconvert_canbeuninitialized(::ReadRepresentation{RelOffset,RelOffset}) = false

# Reading references as other types
@inline function jlconvert{T}(::ReadRepresentation{T,RelOffset}, f::JLDFile, ptr::Ptr,
                              ::RelOffset)
    x = load_dataset(f, unsafe_load(convert(Ptr{RelOffset}, ptr)))
    (isa(x, T) ? x : convert(T, x))::T
end
jlconvert_canbeuninitialized{T}(::ReadRepresentation{T,RelOffset}) = true
jlconvert_isinitialized{T}(::ReadRepresentation{T,RelOffset}, ptr::Ptr) =
    unsafe_load(convert(Ptr{RelOffset}, ptr)) != NULL_REFERENCE

## Routines for variable-length datatypes

# Write variable-length data and store the offset and length to out pointer
@inline function store_vlen!(out::Pointers, odr, f::JLDFile, x::AbstractVector,
                             wsession::JLDWriteSession)
    unsafe_store!(convert(Ptr{UInt32}, out), length(x))
    obj = write_heap_object(f, odr, x, wsession)
    unsafe_store!(convert(Ptr{GlobalHeapID}, out)+4, obj)
    nothing
end

h5convert!(out::Pointers, ::Type{Vlen{T}}, f::JLDFile, x, wsession::JLDWriteSession) where {T} =
    store_vlen!(out, T, f, x, wsession)

@assert odr_sizeof(Vlen) == sizeof(UInt128)
h5convert_uninitialized!(out::Pointers, odr::Type{T}) where {T<:Vlen} =
    (unsafe_store!(convert(Ptr{Int128}, out), 0); nothing)

# Read variable-length data given offset and length in ptr
jlconvert{T,S}(::ReadRepresentation{T,Vlen{S}}, f::JLDFile, ptr::Ptr, ::RelOffset) =
    read_heap_object(f, unsafe_load(convert(Ptr{GlobalHeapID}, ptr+4)), ReadRepresentation{T, S}())
jlconvert_canbeuninitialized{T,S}(::ReadRepresentation{T,Vlen{S}}) = true
jlconvert_isinitialized{T,S}(::ReadRepresentation{T,Vlen{S}}, ptr::Ptr) =
    unsafe_load(convert(Ptr{GlobalHeapID}, ptr+4)) != GlobalHeapID(RelOffset(0), 0)

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
struct FixedLengthString{T<:AbstractString}
    length::Int
end
odr_sizeof(x::FixedLengthString{String}) = x.length

h5type(f::JLDFile, writeas::Type{String}, x::String) =
    StringDatatype(typeof(x), sizeof(x))
h5type(f::JLDFile, writeas::Type{String}, x) =
    h5fieldtype(f, writeas, typeof(x), Val{true})
odr(::Type{String}) = fieldodr(String, true)
objodr(x::String) = FixedLengthString{String}(sizeof(x))

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
    fls.length == sizeof(x) || throw(InvalidDataException())
    (unsafe_copy!(convert(Ptr{UInt8}, out), pointer(x), fls.length); nothing)
end
h5convert!(out::Pointers, ::Type{Vlen{String}}, f::JLDFile, x, wsession::JLDWriteSession) =
    store_vlen!(out, UInt8, f, Vector{UInt8}(x), wsession)

jlconvert(::ReadRepresentation{String,Vlen{String}}, f::JLDFile, ptr::Ptr, ::RelOffset) =
    String(jlconvert(ReadRepresentation{UInt8,Vlen{UInt8}}(), f, ptr, NULL_REFERENCE))
function jlconvert(rr::FixedLengthString{String}, ::JLDFile, ptr::Ptr, ::RelOffset)
    data = Vector{UInt8}(rr.length)
    unsafe_copy!(pointer(data), convert(Ptr{UInt8}, ptr), rr.length)
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

h5convert!(out::Pointers, ::Type{Vlen{String}}, f::JLDFile, x::Symbol, ::JLDWriteSession) =
    store_vlen!(out, UInt8, f, Vector{UInt8}(String(x)), f.datatype_wsession)

constructrr(::JLDFile, ::Type{Symbol}, dt::VariableLengthDatatype{FixedPointDatatype}, ::Vector{ReadAttribute}) =
    dt == H5TYPE_VLEN_UTF8 ? (ReadRepresentation{Symbol,Vlen{String}}(), true) :
                             throw(UnsupportedFeatureException())

jlconvert(::ReadRepresentation{Symbol,Vlen{String}}, f::JLDFile, ptr::Ptr, ::RelOffset) =
    Symbol(jlconvert(ReadRepresentation{UInt8,Vlen{UInt8}}(), f, ptr, NULL_REFERENCE))

## BigInts and BigFloats

writeas(::Union{Type{BigInt},Type{BigFloat}}) = String
wconvert(::Type{String}, x::BigInt) = base(62, x)
wconvert(::Type{String}, x::BigFloat) = string(x)
rconvert(::Type{BigInt}, x::String) = parse(BigInt, x, 62)
rconvert(::Type{BigFloat}, x::String) = parse(BigFloat, x)

## DataTypes

const H5TYPE_DATATYPE = CompoundDatatype(
    odr_sizeof(Vlen{String})+odr_sizeof(Vlen{RelOffset}),
    ["name", "parameters"],
    [0, odr_sizeof(Vlen{String})],
    [H5TYPE_VLEN_UTF8, VariableLengthDatatype(ReferenceDatatype())]
)

function h5fieldtype(f::JLDFile, ::Type{T}, readas::Type, ::Initialized) where T<:DataType
    if !(readas <: DataType)
        @lookup_committed f readas
        return commit(f, H5TYPE_DATATYPE, DataType, readas)
    end

    @lookup_committed f DataType
    io = f.io
    offset = f.end_of_data

    seek(io, offset)
    id = length(f.datatypes)+1
    h5o = h5offset(f, offset)
    cdt = CommittedDatatype(h5o, id)
    f.datatype_locations[h5o] = cdt
    f.jlh5type[DataType] = cdt
    f.h5jltype[cdt] = ReadRepresentation{DataType,DataTypeODR()}()
    push!(f.datatypes, H5TYPE_DATATYPE)
    f.types_group[@sprintf("%08d", id)] = h5o

    commit(f, H5TYPE_DATATYPE, (WrittenAttribute(:julia_type, WriteDataspace(f, DataType, odr(DataType)), cdt, DataType),))

    cdt
end
fieldodr{T<:DataType}(::Type{T}, ::Bool) = DataTypeODR()

h5type(f::JLDFile, ::Type{T}, x) where {T<:DataType} =
    h5fieldtype(f, DataType, typeof(x), Val{true})
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

function h5convert!(out::Pointers, ::DataTypeODR, f::JLDFile, T::DataType, wsession::JLDWriteSession)
    store_vlen!(out, UInt8, f, Vector{UInt8}(typename(T)), f.datatype_wsession)
    if isempty(T.parameters)
        h5convert_uninitialized!(out+odr_sizeof(Vlen{UInt8}), Vlen{UInt8})
    else
        refs = RelOffset[begin
            if isa(x, DataType)
                # The heuristic here is that, if the field type is a committed data type,
                # then we commit the datatype and write it as a reference to the committed
                # datatype. Otherwise we write it as a name. This ensures that type
                # parameters that affect the structure of a type are written to the file,
                # so that we can reconstruct the type when the layout depends on the
                # parameters.
                dt = h5fieldtype(f, writeas(x), x, Val{true})
                if isa(dt, CommittedDatatype)
                    (dt::CommittedDatatype).header_offset
                else
                    write_ref(f, x, wsession)
                end
            else
                write_ref(f, x, wsession)
            end
        end for x in T.parameters]
        store_vlen!(out+odr_sizeof(Vlen{UInt8}), RelOffset, f, refs, f.datatype_wsession)
    end
    nothing
end

# Read a type. Returns an instance of UnknownType if the type or parameters
# could not be resolved.
function jlconvert{T}(::ReadRepresentation{T,DataTypeODR()}, f::JLDFile,
                      ptr::Ptr, header_offset::RelOffset)
    hasparams = unsafe_load(convert(Ptr{UInt32}, ptr+odr_sizeof(Vlen{UInt8}))) != 0
    unknown_params = false
    if hasparams
        paramrefs = jlconvert(ReadRepresentation{RelOffset,Vlen{RelOffset}}(), f,
                              ptr+odr_sizeof(Vlen{UInt8}), NULL_REFERENCE)
        params = Any[begin
            # If the reference is to a committed datatype, read the datatype
            nulldt = CommittedDatatype(UNDEFINED_ADDRESS, 0)
            cdt = get(f.datatype_locations, ref, nulldt)
            res = cdt !== nulldt ? (typeof(jltype(f, cdt)::ReadRepresentation)::DataType).parameters[1] : load_dataset(f, ref)
            unknown_params = unknown_params || isa(res, UnknownType)
            res
        end for ref in paramrefs]
    end

    path = String(jlconvert(ReadRepresentation{UInt8,Vlen{UInt8}}(), f, ptr, NULL_REFERENCE))
    parts = split(path, '.')
    m = Main
    for part in parts
        sym = Symbol(part)
        if !isdefined(m, sym)
            return hasparams ? UnknownType(path, params) : UnknownType(path)
        end
        m = getfield(m, sym)
    end
    if !isa(m, DataType) && !isa(m, UnionAll)
        return hasparams ? UnknownType(path, params) : UnknownType(path)
    end

    if hasparams
        unknown_params && return UnknownType(m, params)
        try
            m = m{params...}
        catch e
            return UnknownType(m, params)
        end
    elseif m === Tuple
        # Need to instantiate with no parameters, since Tuple is really
        # Tuple{Vararg{Any}}
        m = Tuple{}
    end
    track_weakref!(f, header_offset, m)
    m
end

constructrr{T<:DataType}(::JLDFile, ::Type{T}, dt::CompoundDatatype, ::Vector{ReadAttribute}) =
    dt == H5TYPE_DATATYPE ? (ReadRepresentation{DataType,DataTypeODR()}(), true) :
                            throw(UnsupportedFeatureException())

## Union Types

const H5TYPE_UNION = VariableLengthDatatype(H5TYPE_DATATYPE)

function h5fieldtype(f::JLDFile, ::Type{T}, readas::Type{S}, ::Initialized) where {T<:Union,S<:Union}
    @lookup_committed f Union
    commit(f, H5TYPE_UNION, Union, Union)
end
function h5fieldtype(f::JLDFile, ::Type{T}, readas::Type, ::Initialized) where T<:Union
    @lookup_committed f readas
    commit(f, H5TYPE_UNION, Union, readas)
end
fieldodr{T<:Union}(::Type{T}, ::Bool) = Vlen{DataTypeODR()}
h5fieldtype(f::JLDFile, ::Type{Union{}}, ::Initialized) = nothing
fieldodr(::Type{Union{}}, initialized::Bool) = nothing

h5type(f::JLDFile, ::Type{T}, x::Any) where {T<:Union} =
    h5fieldtype(f, Union, typeof(x), Val{true})
odr(::Type{Union}) = fieldodr(Union, true)

h5convert!(out::Pointers, ::Type{Vlen{DataTypeODR()}}, f::JLDFile, x::Union, wsession::JLDWriteSession) =
    store_vlen!(out, DataTypeODR(), f, Vector{DataType}(Base.uniontypes(x)), wsession)

function jlconvert(::ReadRepresentation{Union, Vlen{DataTypeODR()}}, f::JLDFile,
                   ptr::Ptr, header_offset::RelOffset)
    v = Union{jlconvert(ReadRepresentation{DataType,Vlen{DataTypeODR()}}(), f, ptr, NULL_REFERENCE)...}
    track_weakref!(f, header_offset, v)
    v
end

constructrr{T<:Union}(::JLDFile, ::Type{T}, dt::VariableLengthDatatype, ::Vector{ReadAttribute}) =
    dt == H5TYPE_UNION ? (ReadRepresentation{Union,Vlen{DataTypeODR()}}(), true) :
                         throw(UnsupportedFeatureException())

## UnionAll

# This needs its own h5convert! method, since otherwise we will attempt to specialize the
# generic h5convert! method for the specific UnionAll type rather than for UnionAll
# more generally.
function h5convert!(out::Pointers,
                    odr::OnDiskRepresentation{(0, 8),Tuple{TypeVar,Any},Tuple{JLD2.RelOffset,JLD2.RelOffset}},
                    f::JLDFile, x::UnionAll, wsession::JLDWriteSession)
    h5convert!(out, RelOffset, f, x.var, f.datatype_wsession)
    h5convert!(out+odr_sizeof(RelOffset), RelOffset, f, x.body, f.datatype_wsession)
end

## Pointers

struct PointerException <: Exception; end
Base.showerror(io::IO, ::PointerException) = print(io, "cannot write a pointer to JLD file")
h5fieldtype{T<:Ptr}(::JLDFile, ::Type{T}, ::Type, ::Initialized) = throw(PointerException())
h5type{T<:Ptr}(::JLDFile, ::Type{T}, ::ANY) = throw(PointerException())

## Arrays

h5fieldtype{T<:Array}(::JLDFile, ::Type{T}, ::Type{T}, ::Initialized) =
    ReferenceDatatype()
fieldodr{T<:Array}(::Type{T}, ::Bool) = RelOffset

@inline function odr{T,N}(::Type{Array{T,N}})
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
_odr(writtenas::Type{T}, readas::Type{T}, odr) where {T<:Array} = odr
_odr(writtenas::Type{T}, readas::DataType, odr) where {T<:Array} =
    CustomSerialization{writtenas,RelOffset}

function constructrr{T<:Array}(::JLDFile, ::Type{T}, dt::BasicDatatype,
                               attrs::Vector{ReadAttribute})
    dt.class == DT_REFERENCE || throw(UnsupportedFeatureException())
    (ReadRepresentation{Array, RelOffset}(), true)
end

## SimpleVectors

writeas(::Type{SimpleVector}) = Vector{Any}
wconvert(::Type{Vector{Any}}, x::SimpleVector) = Any[x for x in x]
rconvert(::Type{SimpleVector}, x::Vector{Any}) = Core.svec(x...)

## Dicts

writeas{K,V}(::Type{Dict{K,V}}) = Vector{Pair{K,V}}
writeas(::Type{ObjectIdDict}) = Vector{Pair{Any,Any}}
wconvert{K,V}(::Type{Vector{Pair{K,V}}}, x::Associative{K,V}) = collect(x)
function rconvert{T<:Associative,K,V}(::Type{T}, x::Vector{Pair{K,V}})
    d = T()
    isa(d, Dict) && sizehint!(d::Dict, length(x))
    for (k,v) in x
        d[k] = v
    end
    d
end

## Type reconstruction

module ReconstructedTypes end

function reconstruct_bitstype(name::Union{Symbol,String}, size::Integer, empty::Bool)
    sym = gensym(name)
    eval(ReconstructedTypes, empty ? :(struct $(sym) end) : :(primitive type $(sym) $(Int(size)*8) end))
    T = getfield(ReconstructedTypes, sym)
    (ReadRepresentation{T, empty ? nothing : T}(), false)
end

function constructrr(f::JLDFile, unk::UnknownType, dt::BasicDatatype,
                     attrs::Vector{ReadAttribute})
    warn("type ", typestring(unk), " does not exist in workspace; reconstructing")
    reconstruct_bitstype(typestring(unk), dt.size, check_empty(attrs))
end

# Convert an ordinary type or an UnknownType to a corresponding string. This is
# only used to create gensymmed names for reconstructed types.
function typestring(T::UnknownType)
    tn = IOBuffer()
    print(tn, T.name)
    if isdefined(T, :parameters)
        write(tn, '{')
        for i = 1:length(T.parameters)
            x = T.parameters[i]
            i != 1 && write(tn, ',')
            if isa(x, UnknownType)
                write(tn, typestring(x))
            else
                print(tn, x)
            end
        end
        write(tn, '}')
    end
    String(take!(tn))
end

function constructrr(f::JLDFile, unk::UnknownType{DataType}, dt::CompoundDatatype,
                     attrs::Vector{ReadAttribute})
    field_datatypes = read_field_datatypes(f, attrs)
    if unk.name == Tuple
        # For a tuple with unknown fields, we should reconstruct the fields
        rodr = reconstruct_odr(f, dt, field_datatypes)
        # This is a "pseudo-RR" since the tuple is not fully parametrized, but
        # the parameters must depend on the types actually encoded in the file
        (ReadRepresentation{Tuple,rodr}(), false)
    else
        warn("read type ", typestring(unk), " was parametrized, but type ",
             unk.name, " in workspace is not; reconstructing")
        reconstruct_compound(f, typestring(unk), dt, field_datatypes)
    end
end

"""
    behead(T)

Given a UnionAll type, recursively eliminates the `where` clauses
"""
behead(T::UnionAll) = behead(T.body)
behead(T::ANY) = T

function constructrr(f::JLDFile, unk::UnknownType{UnionAll}, dt::CompoundDatatype,
                     attrs::Vector{ReadAttribute})
    field_datatypes = read_field_datatypes(f, attrs)
    body = behead(unk.name)
    if length(body.parameters) != length(unk.parameters)
        warn("read type ", typestring(unk), " has a different number of parameters from type ",
             unk.name, " in workspace; reconstructing")
        reconstruct_compound(f, typestring(unk), dt, field_datatypes)
    else
        params = copy(unk.parameters)
        for i = 1:length(params)
            if isa(params[i], UnknownType)
                param = body.parameters[i]::TypeVar
                params[i] = param.ub
            end
        end

        # Try to construct the rr for the relaxed type. On failure, fall back to
        # reconstruct_compound.
        local T
        try
            T = unk.name{params...}
        catch err
            warn("type parameters for ", typestring(unk), " do not match type ", unk.name, " in workspace; reconstructing")
            return reconstruct_compound(f, typestring(unk), dt, field_datatypes)
        end

        try
            (rr,) = constructrr(f, T, dt, attrs, true)
            warn("some parameters could not be resolved for type ", typestring(unk), "; reading as ", T)
            return (rr, false)
        catch err
            !isa(err, TypeMappingException) && rethrow(err)
            warn("some parameters could not be resolved for type ", typestring(unk), "; reconstructing")
            return reconstruct_compound(f, typestring(unk), dt, field_datatypes)
        end
    end
end

# An UnknownType for which we were not able to resolve the name.
function constructrr(f::JLDFile, unk::UnknownType{String}, dt::CompoundDatatype,
                     attrs::Vector{ReadAttribute})
    ts = typestring(unk)
    warn("type ", ts, " does not exist in workspace; reconstructing")
    reconstruct_compound(f, ts, dt, read_field_datatypes(f, attrs))
end

# Reconstruct the ODR of a type from the CompoundDatatype and field_datatypes
# attribute
function reconstruct_odr(f::JLDFile, dt::CompoundDatatype,
                         field_datatypes::Vector{RelOffset})
    # Get the type and ODR information for each field
    types = Vector{Any}(length(dt.names))
    h5types = Vector{Any}(length(dt.names))
    for i = 1:length(dt.names)
        if !isempty(field_datatypes) && (ref = field_datatypes[i]) != NULL_REFERENCE
            dtrr = jltype(f, f.datatype_locations[ref])
        else
            dtrr = jltype(f, dt.members[i])
        end
        types[i], h5types[i] = typeof(dtrr).parameters
    end
    return OnDiskRepresentation{(dt.offsets...), Tuple{types...}, Tuple{h5types...}}()
end

# Reconstruct type that is a "lost cause": either we were not able to resolve
# the name, or the workspace type has additional fields, or cannot convert
# fields to workspace types
function reconstruct_compound(f::JLDFile, T::String, dt::H5Datatype,
                              field_datatypes::Union{Vector{RelOffset},Void})
    rodr = reconstruct_odr(f, dt, field_datatypes)
    types = typeof(rodr).parameters[2].parameters

    # Now reconstruct the type
    reconname = gensym(T)
    eval(ReconstructedTypes,
         Expr(VERSION >= v"0.7.0-DEV.1263" ? :struct_type : :composite_type, reconname,
              Core.svec(), Core.svec(dt.names...), Any, types, false, 0))
    T = getfield(ReconstructedTypes, reconname)

    (ReadRepresentation{T,rodr}(), false)
end

# These need to go at the bottom. Also, JLD2 doesn't support custom serialization because
# these methods are not guaranteed to work if you add methods to `writeas`.

@generated function h5type(f::JLDFile, ::Type{T}, ::T) where T<:Array
    if T <: Array{Union{}}
        return :(ReferenceDatatype())
    end
    ty = T.parameters[1]
    writtenas = writeas(ty)
    !hasfielddata(writtenas) ? :(h5type(f, $writtenas, $(Expr(:new, ty)))) : :(h5fieldtype(f, $writtenas, $ty, Val{false}))
end

# jlconvert for empty objects
@generated function jlconvert{T}(::ReadRepresentation{T,nothing}, f::JLDFile, ptr::Ptr,
                                 header_offset::RelOffset)
    T.size == 0 && return Expr(:new, T)

    # In this case, T is a non-empty object, but the written data was empty
    # because the custom serializers for the fields all resulted in empty
    # objects
    return Expr(:new, T, [begin
        writtenas = writeas(ty)
        @assert writtenas.size == 0
        if writtenas === ty
            Expr(:new, ty)
        else
            :(rconvert($ty, $(Expr(:new, writtenas))))
        end
    end for ty in T.types]...)
end

# At present, we write Union{} as an object of Core.TypeofBottom. The method above
# basically works, but `Expr(:new, Type{Union{}})` is a bit weird and causes problems for
# inference. Better to define a separate method.
jlconvert(::ReadRepresentation{Core.TypeofBottom,nothing}, f::JLDFile, ptr::Ptr,
          header_offset::RelOffset) = Union{}

# This jlconvert method handles compound types with padding or references
@generated function jlconvert{T,S}(::ReadRepresentation{T,S}, f::JLDFile, ptr::Ptr,
                                   header_offset::RelOffset)
    isa(S, DataType) && return :(convert(T, unsafe_load(convert(Ptr{S}, ptr))))
    @assert isa(S, OnDiskRepresentation)

    offsets = typeof(S).parameters[1]
    types = typeof(S).parameters[2].parameters
    odrs = typeof(S).parameters[3].parameters

    blk = Expr(:block)
    args = blk.args
    if isbits(T)
        # For bits types, we should always inline, because otherwise we'll just
        # pass a lot of crap around in registers
        push!(args, Expr(:meta, :inline))
    elseif T.mutable
        push!(args, quote
            obj = $(Expr(:new, T))
            track_weakref!(f, header_offset, obj)
        end)
    end
    fsyms = []
    fn = T === Tuple ? [Symbol(i) for i = 1:length(types)] : fieldnames(T)
    for i = 1:length(types)
        offset = offsets[i]
        rtype = types[i]
        odr = odrs[i]

        fsym = T.mutable ? Expr(:., :obj, QuoteNode(fn[i])) : Symbol("field_", fn[i])
        push!(fsyms, fsym)

        rr = ReadRepresentation{rtype,odr}()

        if odr === nothing
            # Type is not stored or single instance
            if T.types[i] == Union{}
                # This cannot be defined
                push!(args, Expr(:return, Expr(:new, T, fsyms[1:i-1]...)))
                return blk
            else
                push!(args, :($fsym = $(Expr(:new, T.types[i]))))
            end
        else
            if jlconvert_canbeuninitialized(rr)
                push!(args, quote
                    if !jlconvert_isinitialized($rr, ptr+$offset)
                        $(if T <: Tuple || i <= T.ninitialized
                            # Reference must always be initialized
                            :(throw(UndefinedFieldException(T,$(QuoteNode(fn[i])))))
                        elseif T.mutable
                            # Reference could be uninitialized
                            :(return obj)
                        else
                            Expr(:return, Expr(:new, T, fsyms[1:i-1]...))
                        end)
                    end
                end)
            end

            if T === Tuple
                # Special case for reconstructed tuples, where we don't know the
                # field types in advance
                push!(args, :($fsym = jlconvert($rr, f, ptr+$offset, NULL_REFERENCE)::$rtype))
            else
                ttype = T.types[i]
                push!(args, :($fsym = convert($ttype, jlconvert($rr, f, ptr+$offset, NULL_REFERENCE)::$rtype)::$ttype))
            end
        end
    end

    push!(args, T.mutable ? (:obj) : T <: Tuple ? Expr(:tuple, fsyms...) : Expr(:new, T, fsyms...))

    blk
end

# odr gives the on-disk representation of a given type, similar to
# fieldodr, but actually encoding the data for things that odr stores
# as references
@generated function odr{T}(::Type{T})
    if !hasdata(T)
        # A pointer singleton or ghost. We need to write something, but we'll
        # just write a single byte.
        return nothing
    elseif isbits(T) && samelayout(T)
        # Has a specialized convert method or is an unpadded type
        return T
    end

    offsets = zeros(Int, length(T.types))
    odrs = Vector{Any}(length(T.types))
    offset = 0
    for i = 1:length(T.types)
        ty = T.types[i]
        writtenas = writeas(ty)
        fodr = fieldodr(writtenas, i <= T.ninitialized)
        if writtenas !== ty && fodr !== nothing
            odrs[i] = CustomSerialization{writtenas,fodr}
        else
            odrs[i] = fodr
        end
        offsets[i] = offset
        offset += odr_sizeof(fodr)
    end

    OnDiskRepresentation{(offsets...), Tuple{T.types...}, Tuple{odrs...}}()
end

abstract type DataMode end
struct ReferenceFree <: DataMode end
struct HasReferences <: DataMode end

@Base.pure datamode{WrittenAs,ODR}(::Type{CustomSerialization{WrittenAs,ODR}}) = datamode(ODR)
@Base.pure datamode(::Union{Type{<:Vlen},Type{RelOffset}}) = HasReferences()
@Base.pure datamode(::DataType) = ReferenceFree()
@Base.pure datamode(::FixedLengthString) = ReferenceFree()
@Base.pure datamode(::Void) = ReferenceFree()
@generated function datamode(odr::OnDiskRepresentation{Offsets,JLTypes,H5Types} where {Offsets,JLTypes}) where H5Types
    for ty in H5Types.parameters
        datamode(ty) == HasReferences() && return HasReferences()
    end
    return ReferenceFree()
end
