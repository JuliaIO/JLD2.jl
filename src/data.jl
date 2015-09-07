typealias TupleType{T<:Tuple} Type{T}
typealias Initialized Union{Type{Val{true}}, Type{Val{false}}}

immutable OnDiskRepresentation
    offsets::Tuple{Vararg{Int}}
    types::Base.SimpleVector
    odrs::Vector{Any}
end

immutable ODRIndex{N} end

immutable UnknownType{T}
    name::T
    parameters::Vector{Any}

    UnknownType(name) = new(name)
    UnknownType(name, parameters) = new(name, parameters)
end
UnknownType(name) = UnknownType{typeof(name)}(name)
UnknownType(name, parameters) = UnknownType{typeof(name)}(name, parameters)

immutable Vlen{T}
    size::UInt32
    id::GlobalHeapID
end
sizeof{T<:Vlen}(::Type{T}) = 4 + sizeof(GlobalHeapID)

const ODRS = OnDiskRepresentation[
    OnDiskRepresentation((0,sizeof(Vlen{UTF8String})),Base.svec(UTF8String,Vector{Any}),Any[Vlen{UTF8String},Vlen{RelOffset}])
]
typealias DataTypeODR ODRIndex{1}

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
ReadRepresentation(T, S) = ReadRepresentation{T,S}()
ReadRepresentation(T) = ReadRepresentation{T,T}()
sizeof{T,S}(::ReadRepresentation{T,S}) = sizeof(S)

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
@generated function sizeof{N}(::ODRIndex{N})
    odr = ODRS[N]
    odr.offsets[end]+sizeof(odr.odrs[end])
end

# Determines whether a type will have the same layout on disk as in memory
function samelayout(T::DataType)
    isempty(T.types) && return true
    fo = fieldoffsets(T)
    offset = 0
    for i = 1:length(T.types)
        offset != fo[i] && return false
        ty = T.types[i]
        ty !== writeas(ty) && return false
        !samelayout(ty) && return false
        offset += ty.size
    end
    return offset == T.size
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
                return :(initialized ? odr(T) : RelOffset)
            end
        end
    end
    RelOffset
end

# h5fieldtype is fieldodr's HDF5 companion. It should give the HDF5
# datatype reflecting the on-disk representation.
@generated function h5fieldtype{T}(f::JLDFile, writeas::Type{T}, readas::Type,
                                   initialized::Initialized)
    if isleaftype(T)
        if !hasfielddata(T)
            return nothing
        elseif isbits(T) || (isa(initialized, Type{Type{Val{true}}}) && !T.mutable)
            return quote
                @lookup_committed f T
                $(if isempty(T.types)
                    # bitstype
                    :(return commit(f, OpaqueDatatype(sizeof(T)), T, readas))
                else
                    # Compound type
                    :(return commit_compound(f, fieldnames(T), T, readas))
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
    if !hasdata(T)
        # A pointer singleton or ghost. We need to write something, but we'll
        # just write a single byte.
        return nothing
    elseif isbits(T) && samelayout(T)
        # Has a specialized convert method or is an unpadded type
        return T
    end

    offsets = zeros(Int, length(T.types))
    odrs = Array(Any, length(T.types))
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
        offset += sizeof(fodr)
    end

    push!(ODRS, OnDiskRepresentation((offsets...), T.types, odrs))
    Expr(:new, ODRIndex{length(ODRS)})
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
function h5type(f::JLDFile, writtenas::DataType, x)
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
h5type(::JLDFile, writeas::Type, ::Any) =
    throw(ArgumentError("writeas(leaftype) must return a leaf type"))
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
function commit(f::JLDFile, dtype::H5Datatype, writeas::DataType, readas::Type,
                attributes::WrittenAttribute...)
    io = f.io

    # This needs to be written this way or type inference gets unhappy...
    # Also needs to happen here so that we write the DataType type
    # before we try to find where this type will be written
    typeattr = WrittenAttribute(:julia_type,
                                WriteDataspace(f, DataType, odr(DataType)),
                                h5type(f, DataType, DataType), readas)

    offset = f.end_of_data

    seek(io, offset)
    id = length(f.datatypes)+1
    h5o = h5offset(f, offset)
    cdt = CommittedDatatype(h5o, id)
    f.datatype_locations[h5o] = cdt
    f.jlh5type[readas] = cdt
    push!(f.datatypes, dtype)

    if writeas !== readas
        wrtypeattr = WrittenAttribute(:written_type,
                                      WriteDataspace(f, DataType, odr(DataType)),
                                      h5type(f, DataType, DataType), writeas)
        f.h5jltype[cdt] = ReadRepresentation(readas, CustomSerialization{writeas, odr(writeas)})
        commit(f, dtype, tuple(typeattr, wrtypeattr, attributes...))
    else
        f.h5jltype[cdt] = ReadRepresentation(writeas, odr(writeas))
        commit(f, dtype, tuple(typeattr, attributes...))
    end

    cdt::CommittedDatatype
end

function read_field_datatypes(f::JLDFile, attrs::Vector{ReadAttribute})
    for attr in attrs
        if attr.name == :field_datatypes
            return read_attr_data(f, attr, ReferenceDatatype(),
                                  ReadRepresentation(RelOffset))
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
        return (f.h5jltype[cdt] = ReadRepresentation(DataType, DataTypeODR()))
    end

    datatype = read_attr_data(f, julia_type_attr)
    if written_type_attr !== nothing
        # Custom serialization
        readas = datatype
        datatype = read_attr_data(f, written_type_attr)
        rr, canonical = constructrr(f, datatype, dt, attrs)::Tuple{ReadRepresentation,Bool}
        rrty = typeof(rr)
        rr = ReadRepresentation{readas, CustomSerialization{rrty.parameters[1], rrty.parameters[2]}}()
        canonical = canonical && writeas(readas) === datatype
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
    if sizeof(T) == dt.size
        (ReadRepresentation(T), true)
    else
        empty = check_empty(attrs)
        if empty
            if !hasdata(T)
                (ReadRepresentation(T, nothing), true)
            else
                warn("$T has $(sizeof(T)*8) bytes, but written type was empty; reconstructing")
                reconstruct_bitstype(T.name.name, dt.size, empty)
            end
        else
            if isempty(T.types)
                warn("bitstype $T has $(sizeof(T)*8) bits, but written type has $(dt.size*8) bits; reconstructing")
            else
                warn("$T is a non-bitstype, but written type is a bitstype with $(dt.size*8) bits; reconstructing")
            end
            reconstruct_bitstype(T.name.name, dt.size, empty)
        end
    end
end

# constructrr constructs a ReadRepresentation for a given type. This is the
# generic method for all types not specially handled below.
#
# For this method, if hard_failure is true, then we will throw a
# TypeMappingException instead of attempting reconstruction. This helps in cases
# where we can't know if reconstructed parametric types will have a matching
# memory layout without first inspecting the memory layout.
immutable TypeMappingException <: Exception; end
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

    offsets = Array(Int, length(T.types))
    types = Array(Any, length(T.types))
    odrs = Array(Any, length(T.types))
    fn = fieldnames(T)
    fo = fieldoffsets(T)
    samelayout = isbits(T) && sizeof(T) == dt.size
    dtindex = 0
    for i = 1:length(T.types)
        wstype = T.types[i]
        writtenas = writeas(wstype)
        if !hasfielddata(writtenas)
            types[i] = wstype
            if wstype === writtenas
                odrs[i] = nothing
            else
                odrs[i] = CustomSerialization{writtenas,nothing}
            end
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

            readtype, odr = typeof(dtrr).parameters

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
        wodrindex = odr(T)
        offsets = (offsets...)
        if isa(wodrindex, ODRIndex)
            wodr = ODRS[typeof(wodrindex::ODRIndex).parameters[1]::Int]
            tequal = length(wodr.types) == length(types)
            if tequal
                for i = 1:length(wodr.types)
                    if !(wodr.types[i] <: types[i] || (wodr.types[i] === ByteString && types[i] === UTF8String))
                        tequal = false
                        break
                    end
                end
                if tequal && wodr.offsets == offsets && wodr.odrs == odrs
                    return (ReadRepresentation(T, wodrindex), true)
                end
            end
        end
        push!(ODRS, OnDiskRepresentation(offsets, Base.svec(types...), odrs))
        return (ReadRepresentation(T, ODRIndex{length(ODRS)}()), false)
    end
end

# h5convert! stores the HDF5 representation of Julia data to a pointer. This
# method handles types with no padding or references where this is just a simple
# store
h5convert!{T}(out::Ptr, ::Type{T}, ::JLDFile, x::T, ::JLDWriteSession) =
    (unsafe_store!(convert(Ptr{typeof(x)}, out), x); nothing)

# We pack types that have padding using a staged h5convert! method
@generated function h5convert!{N}(out::Ptr, ::ODRIndex{N}, file::JLDFile, x, wsession::JLDWriteSession)
    T = x
    odr = ODRS[N]
    offsets = odr.offsets
    types = odr.types
    members = odr.odrs

    getindex_fn = isa(T, TupleType) ? (:getindex) : (:getfield)
    ex = Expr(:block)
    args = ex.args
    for i = 1:length(offsets)
        member = members[i]
        isa(member, Void) && continue

        offset = offsets[i]
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
immutable UndefinedFieldException
    ty::DataType
    fieldname::Symbol
end
Base.showerror(io::IO, x::UndefinedFieldException) =
    print(io, "field \"", x.fieldname, "\" of type ", x.ty,
          " must be defined in the current workspace, but was undefined in the file")

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

# This jlconvert method handles compound types with padding or references
@generated function jlconvert{T,S}(::ReadRepresentation{T,S}, f::JLDFile, ptr::Ptr,
                                   header_offset::RelOffset)
    isa(S, DataType) && return :(convert(T, unsafe_load(convert(Ptr{S}, ptr))))
    @assert isa(S, ODRIndex)
    odr = ODRS[typeof(S).parameters[1]]

    offsets = odr.offsets
    types = odr.types
    odrs = odr.odrs

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
    fn = T === Tuple ? [symbol(i) for i = 1:length(types)] : fieldnames(T)
    for i = 1:length(types)
        offset = offsets[i]
        rtype = types[i]
        odr = odrs[i]

        fsym = T.mutable ? Expr(:., :obj, QuoteNode(fn[i])) : symbol(string("field_", fn[i]))
        push!(fsyms, fsym)

        rr = ReadRepresentation{rtype,odr}()

        if odr === nothing
            # Type is not stored or single instance
            push!(args, :($fsym = $(Expr(:new, T.types[i]))))
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

## Custom serialization

# wconvert and rconvert do type conversion before reading and writing,
# respectively. These fall back to convert.
wconvert(T, x) = convert(T, x)
rconvert(T, x) = convert(T, x)

sizeof{T,ODR}(::Type{CustomSerialization{T,ODR}}) = sizeof(ODR)

# Usually we want to convert the object and then write it.
@inline h5convert!{T,ODR}(out::Ptr, ::Type{CustomSerialization{T,ODR}}, f::JLDFile,
                          x, wsession::JLDWriteSession) =
    h5convert!(out, ODR, f, wconvert(T, x)::T, wsession)

# When writing as a reference, we don't want to convert the object first. That
# should happen automatically after write_dataset is called so that the written
# object gets the right written_type attribute.
@inline h5convert!{T}(out::Ptr, odr::Type{CustomSerialization{T,RelOffset}},
                      f::JLDFile, x, wsession::JLDWriteSession) =
    h5convert!(out, RelOffset, f, x, wsession)

# When writing as a reference to something that's being custom-serialized as an
# array, we have to convert the object first.
@inline h5convert!{T<:Array}(out::Ptr, odr::Type{CustomSerialization{T,RelOffset}},
                      f::JLDFile, x, wsession::JLDWriteSession) =
    h5convert!(out, RelOffset, f, wconvert(T, x)::T, wsession)

h5convert_uninitialized!{T,ODR}(out::Ptr, odr::Type{CustomSerialization{T,ODR}}) =
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
typealias SignedTypes        Union{Type{Int8}, Type{Int16}, Type{Int32}, Type{Int64}, Type{Int128}}
typealias UnsignedTypes      Union{Type{UInt8}, Type{UInt16}, Type{UInt32}, Type{UInt64}, Type{UInt128}}
typealias FloatTypes         Union{Type{Float16}, Type{Float32}, Type{Float64}}
typealias PrimitiveTypeTypes Union(SignedTypes, UnsignedTypes, FloatTypes)
typealias PrimitiveTypes     Union(Int8, Int16, Int32, Int64, Int128, UInt8, UInt16, UInt32,
                                   UInt64, UInt128, Float16, Float32, Float64)

for T in SignedTypes.types
    @eval h5fieldtype(::JLDFile, ::$T, ::$T, ::Initialized) =
        FixedPointDatatype(sizeof($(T.parameters[1])), true)
end
for T in UnsignedTypes.types
    @eval h5fieldtype(::JLDFile, ::$T, ::$T, ::Initialized) =
        FixedPointDatatype(sizeof($(T.parameters[1])), false)
end

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

h5fieldtype(::JLDFile, ::Type{Float16}, ::Type{Float16}, ::Initialized) =
    FloatingPointDatatype(DT_FLOATING_POINT, 0x20, 0x0f, 0x00, 2, 0, 16, 10, 5, 0, 10, 0x0000000f)
h5fieldtype(::JLDFile, ::Type{Float32}, ::Type{Float32}, ::Initialized) =
    FloatingPointDatatype(DT_FLOATING_POINT, 0x20, 0x1f, 0x00, 4, 0, 32, 23, 8, 0, 23, 0x0000007f)
h5fieldtype(::JLDFile, ::Type{Float64}, ::Type{Float64}, ::Initialized) =
    FloatingPointDatatype(DT_FLOATING_POINT, 0x20, 0x3f, 0x00, 8, 0, 64, 52, 11, 0, 52, 0x000003ff)

function jltype(f::JLDFile, dt::FloatingPointDatatype)
    if dt == h5fieldtype(f, Float64, Float64, Val{true})
        return ReadRepresentation(Float64)
    elseif dt == h5fieldtype(f, Float32, Float32, Val{true})
        return ReadRepresentation(Float32)
    elseif dt == h5fieldtype(f, Float16, Float16, Val{true})
        return ReadRepresentation(Float16)
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
constructrr(::JLDFile, T::PrimitiveTypeTypes, dt::Union{FixedPointDatatype,FloatingPointDatatype},
            ::Vector{ReadAttribute}) =
    dt == h5fieldtype(f, T, T, Val{true}) ? (ReadRepresentation(T), true) :
                                            throw(UnsupportedFeatureException())

## References

h5type(::JLDFile, ::Type{RelOffset}, ::RelOffset) = ReferenceDatatype()
odr(::Type{RelOffset}) = RelOffset

# A hack to prevent us from needing to box the output pointer
# XXX not thread-safe!
const BOXED_PTR = Ref{Ptr{Void}}()
@inline function h5convert!(out::Ptr, odr::Type{RelOffset}, f::JLDFile, x::Any, wsession::JLDWriteSession)
    BOXED_PTR[] = out
    h5convert_with_boxed_ptr!(f, x, wsession)
end
function h5convert_with_boxed_ptr!(f::JLDFile, x::RelOffset, wsession::JLDWriteSession)
    unsafe_store!(convert(Ptr{RelOffset}, BOXED_PTR[]), x)
    nothing
end
function h5convert_with_boxed_ptr!(f::JLDFile, x, wsession::JLDWriteSession)
    ptr = BOXED_PTR[]
    unsafe_store!(convert(Ptr{RelOffset}, ptr), write_ref(f, x, wsession))
    nothing
end
h5convert_uninitialized!(out::Ptr, odr::Type{RelOffset}) =
    (unsafe_store!(convert(Ptr{RelOffset}, out), NULL_REFERENCE); nothing)

# Reading references as references
jlconvert(::ReadRepresentation{RelOffset,RelOffset}, f::JLDFile, ptr::Ptr,
          ::RelOffset) =
    unsafe_load(convert(Ptr{RelOffset}, ptr))
jlconvert_canbeuninitialized(::ReadRepresentation{RelOffset,RelOffset}) = false

# Reading references as other types
@inline function jlconvert{T}(::ReadRepresentation{T,RelOffset}, f::JLDFile, ptr::Ptr,
                              ::RelOffset)
    x = read_dataset(f, unsafe_load(convert(Ptr{RelOffset}, ptr)))
    (isa(x, T) ? x : convert(T, x))::T
end
jlconvert_canbeuninitialized{T}(::ReadRepresentation{T,RelOffset}) = true
jlconvert_isinitialized{T}(::ReadRepresentation{T,RelOffset}, ptr::Ptr) =
    unsafe_load(convert(Ptr{RelOffset}, ptr)) != NULL_REFERENCE

## Routines for variable-length datatypes

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
jlconvert{T,S}(::ReadRepresentation{T,Vlen{S}}, f::JLDFile, ptr::Ptr, ::RelOffset) =
    read_heap_object(f, unsafe_load(convert(Ptr{GlobalHeapID}, ptr+4)), ReadRepresentation{T, S}())
jlconvert_canbeuninitialized{T,S}(::ReadRepresentation{T,Vlen{S}}) = true
jlconvert_isinitialized{T,S}(::ReadRepresentation{T,Vlen{S}}, ptr::Ptr) =
    unsafe_load(convert(Ptr{GlobalHeapID}, ptr+4)) != GlobalHeapID(RelOffset(0), 0)

## ByteStrings

const H5TYPE_VLEN_ASCII = VariableLengthDatatype(DT_VARIABLE_LENGTH, 0x11, 0x00, 0x00,
                                                 sizeof(Vlen{UInt8}),
                                                 FixedPointDatatype(1, false))
const H5TYPE_VLEN_UTF8 = VariableLengthDatatype(DT_VARIABLE_LENGTH, 0x11, 0x01, 0x00,
                                               sizeof(Vlen{UInt8}),
                                               FixedPointDatatype(1, false))

h5fieldtype(::JLDFile, ::Type{ASCIIString}, ::Type{ASCIIString}, ::Initialized) =
    H5TYPE_VLEN_ASCII
h5fieldtype(::JLDFile, ::Type{UTF8String}, ::Type{UTF8String}, ::Initialized) =
    H5TYPE_VLEN_UTF8
h5fieldtype(::JLDFile, ::Type{ByteString}, ::Type{ByteString}, ::Initialized) =
    H5TYPE_VLEN_UTF8
function h5fieldtype(f::JLDFile, writeas::Union{Type{ASCIIString},Type{UTF8String},Type{ByteString}},
                     readas::Type, init::Initialized)
    @lookup_committed f readas
    commit(f, h5fieldtype(f, writeas, writeas, init), writeas, readas)
end

fieldodr(::Type{ASCIIString}, ::Bool) = Vlen{ASCIIString}
fieldodr(::Union{Type{UTF8String}, Type{ByteString}}, ::Bool) = Vlen{UTF8String}

# Stored as variable-length strings
immutable FixedLengthString{T<:AbstractString}
    length::Int
end
sizeof{T<:ByteString}(x::FixedLengthString{T}) = x.length

h5type(f::JLDFile, writeas::Type{ASCIIString}, x::ASCIIString) =
    StringDatatype(typeof(x), sizeof(x))
h5type(f::JLDFile, writeas::Type{UTF8String}, x::UTF8String) =
    StringDatatype(typeof(x), sizeof(x))
h5type(f::JLDFile, writeas::Union{Type{ASCIIString},Type{UTF8String},Type{ByteString}}, x) =
    h5fieldtype(f, writeas, typeof(x), Val{true})
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
        return ReadRepresentation(Any, RelOffset)
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

jlconvert{S<:ByteString}(::ReadRepresentation{ByteString,Vlen{S}},
                                       f::JLDFile, ptr::Ptr, ::RelOffset) =
    UTF8String(jlconvert(ReadRepresentation(UInt8, Vlen{UInt8}), f, ptr, NULL_REFERENCE))
jlconvert{T<:ByteString,S<:ByteString}(::ReadRepresentation{T,Vlen{S}},
                                       f::JLDFile, ptr::Ptr, ::RelOffset) =
    T(jlconvert(ReadRepresentation(UInt8, Vlen{UInt8}), f, ptr, NULL_REFERENCE))
function jlconvert{S<:ByteString}(rr::FixedLengthString{S}, ::JLDFile,
                                  ptr::Ptr, ::RelOffset)
    data = Array(UInt8, rr.length)
    unsafe_copy!(pointer(data), convert(Ptr{UInt8}, ptr), rr.length)
    S(data)
end

# Used only for custom serialization
constructrr(::JLDFile, ::Type{ASCIIString}, dt::VariableLengthDatatype{FixedPointDatatype}, ::Vector{ReadAttribute}) =
    dt == H5TYPE_VLEN_ASCII ? (ReadRepresentation(ASCIIString, Vlen{ASCIIString}), true) :
                              throw(UnsupportedFeatureException())
constructrr(::JLDFile, ::Type{UTF8String}, dt::VariableLengthDatatype{FixedPointDatatype}, ::Vector{ReadAttribute}) =
    dt == H5TYPE_VLEN_UTF8 ? (ReadRepresentation(UTF8String, Vlen{UTF8String}), true) :
                             throw(UnsupportedFeatureException())

## UTF16Strings

const H5TYPE_VLEN_UINT16 = VariableLengthDatatype(FixedPointDatatype(2, true))
function h5fieldtype(f::JLDFile, ::Type{UTF16String}, readas::Type, ::Initialized)
    @lookup_committed f readas
    commit(f, H5TYPE_VLEN_UINT16, UTF16String, readas)
end
fieldodr(::Type{UTF16String}, ::Bool) = Vlen{UInt16}

h5type(f::JLDFile, writeas::Type{UTF16String}, x) =
    h5fieldtype(f, UTF16String, typeof(x), Val{true})
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
function jlconvert(T::ReadRepresentation{UTF16String,Vlen{UInt16}}, f::JLDFile,
                   ptr::Ptr, ::RelOffset)
    vl = jlconvert(ReadRepresentation(UInt16, Vlen{UInt16}), f, ptr, NULL_REFERENCE)
    push!(vl, 0)
    UTF16String(vl)
end

constructrr(::JLDFile, ::Type{UTF16String}, dt::VariableLengthDatatype{FixedPointDatatype}, ::Vector{ReadAttribute}) =
    dt == H5TYPE_VLEN_UINT16 ? (ReadRepresentation(UTF16String, Vlen{UInt16}), true) :
                               throw(UnsupportedFeatureException())

## Symbols

function h5fieldtype(f::JLDFile, ::Type{Symbol}, readas::Type, ::Initialized)
    @lookup_committed f readas
    commit(f, H5TYPE_VLEN_UTF8, Symbol, readas)
end
fieldodr(::Type{Symbol}, ::Bool) = Vlen{UTF8String}

h5type(f::JLDFile, ::Type{Symbol}, x) = h5fieldtype(f, Symbol, typeof(x), Val{true})
odr(::Type{Symbol}) = Vlen{UTF8String}

h5convert!(out::Ptr, ::Type{Vlen{UTF8String}}, f::JLDFile, x::Symbol, ::JLDWriteSession) =
    store_vlen!(out, UInt8, f, string(x).data, f.datatype_wsession)

constructrr(::JLDFile, ::Type{Symbol}, dt::VariableLengthDatatype{FixedPointDatatype}, ::Vector{ReadAttribute}) =
    dt == H5TYPE_VLEN_UTF8 ? (ReadRepresentation(Symbol, Vlen{UTF8String}), true) :
                             throw(UnsupportedFeatureException())

jlconvert(::ReadRepresentation{Symbol,Vlen{UTF8String}}, f::JLDFile, ptr::Ptr,
          ::RelOffset) =
    symbol(jlconvert(ReadRepresentation(UInt8, Vlen{UInt8}), f, ptr, NULL_REFERENCE))

## BigInts and BigFloats

writeas(::Union{Type{BigInt},Type{BigFloat}}) = ASCIIString
wconvert(::Type{ASCIIString}, x::BigInt) = base(62, x)
wconvert(::Type{ASCIIString}, x::BigFloat) = string(x)
rconvert(::Type{BigInt}, x::ASCIIString) = parse(BigInt, x, 62)
rconvert(::Type{BigFloat}, x::ASCIIString) = parse(BigFloat, x)

## DataTypes

const H5TYPE_DATATYPE = CompoundDatatype(
    sizeof(Vlen{UTF8String})+sizeof(Vlen{RelOffset}),
    ["name", "parameters"],
    [0, sizeof(Vlen{UTF8String})],
    [H5TYPE_VLEN_UTF8, VariableLengthDatatype(ReferenceDatatype())]
)

function h5fieldtype{T<:DataType}(f::JLDFile, ::Type{T}, readas::Type, ::Initialized)
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
    f.h5jltype[cdt] = ReadRepresentation(DataType, DataTypeODR())
    push!(f.datatypes, H5TYPE_DATATYPE)

    commit(f, H5TYPE_DATATYPE, (WrittenAttribute(:julia_type, WriteDataspace(f, DataType, odr(DataType)), cdt, DataType),))

    cdt
end
fieldodr{T<:DataType}(::Type{T}, ::Bool) = DataTypeODR()

h5type{T<:DataType}(f::JLDFile, ::Type{T}, x) =
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

function h5convert!(out::Ptr, ::DataTypeODR, f::JLDFile, T::DataType, wsession::JLDWriteSession)
    store_vlen!(out, UInt8, f, typename(T).data, f.datatype_wsession)
    if !isempty(T.parameters)
        refs = RelOffset[begin
            if isa(x, DataType)
                # The heuristic here is that, if the field type is a committed
                # data type, then we commit the datatype and write it as a
                # reference to the committed datatype. Otherwise we write it
                # as a name. This ensures that type parameters that affect the
                # structure of a type are written to the file, so that we can
                # reconstruct the type when the layout depends on the
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
        store_vlen!(out+sizeof(Vlen{UInt8}), RelOffset, f, refs, f.datatype_wsession)
    end
    nothing
end

# Read a type. Returns an instance of UnknownType if the type or parameters
# could not be resolved.
function jlconvert{T}(::ReadRepresentation{T,DataTypeODR()}, f::JLDFile,
                      ptr::Ptr, header_offset::RelOffset)
    hasparams = unsafe_load(convert(Ptr{UInt32}, ptr+sizeof(Vlen{UInt8}))) != 0
    unknown_params = false
    if hasparams
        paramrefs = jlconvert(ReadRepresentation(RelOffset, Vlen{RelOffset}), f,
                              ptr+sizeof(Vlen{UInt8}), NULL_REFERENCE)
        params = Any[begin
            # If the reference is to a committed datatype, read the datatype
            nulldt = CommittedDatatype(UNDEFINED_ADDRESS, 0)
            cdt = get(f.datatype_locations, ref, nulldt)
            res = cdt !== nulldt ? (typeof(jltype(f, cdt)::ReadRepresentation)::DataType).parameters[1] : read_dataset(f, ref)
            unknown_params = unknown_params || isa(res, UnknownType)
            res
        end for ref in paramrefs]
    end

    path = bytestring(jlconvert(ReadRepresentation(UInt8, Vlen{UInt8}), f, ptr,
                                NULL_REFERENCE))
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
            m = (m::DataType){params...}
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

## Union Types

const H5TYPE_UNION = VariableLengthDatatype(H5TYPE_DATATYPE)

function h5fieldtype{T<:Union,S<:Union}(f::JLDFile, ::Type{T}, readas::Type{S}, ::Initialized)
    @lookup_committed f Union
    commit(f, H5TYPE_UNION, Union, Union)
end
function h5fieldtype{T<:Union}(f::JLDFile, ::Type{T}, readas::Type, ::Initialized)
    @lookup_committed f readas
    commit(f, H5TYPE_UNION, Union, readas)
end
fieldodr{T<:Union}(::Type{T}, ::Bool) = Vlen{DataTypeODR()}
h5fieldtype(f::JLDFile, ::Type{Union{}}, ::Initialized) = nothing
fieldodr(::Type{Union{}}, initialized::Bool) = nothing

h5type{T<:Union}(f::JLDFile, ::Type{T}, x::Any) =
    h5fieldtype(f, Union, typeof(x), Val{true})
odr(::Type{Union}) = fieldodr(Union, true)

h5convert!(out::Ptr, ::Type{Vlen{DataTypeODR()}}, f::JLDFile, x::Union, wsession::JLDWriteSession) =
    store_vlen!(out, DataTypeODR(), f, DataType[x for x in x.types], wsession)

function constructrr(::JLDFile, ::Type{Union}, dt::VariableLengthDatatype, ::Vector{ReadAttribute})
    dt == H5TYPE_UNION ? (ReadRepresentation(Union, Vlen{DataTypeODR()}), true) :
                         throw(UnsupportedFeatureException())
end

function jlconvert(::ReadRepresentation{Union, Vlen{DataTypeODR()}}, f::JLDFile,
                   ptr::Ptr, header_offset::RelOffset)
    v = Union{jlconvert(ReadRepresentation(DataType, Vlen{DataTypeODR()}), f, ptr, NULL_REFERENCE)...}
    track_weakref!(f, header_offset, v)
    v
end

## Pointers

immutable PointerException <: Exception; end
Base.showerror(io::IO, ::PointerException) = print(io, "cannot write a pointer to JLD file")
h5fieldtype{T<:Ptr}(::JLDFile, ::Type{T}, ::Type, ::Initialized) = throw(PointerException())
h5type{T<:Ptr}(::JLDFile, ::Type{T}, ::ANY) = throw(PointerException())

## Arrays

h5fieldtype{T<:Array}(::JLDFile, ::Type{T}, ::Type{T}, ::Initialized) =
    ReferenceDatatype()
fieldodr{T<:Array}(::Type{T}, ::Bool) = RelOffset

@generated function h5type{T<:Array}(f::JLDFile, ::Type{T}, ::T)
    if T <: Array{Union{}}
        return :(ReferenceDatatype())
    end
    ty = T.parameters[1]
    writtenas = writeas(ty)
    !hasfielddata(writtenas) ? :(h5type(f, $writtenas, $(Expr(:new, ty)))) : :(h5fieldtype(f, $writtenas, $ty, Val{false}))
end
@inline function odr{T,N}(::Type{Array{T,N}})
    writtenas = writeas(T)
    CustomSerialization(writtenas, T, fieldodr(writtenas, false))
end

# This is all so that when you define a writeas method to write something as an
# array, it writes a reference to the actual array where the datatype is
# committed and has a written_type attribute.
function h5fieldtype{T<:Array}(f::JLDFile, ::Type{T}, readas::DataType,
                               ::Initialized)
    @lookup_committed f readas
    commit(f, ReferenceDatatype(), T, readas)
end
h5type{T<:Array}(f::JLDFile, ::Type{T}, x) =
    h5fieldtype(f, T, typeof(x), Val{true})
_odr{T<:Array}(writtenas::Type{T}, readas::Type{T}, odr) = odr
_odr{T<:Array}(writtenas::Type{T}, readas::DataType, odr) =
    CustomSerialization{writtenas,RelOffset}

function constructrr{T<:Array}(::JLDFile, ::Type{T}, dt::BasicDatatype,
                               attrs::Vector{ReadAttribute})
    dt.class == DT_REFERENCE || throw(UnsupportedFeatureException())
    (ReadRepresentation{Array, RelOffset}(), true)
end

## SimpleVectors

writeas(::Type{SimpleVector}) = Vector{Any}
wconvert(::Type{Vector{Any}}, x::SimpleVector) = Any[x for x in x]
rconvert(::Type{SimpleVector}, x::Vector{Any}) = Base.svec(x...)

## Dicts

function h5type{K,V}(f::JLDFile, ::Type{Dict{K,V}}, x)
    @lookup_committed f typeof(x)
    pairtype = h5fieldtype(f, Pair{K,V}, Pair{K,V}, Val{true})
    eltype_attr = WrittenAttribute(f, :element_type, pairtype.header_offset)
    commit(f, VariableLengthDatatype(f.datatypes[pairtype.index]), Dict{K,V}, typeof(x), eltype_attr)
end

function h5type(f::JLDFile, ::Type{ObjectIdDict}, x)
    @lookup_committed f typeof(x)
    pairtype = h5fieldtype(f, Pair{Any,Any}, Pair{Any,Any}, Val{true})
    eltype_attr = WrittenAttribute(f, :element_type, pairtype.header_offset)
    commit(f, VariableLengthDatatype(f.datatypes[pairtype.index]), ObjectIdDict, typeof(x), eltype_attr)
end

odr{T<:Union(Dict,ObjectIdDict)}(::Type{T}) = Vlen{eltype(T)}

h5convert!{K,V}(out::Ptr, ::Type{Vlen{Pair{K,V}}}, f::JLDFile, x::Associative{K,V}, wsession::JLDWriteSession) =
    store_vlen!(out, odr(Pair{K,V}), f, collect(x), wsession)

function constructrr{T<:Union(Dict,ObjectIdDict)}(f::JLDFile, ::Type{T}, dt::VariableLengthDatatype, attrs::Vector{ReadAttribute})
    for attr in attrs
        if attr.name == :element_type
            ref = read_attr_data(f, attr, ReferenceDatatype(), ReadRepresentation(RelOffset))
            return (ReadRepresentation(T, Vlen{typeof(jltype(f, f.datatype_locations[ref])).parameters[2]}), true)
        end
    end
    throw(InvalidDataException())
end

function constructrr(f::JLDFile, T::UnknownType{DataType}, dt::VariableLengthDatatype, attrs::Vector{ReadAttribute})
    T.name === Dict || throw(InvalidDataException())
    for attr in attrs
        if attr.name == :element_type
            ref = read_attr_data(f, attr, ReferenceDatatype(), ReadRepresentation(RelOffset))
            typ = typeof(jltype(f, f.datatype_locations[ref]))
            return (ReadRepresentation(Dict{typ.parameters[1].parameters...}, Vlen{typ.parameters[2]}), false)
        end
    end
    throw(InvalidDataException())
end

function jlconvert{T<:Union(Dict,ObjectIdDict),P}(::ReadRepresentation{T,Vlen{P}},
                                                  f::JLDFile, ptr::Ptr,
                                                  header_offset::RelOffset)
    h = T()
    track_weakref!(f, header_offset, h)
    pairs = jlconvert(ReadRepresentation(eltype(T), Vlen{P}), f, ptr, NULL_REFERENCE)
    isa(h, Dict) && sizehint!(h::Dict, length(pairs))
    for (k,v) in pairs
        h[k] = v
    end
    h
end

## Type reconstruction

module ReconstructedTypes end

function reconstruct_bitstype(name::Union(Symbol,ByteString), size::Integer, empty::Bool)
    sym = gensym(name)
    eval(ReconstructedTypes, empty ? :(immutable $(sym) end) : :(bitstype $(size*8) $(sym)))
    T = getfield(ReconstructedTypes, sym)
    (ReadRepresentation(T, empty ? nothing : T), false)
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
    if isa(T.name, DataType)
        write(tn, typename(T.name))
    else
        print(tn, T.name)
    end
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
    takebuf_string(tn)
end

# Reconstruct an UnknownType for which we were able to resolve the name, but not
# some of the parameters, by attempting to replace unknown parameters with the
# UB on the type.
function constructrr(f::JLDFile, unk::UnknownType{DataType}, dt::CompoundDatatype,
                     attrs::Vector{ReadAttribute})
    field_datatypes = read_field_datatypes(f, attrs)
    if unk.name === Tuple
        # For a tuple with unknown fields, we should reconstruct the fields
        rodr = reconstruct_odr(f, dt, field_datatypes)[1]
        # This is a "pseudo-RR" since the tuple is not fully parametrized, but
        # the parameters must depend on the types actually encoded in the file
        (ReadRepresentation(Tuple, rodr), false)
    elseif length(unk.name.parameters) != length(unk.parameters)
        warn("read type ", typestring(unk), " has a different number of parameters from type ",
             unk.name, " in workspace; reconstructing")
        reconstruct_compound(f, typestring(unk), dt, field_datatypes)
    else
        params = copy(unk.parameters)
        for i = 1:length(params)
            if isa(params[i], UnknownType)
                param = unk.name.parameters[i]::TypeVar
                params[i] = param.ub
            end
        end

        # Try to construct the rr for the relaxed type. On failure, fall back to
        # reconstruct_compound.
        local T
        try
            T = unk.name{params...}
        catch err
            warn("type parameters for ", typestring(unk)" do not match type ", unk.name, " in workspace; reconstructing")
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
function constructrr(f::JLDFile, unk::UnknownType, dt::CompoundDatatype,
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
    types = Array(Any, length(dt.names))
    odrs = Array(Any, length(dt.names))
    for i = 1:length(dt.names)
        if !isempty(field_datatypes) && (ref = field_datatypes[i]) != NULL_REFERENCE
            dtrr = jltype(f, f.datatype_locations[ref])
        else
            dtrr = jltype(f, dt.members[i])
        end
        types[i], odrs[i] = typeof(dtrr).parameters
    end
    odr = OnDiskRepresentation((dt.offsets...), Base.svec(types...), odrs)
    push!(ODRS, odr)
    return (ODRIndex{length(ODRS)}(), odr)
end

# Reconstruct type that is a "lost cause": either we were not able to resolve
# the name, or the workspace type has additional fields, or cannot convert
# fields to workspace types
function reconstruct_compound(f::JLDFile, T::ByteString, dt::H5Datatype,
                              field_datatypes::Union{Vector{RelOffset},Void})
    rodrindex, rodr = reconstruct_odr(f, dt, field_datatypes)

    # Now reconstruct the type
    reconname = gensym(T)
    eval(ReconstructedTypes, Expr(:composite_type, reconname, Base.svec(),
                                  Base.svec(dt.names...), Any,
                                  rodr.types, false, 0))
    T = getfield(ReconstructedTypes, reconname)

    (ReadRepresentation(T, rodrindex), false)
end
