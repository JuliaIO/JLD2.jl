# Initial ODR for DataType
const DataTypeODR = OnDiskRepresentation{(0, odr_sizeof(Vlen{String})),Tuple{String,Vector{Any}},Tuple{Vlen{String},Vlen{RelOffset}}, odr_sizeof(Vlen{String})+odr_sizeof(Vlen{RelOffset})}

const NULL_COMMITTED_DATATYPE = CommittedDatatype(UNDEFINED_ADDRESS, 0)

function track_weakref!(f::JLDFile, header_offset::RelOffset, @nospecialize v)
    header_offset !== NULL_REFERENCE && (f.jloffset[header_offset] = WeakRef(v))
    nothing
end

function track_weakref_if_untracked!(f::JLDFile, header_offset::RelOffset, @nospecialize v)
    if header_offset !== NULL_REFERENCE
        if !haskey(f.jloffset, header_offset) || isnothing(f.jloffset[header_offset].value)
            f.jloffset[header_offset] = WeakRef(v)
        end
    end
    nothing
end

## Generic machinery

# Carries the type and on-disk representation of data to be read from
# the disk
odr_sizeof(::ReadRepresentation{T,S}) where {T,S} = odr_sizeof(S)::Int

# Determines whether a specific field type should be saved in the file
function hasfielddata(@nospecialize(T), encounteredtypes=DataType[])::Bool
    T === Union{} && return false
    !isconcretetype(T) && return true
    T = T::DataType
    T in encounteredtypes && return true
    push!(encounteredtypes, T)
    (ismutabletype(T) || T <: Type) && return true
    hasdata(T, encounteredtypes)
end

# Determines whether a specific type has fields that should be saved in the file
@nospecializeinfer function hasdata(@nospecialize(T::DataType), encounteredtypes=DataType[])::Bool
    isempty(T.types) && sizeof(T) != 0 && return true
    for ty in T.types
        hasfielddata(writeas(ty), copy(encounteredtypes)) && return true
    end
    false
end

# Gets the size of an on-disk representation
function odr_sizeof(::Type{OnDiskRepresentation{Offsets,JLTypes,H5Types,Size}}) where {Offsets,JLTypes,H5Types,Size}
    Size::Int
end

# Determines whether a type will have the same layout on disk as in memory
function samelayout(@nospecialize(T::DataType))::Bool
    isempty(T.types) && return true
    offset = 0
    for i = 1:length(T.types)
        offset != fieldoffset(T, i) && return false
        ty = T.types[i]
        ty !== writeas(ty) && return false
        !samelayout(ty) && return false
        offset += sizeof(ty)
    end
    return offset == Base.aligned_sizeof(T)
end
samelayout(::Type) = false

fieldnames(@nospecialize(x::Type{<:Tuple})) = [Symbol(i) for i = 1:length(x.types)]
fieldnames(@nospecialize x) = collect(Base.fieldnames(x))

const MAX_INLINE_SIZE = 2^10
# fieldodr gives the on-disk representation of a field of a given type,
# which is either always initialized (initialized=true) or potentially
# uninitialized (initialized=false)
function fieldodr(::Type{T}, initialized::Bool) where T
    if isconcretetype(T)
        if !hasfielddata(T)
            # A ghost type, so no need to store at all
            return nothing
        elseif isa(T, DataType) && sizeof(T) ≤ MAX_INLINE_SIZE
            if isbitstype(T)
                return odr(T)
            elseif !ismutabletype(T)
                return initialized ? odr(T) : RelOffset
            end
        end
    end
    RelOffset
end

# h5fieldtype is fieldodr's HDF5 companion. It should give the HDF5
# datatype reflecting the on-disk representation.
@nospecializeinfer function h5fieldtype(f::JLDFile, @nospecialize(writeas), @nospecialize(readas::Type),
                                initialized::Initialized)::Union{CommittedDatatype, H5Datatype, Nothing}
    T = writeas
    if isconcretetype(T)
        if !hasfielddata(T)
            return nothing
        elseif (isbitstype(T) || (isa(initialized, Type{Val{true}}) && !ismutabletype(T))) && sizeof(T) ≤ MAX_INLINE_SIZE
            @lookup_committed f T
            if isempty(T.types)
                # Opaque datatype
                return commit(f, OpaqueDatatype(sizeof(T)), T, readas)
            else
                # Compound type
                return commit_compound(f, fieldnames(T), T, readas)
            end
        end
    end
    ReferenceDatatype()
end


# objodr gives the on-disk representation of a given object. This is
# almost always the on-disk representation of the type. The only
# exception is strings, where the length is encoded in the datatype in
# HDF5, but in the object in Julia.
@nospecializeinfer function objodr(@nospecialize(x))
    writtenas = writeas(typeof(x))
    _odr(writtenas, typeof(x), odr(writtenas))
end
_odr(writtenas::DataType, readas::DataType, odr) =
    CustomSerialization(writtenas, readas, odr)

# h5type is objodr's HDF5 companion. It should give the HDF5 datatype
# reflecting the on-disk representation
#
# Performance note: this should be inferable.
@nospecializeinfer function h5type(f::JLDFile, @nospecialize(writtenas), @nospecialize(x))
    check_writtenas_type(writtenas)
    T = typeof(x)
    @lookup_committed f T
    if !hasdata(writtenas)
        commit(f, OpaqueDatatype(1), writtenas, T, WrittenAttribute(f, :empty, UInt8(1)))
    elseif isempty(writtenas.types) # bitstype
        commit(f, OpaqueDatatype(sizeof(writtenas)), writtenas, T)
    else
        commit_compound(f, fieldnames(writtenas), writtenas, T)
    end
end
check_writtenas_type(::DataType) = nothing
check_writtenas_type(::Any) = throw(ArgumentError("writeas(leaftype) must return a leaf type"))
@nospecializeinfer h5type(f::JLDFile, @nospecialize(x)) = h5type(f, writeas(typeof(x)), x)

# Make a compound datatype from a set of names and types
@nospecializeinfer  function commit_compound(f::JLDFile, names::AbstractVector{Symbol},
                         @nospecialize(writtenas::DataType), @nospecialize(readas::Type))
    if f.disable_commit
        throw(ArgumentError("Attempted to commit DataType $writtenas but committing is disabled."))
    end
    types = writtenas.types
    offsets = Int[]
    h5names = Symbol[]
    members = H5Datatype[]
    field_names = String[]
    field_types = RelOffset[]
    anynondefault = false
    offset = 0
    for i = 1:length(types)
        fieldty = types[i]
        fieldwrittenas = writeas(fieldty)
        dtype = h5fieldtype(f, fieldwrittenas, fieldty, Val{i <= ninitialized(writtenas)})
        if isnothing(dtype)
            # this is the function body of h5type(f, fieldwrittenas, x) but x is an instance of fieldty unknownavailable here
            cdt = get(f.jlh5type, fieldty, nothing)
            dtype = if !isnothing(cdt)
                cdt
            elseif !hasdata(fieldwrittenas)
                commit(f, OpaqueDatatype(1), fieldwrittenas, fieldty, WrittenAttribute(f, :empty, UInt8(1)))
            elseif isempty(fieldwrittenas.types) # bitstype
                commit(f, OpaqueDatatype(sizeof(fieldwrittenas)), fieldwrittenas, fieldty)
            else
                commit_compound(f, fieldnames(fieldwrittenas), fieldwrittenas, fieldty)
            end
            push!(field_names, string(names[i]))
            push!(field_types, dtype.header_offset)
            anynondefault = true
            continue
        end

        if isa(dtype, CommittedDatatype)
            # HDF5 cannot store relationships among committed
            # datatypes. We store these separately in an attribute.
            type_offset = dtype.header_offset
            dtype = f.datatypes[dtype.index]
            anynondefault = true
        else
            type_offset = NULL_REFERENCE
        end
        push!(field_names, string(names[i]))
        push!(field_types, type_offset)
        push!(h5names, names[i])
        push!(members, dtype)
        push!(offsets, offset)
        offset += dtype.size::UInt32
    end

    @assert offset != 0
    compound = CompoundDatatype(offset, h5names, offsets, members)
    if anynondefault
        commit(f, compound, writtenas, readas,
            WrittenAttribute(:field_names,
                WriteDataspace(f, field_names, Vlen{String}),
                h5type(f, field_names),
                field_names),
            WrittenAttribute(:field_types,
                WriteDataspace(f, field_types, DataType),
                ReferenceDatatype(),
                field_types))
    else
        commit(f, compound, writtenas, readas)::CommittedDatatype
    end
end

# Write an HDF5 datatype to the file
@nospecializeinfer function commit(f::JLDFile,
        @nospecialize(dtype),#::H5Datatype,
        @nospecialize(writeas::DataType),
        @nospecialize(readas::DataType),
        attributes::WrittenAttribute...)
    if f.disable_commit
        throw(ArgumentError("Attempted to commit DataType $readas but committing is disabled."))
    end

    # This needs to be written this way or type inference gets unhappy...
    # Also needs to happen here so that we write the DataType type
    # before we try to find where this type will be written
    typeattr = WrittenAttribute(
        :julia_type, WriteDataspace(f, DataType, odr(DataType)), h5type(f, DataType, DataType), readas)

    offset = f.end_of_data

    seek(f.io, offset)
    id = length(f.datatypes)+1
    h5o = h5offset(f, offset)
    cdt = CommittedDatatype(h5o, id)
    f.datatype_locations[h5o] = cdt
    f.jlh5type[readas] = cdt
    push!(f.datatypes, dtype)
    f.types_group[string(id, pad=8)] = h5o

    if writeas !== readas
        wrtypeattr = WrittenAttribute(:written_type,
                                      WriteDataspace(f, DataType, odr(DataType)),
                                      h5type(f, DataType, DataType), writeas)
        f.h5jltype[cdt] = MappedRepr{readas,CustomSerialization{writeas, odr(writeas)}}()
        commit(f, dtype, tuple(typeattr, wrtypeattr, attributes...))
    else
        f.h5jltype[cdt] = ReadRepresentation(writeas,odr(writeas))
        commit(f, dtype, tuple(typeattr, attributes...))
    end

    cdt::CommittedDatatype
end



# h5convert! stores the HDF5 representation of Julia data to a pointer. This
# method handles types with no padding or references where this is just a simple
# store
h5convert!(out::Pointers, ::Type{T}, ::JLDFile, x, ::JLDWriteSession) where {T} =
    (jlunsafe_store!(pconvert(Ptr{T}, out), x); nothing)

# We pack types that have padding using a staged h5convert! method
@generated function h5convert!(out::Pointers,
       ::Type{OnDiskRepresentation{Offsets,Types,H5Types,Size}},
       file::JLDFile, x, wsession::JLDWriteSession) where {Offsets,Types,H5Types,Size}
    T = x
    types = Types.parameters
    members = H5Types.parameters

    getindex_fn = isa(T, Type{T} where T<:Tuple) ? (:getindex) : (:getfield)
    ex = Expr(:block)
    args = ex.args
    for i = 1:length(Offsets)
        member = members[i]
        isa(member, Nothing) && continue

        offset = Offsets[i]
        conv = :(h5convert!(out+$offset, $(member), file, convert($(types[i]), $getindex_fn(x, $i)), wsession))
        if i > ninitialized(T) && (!isconcretetype(x.types[i]) || !isbitstype(x.types[i]))
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
jlconvert(::SameRepr{T}, ::JLDFile, ptr::Ptr, ::RelOffset) where {T} =
    jlunsafe_load(pconvert(Ptr{T}, ptr))

# When fields are undefined in the file but can't be in the workspace, we need
# to throw exceptions to prevent errors on null pointer loads
struct UndefinedFieldException
    ty::DataType
    fieldname::Symbol
end
Base.showerror(io::IO, x::UndefinedFieldException) =
    print(io, "field \"", x.fieldname, "\" of type ", x.ty,
          " must be defined in the current workspace, but was undefined in the file")


## References

h5type(::JLDFile, ::Type{RelOffset}, ::RelOffset) = ReferenceDatatype()
odr(::Type{RelOffset}) = RelOffset

function h5convert!(out::Pointers, odr::Type{RelOffset}, f::JLDFile, x::Any,
                            wsession::JLDWriteSession)
    ref = write_ref(f, x, wsession)
    jlunsafe_store!(pconvert(Ptr{RelOffset}, out), ref)
    nothing
end
h5convert_uninitialized!(out::Pointers, odr::Type{RelOffset}) =
    (jlunsafe_store!(pconvert(Ptr{RelOffset}, out), NULL_REFERENCE); nothing)

# Reading references as references
jlconvert(::SameRepr{RelOffset}, f::JLDFile, ptr::Ptr, ::RelOffset) =
    jlunsafe_load(pconvert(Ptr{RelOffset}, ptr))
jlconvert_canbeuninitialized(::SameRepr{RelOffset}) = false

# Reading references as other types
function jlconvert(::MappedRepr{T,RelOffset}, f::JLDFile, ptr::Ptr,
                           ::RelOffset) where T
    x = load_dataset(f, jlunsafe_load(pconvert(Ptr{RelOffset}, ptr)))
    (isa(x, T) ? x : rconvert(T, x))::T
end

jlconvert_canbeuninitialized(::MappedRepr{T,RelOffset}) where {T} = true
jlconvert_isinitialized(::MappedRepr{T,RelOffset}, ptr::Ptr) where {T} =
    jlunsafe_load(pconvert(Ptr{RelOffset}, ptr)) != NULL_REFERENCE

## Routines for variable-length datatypes

# Write variable-length data and store the offset and length to out pointer
function store_vlen!(out::Pointers, odr, f::JLDFile, x::AbstractVector,
                             wsession::JLDWriteSession)
    jlunsafe_store!(pconvert(Ptr{UInt32}, out), length(x))
    obj = write_heap_object(f, odr, x, wsession)
    jlunsafe_store!(pconvert(Ptr{GlobalHeapID}, out)+4, obj)
    nothing
end

h5convert!(out::Pointers, ::Type{Vlen{T}}, f::JLDFile, x, wsession::JLDWriteSession) where {T} =
    store_vlen!(out, T, f, x, wsession)

@assert odr_sizeof(Vlen) == jlsizeof(UInt128)
h5convert_uninitialized!(out::Pointers, odr::Type{T}) where {T<:Vlen} =
    (jlunsafe_store!(pconvert(Ptr{Int128}, out), 0); nothing)

# Read variable-length data given offset and length in ptr
jlconvert(::MappedRepr{T,Vlen{S}}, f::JLDFile, ptr::Ptr, ::RelOffset) where {T,S} =
    read_heap_object(f, jlunsafe_load(pconvert(Ptr{GlobalHeapID}, ptr+4)), ReadRepresentation(T, S))
jlconvert_canbeuninitialized(::MappedRepr{<: Any, <: Vlen}) = true
jlconvert_isinitialized(::MappedRepr{T,Vlen{S}}, ptr::Ptr) where {T,S} =
    jlunsafe_load(pconvert(Ptr{GlobalHeapID}, ptr+4)) != GlobalHeapID(RelOffset(0), 0)



## DataTypes

const H5TYPE_DATATYPE = CompoundDatatype(
    odr_sizeof(Vlen{String})+odr_sizeof(Vlen{RelOffset}),
    [:name, :parameters],
    [0, odr_sizeof(Vlen{String})],
    [H5TYPE_VLEN_UTF8, VariableLengthDatatype(ReferenceDatatype())]
)

function h5fieldtype(f::JLDFile, ::Type{T}, readas::Type, ::Initialized) where T<:DataType
    if f.disable_commit
        throw(ArgumentError("Attempted to commit DataType $readas but committing is disabled."))
    end
    if !(readas <: DataType) || (T isa Type{Type{T}} where T)
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
    f.h5jltype[cdt] = MappedRepr{DataType,DataTypeODR}()
    push!(f.datatypes, H5TYPE_DATATYPE)
    f.types_group[string(id, pad=8)] = h5o


    commit(f, H5TYPE_DATATYPE, (WrittenAttribute(:julia_type, WriteDataspace(f, DataType, odr(DataType)), cdt, DataType),))

    cdt
end
fieldodr(::Type{T}, ::Bool) where {T<:DataType} = DataTypeODR

h5type(f::JLDFile, ::Type{T}, x) where {T<:DataType} =
    h5fieldtype(f, DataType, typeof(x), Val{true})
odr(::Type{T}) where {T<:DataType} = DataTypeODR

function typename(T::DataType)
    s = IOBuffer()
    join(s, fullname(T.name.module), '.')
    print(s, '.', T.name.name)
    return String(take!(s))
end

function refs_from_types(f::JLDFile, types, wsession::JLDWriteSession)
    refs = RelOffset[
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
    for x in types]
end

function h5convert!(out::Pointers, ::Type{DataTypeODR}, f::JLDFile, T::DataType, wsession::JLDWriteSession)
    t = typename(T)
    if T <: Function && isgensym(Symbol(T))
        @warn LazyString("Attempting to store ", T, ".\n",
                         "JLD2 only stores functions by name.\n",
                         " This may not be useful for anonymous functions.")
    end
    store_vlen!(out, UInt8, f, unsafe_wrap(Vector{UInt8}, t), f.datatype_wsession)
    if isempty(T.parameters)
        h5convert_uninitialized!(out+odr_sizeof(Vlen{UInt8}), Vlen{UInt8})
    else
        refs = refs_from_types(f, T.parameters, wsession)
        store_vlen!(out+odr_sizeof(Vlen{UInt8}), RelOffset, f, refs, f.datatype_wsession)
    end
    nothing
end


# This is a trick to compactly write long NTuple
# This uses that NTuple{N,T} === Tuple{T,T,T,T,...,T}
function h5convert!(out::Pointers, ::Type{DataTypeODR}, f::JLDFile, T::Type{<: NTuple}, wsession::JLDWriteSession)
    params = T.parameters
    N = length(params)
    if N ≤ 1 || !(reduce(==, params))
        store_vlen!(out, UInt8, f, unsafe_wrap(Vector{UInt8}, "Tuple"), f.datatype_wsession)
        if N == 0
            h5convert_uninitialized!(out+odr_sizeof(Vlen{UInt8}), Vlen{UInt8})
        else # N==1
            # this also catches NTuples with indeterminate length
            refs = refs_from_types(f, params, wsession)
            store_vlen!(out+odr_sizeof(Vlen{UInt8}), RelOffset, f, refs, f.datatype_wsession)
        end
    else # actual NTuple with more than one entry
        store_vlen!(out, UInt8, f, unsafe_wrap(Vector{UInt8}, "NTuple"), f.datatype_wsession)
        ET = params[1] # T === Tuple{ET,ET,ET,...}
        refs = refs_from_types(f, Any[N,ET], wsession)
        store_vlen!(out+odr_sizeof(Vlen{UInt8}), RelOffset, f, refs, f.datatype_wsession)
    end
    nothing
end

## Union Types

const H5TYPE_UNION = CompoundDatatype(
      odr_sizeof(Vlen{String})+2*odr_sizeof(Vlen{RelOffset}),
      [:description, :datatype, :unionall],
      [0, odr_sizeof(Vlen{String}), odr_sizeof(Vlen{String})+odr_sizeof(Vlen{RelOffset})],
      [H5TYPE_VLEN_UTF8, VariableLengthDatatype(ReferenceDatatype()), VariableLengthDatatype(ReferenceDatatype())]
      )

# ODR for UnionDataType
const UnionTypeODR = OnDiskRepresentation{
      (0, odr_sizeof(Vlen{String}), odr_sizeof(Vlen{String})+odr_sizeof(Vlen{RelOffset})),
      Tuple{String, Vector{Any}, Vector{Any}},
      Tuple{Vlen{String}, Vlen{RelOffset}, Vlen{RelOffset}},
      odr_sizeof(Vlen{String})+2*odr_sizeof(Vlen{RelOffset})}

function h5fieldtype(f::JLDFile, ::Type{T}, readas::Type{S}, ::Initialized) where {T<:Union,S<:Union}
    @lookup_committed f Union
    commit(f, H5TYPE_UNION, Union, Union)
end
function h5fieldtype(f::JLDFile, ::Type{T}, readas::Type, ::Initialized) where T<:Union
    @lookup_committed f readas
    commit(f, H5TYPE_UNION, Union, readas)
end

fieldodr(::Type{T}, ::Bool) where {T<:Union} = UnionTypeODR
h5fieldtype(f::JLDFile, ::Type{Union{}}, ::Initialized) = nothing
fieldodr(::Type{Union{}}, initialized::Bool) = nothing

h5type(f::JLDFile, ::Type{T}, x::Any) where {T<:Union} =
    h5fieldtype(f, Union, typeof(x), Val{true})
odr(::Type{Union}) = fieldodr(Union, true)

function h5convert!(out::Pointers, ::Type{UnionTypeODR}, f::JLDFile, x::Union, wsession::JLDWriteSession)
    # Write a description of the union type
    # This is not needed for loading the file but makes h5dump output clearer
    t = string(x)
    store_vlen!(out, UInt8, f, unsafe_wrap(Vector{UInt8}, t), f.datatype_wsession)
    out += odr_sizeof(Vlen{String})
    dts = filter(t -> t isa DataType, Base.uniontypes(x))
    uls = filter(t -> t isa UnionAll, Base.uniontypes(x))
    if !isempty(dts)
        refs = refs_from_types(f, dts, wsession)
        store_vlen!(out, RelOffset, f, refs, f.datatype_wsession)
    else
        h5convert_uninitialized!(out, Vlen{RelOffset})
    end
    if !isempty(uls)
        refs = RelOffset[write_ref(f, x, wsession) for x in uls]
        store_vlen!(out+odr_sizeof(Vlen{RelOffset}), RelOffset, f, refs, f.datatype_wsession)
    else
        h5convert_uninitialized!(out+odr_sizeof(Vlen{RelOffset}), Vlen{RelOffset})
    end
end

function jlconvert(::MappedRepr{Union, UnionTypeODR}, f::JLDFile,
                   ptr::Ptr, header_offset::RelOffset)
    # Skip union type description in the beginning
    ptr += odr_sizeof(Vlen{String})
    # Reconstruct a Union by reading a list of DataTypes and UnionAlls
    # Lookup of RelOffsets is taken from jlconvert of DataTypes
    datatypes = types_from_refs(f, ptr)
    unionalls = types_from_refs(f, ptr+odr_sizeof(Vlen{RelOffset}))

    v = Union{datatypes..., unionalls...}
    track_weakref!(f, header_offset, v)
    v
end


function constructrr(::JLDFile, ::Type{T}, dt::CompoundDatatype, ::Vector{ReadAttribute}) where {T<:Union}
    dt == H5TYPE_UNION ? (MappedRepr{Union,UnionTypeODR}(), true) :
                         throw(UnsupportedFeatureException())
end

## UnionAll

const UnionAllODR = OnDiskRepresentation{(0, 8),Tuple{TypeVar,Any},Tuple{RelOffset,RelOffset}, 16}

# This needs its own h5convert! method, since otherwise we will attempt to specialize the
# generic h5convert! method for the specific UnionAll type rather than for UnionAll
# more generally.
function h5convert!(out::Pointers,
                    odr::Type{UnionAllODR},
                    f::JLDFile, x::UnionAll, wsession::JLDWriteSession)
    h5convert!(out, RelOffset, f, x.var, f.datatype_wsession)
    h5convert!(out+odr_sizeof(RelOffset), RelOffset, f, x.body, f.datatype_wsession)
end



# The following two definitions are borrowed from BSON.
# They are used to generate instances of arbitrary types
# given their fields regardless of potential constructors.
# It is unclear to the author whether this approach is
# optimal.
newstruct(T) = ccall(:jl_new_struct_uninit, Any, (Any,), T)

function newstruct(T, fields)
    if !ismutabletype(T)
        return ccall(:jl_new_structv, Any, (Any,Ptr{Cvoid},UInt32), T, fields, length(fields))
    else
        # Manual inline of newstruct! to work around bug
        # https://github.com/MikeInnes/BSON.jl/issues/2#issuecomment-452204339
        x = newstruct(T)

        for (i, f) = enumerate(fields)
            ccall(:jl_set_nth_field, Nothing, (Any, Csize_t, Any), x, i-1, f)
        end
        x
    end
end


# jlconvert for empty objects
function jlconvert(@nospecialize(rr::MappedRepr{T,nothing} where T), f::JLDFile, ptr::Ptr,
                              header_offset::RelOffset)::julia_repr(rr)
    T = julia_repr(rr)
    sizeof(T) == 0 && return newstruct(T)::T

    # In this case, T is a non-empty object, but the written data was empty
    # because the custom serializers for the fields all resulted in empty
    # objects
    fields = map(T.types) do ty
        writtenas = writeas(ty)
        @assert sizeof(writtenas) == 0
        if writtenas === ty
            # This will usually equal `ty()` unless ty does not have a
            # constructor without arguments
            jlconvert(MappedRepr{ty,nothing}(), f, ptr, header_offset)
        else
            rconvert(ty,
                jlconvert(MappedRepr{writtenas,nothing}(), f, ptr, header_offset)
            )
        end
    end
    if T <: Tuple
        # Tuples are weird in that you can't instantiate them with Tuple{T,S}(t,s)
        return (fields...,)::T
    end
    return newstruct(T, fields)::T
end

# odr gives the on-disk representation of a given type, similar to
# fieldodr, but actually encoding the data for things that odr stores
# as references
@nospecializeinfer function odr(@nospecialize(T::Type))
    if !hasdata(T)
        # A pointer singleton or ghost. We need to write something, but we'll
        # just write a single byte.
        return nothing
    elseif isbitstype(T) && samelayout(T)
        # Has a specialized convert method or is an unpadded type
        return T
    end

    offsets = zeros(Int, length(T.types))
    odrs = Vector{Any}(undef, length(T.types))
    offset = 0
    for i = 1:length(T.types)
        ty = T.types[i]
        writtenas = writeas(ty)
        fodr = fieldodr(writtenas, i <= ninitialized(T))
        if writtenas !== ty && fodr !== nothing
            odrs[i] = CustomSerialization{writtenas,fodr}
        else
            odrs[i] = fodr
        end
        offsets[i] = offset
        offset += odr_sizeof(fodr)
    end

    OnDiskRepresentation{(offsets...,), Tuple{T.types...}, Tuple{odrs...}, Int(offset)}
end

abstract type DataMode end
struct ReferenceFree <: DataMode end
struct HasReferences <: DataMode end

datamode(::Type{CustomSerialization{WrittenAs,ODR}}) where {WrittenAs,ODR} = datamode(ODR)
datamode(::Union{Type{<:Vlen},Type{RelOffset}}) = HasReferences()
datamode(::DataType) = ReferenceFree()
datamode(::FixedLengthString) = ReferenceFree()
datamode(::AsciiString) = ReferenceFree()
datamode(::Nothing) = ReferenceFree()
function datamode(::Type{OnDiskRepresentation{Offsets,JLTypes,H5Types,Size}}) where {Offsets,JLTypes,H5Types,Size}
    for ty in H5Types.parameters
        datamode(ty) == HasReferences() && return HasReferences()
    end
    return ReferenceFree()
end
