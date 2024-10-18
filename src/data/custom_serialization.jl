## Custom serialization

# The default, write a type as itself
writeas(T::Type) = T

# wconvert and rconvert do type conversion before reading and writing,
# respectively. These fall back to convert.
wconvert(T, x) = convert(T, x)
rconvert(T, x) = convert(T, x)
rconvert(::Type{Array{T,N}}, x::Array{T,N}) where {T,N} = x
function rconvert(::Type{Array{T,N}}, x::Array{T2,N}) where {T, T2, N}
    res = Array{T,N}(undef, size(x)...)
    for i in eachindex(x)
        if isassigned(x, i)
            res[i] = rconvert(T, x[i])
        end
    end
    res
end

# Select an ODR, incorporating custom serialization only if the types do not
# match
CustomSerialization(::Type{WrittenAs}, ::Type{WrittenAs}, odr) where {WrittenAs} = odr
CustomSerialization(::Type{WrittenAs}, ::Type{ReadAs}, odr) where {WrittenAs,ReadAs} =
    CustomSerialization{WrittenAs,odr}

odr_sizeof(::Type{CustomSerialization{T,ODR}}) where {T,ODR} = odr_sizeof(ODR)::Int

# Usually we want to convert the object and then write it.
h5convert!(out::Pointers, ::Type{CustomSerialization{T,ODR}}, f::JLDFile,
                   x, wsession::JLDWriteSession) where {T,ODR} =
    h5convert!(out, ODR, f, wconvert(T, x)::T, wsession)

# When writing as a reference, we don't want to convert the object first. That
# should happen automatically after write_dataset is called so that the written
# object gets the right written_type attribute.
h5convert!(out::Pointers, odr::Type{CustomSerialization{T,RelOffset}},
                   f::JLDFile, x, wsession::JLDWriteSession) where {T} =
    h5convert!(out, RelOffset, f, x, wsession)

# When writing as a reference to something that's being custom-serialized as an
# array, we have to convert the object first.
h5convert!(out::Pointers, odr::Type{CustomSerialization{T,RelOffset}},
            f::JLDFile, x, wsession::JLDWriteSession) where {T<:Array} =
    h5convert!(out, RelOffset, f, wconvert(T, x)::T, wsession)

h5convert_uninitialized!(out::Pointers, odr::Type{CustomSerialization{T,ODR}}) where {T,ODR} =
    h5convert_uninitialized!(out, ODR)

jlconvert_canbeuninitialized(::ChangedLayout{T,CustomSerialization{S,ODR}}) where {T,S,ODR} =
    jlconvert_canbeuninitialized(ChangedLayout{S,ODR}())
jlconvert_isinitialized(::ChangedLayout{T,CustomSerialization{S,ODR}}, ptr::Ptr) where {T,S,ODR} =
    jlconvert_isinitialized(ChangedLayout{S,ODR}(), ptr)

function jlconvert(::ChangedLayout{T,CustomSerialization{S,RelOffset}},
        f::JLDFile, ptr::Ptr, header_offset::RelOffset) where {T,S}
    # Concerns objects whose custom serialization is itself only referenced by a RelOffset
    # This be important when the original object is mutable
    offset = jlunsafe_load(pconvert(Ptr{RelOffset}, ptr))
    return rconvert(T, load_dataset(f, offset))
end

function jlconvert(::ChangedLayout{T,CustomSerialization{S,ODR}},
          f::JLDFile, ptr::Ptr, header_offset::RelOffset) where {T,S,ODR}

    if ismutabletype(T) && !(T <: Core.SimpleVector)
        # May encounter a self-referential struct that used custom serialization
        # provide an uninitialized struct and later fill it with values
        obj = newstruct(T)
        track_weakref!(f, header_offset, obj)

        # actually load the data
        v = rconvert(T, jlconvert(ReadRepresentation(S,ODR), f, ptr, header_offset))::T
        # copy fields to initial struct
        for i in 0:nfields(obj)-1
            fieldval = ccall(:jl_get_nth_field, Any, (Any, Csize_t), v, i)
            ccall(:jl_set_nth_field, Nothing, (Any, Csize_t, Any), obj, i, fieldval)
        end
        return obj
    else
        v = rconvert(T, jlconvert(ReadRepresentation(S,ODR), f, ptr, header_offset))::T
        track_weakref!(f, header_offset, v)
        return v
    end
end
