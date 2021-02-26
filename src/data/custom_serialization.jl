## Custom serialization

# The default, write a type as itself
writeas(T::Type) = T


# wconvert and rconvert do type conversion before reading and writing,
# respectively. These fall back to convert.
wconvert(T, x) = convert(T, x)
rconvert(T, x) = convert(T, x)

# Select an ODR, incorporating custom serialization only if the types do not
# match
CustomSerialization(::Type{WrittenAs}, ::Type{WrittenAs}, odr) where {WrittenAs} = odr
CustomSerialization(::Type{WrittenAs}, ::Type{ReadAs}, odr) where {WrittenAs,ReadAs} =
    CustomSerialization{WrittenAs,odr}

odr_sizeof(::Type{CustomSerialization{T,ODR}}) where {T,ODR} = odr_sizeof(ODR)

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

jlconvert_canbeuninitialized(::ReadRepresentation{T,CustomSerialization{S,ODR}}) where {T,S,ODR} =
    jlconvert_canbeuninitialized(ODR)
jlconvert_isinitialized(::ReadRepresentation{T,CustomSerialization{S,ODR}}, ptr::Ptr) where {T,S,ODR} =
    jlconvert_isinitialized(ReadRepresentation{S,ODR}(), ptr)
jlconvert(::ReadRepresentation{T,CustomSerialization{S,ODR}},
          f::JLDFile, ptr::Ptr, header_offset::RelOffset) where {T,S,ODR} =
    rconvert(T, jlconvert(ReadRepresentation{S,ODR}(), f, ptr, header_offset))::T
