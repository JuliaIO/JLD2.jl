# The following block allows reading Union types written prior to v0.2
const LEGACY_H5TYPE_UNION = VariableLengthDatatype(H5TYPE_DATATYPE)

function jlconvert(::ReadRepresentation{Union, Vlen{DataTypeODR()}}, f::JLDFile,
                   ptr::Ptr, header_offset::RelOffset)
    v = Union{jlconvert(ReadRepresentation{DataType,Vlen{DataTypeODR()}}(), f, ptr, NULL_REFERENCE)...}
    track_weakref!(f, header_offset, v)
    v
end

constructrr(::JLDFile, ::Type{T}, dt::VariableLengthDatatype, ::Vector{ReadAttribute}) where {T<:Union} =
    dt == LEGACY_H5TYPE_UNION ? (ReadRepresentation{Union,Vlen{DataTypeODR()}}(), true) :
                         throw(UnsupportedFeatureException())
