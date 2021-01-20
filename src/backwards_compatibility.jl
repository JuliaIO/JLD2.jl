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

# The following definition is needed to correctly load Strings written
# with JLD2 with versions v0.1.12 - v0.3.1
function read_array(f::JLDFile, dataspace::ReadDataspace,
                    rr::FixedLengthString{String}, data_length::Int,
                    filter_id::UInt16, header_offset::RelOffset,
                    attributes::Union{Vector{ReadAttribute},Nothing})
    rrv = ReadRepresentation{UInt8,odr(UInt8)}()
    v = read_array(f, dataspace, rrv, data_length, filter_id, NULL_REFERENCE, attributes)
    String(v)
end
