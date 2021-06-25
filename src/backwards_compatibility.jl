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


@static if VERSION < v"1.7.0-A"
    # Location of `mutable` flag is moved from datatype to typename in julia v1.7
    # Switch to using accessor function added in v1.7

    # Borrowed from julia base
    function ismutabletype(@nospecialize(t::Type))
        t = Base.unwrap_unionall(t)
        # TODO: what to do for `Union`?
        return isa(t, DataType) && t.mutable
    end
end

if :ninitialized in fieldnames(DataType)
    # https://github.com/JuliaIO/JLD2.jl/issues/327
    function ninitialized(@nospecialize(T::Type))::Int
        T.ninitialized
    end
else
    function ninitialized(@nospecialize(T::Type))::Int
        fieldcount(T) - T.name.n_uninitialized
    end
end
