#
# Attributes
#

# TODO: fix inference when there are attributes
struct WrittenAttribute{DS<:WriteDataspace,H5T<:H5Datatype,T}
    name::String
    dataspace::DS
    datatype::H5T
    data::T
end
WrittenAttribute(name, dataspace, datatype, data) = WrittenAttribute(String(name), dataspace, datatype, data)
function WrittenAttribute(f::JLDFile, name, data::T) where T
    WrittenAttribute(String(name), WriteDataspace(f, data, objodr(data)), h5type(f, data), data)
end

struct ReadAttribute
    name::Symbol
    dataspace::ReadDataspace
    datatype::H5Datatype
    data_offset::Int64
end

const EMPTY_READ_ATTRIBUTES = ReadAttribute[]

struct AttributeHeader
    version::UInt8
    flags::UInt8
    name_size::UInt16
    datatype_size::UInt16
    dataspace_size::UInt16
end
define_packed(AttributeHeader)

jlsizeof(attr::WrittenAttribute) = 8 + sizeof(attr.name) + 1 + jlsizeof(attr.datatype) + jlsizeof(attr.dataspace) +
                                     numel(attr.dataspace) * odr_sizeof(objodr(attr.data))

function write_attribute(io::IO, f::JLDFile, attr::WrittenAttribute, wsession::JLDWriteSession)
    namelen = sizeof(attr.name)
    jlwrite(io, AttributeHeader(0x02, isa(attr.datatype, CommittedDatatype), namelen+1,
                              jlsizeof(attr.datatype), jlsizeof(attr.dataspace)))
    jlwrite(io, attr.name)
    jlwrite(io, UInt8(0))
    jlwrite(io, attr.datatype)
    jlwrite(io, attr.dataspace)
    odr = objodr(attr.data)
    write_data(io, f, attr.data, odr, datamode(odr), wsession)
end

function read_attribute(f::JLDFile, msg)
    hm = HmWrap(HmAttribute, msg)
    committed = hm.flags == 1
    !committed && hm.flags != 0 && throw(UnsupportedFeatureException())

    hm.flags & 0x10 == 0x10 && @warn "We've got a shared dataspace"
    dsm = Message(HmDataspace, f, hm.dataspace_offset)
    dataspace = ReadDataspace(f, dsm)

    data_offset = fileoffset(f, hm.data_offset)
    ReadAttribute(Symbol(hm.name), dataspace, hm.datatype, data_offset)
end

"""
    load_attributes(f::JLDFile, name::AbstractString)
    load_attributes(g::Group, name::AbstractString)
    load_attributes(g::Group)
    load_attributes(f::JLDFile, offset::RelOffset)

Return a list of attributes attached to the dataset or group.
"""
function load_attributes end

load_attributes(g::Union{JLDFile,Group}, name::AbstractString) =
    attributes(get_dataset(g, name))

function load_attributes(g::JLD2.Group)
    offset = group_offset(g)
    offset == UNDEFINED_ADDRESS && throw(InternalError("Unable to find group in file."))
    attributes(get_dataset(g.f, offset, g, ""))
end

load_attributes(f::Union{JLDFile, Group}, offset::RelOffset) =
    attributes(get_dataset(f, offset))


"""
    read_attr_data(f::JLDFile, attr::ReadAttribute)

[`jlread`](@ref) data from an attribute.
"""
read_attr_data(f::JLDFile, attr::ReadAttribute) =
    read_data(f, attr.dataspace, attr.datatype,
              DataLayout(0,LcCompact,-1,h5offset(f, attr.data_offset)))

"""
    read_attr_data(f::JLDFile, attr::ReadAttribute, expected_datatype::H5Datatype,
                   rr::ReadRepresentation)

[`jlread`](@ref) data from an attribute, assuming a specific HDF5 datatype and
[`ReadRepresentation`](@ref). If the HDF5 datatype does not match, throws an
`UnsupportedFeatureException`. This allows better type stability while
simultaneously validating the data.
"""
function read_attr_data(f::JLDFile, attr::ReadAttribute, expected_datatype::H5Datatype,
                        rr::ReadRepresentation)
    if attr.datatype == expected_datatype
        seek(f.io, attr.data_offset)
        read_dataspace = (attr.dataspace, NULL_REFERENCE, DataLayout(0,LcCompact,-1,h5offset(f, attr.data_offset)), FilterPipeline(), nothing)
        return read_data(f, rr, read_dataspace)
    end
    throw(UnsupportedFeatureException())
end
