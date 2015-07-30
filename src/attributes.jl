#
# Attributes
#

# TODO: fix inference when there are attributes
immutable WrittenAttribute{DS<:WriteDataspace,H5T<:H5Datatype,T}
    name::Symbol
    dataspace::DS
    datatype::H5T
    data::T
end

function WrittenAttribute{T}(f::JLDFile, name::Symbol, data::T)
    WrittenAttribute(name, WriteDataspace(f, data, objodr(data)), h5type(f, data), data)
end

immutable ReadAttribute
    name::Symbol
    dataspace::ReadDataspace
    datatype_class::UInt8
    datatype_offset::FileOffset
    data_offset::FileOffset
end

const EMPTY_READ_ATTRIBUTES = ReadAttribute[]

immutable AttributeHeader
    version::UInt8
    flags::UInt8
    name_size::UInt16
    datatype_size::UInt16
    dataspace_size::UInt16
end
define_packed(AttributeHeader)

Base.sizeof(attr::WrittenAttribute) = 8 + symbol_length(attr.name) + 1 + sizeof(attr.datatype) + sizeof(attr.dataspace) +
                                     numel(attr.dataspace) * sizeof(objodr(attr.data))

function write_attribute(io::IO, f::JLDFile, attr::WrittenAttribute, wsession::JLDWriteSession)
    namelen = symbol_length(attr.name)
    write(io, AttributeHeader(0x02, isa(attr.datatype, CommittedDatatype), namelen+1,
                              sizeof(attr.datatype), sizeof(attr.dataspace)))
    write(io, Base.unsafe_convert(Ptr{Cchar}, attr.name), namelen)
    write(io, UInt8(0))
    write(io, attr.datatype)
    write(io, attr.dataspace)
    write_data(f, attr.data, objodr(attr.data), wsession)
end

function read_attribute(io::IO, f::JLDFile)
    ah = read(io, AttributeHeader)
    ah.version == 0x02 || throw(UnsupportedVersionException())
    committed = ah.flags == 1
    !committed && ah.flags != 0 && throw(UnsupportedFeatureException())

    name = symbol(read(io, UInt8, ah.name_size-1))
    read(io, UInt8) == 0 || throw(InvalidDataException())

    datatype_end = position(io) + ah.datatype_size
    datatype_class, datatype_offset = read_datatype_message(io, committed)
    seek(io, datatype_end)

    dataspace_end = position(io) + ah.dataspace_size
    dataspace = read_dataspace_message(io)
    seek(io, dataspace_end)

    ReadAttribute(name, dataspace, datatype_class, datatype_offset, FileOffset(position(io)))
end
