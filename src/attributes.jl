#
# Attributes
#

# TODO: fix inference when there are attributes
struct WrittenAttribute{DS<:WriteDataspace,H5T<:H5Datatype,T}
    name::Symbol
    dataspace::DS
    datatype::H5T
    data::T
end

function WrittenAttribute(f::JLDFile, name::Symbol, data::T) where T
    WrittenAttribute(name, WriteDataspace(f, data, objodr(data)), h5type(f, data), data)
end

struct ReadAttribute
    name::Symbol
    dataspace::ReadDataspace
    datatype_class::UInt8
    datatype_offset::Int64
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

jlsizeof(attr::WrittenAttribute) = 8 + symbol_length(attr.name) + 1 + jlsizeof(attr.datatype) + jlsizeof(attr.dataspace) +
                                     numel(attr.dataspace) * odr_sizeof(objodr(attr.data))

function write_attribute(io::IO, f::JLDFile, attr::WrittenAttribute, wsession::JLDWriteSession)
    namelen = symbol_length(attr.name)
    jlwrite(io, AttributeHeader(0x02, isa(attr.datatype, CommittedDatatype), namelen+1,
                              jlsizeof(attr.datatype), jlsizeof(attr.dataspace)))
    unsafe_write(io, Base.unsafe_convert(Ptr{Cchar}, attr.name), namelen)
    jlwrite(io, UInt8(0))
    jlwrite(io, attr.datatype)
    jlwrite(io, attr.dataspace)
    odr = objodr(attr.data)
    write_data(io, f, attr.data, odr, datamode(odr), wsession)
end

function read_attribute(io::IO, f::JLDFile)
    ah = jlread(io, AttributeHeader)
    ah.version == 0x02 || throw(UnsupportedVersionException())
    committed = ah.flags == 1
    !committed && ah.flags != 0 && throw(UnsupportedFeatureException())

    name = Symbol(jlread(io, UInt8, ah.name_size-1))
    jlread(io, UInt8) == 0 || throw(InvalidDataException())

    datatype_end = position(io) + ah.datatype_size
    datatype_class, datatype_offset = read_datatype_message(io, f, committed)
    seek(io, datatype_end)

    dataspace_end = position(io) + ah.dataspace_size
    dataspace = read_dataspace_message(io)
    seek(io, dataspace_end)

    ReadAttribute(name, dataspace, datatype_class, datatype_offset, position(io))
end
