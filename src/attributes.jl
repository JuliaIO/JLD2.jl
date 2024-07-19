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

function read_attribute(f::JLDFile, hm::AbstractHeaderMessage)
    committed = hm.flags == 1
    !committed && hm.flags != 0 && throw(UnsupportedFeatureException())

    hm.hflags & 0x10 == 0x10 && @warn "We've got a shared dataspace"
    dshm = Hmessage(HM_DATASPACE, hm.dataspace_size, hm.hflags,  [hm.dataspace_message...], UNDEFINED_ADDRESS, hm.dataspace_offset)
    dataspace = ReadDataspace(f, dshm)
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

function load_attributes(f::JLDFile, name::AbstractString)
    if isempty(name) || name == "/"
        load_attributes(f, f.root_group_offset)
    else 
        load_attributes(f.root_group,name)
    end
end

function load_attributes(g::Group, name::AbstractString)
    dset = get_dataset(g, name)
    attributes(dset)
end

function load_attributes(g::JLD2.Group)
    f = g.f
    # get offset of group in the file (file handle keeps track)
    if g == f.root_group
        reloffset = f.root_group_offset
    else
        reloffset = findfirst(==(g), f.loaded_groups)
    end
    isnothing(reloffset) && throw(InternalError("Unable to find group in file."))
    # load attributes using file handle and offset
    dset = get_dataset(f, reloffset, g, "")
    attributes(dset)
end

function load_attributes(f::Union{JLDFile, Group}, offset::RelOffset)
    dset = get_dataset(f, offset)
    attributes(dset)
end
