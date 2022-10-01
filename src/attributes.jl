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

"""
    read_attribute(io::IO, f::JLDFile)

Read an attribute message at the current postion of the `io` object.
Supports attribute message version 1 and 2.
"""
function read_attribute(io::IO, f::JLDFile)
    pos = position(io)
    ah = jlread(io, AttributeHeader)
    if ah.version == 1
        committed = false
        name = Symbol(jlread(io, UInt8, ah.name_size-1))
        jlread(io, UInt8) == 0 || throw(InvalidDataException())
        skip_to_aligned!(io, pos)

        datatype_end = position(io) + ah.datatype_size
        datatype_class, datatype_offset = read_datatype_message(io, f, committed)
        seek(io, datatype_end)
        skip_to_aligned!(io, pos)


        dataspace_end = position(io) + ah.dataspace_size
        dataspace = read_dataspace_message(io)
        seek(io, dataspace_end)
        skip_to_aligned!(io, pos)

        ReadAttribute(name, dataspace, datatype_class, datatype_offset, position(io))
    elseif ah.version == 2 || ah.version == 3
        committed = ah.flags == 1
        !committed && ah.flags != 0 && throw(UnsupportedFeatureException())

        if ah.version == 3
            name_charset_encoding = jlread(io, UInt8)
        end

        name = Symbol(jlread(io, UInt8, ah.name_size-1))
        jlread(io, UInt8) == 0 || throw(InvalidDataException())

        datatype_end = position(io) + ah.datatype_size
        datatype_class, datatype_offset = read_datatype_message(io, f, committed)
        seek(io, datatype_end)

        dataspace_end = position(io) + ah.dataspace_size
        dataspace = read_dataspace_message(io)
        seek(io, dataspace_end)

        ReadAttribute(name, dataspace, datatype_class, datatype_offset, position(io))
    else
        throw(UnsupportedVersionException("Unknown Attribute Header Version $(ah.version)"))
    end
end

"""
    load_attributes(f::JLDFile, name::AbstractString)
    load_attributes(g::Group, name::AbstractString)
    load_attributes(f::JLDFile, offset::RelOffset)
    
Return a list of attributes attached to the dataset or group.    
"""
function load_attributes(f::JLDFile, name::AbstractString)
    if isempty(name) || name == "/"
        load_attributes(f, f.root_group_offset)
    else 
        load_attributes(f.root_group,name)
    end
end

function load_attributes(g::Group, name::AbstractString)
    f = g.f
    f.n_times_opened == 0 && throw(ArgumentError("file is closed"))
    (g, name) = pathize(g, name, false)
    roffset = lookup_offset(g, name)
    roffset != UNDEFINED_ADDRESS || throw(ArgumentError("did not find a group or dataset named \"$name\""))
    load_attributes(f, roffset)
end

function load_attributes(f::JLDFile, offset::RelOffset)
    io = f.io
    chunk_start::Int64 = fileoffset(f, offset)
    seek(io, chunk_start)
    # Version 1 object header have no signature and start with version
    header_version = jlread(io, UInt8)


    if header_version == 1
        seek(io, chunk_start)
        cio = io
        sz,_,groupflags = read_obj_start(cio)
        chunk_end = position(cio) + sz
        # Skip to nearest 8byte aligned position
        skip_to_aligned!(cio, fileoffset(f, offset))
    else
        header_version = 2
        seek(io, chunk_start)
        cio = begin_checksum_read(io)
        sz,_,groupflags = read_obj_start(cio)
        chunk_end = position(cio) + sz #- 4
    end

    # Messages
    chunks = [(; chunk_start, chunk_end)]
    chunk_number = 0

    attrs = Any[]
    while !isempty(chunks)
        chunk = popfirst!(chunks)
        chunk_start = chunk.chunk_start
        chunk_end = chunk.chunk_end

        if chunk_number > 0 # Don't do this the first time around
            seek(io, chunk_start)
            if header_version == 2
                chunk_end -= 4
                cio = begin_checksum_read(io)
                jlread(cio, UInt32) == OBJECT_HEADER_CONTINUATION_SIGNATURE || throw(InvalidDataException())
            end
        end
        chunk_number += 1

        while position(cio) <= chunk_end-4
            if header_version == 1
                # Message start 8byte aligned relative to object start
                skip_to_aligned!(cio, chunk_start)
                # Version 1 header message is padded
                msg = HeaderMessage(jlread(cio, UInt16), jlread(cio, UInt16), jlread(cio, UInt8))
                skip(cio, 3)
            else # header_version == 2
                msg = jlread(cio, HeaderMessage)
                (groupflags & 4) == 4 && skip(cio, 2) 
            end
            endpos = position(cio) + msg.size

            if msg.msg_type == HM_ATTRIBUTE
                    push!(attrs, read_attribute(cio, f))
            elseif msg.msg_type == HM_OBJECT_HEADER_CONTINUATION
                continuation_offset = fileoffset(f, jlread(cio, RelOffset))
                continuation_length = jlread(cio, Length)
                push!(chunks, (; chunk_start = continuation_offset,
                                 chunk_end = continuation_offset + continuation_length))
            elseif (msg.flags & 2^3) != 0
                throw(UnsupportedFeatureException())
            end
            seek(cio, endpos)
        end
        seek(cio, chunk_end)

        if header_version == 2
            # Checksum
            end_checksum(cio) == jlread(io, UInt32) || throw(InvalidDataException("Invalid Checksum"))
        end
    end

    map(attrs) do attr
        attr_data = 
            try
                read_attr_data(f, attr)
            catch e
                rethrow(e)
                nothing
            end

        attr.name => attr_data
    end
end