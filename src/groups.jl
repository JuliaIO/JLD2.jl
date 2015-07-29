#
# Groups
#

immutable Group{T}
    names::T
    offsets::Vector{Offset}
end

immutable LinkInfo
    version::UInt8
    flags::UInt8
    fractal_heap_address::Offset
    name_index_btree::Offset
end
define_packed(LinkInfo)

LinkInfo() = LinkInfo(0, 0, UNDEFINED_ADDRESS, UNDEFINED_ADDRESS)

@enum(CharacterSet,
      CSET_ASCII,
      CSET_UTF8)

const LM_CREATION_ORDER_PRESENT = UInt8(2^2)
const LM_LINK_TYPE_FIELD_PRESENT = UInt8(2^3)
const LM_LINK_NAME_CHARACTER_SET_FIELD_PRESENT = UInt8(2^4)

function read_link(io::IO)
    # Version
    version = read(io, UInt8)
    version == 1 || throw(UnsupportedVersionException())

    # Flags
    flags = read(io, UInt8)

    if (flags & LM_LINK_TYPE_FIELD_PRESENT) != 0
        read(io, UInt8) == 0 || throw(UnsupportedFeatureException())
    end

    if (flags & LM_CREATION_ORDER_PRESENT) != 0
        skip(io, 8)
    end

    # Link name character set
    cset = CSET_ASCII
    if (flags & LM_LINK_NAME_CHARACTER_SET_FIELD_PRESENT) != 0
        cset_byte = read(io, UInt8)
        cset = CharacterSet(cset_byte)
    end

    sz = read_size(io, flags)  # Size
    name = read(io, UInt8, sz) # Link name
    target = read(io, Offset)  # Link information

    if cset == CSET_ASCII
        (ASCIIString(name), target)
    else
        (UTF8String(name), target)
    end
end

sizeof_link(name::ByteString) =
    2 + size_size(sizeof(name)) + sizeof(name) + sizeof(Offset) + isa(name, UTF8String)

function write_link(io::IO, name::ByteString, target::Offset)
    # Version
    write(io, UInt8(1))

    # Flags
    flags = size_flag(sizeof(name))
    if isa(name, UTF8String)
        flags |= LM_LINK_NAME_CHARACTER_SET_FIELD_PRESENT
    end
    write(io, flags::UInt8)

    # Link name character set
    if isa(name, UTF8String)
        write(io, UInt8(CSET_UTF8))
    end

    # Length of link name
    write_size(io, sizeof(name))

    # Link name
    write(io, name)

    # Link target
    write(io, target::Offset)
end

const OH_ATTRIBUTE_CREATION_ORDER_TRACKED = 2^2
const OH_ATTRIBUTE_CREATION_ORDER_INDEXED = 2^3
const OH_ATTRIBUTE_PHASE_CHANGE_VALUES_STORED = 2^4
const OH_TIMES_STORED = 2^5

function payload_size(group::Group)
    # 2 extra headers for link info and group info
    nheaders = length(group.names) + 2
    sz = sizeof(LinkInfo) + 2 + nheaders * sizeof(HeaderMessage)
    for name in group.names
        sz += sizeof_link(name)
    end
    sz
end

function Base.read(io::IO, ::Type{Group})
    cio = begin_checksum(io)
    sz = read_obj_start(cio)
    pmax = position(cio) + sz

    # Messages
    names = ByteString[]
    offsets = Offset[]
    while position(cio) < pmax
        msg = read(cio, HeaderMessage)
        endpos = position(cio) + msg.size
        if msg.msg_type == HM_LINK_INFO
            link_info = read(cio, LinkInfo)
            link_info.fractal_heap_address == UNDEFINED_ADDRESS || throw(UnsupportedFeatureException())
        elseif msg.msg_type == HM_GROUP_INFO
            # This message doesn't help us much, so we ignore it for now
        elseif msg.msg_type == HM_LINK_MESSAGE
            name, offset = read_link(cio)
            push!(names, name)
            push!(offsets, offset)
        elseif (msg.flags & 2^3) != 0
            throw(UnsupportedFeatureException())
        elseif msg.msg_type == HM_NIL
            break
        end
        seek(cio, endpos)
    end
    seek(cio, pmax)

    # Checksum
    end_checksum(cio) == read(io, UInt32) || throw(InvalidDataException())

    Group(names, offsets)
end

function Base.write(io::IO, group::Group)
    psz = payload_size(group)
    sz = sizeof(ObjectStart) + size_size(psz) + psz + 4

    cio = begin_checksum(io, sz - 4)

    # Object header
    write(cio, ObjectStart(size_flag(psz)))
    write_size(cio, psz)
    x = position(cio)

    # Link info message
    write(cio, HeaderMessage(HM_LINK_INFO, sizeof(LinkInfo), 0))
    write(cio, LinkInfo())

    # Group info message
    write(cio, HeaderMessage(HM_GROUP_INFO, 2, 0))
    write(cio, UInt16(0))

    # Datatypes
    for i = 1:length(group.names)
        write(cio, HeaderMessage(HM_LINK_MESSAGE, sizeof_link(group.names[i]), 0))
        write_link(io, group.names[i], group.offsets[i])
    end

    # Checksum
    write(io, end_checksum(cio))
end
