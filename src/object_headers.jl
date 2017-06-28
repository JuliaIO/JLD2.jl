#
# Object headers
#

const HM_NIL = 0x00
const HM_DATASPACE = 0x01
const HM_LINK_INFO = 0x02
const HM_DATATYPE = 0x03
const HM_FILL_VALUE_OLD = 0x04
const HM_FILL_VALUE = 0x05
const HM_LINK_MESSAGE = 0x06
const HM_EXTERNAL_FILE_LIST = 0x07
const HM_DATA_LAYOUT = 0x08
const HM_BOGUS = 0x09
const HM_GROUP_INFO = 0x0a
const HM_FILTER_PIPELINE = 0x0b
const HM_ATTRIBUTE = 0x0c
const HM_OBJECT_COMMENT = 0x0d
const HM_SHARED_MESSAGE_TABLE = 0x0f
const HM_OBJECT_HEADER_CONTINUATION = 0x10
const HM_SYMBOL_TABLE = 0x11
const HM_MODIFICATION_TIME = 0x12
const HM_BTREE_K_VALUES = 0x13
const HM_DRIVER_INFO = 0x14
const HM_ATTRIBUTE_INFO = 0x15
const HM_REFERENCE_COUNT = 0x16

const OH_ATTRIBUTE_CREATION_ORDER_TRACKED = 2^2
const OH_ATTRIBUTE_CREATION_ORDER_INDEXED = 2^3
const OH_ATTRIBUTE_PHASE_CHANGE_VALUES_STORED = 2^4
const OH_TIMES_STORED = 2^5

const OBJECT_HEADER_CONTINUATION_SIGNATURE = htol(0x4b48434f) # "OCHK"

struct ObjectStart
    signature::UInt32
    version::UInt8
    flags::UInt8
end
ObjectStart(flags::UInt8) = ObjectStart(OBJECT_HEADER_SIGNATURE, 2, flags)
define_packed(ObjectStart)

# Reads the start of an object including the signature, version, flags,
# and (payload) size. Returns the size.
function read_obj_start(io::IO)
    os = read(io, ObjectStart)
    os.signature == OBJECT_HEADER_SIGNATURE || throw(InvalidDataException())
    os.version == 2 || throw(UnsupportedVersionException())

    if (os.flags & OH_TIMES_STORED) != 0
        # Skip access, modification, change and birth times
        skip(io, 128)
    end
    if (os.flags & OH_ATTRIBUTE_PHASE_CHANGE_VALUES_STORED) != 0
        # Skip maximum # of attributes fields
        skip(io, 32)
    end

    read_size(io, os.flags)
end

struct HeaderMessage
    msg_type::UInt8
    size::UInt16
    flags::UInt8
end
define_packed(HeaderMessage)


function isgroup(f::JLDFile, roffset::RelOffset)
    io = f.io
    seek(io, fileoffset(f, roffset))

    sz = read_obj_start(io)
    pmax::Int64 = position(io) + sz
    while position(io) <= pmax-4
        msg = read(io, HeaderMessage)
        endpos = position(io) + msg.size
        if msg.msg_type == HM_LINK_INFO || msg.msg_type == HM_GROUP_INFO || msg.msg_type == HM_LINK_MESSAGE
            return true
        elseif msg.msg_type == HM_DATASPACE || msg.msg_type == HM_DATATYPE || msg.msg_type == HM_FILL_VALUE || msg.msg_type == HM_DATA_LAYOUT
            return false
        end
        seek(io, endpos)
    end
    return false
end
