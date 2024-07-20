## Signatures
const OBJECT_HEADER_SIGNATURE = htol(0x5244484f) # "OHDR"
const OH_ATTRIBUTE_CREATION_ORDER_TRACKED = 2^2
const OH_ATTRIBUTE_CREATION_ORDER_INDEXED = 2^3
const OH_ATTRIBUTE_PHASE_CHANGE_VALUES_STORED = 2^4
const OH_TIMES_STORED = 2^5
const OBJECT_HEADER_CONTINUATION_SIGNATURE = htol(0x4b48434f) # "OCHK"

## Enums
@enum LayoutClass::UInt8 begin
    LC_COMPACT_STORAGE = 0x00
    LC_CONTIGUOUS_STORAGE = 0x01
    LC_CHUNKED_STORAGE = 0x02
    LC_VIRTUAL_STORAGE = 0x03
end
Base.convert(::Type{UInt8}, l::LayoutClass) = UInt8(l)

@enum(CharacterSet::UInt8,
      CSET_ASCII,
      CSET_UTF8)


# Header Message Types
# These are identified by their UInt8 value in the file
@enum HeaderMessageTypes::UInt8 begin
    HM_NIL = 0x00
    HM_DATASPACE = 0x01
    HM_LINK_INFO = 0x02
    HM_DATATYPE = 0x03
    HM_FILL_VALUE_OLD = 0x04
    HM_FILL_VALUE = 0x05
    HM_LINK_MESSAGE = 0x06
    HM_EXTERNAL_FILE_LIST = 0x07
    HM_DATA_LAYOUT = 0x08
    HM_BOGUS = 0x09
    HM_GROUP_INFO = 0x0a
    HM_FILTER_PIPELINE = 0x0b
    HM_ATTRIBUTE = 0x0c
    HM_OBJECT_COMMENT = 0x0d
    HM_SHARED_MESSAGE_TABLE = 0x0f
    HM_OBJECT_HEADER_CONTINUATION = 0x10
    HM_SYMBOL_TABLE = 0x11
    HM_MODIFICATION_TIME = 0x12
    HM_BTREE_K_VALUES = 0x13
    HM_DRIVER_INFO = 0x14
    HM_ATTRIBUTE_INFO = 0x15
    HM_REFERENCE_COUNT = 0x16
end

Base.convert(::Type{UInt8}, h::HeaderMessageTypes) = UInt8(h)



## Exceptions 

struct UnsupportedVersionException <: Exception
    msg::String
end
struct UnsupportedFeatureException <: Exception
    msg::String
end
struct InvalidDataException <: Exception
    msg::String
end
struct InternalError <: Exception
    msg::String
end

# In the future a more descriptive error should be returned
UnsupportedVersionException() = UnsupportedVersionException("")
UnsupportedFeatureException() = UnsupportedFeatureException("")
InvalidDataException() = InvalidDataException("")
InternalError() = InternalError("")


## Internal types

# Currently we specify that all offsets and lengths are 8 bytes
const Length = UInt64

"""
    RelOffset

Represents an HDF5 relative offset. This differs from a file offset (used elsewhere) in
that it is relative to the superblock base address. `fileoffset` and `h5offset` convert between
`RelOffset`s and file offsets.
"""
struct RelOffset
    offset::UInt64
end
define_packed(RelOffset)

RelOffset(r::RelOffset) = r
Base.:(==)(x::RelOffset, y::RelOffset) = x === y
Base.hash(x::RelOffset) = hash(x.offset)
Base.:(+)(x::RelOffset, y::Integer) = RelOffset(UInt64(x.offset + y))
Base.:(-)(x::RelOffset, y::Integer) = RelOffset(UInt64(x.offset - y))

const UNDEFINED_ADDRESS = RelOffset(0xffffffffffffffff)
const NULL_REFERENCE = RelOffset(0)

function Base.show(io::IO, x::RelOffset) 
    if x == UNDEFINED_ADDRESS
        print(io, "UNDEFINED_ADDRESS")
    else
        print(io, "RelOffset(", x.offset, ")")
    end
end

"""
    JLDWriteSession{T}

A `JLDWriteSession` keeps track of references to serialized objects. If `T` is a Dict,
`h5offset` maps an object ID (returned by calling `objectid`) to th `RelOffset` of the
written dataset. If it is `Union{}`, then references are not tracked, and objects
referenced multiple times are written multiple times.
"""
struct JLDWriteSession{T<:Union{Dict{UInt,RelOffset},Union{}}}
    h5offset::T
    objects::Vector{Any}

    JLDWriteSession{T}() where T = new()
    JLDWriteSession{T}(h5offset, objects) where T = new(h5offset, objects)
end
JLDWriteSession() = JLDWriteSession{Dict{UInt,RelOffset}}(Dict{UInt,RelOffset}(), Any[])


"""
    GlobalHeap

Represents an HDF5 global heap structure.
"""
mutable struct GlobalHeap
    offset::Int64
    length::Length
    free::Length
    objects::Vector{Int64}
end

"""
    abstract type H5Datatype

Supertype of all HDF5 datatypes.
"""
abstract type H5Datatype end

struct PlaceholderH5Datatype <: H5Datatype end

"""
    SharedDatatype <: H5Datatype

Reference to a shared datatype message (stored elsewhere in a file).
"""
struct SharedDatatype <: H5Datatype
    header_offset::RelOffset
end
define_packed(SharedDatatype)

"""
    CommittedDatatype <: H5Datatype

Reference to a shared datatype message (stored elsewhere in a file).
These are stored in the `_types` group and indexed.
"""
struct CommittedDatatype <: H5Datatype
    header_offset::RelOffset
    index::Int
end


"""
    ReadRepresentation{T,ODR}

A type encoding both the Julia type `T` and the on-disk (HDF5) representation `ODR`.
"""
struct ReadRepresentation{T,ODR} end
Base.eltype(::Type{<:ReadRepresentation{T}}) where T = T

"""
    CustomSerialization{T,S}

On-disk representation for data that is written as if it were of Julia type `T`, but is
read as type `S`.
"""
struct CustomSerialization{T,S} end

"""
    Upgrade(T)

Specify an upgrade path for serialized structs using the `typemap` keyword argument
and `rconvert`.
"""
struct Upgrade
    target
end

struct Filter
    id::UInt16
    flags::UInt16
    name::String
    client_data::Vector{UInt32}
end

struct FilterPipeline
    filters::Vector{Filter}
end

FilterPipeline() = FilterPipeline(Filter[])
iscompressed(fp::FilterPipeline) = !isempty(fp.filters)

"""
    HeaderMessage

Helper struct to read and write the first part of a header message.
"""
struct HeaderMessage
    msg_type::UInt8
    size::UInt16
    flags::UInt8
end
define_packed(HeaderMessage)

## Object header continuation
# object headers can be split into multiple chunks
# the HM_OBJECT_HEADER_CONTINUATION message is used to link them
# it has constant size and a placeholder is put into every header
# by JLD2 to allow later additions.
const CONTINUATION_MSG_SIZE = jlsizeof(HeaderMessage) + jlsizeof(RelOffset) + jlsizeof(Length)


"""
    Message{IO}

Representation of a Message in memory. Provides `getproperty` access
"""
struct Message{IO}
    type::HeaderMessageTypes
    address::UInt64
    offset::RelOffset
    io::IO
    Message(type::HeaderMessageTypes, address::Integer, o::RelOffset, io::IO) =
        new{typeof(io)}(type, UInt64(address), o, io)
end
Message(type::UInt8, args...) = Message(HeaderMessageTypes(type), args...)
Message(type::HeaderMessageTypes, f, offset::RelOffset) = 
    Message(type, fileoffset(f, offset), offset, f.io)
Message(type::HeaderMessageTypes, io::IO) = Message(type, position(io), UNDEFINED_ADDRESS, io)
Message(type::HeaderMessageTypes, data::Vector{UInt8}) = Message(type, 0, UNDEFINED_ADDRESS, IOBuffer(data))


@generated function Base.getproperty(m::Message, s::Symbol)
    ex = Expr(:block)
    for v in instances(HeaderMessageTypes)
        push!(ex.args, :(getfield(m, :type) == $(v) && return iogetprop($(Val(v)), m, s)))
    end
    return quote
        s in (:type, :address, :io) && return getfield(m, s)
        $(ex)
    end
end