"""
    MemoryBackedIO <: IO

Abstract type for IO objects that are backed by memory in such a way that
one can use pointer based `unsafe_load` and `unsafe_store!` operations
after ensuring that there is enough memory allocated.

It needs to provide:
 - `getproperty(io, :curptr)` to get the current pointer
 - `ensureroom(io, nb)` to ensure that there are at least nb bytes available
 - `position(io)` to get the current (zero-based) position
 - `seek(io, pos)` to set the current position (zero-based)
"""
abstract type MemoryBackedIO <: IO end



# Define custom `jlread`, `jlwrite`, `jlunsafe_load`, `jlunsafe_store!` functions for a struct
# this needs to be called immediately after the struct definition (since it itself calls `jlsizeof`)
function define_packed(ty::DataType)
    @assert isbitstype(ty)
    packed_offsets = cumsum([jlsizeof(x) for x in ty.types])
    sz = pop!(packed_offsets)
    pushfirst!(packed_offsets, 0)

    if sz != jlsizeof(ty)
        @eval begin
            function jlunsafe_store!(p::Ptr{$ty}, x::$ty)
                $([:(jlunsafe_store!(pconvert(Ptr{$(ty.types[i])}, p+$(packed_offsets[i])), getfield(x, $i)))
                   for i = 1:length(packed_offsets)]...)
            end
            function jlunsafe_load(p::Ptr{$ty})
                $(Expr(:new, ty, [:(jlunsafe_load(pconvert(Ptr{$(ty.types[i])}, p+$(packed_offsets[i]))))
                                   for i = 1:length(packed_offsets)]...))
            end
            jlsizeof(::Union{$ty,Type{$ty}}) = $(Int(sz))::Int
        end
    end

    @eval begin
        @inline jlwrite(io::MemoryBackedIO, x::$ty) = _write(io, x)
        @inline jlread(io::MemoryBackedIO, x::Type{$ty}) = _read(io, x)
        function jlread(io::IO, ::Type{$ty})
            $(Expr(:new, ty, [:(jlread(io, $(ty.types[i]))) for i = 1:length(packed_offsets)]...))
        end
        function jlwrite(io::IO, x::$ty)
            $([:(jlwrite(io, getfield(x, $i))) for i = 1:length(packed_offsets)]...)
            nothing
        end
    end
    nothing
end

jlsizeof(x) = Base.sizeof(x)
jlunsafe_store!(p, x) = Base.unsafe_store!(p, x)
jlunsafe_load(p) = Base.unsafe_load(p)


## Signatures
const OBJECT_HEADER_SIGNATURE = htol(0x5244484f) # "OHDR"
const OH_ATTRIBUTE_CREATION_ORDER_TRACKED = 2^2
const OH_ATTRIBUTE_CREATION_ORDER_INDEXED = 2^3
const OH_ATTRIBUTE_PHASE_CHANGE_VALUES_STORED = 2^4
const OH_TIMES_STORED = 2^5
const OBJECT_HEADER_CONTINUATION_SIGNATURE = htol(0x4b48434f) # "OCHK"

## Enums
@enum LayoutClass::UInt8 begin
    LcCompact = 0x00
    LcContiguous = 0x01
    LcChunked = 0x02
    LcVirtual = 0x03
end
LayoutClass(lc::LayoutClass) = lc
jlwrite(io, lc::LayoutClass) = jlwrite(io, UInt8(lc))

@enum(CharacterSet::UInt8,
      CSET_ASCII,
      CSET_UTF8)


# Header Message Types
# These are identified by their UInt8 value in the file
@enum HeaderMessageType::UInt8 begin
    HmNil = 0x00
    HmDataspace = 0x01
    HmLinkInfo = 0x02
    HmDatatype = 0x03
    HmFillValueOld = 0x04
    HmFillValue = 0x05
    HmLinkMessage = 0x06
    HmExternalFileList = 0x07
    HmDataLayout = 0x08
    HmBogus = 0x09
    HmGroupInfo = 0x0a
    HmFilterPipeline = 0x0b
    HmAttribute = 0x0c
    HmObjectComment = 0x0d
    HmSharedMessageTable = 0x0f
    HmObjectHeaderContinuation = 0x10
    HmSymbolTable = 0x11
    HmModificationTime = 0x12
    HmBtreeKValues = 0x13
    HmDriverInfo = 0x14
    HmAttributeInfo = 0x15
    HmReferenceCount = 0x16
end


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
Base.hash(x::RelOffset, h::UInt) = hash(x.offset, h)
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
struct JLDWriteSession{T<:Union{Dict{UInt,Tuple{RelOffset,WeakRef}},Union{}}}
    h5offset::T
    JLDWriteSession{T}() where T = new{T}()
    JLDWriteSession(h5offset::T) where T = new{T}(h5offset)
end
JLDWriteSession() = JLDWriteSession(Dict{UInt,Tuple{RelOffset,WeakRef}}())
track!(::JLDWriteSession{Union{}}, args...) = nothing
function track!(s::JLDWriteSession, data, offset::RelOffset)
    if ismutabletype(typeof(data))
        s.h5offset[objectid(data)] = (offset, WeakRef(data))
    end
    nothing
end
get_tracked(::JLDWriteSession{Union{}}, data) = UNDEFINED_ADDRESS
function get_tracked(wsession::JLDWriteSession, data)
    !ismutabletype(typeof(data)) && return UNDEFINED_ADDRESS
    offset, wref = get(wsession.h5offset, objectid(data), (UNDEFINED_ADDRESS, WeakRef(nothing)))
    isnothing(wref.value) && return UNDEFINED_ADDRESS
    return offset
end
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

# Allow dropping the index field
SharedDatatype(dt::CommittedDatatype) = SharedDatatype(dt.header_offset)


"""
    ReadRepresentation{T,ODR}

A type encoding both the Julia type `T` and the on-disk (HDF5) representation `ODR`.
"""
abstract type ReadRepresentation{T, ODR} end
julia_repr(::ReadRepresentation{T}) where T = T
file_repr(::ReadRepresentation{T,ODR}) where {T,ODR} = ODR

struct SameRepr{T} <: ReadRepresentation{T,T} end
struct MappedRepr{T,ODR} <: ReadRepresentation{T,ODR} end

ReadRepresentation(::Type{T}, ::Type{T}) where T = SameRepr{T}()
ReadRepresentation(T, ODR) = MappedRepr{T,ODR}()

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

"""
    HeaderMessage

Helper struct to read and write the first part of a header message.
"""
struct HeaderMessage
    msg_type::HeaderMessageType
    size::UInt16
    flags::UInt8
end
define_packed(HeaderMessage)

## Object header continuation
# object headers can be split into multiple chunks
# the HmObjectHeaderContinuation message is used to link them
# it has constant size and a placeholder is put into every header
# by JLD2 to allow later additions.
const CONTINUATION_MSG_SIZE = jlsizeof(HeaderMessage) + jlsizeof(RelOffset) + jlsizeof(Length)


"""
    Message{IO}

Representation of a Message in memory. Provides `getproperty` access
"""
struct Message{IO}
    type::HeaderMessageType
    address::UInt64
    offset::RelOffset
    io::IO
    Message(type::HeaderMessageType, address::Integer, o::RelOffset, io::IO) =
        new{typeof(io)}(type, UInt64(address), o, io)
end
Message(type::UInt8, args...) = Message(HeaderMessageType(type), args...)
Message(type::HeaderMessageType, f, offset::RelOffset) =
    Message(type, fileoffset(f, offset), offset, f.io)
Message(type::HeaderMessageType, io::IO) = Message(type, position(io), UNDEFINED_ADDRESS, io)
Message(type::HeaderMessageType, data::Vector{UInt8}) = Message(type, 0, UNDEFINED_ADDRESS, IOBuffer(data))


"""
    IndirectPointer

When writing data, we may need to enlarge the memory mapping, which would invalidate any
memory addresses arising from the old `mmap` pointer. `IndirectPointer` holds an offset relative to the
MemoryBackedIO. It defers computing a memory address until converted to a `Ptr{T}`,
so the memory mapping can be enlarged and addresses will remain valid.
"""
struct IndirectPointer{P<:MemoryBackedIO}
    io::P
    offset::Int
end

IndirectPointer(io::MemoryBackedIO) = IndirectPointer(io, Int(bufferpos(io)))

Base.:+(x::IndirectPointer, y::Integer) = IndirectPointer(x.io, x.offset+y)
pconvert(::Type{Ptr{T}}, x::IndirectPointer) where {T} = Ptr{T}(x.io.curptr - bufferpos(x.io) + x.offset)

# Use internal convert function (for pointer conversion) to avoid invalidations
pconvert(T, x) = Base.convert(T, x)


# This function writes a julia object to file or a buffers and first applies the required
# conversions to the on-disk representation.
function h5convert! end
