module JLD2
using ArrayViews, DataStructures
import Base.sizeof
include("Lookup3.jl")
export jldopen

const SUPERBLOCK_SIGNATURE = reinterpret(UInt64, UInt8[0o211, 'H', 'D', 'F', '\r', '\n', 0o032, '\n'])[1]
const OBJECT_HEADER_SIGNATURE = reinterpret(UInt32, UInt8['O', 'H', 'D', 'R'])[1]
const GLOBAL_HEAP_SIGNATURE = reinterpret(UInt32, UInt8['G', 'C', 'O', 'L'])[1]
const UNDEFINED_ADDRESS = 0xffffffffffffffff

# Currently we specify that all offsets and lengths are 8 bytes
typealias Offset UInt64
typealias Length UInt64

immutable UnsupportedVersionException <: Exception end
immutable UnsupportedFeatureException <: Exception end
immutable InvalidDataException <: Exception end

typealias Plain     Union(Int8,Int16,Int32,Int64,Int128,UInt8,UInt16,UInt32,UInt64,UInt128,
                          Float16,Float32,Float64)
typealias PlainType Union(Type{Int8},Type{Int16},Type{Int32},Type{Int64},Type{Int128},
                          Type{UInt8},Type{UInt16},Type{UInt32},Type{UInt64},Type{UInt128},
                          Type{Float16},Type{Float32},Type{Float64})

immutable JLDWriteSession
    h5ref::ObjectIdDict
end
JLDWriteSession() = JLDWriteSession(ObjectIdDict())

immutable Reference
    offset::UInt64
end

type GlobalHeap
    offset::Offset
    length::Length
    free::Length
    objects::Vector{Offset}
end

abstract H5Datatype

immutable CommittedDatatype <: H5Datatype
    header_offset::Offset
    index::Int
end

immutable Group{T}
    names::T
    offsets::Vector{Offset}
end

immutable OnDiskRepresentation{Offsets,Types} end
immutable ReadRepresentation{T,ODR} end

symbol_length(x::Symbol) = ccall(:strlen, Int, (Cstring,), x)

type JLDFile{T<:IO}
    io::T
    datatype_locations::OrderedDict{Offset,CommittedDatatype}
    datatypes::Vector{H5Datatype}
    datatype_wsession::JLDWriteSession
    datasets::OrderedDict{ByteString,Offset}
    jlh5type::Dict{Type,CommittedDatatype}
    h5jltype::Dict{CommittedDatatype,Type}
    end_of_data::Offset
    global_heaps::Dict{Offset,GlobalHeap}
    global_heap::GlobalHeap
end
JLDFile(io::IO) = JLDFile(io, OrderedDict{Offset,CommittedDatatype}(), H5Datatype[], JLDWriteSession(),
                          OrderedDict{ByteString,Offset}(),
                          Dict{Type,CommittedDatatype}(), Dict{CommittedDatatype,Type}(),
                          UInt64(sizeof(Superblock)), Dict{Offset,GlobalHeap}(),
                          GlobalHeap(UNDEFINED_ADDRESS, 0, 0, Offset[]))

#
# MmapIO
#
# An IO built on top of mmap to avoid the overhead of ordinary disk IO
const MMAP_GROW_SIZE = 2^24
const FILE_GROW_SIZE = 2^18

type MmapIO <: IO
    f::IOStream
    arr::Vector{UInt8}
    curptr::Ptr{Void}
    endptr::Ptr{Void}
end

function MmapIO(fname::String, write::Bool, create::Bool, truncate::Bool)
    truncate && !write && throw(ArgumentError("cannot truncate file that is not writable"))

    f = open(fname, true, write, create, truncate, false)
    initialsz = truncate ? 0 : filesize(fname)
    arr = Mmap.mmap(f, Vector{UInt8}, (initialsz + MMAP_GROW_SIZE,); grow=false)
    ptr = Ptr{Void}(pointer(arr))
    io = MmapIO(f, arr, ptr, ptr + initialsz)
end

Base.show(io::IO, ::MmapIO) = print(io, "MmapIO")

function resize!(io::MmapIO, newend::Ptr{Void})
    # Resize file
    ptr = pointer(io.arr)
    newsz = Int(max(newend - ptr, io.curptr - ptr + FILE_GROW_SIZE))
    truncate(io.f, newsz)

    if newsz > length(io.arr)
        # If we have not mapped enough memory, map more
        io.arr = Mmap.mmap(io.f, Vector{UInt8}, (newsz + MMAP_GROW_SIZE,); grow=false)
        newptr = pointer(io.arr)
        io.curptr += newptr - ptr
        ptr = newptr
    end

    # Set new end
    io.endptr = ptr + newsz
    io
end

@inline function ensureroom(io::MmapIO, n::Int)
    ep = io.curptr + n
    if ep > io.endptr
        resize!(io, ep)
    end
    nothing
end

Base.truncate(io::MmapIO, pos) = truncate(io.f, pos)

function Base.close(io::MmapIO)
    Mmap.sync!(io.arr)
    close(io.f)
end

@inline function unsafe_write(io::MmapIO, x)
    cp = io.curptr
    unsafe_store!(Ptr{typeof(x)}(cp), x)
    io.curptr = cp + sizeof(x)
    nothing
end

@inline function _write(io::MmapIO, x)
    cp = io.curptr
    ep = cp + sizeof(x)
    if ep > io.endptr
        resize!(io, ep)
        cp = io.curptr
        ep = cp + sizeof(x)
    end
    unsafe_store!(Ptr{typeof(x)}(cp), x)
    io.curptr = ep
    nothing
end
@inline Base.write(io::MmapIO, x::UInt8) = _write(io, x)
@inline Base.write(io::MmapIO, x::Plain)  = _write(io, x)

function Base.write{T}(io::MmapIO, x::Ptr{T}, n::Integer)
    cp = io.curptr
    ep = cp + sizeof(T)*n
    if ep > io.endptr
        resize!(io, ep)
        cp = io.curptr
        ep = cp + sizeof(T)*n
    end
    unsafe_copy!(Ptr{T}(cp), x, n)
    io.curptr = ep
    nothing
end

Base.write(io::MmapIO, x::ASCIIString) = write(io, pointer(x), sizeof(x))
Base.write(io::MmapIO, x::UTF8String) = write(io, pointer(x), sizeof(x))
Base.write(io::MmapIO, x::Array) = write(io, pointer(x), sizeof(x))

@inline function _read(io::MmapIO, T::DataType)
    cp = io.curptr
    ep = cp + sizeof(T)
    ep > io.endptr && throw(EOFError())
    v = unsafe_load(Ptr{T}(cp)) 
    io.curptr = ep
    v
end
@inline Base.read(io::MmapIO, T::Type{UInt8}) = _read(io, T)
@inline Base.read(io::MmapIO, T::Type{Int8}) = _read(io, T)
@inline Base.read(io::MmapIO, T::PlainType) = _read(io, T)

function Base.seek(io::MmapIO, offset)
    io.curptr = pointer(io.arr) + offset
    nothing
end

function Base.skip(io::MmapIO, offset)
    io.curptr += offset
    nothing
end

Base.position(io::MmapIO) = Int(io.curptr - pointer(io.arr))

# We sometimes need to compute checksums. We do this by first calling
# begin_checksum when starting to handle whatever needs checksumming,
# and calling end_checksum afterwards. Note that we never compute
# nested checksums.
# XXX not thread-safe!

const CHECKSUM_PTR = Ptr{Void}[]
const NCHECKSUM = Ref{Int}(0)
function begin_checksum(io::MmapIO)
    idx = NCHECKSUM[] += 1
    if idx > length(CHECKSUM_PTR)
        push!(CHECKSUM_PTR, io.curptr)
    else
        CHECKSUM_PTR[idx] = io.curptr
    end
    io
end
function begin_checksum(io::MmapIO, sz::Int)
    # Ensure that we have enough room for sz bytes
    cp = io.curptr
    if cp+sz > io.endptr
        resize!(io, cp+sz)
        cp = io.curptr
    end
    begin_checksum(io)
end
function end_checksum(io::MmapIO)
    v = CHECKSUM_PTR[NCHECKSUM[]]
    NCHECKSUM[] -= 1
    Lookup3.hash(UnsafeContiguousView(Ptr{UInt8}(v), (Int(io.curptr - v),)))
end

# Redefine unsafe_load and unsafe_store! so that they pack the type
# Assumes default constructor
function define_packed(ty::DataType)
    @assert isbits(ty)
    packed_offsets = cumsum([sizeof(x) for x in ty.types])
    sz = pop!(packed_offsets)
    unshift!(packed_offsets, 0)

    if sz != sizeof(ty)
        @eval begin
            function Base.unsafe_store!(p::Ptr{$ty}, x::$ty)
                $([:(unsafe_store!(convert(Ptr{$(ty.types[i])}, p+$(packed_offsets[i])), getfield(x, $i)))
                   for i = 1:length(packed_offsets)]...)
            end
            function Base.unsafe_load(p::Ptr{$ty}, ::Type{$ty})
                $(Expr(:call, ty, [:(unsafe_load(convert(Ptr{$(ty.types[i])}, p+$(packed_offsets[i]))))
                                   for i = 1:length(packed_offsets)]...))
            end
            Base.sizeof(::Union($ty, Type{$ty})) = $sz
        end
    end

    @eval begin
        @inline Base.write(io::MmapIO, x::$ty) = _write(io, x)
        function Base.read(io::IO, ::Type{$ty})
            $(Expr(:call, ty, [:(read(io, $(ty.types[i]))) for i = 1:length(packed_offsets)]...))
        end
        function Base.write(io::IO, x::$ty)
            $([:(write(io, getfield(x, $i))) for i = 1:length(packed_offsets)]...)
            nothing
        end
    end
    nothing
end

# Loads a variable-length size according to flags
# Expects that the first two bits of flags mean:
# 0   The size of the Length of Link Name field is 1 byte.
# 1   The size of the Length of Link Name field is 2 bytes.
# 2   The size of the Length of Link Name field is 4 bytes.
# 3   The size of the Length of Link Name field is 8 bytes.
# Returns the size as a UInt and a new pointer that is offset by the
# size of the size field
function read_size(io::IO, flags::UInt8)
    if (flags & 1) == 0 && (flags & 2) == 0
        read(io, UInt8)
    elseif (flags & 1) == 1 && (flags & 2) == 0
        read(io, UInt16)
    elseif (flags & 1) == 0 && (flags & 2) == 2
        read(io, UInt32)
    else
        read(io, UInt64)
    end
end

# Determine what the size flag should be
# Same rules as above
function size_flag(sz::Integer)
    if sz <= typemax(UInt8)
        UInt8(0)
    elseif sz <= typemax(UInt16)
        UInt8(1)
    elseif sz <= typemax(UInt32)
        UInt8(2)
    else
        UInt8(3)
    end
end

# Store a size
function write_size(io::IO, sz::Integer)
    if sz <= typemax(UInt8)
        write(io, UInt8(sz))
    elseif sz <= typemax(UInt16)
        write(io, UInt16(sz))
    elseif sz <= typemax(UInt32)
        write(io, UInt32(sz))
    else
        write(io, UInt64(sz))
    end
end

# Get the size of the size
function size_size(sz::Integer)
    if sz <= typemax(UInt8)
        1
    elseif sz <= typemax(UInt16)
        2
    elseif sz <= typemax(UInt32)
        4
    else
        8
    end
end

#
# Superblock
#

# https://www.hdfgroup.org/HDF5/doc/H5.format.html#FileMetaData
# Superblock (Version 2)
type Superblock
    file_consistency_flags::UInt8
    base_address::Offset
    superblock_extension_address::Offset
    end_of_file_address::Offset
    root_group_object_header_address::Offset
end

Base.sizeof(::Union(Type{Superblock}, Superblock)) = 
    12+sizeof(Offset)*4+4

function Base.read(io::IO, ::Type{Superblock})
    cio = begin_checksum(io)

    # Signature
    signature = read(cio, UInt64)
    signature == SUPERBLOCK_SIGNATURE || throw(UnsupportedVersionException())

    # Version
    version  = read(cio, UInt8)
    version == 2 || throw(UnsupportedVersionException())

    # Size of offsets and size of lengths
    size_of_offsets = read(cio, UInt8)
    size_of_lengths = read(cio, UInt8)
    (size_of_offsets == 8 && size_of_lengths == 8) || throw(UnsupportedFeatureException())

    # File consistency flags
    file_consistency_flags = read(cio, UInt8)

    # Addresses
    base_address = read(cio, Offset)
    superblock_extension_address = read(cio, Offset)
    end_of_file_address = read(cio, Offset)
    root_group_object_header_address = read(cio, Offset)

    # Checksum
    cs = end_checksum(cio)
    read(io, UInt32) == cs || throw(InvalidDataException())

    Superblock(file_consistency_flags, base_address, superblock_extension_address,
                end_of_file_address, root_group_object_header_address)
end

function Base.write(io::IO, s::Superblock)
    cio = begin_checksum(io, sizeof(s))
    write(cio, SUPERBLOCK_SIGNATURE::UInt64)    # Signature
    write(cio, UInt8(2))                        # Version
    write(cio, UInt8(8))                        # Size of offsets
    write(cio, UInt8(8))                        # Size of lengths
    write(cio, s.file_consistency_flags::UInt8)
    write(cio, s.base_address::Offset)
    write(cio, s.superblock_extension_address::Offset)
    write(cio, s.end_of_file_address::Offset)
    write(cio, s.root_group_object_header_address::Offset)
    write(io, end_checksum(cio))
end

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
const HM_SHARED_MESSAGE_TABLE = 0x0e
const HM_OBJECT_HEADER_CONTINUATION = 0x0f
const HM_SYMBOL_TABLE = 0x10
const HM_MODIFICATION_TIME = 0x11
const HM_BTREE_K_VALUES = 0x12
const HM_DRIVER_INFO = 0x13
const HM_ATTRIBUTE_INFO = 0x14
const HM_REFERENCE_COUNT = 0x15

immutable ObjectStart
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

immutable HeaderMessage
    msg_type::UInt8
    size::UInt16
    flags::UInt8
end
define_packed(HeaderMessage)

#
# Groups
#

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
        flags &= LM_LINK_NAME_CHARACTER_SET_FIELD_PRESENT
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

function Base.sizeof(group::Group)
    sz = payload_size(group)
    sz = sizeof(ObjectStart) + size_size(sz) + sz + 4
    for v in values(group.objects)
        sz += sizeof(v)
    end
    for v in values(group.children)
        sz += sizeof(v)
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

#
# Dataspaces
#

const DS_SCALAR = 0x00
const DS_SIMPLE = 0x01
const DS_NULL = 0x02

immutable Dataspace{N,A}
    dataspace_type::UInt8
    size::NTuple{N,Length}
    attributes::A
end

immutable DataspaceStart
    version::UInt8
    dimensionality::UInt8
    flags::UInt8
    dataspace_type::UInt8
end
define_packed(DataspaceStart)

function nulldataspace{N}(x::NTuple{N,Int})
    Dataspace(DS_NULL, (), (Attribute(:dimensions, Dataspace(DS_SIMPLE, (Length(N),), ()),
                            FixedPointDatatype(sizeof(Length), false),
                            Length, Length[x for x in reverse(x)]),))
end

Dataspace() = Dataspace(DS_NULL, (), ())
@generated function Dataspace(x::Any)
    if x.size == 0
        Dataspace()
    else
        Dataspace(DS_SCALAR, (), ())
    end
end
@generated function Dataspace{T,N}(x::Array{T,N})
    if !hasdata(T)
        :(nulldataspace(size(x)))
    else
        :(Dataspace(DS_SIMPLE, convert(Tuple{Vararg{Length}}, reverse(size(x))), ()))
    end
end

sizeof{N}(::Union(Dataspace{N},Type{Dataspace{N}})) = 4 + sizeof(Length)*N
numel(x::Dataspace{0}) = x.dataspace_type == DS_SCALAR ? 1 : 0
numel(x::Dataspace) = Int(prod(x.size))

function Base.write{N}(io::IO, dspace::Dataspace{N})
    write(io, DataspaceStart(2, N, 0, dspace.dataspace_type))
    for x in dspace.size
        write(io, x::Length)
    end
end

#
# Attributes
#

immutable Attribute{DS<:Dataspace,H5T<:H5Datatype,ODR,T}
    name::Symbol
    dataspace::DS
    datatype::H5T
    odr::ODR
    data::T
end

immutable AttributeHeader
    version::UInt8
    flags::UInt8
    name_size::UInt16
    datatype_size::UInt16
    dataspace_size::UInt16
end
define_packed(AttributeHeader)

Base.sizeof(attr::Attribute) = 8 + symbol_length(attr.name) + 1 + sizeof(attr.datatype) + sizeof(attr.dataspace) +
                               numel(attr.dataspace) * sizeof(attr.odr)

function Base.write(io::IO, f::JLDFile, attr::Attribute, wsession::JLDWriteSession)
    namelen = symbol_length(attr.name)
    write(io, AttributeHeader(0x02, isa(attr.datatype, CommittedDatatype), namelen+1,
                              sizeof(attr.datatype), sizeof(attr.dataspace)))
    write(io, Base.unsafe_convert(Ptr{Cchar}, attr.name), namelen)
    write(io, UInt8(0))
    write(io, attr.datatype)
    write(io, attr.dataspace)
    write_data(f, attr.data, attr.odr, wsession)
end

#
# Datatypes
#

const DT_FIXED_POINT = UInt8(0) | (UInt8(3) << 4)
const DT_FLOATING_POINT = UInt8(1) | (UInt8(3) << 4)
const DT_TIME = UInt8(2) | (UInt8(3) << 4)
const DT_STRING = UInt8(3) | (UInt8(3) << 4)
const DT_BITFIELD = UInt8(4) | (UInt8(3) << 4)
const DT_OPAQUE = UInt8(5) | (UInt8(3) << 4)
const DT_COMPOUND = UInt8(6) | (UInt8(3) << 4)
const DT_REFERENCE = UInt8(7) | (UInt8(3) << 4)
const DT_ENUMERATED = UInt8(8) | (UInt8(3) << 4)
const DT_VARIABLE_LENGTH = UInt8(9) | (UInt8(3) << 4)
const DT_ARRAY = UInt8(10) | (UInt8(3) << 4)

# This is the description for:
#    Strings
#    References
immutable BasicDatatype <: H5Datatype
    class::UInt8
    bitfield1::UInt8
    bitfield2::UInt8
    bitfield3::UInt8
    size::UInt32
end
define_packed(BasicDatatype)
StringDatatype(::Type{ASCIIString}, size::Integer) =
    BasicDatatype(DT_STRING, 0x01, 0x00, 0x00, size)
StringDatatype(::Type{UTF8String}, size::Integer) =
    BasicDatatype(DT_STRING, 0x11, 0x00, 0x00, size)
OpaqueDatatype(size::Integer) =
    BasicDatatype(DT_OPAQUE, 0x00, 0x00, 0x00, size) # XXX make sure ignoring the tag is OK
ReferenceDatatype() =
    BasicDatatype(DT_REFERENCE, 0x00, 0x00, 0x00, sizeof(Offset))

immutable FixedPointDatatype <: H5Datatype
    class::UInt8
    bitfield1::UInt8
    bitfield2::UInt8
    bitfield3::UInt8
    size::UInt32
    bitoffset::UInt16
    bitprecision::UInt16
end
define_packed(FixedPointDatatype)
FixedPointDatatype(size::Integer, signed::Bool) =
    FixedPointDatatype(DT_FIXED_POINT, ifelse(signed, 0x08, 0x00), 0x00, 0x00, size, 0, 8*size)

immutable FloatingPointDatatype <: H5Datatype
    class::UInt8
    bitfield1::UInt8
    bitfield2::UInt8
    bitfield3::UInt8
    size::UInt32
    bitoffset::UInt16
    bitprecision::UInt16
    exponentlocation::UInt8
    exponentsize::UInt8
    mantissalocation::UInt8
    mantissasize::UInt8
    exponentbias::UInt32
end
define_packed(FloatingPointDatatype)

immutable CompoundDatatype <: H5Datatype
    size::UInt32
    names::Vector{ByteString}
    offsets::Vector{Int}
    members::Vector{H5Datatype}

    function CompoundDatatype(size, names, offsets, members)
        length(names) == length(offsets) == length(members) ||
            throw(ArgumentError("names, offsets, and members must have same length"))
        new(size, names, offsets, members)
    end
end

function Base.sizeof(dt::CompoundDatatype)
    sz = sizeof(BasicDatatype) + size_size(dt.size)*length(dt.names)
    for i = 1:length(dt.names)
        # Extra byte for null padding of name
        sz += sizeof(dt.names[i]) + 1 + sizeof(dt.members[i])
    end
    sz
end

function Base.write(io::IO, dt::CompoundDatatype)
    n = length(dt.names)
    write(io, BasicDatatype(DT_COMPOUND, n % UInt8, (n >> 8) % UInt8, 0x00, dt.size))
    for i = 1:length(dt.names)
        # Name
        write(io, dt.names[i])
        write(io, UInt8(0x00))

        # Byte offset of member
        if dt.size <= typemax(UInt8)
            write(io, UInt8(dt.offsets[i]))
        elseif dt.size <= typemax(UInt16)
            write(io, UInt16(dt.offsets[i]))
        else
            write(io, UInt32(dt.offsets[i]))
        end

        # Member type message
        write(io, dt.members[i])
    end
end

immutable VariableLengthDatatype{T<:H5Datatype} <: H5Datatype
    class::UInt8
    bitfield1::UInt8
    bitfield2::UInt8
    bitfield3::UInt8
    size::UInt32
    basetype::T
end
VariableLengthDatatype(basetype::H5Datatype) =
    VariableLengthDatatype{typeof(basetype)}(DT_VARIABLE_LENGTH, 0x00, 0x00, 0x00, 8+sizeof(Offset), basetype)
VariableLengthDatatype(class, bitfield1, bitfield2, bitfield3, size, basetype::H5Datatype) =
    VariableLengthDatatype{typeof(basetype)}(class, bitfield1, bitfield2, bitfield3, size, basetype)

Base.sizeof(dt::VariableLengthDatatype) =
    sizeof(BasicDatatype) + sizeof(dt.basetype)

function Base.write(io::IO, dt::VariableLengthDatatype)
    write(io, BasicDatatype(DT_VARIABLE_LENGTH, dt.bitfield1, dt.bitfield2, dt.bitfield3, dt.size))
    write(io, dt.basetype)
end

function Base.read(io::IO, ::Type{VariableLengthDatatype})
    dtype = read(io, BasicDatatype)
    datatype_class = read(io, UInt8)
    skip(io, -1)
    if datatype_class == DT_FIXED_POINT
        VariableLengthDatatype(dtype.class, dtype.bitfield1, dtype.bitfield2, dtype.bitfield3, dtype.size, read(io, FixedPointDatatype))
    elseif datatype_class == DT_FLOATING_POINT
        VariableLengthDatatype(dtype.class, dtype.bitfield1, dtype.bitfield2, dtype.bitfield3, dtype.size, read(io, FloatingPointDatatype))
    elseif datatype_class == DT_STRING
        VariableLengthDatatype(dtype.class, dtype.bitfield1, dtype.bitfield2, dtype.bitfield3, dtype.size, read(io, StringDatatype))
    elseif datatype_class == DT_VARIABLE_LENGTH
        VariableLengthDatatype(dtype.class, dtype.bitfield1, dtype.bitfield2, dtype.bitfield3, dtype.size, read(io, VariableLengthDatatype))
    else
        throw(UnsupportedFeatureException())
    end
end

Base.sizeof(dt::CommittedDatatype) = 2 + sizeof(Offset)

function Base.write(io::IO, dt::CommittedDatatype)
    write(io, UInt8(3))
    write(io, UInt8(2))
    write(io, dt.header_offset)
end

function commit(f::JLDFile, dt::H5Datatype, attrs::Tuple{Vararg{Attribute}}=())
    psz = sizeof(HeaderMessage) * (length(attrs) + 1) + sizeof(dt)
    for attr in attrs
        psz += sizeof(attr)
    end
    io = f.io

    sz = sizeof(ObjectStart) + size_size(psz) + psz
    offset = f.end_of_data
    seek(io, offset)
    f.end_of_data = offset + sz + 4

    cio = begin_checksum(io, sz)
    write(cio, ObjectStart(size_flag(psz)))
    write_size(cio, psz)
    write(cio, HeaderMessage(HM_DATATYPE, sizeof(dt), 64))
    write(cio, dt)
    for attr in attrs
        write(cio, HeaderMessage(HM_ATTRIBUTE, sizeof(attr), 0))
        write(cio, f, attr, f.datatype_wsession)
    end
    seek(io, offset + sz)
    write(io, end_checksum(cio))
end

# Read the actual datatype for a committed datatype
function Base.read(f::JLDFile, dt::CommittedDatatype)
    if datatype_class == DT_FIXED_POINT
        read_data(dataspace_type, dataspace_dimensions, jltype(read(io, FixedPointDatatype)), data_offset)
    elseif datatype_class == DT_FLOATING_POINT
        read_data(dataspace_type, dataspace_dimensions, jltype(read(io, FloatingPointDatatype)), data_offset)
    elseif datatype_class == DT_STRING
        read_data(dataspace_type, dataspace_dimensions, jltype(read(io, StringDatatype)), data_offset)
    elseif datatype_class == DT_VARIABLE_LENGTH
        read_data(dataspace_type, dataspace_dimensions, jltype(read(io, VariableLengthDatatype)), data_offset)
    end
end

#
# Datasets
#

# Use an Enum here when it doesn't make us allocate
const LC_COMPACT_STORAGE = 0x00
const LC_CONTIGUOUS_STORAGE = 0x01
const LC_CHUNKED_STORAGE = 0x02

const EMPTY_DIMENSIONS = Length[]
function read_data(f::JLDFile)
    io = f.io
    cio = begin_checksum(io)
    sz = read_obj_start(cio)
    pmax = position(cio) + sz

    # Messages
    dataspace_type = typemin(UInt8)
    dataspace_dimensions = EMPTY_DIMENSIONS
    datatype_class::UInt8 = 0
    datatype_offset::Int = 0
    data_offset::Int = 0
    data_length::Int = 0
    while position(cio) < pmax
        msg = read(cio, HeaderMessage)
        endpos = position(cio) + msg.size
        if msg.msg_type == HM_DATASPACE
            dspace_start = read(cio, DataspaceStart)
            dspace_start.version == 2 || throw(UnsupportedVersionException())
            dataspace_type = dspace_start.dataspace_type
            if dspace_start.dimensionality != 0
                dataspace_dimensions = read(cio, Length, dspace_start.dimensionality)
            end
        elseif msg.msg_type == HM_DATATYPE
            if (msg.flags & 2) == 2
                # Shared datatype
                read(cio, UInt8) == 3 || throw(UnsupportedVersionException())
                read(cio, UInt8) == 2 || throw(UnsupportedFeatureException())
                datatype_offset = read(cio, Offset)
                datatype_class = typemax(UInt8)
            else
                # Datatype stored here
                datatype_offset = position(cio)
                datatype_class = read(cio, UInt8)
            end
        elseif msg.msg_type == HM_FILL_VALUE
            (read(cio, UInt8) == 3 && read(cio, UInt8) == 0x09) || throw(UnsupportedFeatureException())
        elseif msg.msg_type == HM_DATA_LAYOUT
            read(cio, UInt8) == 3 || throw(UnsupportedVersionException())
            if read(cio, UInt8) == LC_COMPACT_STORAGE
                data_length = read(cio, UInt16)
                data_offset = position(cio)
            elseif read(cio, UInt8) == LC_CONTIGUOUS_STORAGE
                data_offset = read(cio, Offset)
                data_length = read(cio, Length)
            else
                throw(UnsupportedFeatureException())
            end
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

    # TODO verify that data length matches

    # Read the data
    # For efficiency reasons, we do not use dispatch here
    seek(io, datatype_offset)
    if datatype_class == DT_FIXED_POINT
        read_data(f, dataspace_type, dataspace_dimensions, jltype(f, read(io, FixedPointDatatype)), data_offset)
    elseif datatype_class == DT_FLOATING_POINT
        read_data(f, dataspace_type, dataspace_dimensions, jltype(f, read(io, FloatingPointDatatype)), data_offset)
    elseif datatype_class == DT_STRING
        read_data(f, dataspace_type, dataspace_dimensions, jltype(f, read(io, StringDatatype)), data_offset)
    elseif datatype_class == DT_VARIABLE_LENGTH
        read_data(f, dataspace_type, dataspace_dimensions, jltype(f, read(io, VariableLengthDatatype)), data_offset)
    elseif datatype_class == typemax(UInt8) # Committed datatype
        read_data(f, dataspace_type, dataspace_dimensions, jltype(f, f.datatype_locations[datatype_offset]), data_offset)
    else
        throw(UnsupportedFeatureException())
    end
end

function read_data{T,ODR}(f::JLDFile{MmapIO}, dataspace_type::UInt8, dataspace_dimensions::Vector{Length},
                          rr::ReadRepresentation{T,ODR}, data_offset::Int)
    io = f.io
    seek(io, data_offset)
    inptr = io.curptr
    if isempty(dataspace_dimensions)
        jlconvert(rr, f, inptr)
    else
        v = Array(T, dataspace_dimensions...)
        if isbits(T)
            outptr = pointer(v)
            if ODR <: T
                unsafe_copy!(pointer(v), convert(Ptr{T}, inptr), Int(prod(dataspace_dimensions)))
            else
                @simd for i = 1:prod(dataspace_dimensions)::Offset
                    jlconvert!(outptr, rr, f, inptr)
                    inptr += sizeof(ODR)
                    outptr += sizeof(T)
                end
            end
        else
            @simd for i = 1:prod(dataspace_dimensions)::Offset
                @inbounds v[i] = jlconvert(rr, f, inptr)
            end
        end
        v
    end
end

function payload_size(dataspace::Dataspace, datatype::H5Datatype, datasz::Int, layout_class::UInt8)
    sz = sizeof(dataspace) + sizeof(datatype) + 2 + (4 + length(dataspace.attributes))*sizeof(HeaderMessage) + 2
    for attr in dataspace.attributes
        sz += sizeof(attr)
    end
    if layout_class == LC_COMPACT_STORAGE
        sz + 2 + datasz
    else
        sz + sizeof(Offset) + sizeof(Length)
    end
end

# Might need to do something else someday for non-mmapped IO
function write_data(f::JLDFile, data, odr, wsession::JLDWriteSession)
    io = f.io
    ensureroom(io, sizeof(odr))
    arr = io.arr
    cp = io.curptr
    h5convert!(cp, odr, f, data, wsession)
    io.curptr = cp + sizeof(odr)
    arr # Keep old array rooted until the end
end

# Like isdefined, but assumes arr is a pointer array, and can be inlined
unsafe_isdefined(arr::Array, i::Int) =
    unsafe_load(Ptr{Ptr{Void}}(pointer(arr)+(i-1)*sizeof(Ptr{Void}))) != Ptr{Void}(0)

function write_data{T}(f::JLDFile, data::Array{T}, odr, wsession::JLDWriteSession)
    io = f.io
    ensureroom(io, sizeof(odr) * length(data))
    arr = io.arr
    cp = io.curptr
    ep = io.endptr
    @simd for i = 1:length(data)
        if (isleaftype(T) && isbits(T)) || unsafe_isdefined(data, i)
            # For now, just don't write anything unless the field is defined
            @inbounds h5convert!(cp, odr, f, data[i], wsession)
        else
            @inbounds h5convert_uninitialized!(cp, odr)
        end
        cp += sizeof(odr)
    end
    io.curptr = cp
    arr # Keep old array rooted until the end
end
# Force specialization on DataType
write_data(f::JLDFile, data::Array, odr::Type{Tuple{}}, wsession::JLDWriteSession) = error("ODR is invalid")

function write_dataset(f::JLDFile, dataspace::Dataspace, datatype::H5Datatype, odr, data, wsession::JLDWriteSession)
    io = f.io
    startpos = f.end_of_data
    seek(io, startpos)
    datasz = sizeof(odr) * numel(dataspace)
    layout_class = datasz < 8192 ? LC_COMPACT_STORAGE : LC_CONTIGUOUS_STORAGE
    psz = payload_size(dataspace, datatype, datasz, layout_class)
    fullsz = sizeof(ObjectStart) + size_size(psz) + psz + 4

    header_offset = position(io)
    cio = begin_checksum(io, fullsz - 4)

    write(cio, ObjectStart(size_flag(psz)))
    write_size(cio, psz)

    # Dataspace
    write(cio, HeaderMessage(HM_DATASPACE, sizeof(dataspace), 0))
    write(cio, dataspace)

    # Datatype
    write(cio, HeaderMessage(HM_DATATYPE, sizeof(datatype), 1+2*isa(datatype, CommittedDatatype)))
    write(cio, datatype)

    # Fill value
    write(cio, HeaderMessage(HM_FILL_VALUE, 2, 0))
    write(cio, UInt8(3)) # Version
    write(cio, 0x09)     # Flags

    # Attributes
    for attr in dataspace.attributes
        write(cio, HeaderMessage(HM_ATTRIBUTE, sizeof(attr), 0))
        write(cio, f, attr, f.datatype_wsession)
    end

    # Data storage layout
    if layout_class == LC_COMPACT_STORAGE
        write(cio, HeaderMessage(HM_DATA_LAYOUT, 4+datasz, 0))
        write(cio, UInt8(3))                  # Version
        write(cio, LC_COMPACT_STORAGE)        # Layout class
        write(cio, UInt16(datasz))            # Size
        f.end_of_data = header_offset + fullsz
        data_offset = position(io)
        write_data(f, data, odr, wsession)
        seek(io, data_offset + datasz)
        write(io, end_checksum(cio))
    else
        write(cio, HeaderMessage(HM_DATA_LAYOUT, 2+sizeof(Offset)+sizeof(Length), 0))
        write(cio, UInt8(3))                  # Version
        write(cio, LC_CONTIGUOUS_STORAGE)     # Layout class
        write(cio, Offset(startpos + fullsz)) # Offset
        write(cio, Length(sizeof(data)))      # Length
        write(io, end_checksum(cio))
        data_offset = position(io)
        f.end_of_data = header_offset + fullsz + datasz
        write_data(f, data, odr, wsession)
    end

    header_offset
end

# Force specialization on DataType
write_dataset(f::JLDFile, dataspace::Dataspace, datatype::H5Datatype, odr::Type{Tuple{}}, data, wsession::JLDWriteSession) =
    error("ODR is invalid")

write_dataset(f::JLDFile, x, wsession::JLDWriteSession) =
    write_dataset(f, Dataspace(x), h5type(f, x), objodr(x), x, wsession)

function write_dataset(f::JLDFile, x::Array, wsession::JLDWriteSession)
    # Avoid type instability due to empty arrays
    if isempty(x)
        write_dataset(f, nulldataspace(size(x)), h5type(f, x), objodr(x), x, wsession)
    else
        write_dataset(f, Dataspace(x), h5type(f, x), objodr(x), x, wsession)
    end
end

@noinline function write_ref_nonbits(f::JLDFile, wsession::JLDWriteSession, x)
    ref = get(wsession.h5ref, x, Reference(0))::Reference
    ref != Reference(0) && return ref
    ref = Reference(write_dataset(f, x, wsession))::Reference
    wsession.h5ref[x] = ref
    ref
end

@inline function write_ref(f::JLDFile, wsession::JLDWriteSession, x)
    if isbits(typeof(x))
        Reference(write_dataset(f, x, wsession))::Reference
    else
        write_ref_nonbits(f, wsession, x)::Reference
    end
end

#
# Global heap
#

immutable GlobalHeapID
    heap_offset::Offset
    index::UInt32
end
define_packed(GlobalHeapID)

isatend(f::JLDFile, gh::GlobalHeap) =
    gh.offset != UNDEFINED_ADDRESS && f.end_of_data == gh.offset + 8 + sizeof(Length) + gh.length

function write_heap_object{T}(f::JLDFile, ptr::Ptr{T}, n::Int)
    psz = sizeof(T)*n
    objsz = 8 + sizeof(Length) + psz
    objsz += 8 - mod1(objsz, 8)

    io = f.io

    # This is basically a memory allocation problem. Right now we do it
    # in a pretty naive way. We:

    # 1. Put the object in the last created global heap if it fits
    # 2. Extend the last global heap if it's at the end of the file
    # 3. Create a new global heap

    # This is not a great approach if we're writing objects of
    # different sizes interspersed with new datasets. The torture case
    # would be a Vector{Any} of mutable objects, some of which contain
    # large (>4080 byte) strings and some of which contain small
    # strings. In that case, we'd be better off trying to put the small
    # strings into existing heaps, rather than writing new ones. This
    # should be revisited at a later date.

    if objsz < f.global_heap.free
        # Fits in existing global heap
        gh = f.global_heap
    elseif isatend(f, f.global_heap)
        # Global heap is at end and can be extended
        gh = f.global_heap
        gh.length = gh.length - gh.free + objsz
        seek(io, gh.offset + 8)
        write(io, gh.length)
        f.end_of_data += objsz
    else
        # Need to create a new global heap
        heapsz = max(objsz, 4096)
        offset = f.end_of_data + 8 - mod1(f.end_of_data, 8)
        seek(io, offset)
        write(io, GLOBAL_HEAP_SIGNATURE)
        write(io, UInt32(1))      # Version & Reserved
        write(io, Length(heapsz)) # Collection size
        f.end_of_data = position(io) + heapsz
        gh = f.global_heap = f.global_heaps[offset] =  GlobalHeap(offset, heapsz, heapsz, Offset[])
    end

    # Write data
    index = length(gh.objects) + 1
    objoffset = gh.offset + 8 + sizeof(Length) + gh.length - gh.free
    seek(io, objoffset)
    write(io, UInt16(index)) # Heap object index
    write(io, UInt16(1))     # Reference count
    skip(io, 4)              # Reserved
    write(io, Length(psz))   # Object size
    write(io, ptr, n)        # Object data

    # Update global heap object
    gh.free -= objsz
    push!(gh.objects, objoffset)

    # Write free space object
    if gh.free >= 8 + sizeof(Length)
        seek(io, objoffset + objsz)
        skip(io, 8)                # Object index, reference count, reserved
        write(io, Length(gh.free)) # Object size
    end

    GlobalHeapID(gh.offset, index)
end
write_heap_object(f::JLDFile, x::Array) = write_heap_object(f, pointer(x), length(x))

function Base.read(io::IO, ::Type{GlobalHeap})
    offset = position(io)
    read(io, UInt32) == GLOBAL_HEAP_SIGNATURE || throw(InvalidDataException())
    read(io, UInt32) == 1 || throw(UnsupportedVersionException())
    heapsz = read(io, Length)
    index = 1
    objects = Offset[]
    startpos = position(io)
    free = heapsz
    while free > 8 + sizeof(Length)
        push!(objects, position(io))
        objidx = read(io, UInt16)
        objidx == 0 && break
        objidx == index || throw(UnsupportedFeatureException())
        skip(io, 6)                    # Reference count and reserved
        sz = read(io, Length)          # Length
        skip(io, sz + 8 - mod1(sz, 8)) # Payload
        free = position(io) - startpos
        index += 1
    end
    GlobalHeap(offset, heapsz, free, objects)
end

function read_heap_object{T}(f::JLDFile, hid::GlobalHeapID, ::Type{T})
    io = f.io
    if haskey(f.global_heaps, hid.heap_offset)
        gh = f.global_heaps[hid.heap_offset]
    else
        seek(io, hid.heap_offset)
        f.global_heaps[hid.heap_offset] = gh = read(io, GlobalHeap)
    end
    seek(io, gh.objects[hid.index]+8)
    len = read(io, Length)
    n = div(len, sizeof(T))
    len == n * sizeof(T) || throw(InvalidDataException())
    read(io, T, n)
end

#
# File
#

function jldopen(fname::AbstractString, write::Bool, create::Bool, truncate::Bool)
    io = MmapIO(fname, write, create, truncate)
    f = JLDFile(io)

    if !truncate
        superblock = read(io, Superblock)
        f.end_of_data = superblock.end_of_file_address
        seek(io, superblock.root_group_object_header_address)
        root_group = read(io, Group)
        for i = 1:length(root_group.names)
            name = root_group.names[i]
            offset = root_group.offsets[i]
            if name == "_types"
                seek(io, offset)
                types_group = read(io, Group)
                for i = 1:length(types_group.offsets)
                    f.datatype_locations[types_group.offsets[i]] = CommittedDatatype(types_group.offsets[i], i)
                end
            else
                f.datasets[name] = offset
            end
        end
    end

    f
end

function jldopen(fname::AbstractString, mode::AbstractString="r")
    mode == "r"  ? jldopen(fname, false, false, false) :
    mode == "r+" ? jldopen(fname, true, false, false) :
    mode == "a" || mode == "a+" ? jldopen(fname, true, true, false) :
    mode == "w" || mode == "w+" ? jldopen(fname, true, true, true) :
    throw(ArgumentError("invalid open mode: $mode"))
end

function Base.read(f::JLDFile, name::String)
    f.end_of_data == UNDEFINED_ADDRESS && throw(ArgumentError("file is closed"))
    haskey(f.datasets, name) || throw(ArgumentError("file has no dataset $name"))
    offset = f.datasets[name]
    seek(f.io, offset)
    read_data(f)
end

function Base.write(f::JLDFile, name::String, obj, wsession::JLDWriteSession=JLDWriteSession())
    f.end_of_data == UNDEFINED_ADDRESS && throw(ArgumentError("file is closed"))

    io = f.io
    seek(io, f.end_of_data)
    header_offset = write_dataset(f, obj, wsession)
    f.datasets[name] = header_offset
    nothing
end

function Base.close(f::JLDFile)
    io = f.io
    seek(io, f.end_of_data)

    names = ByteString[]
    sizehint!(names, length(f.datasets)+1)
    offsets = Offset[]
    sizehint!(offsets, length(f.datasets)+1)

    # Write types group
    if !isempty(f.datatypes)
        push!(names, "_types")
        push!(offsets, position(io))
        write(io, Group(ASCIIString[@sprintf("%08d", i) for i = 1:length(f.datatypes)],
                        collect(keys(f.datatype_locations))))
    end

    # Write root group
    root_group_object_header_address = position(io)
    for (k, v) in f.datasets
        push!(names, k)
        push!(offsets, v)
    end
    write(io, Group(names, offsets))

    eof_address = position(io)
    truncate(io, eof_address)
    seek(io, 0)
    write(io, Superblock(0, 0, UNDEFINED_ADDRESS, eof_address, root_group_object_header_address))
    f.end_of_data = UNDEFINED_ADDRESS
    close(io)
    nothing
end

include("jld_types.jl")

end # module
