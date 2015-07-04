module JLD2
using ArrayViews
import Base.write
import Base.sizeof
include("Lookup3.jl")

const SUPERBLOCK_SIGNATURE = reinterpret(UInt64, UInt8[0o211, 'H', 'D', 'F', '\r', '\n', 0o032, '\n'])[1]
const OBJECT_HEADER_SIGNATURE = reinterpret(UInt32, UInt8['O', 'H', 'D', 'R'])[1]
const UNDEFINED_ADDRESS = 0xffffffffffffffff
typealias Plain     Union(Int8,Int16,Int32,Int64,Int128,UInt8,UInt16,UInt32,UInt64,UInt128,
                          Float16,Float32,Float64)

const MMAP_GROW_SIZE = 2^24
const FILE_GROW_SIZE = 2^15

#
# MmapIO
#
# An IO built on top of mmap to avoid the overhead of ordinary disk IO
type MmapIO <: IO
    f::IOStream
    arr::Vector{UInt8}
    curptr::Ptr{Void}
    endptr::Ptr{Void}
end

function MmapIO(fname::String, writable::Bool)
    f = open(fname, "w+")
    arr = mmap_array(UInt8, (MMAP_GROW_SIZE,), f, 0; grow=false)
    ptr = Ptr{Void}(pointer(arr))
    io = MmapIO(f, arr, ptr, ptr)
    resize!(io, ptr)
end

function resize!(io::MmapIO, newend::Ptr{Void})
    # Resize file
    ptr = pointer(io.arr)
    newsz = Int(max(newend - ptr, io.curptr - ptr + FILE_GROW_SIZE))
    truncate(io.f, newsz)

    if newsz > length(io.arr)
        # If we have not mapped enough memory, map more
        io.arr = mmap_array(UInt8, (newsz+MMAP_GROW_SIZE,), io.f, 0; grow=false)
        newptr = pointer(io.arr)
        io.curptr += newptr - ptr
        ptr = newptr
    end

    # Set new end
    io.endptr = ptr + newsz
    io
end
Base.truncate(io::MmapIO, pos) = truncate(io.f, pos)

function Base.close(io::MmapIO)
    msync(io.arr)
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

function Base.seek(io::MmapIO, offset)
    io.curptr = pointer(io.arr) + offset
    nothing
end

Base.position(io::MmapIO) = Int(io.curptr - pointer(io.arr))

# We sometimes need to compute checksums. We do this by first calling
# begin_checksum when starting to handle whatever needs checksumming,
# and calling end_checksum afterwards. Note that we never compute
# nested checksums.
# XXX not thread-safe!

const CHECKSUM_PTR = Ref{Ptr{Void}}(0)
function begin_checksum(io::MmapIO)
    CHECKSUM_PTR[] = io.curptr
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
    v = CHECKSUM_PTR[]
    CHECKSUM_PTR[] = Ptr{Void}(0)
    Lookup3.hash(UnsafeContiguousView(Ptr{UInt8}(v), (Int(io.curptr - v),)))
end

# Currently we specify that all offsets and lengths are 8 bytes
typealias Offset UInt64
typealias Length UInt64

immutable UnsupportedVersionException <: Exception end
immutable UnsupportedFeatureException <: Exception end
immutable InvalidDataException <: Exception end

typealias PlainType Union(Type{Int8},Type{Int16},Type{Int32},Type{Int64},Type{Int128},
                          Type{UInt8},Type{UInt16},Type{UInt32},Type{UInt64},Type{UInt128},
                          Type{Float16},Type{Float32},Type{Float64})

immutable Reference
    offset::UInt64
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
        read(p, UInt8)
    elseif (flags & 1) == 1 && (flags & 2) == 0
        read(p, UInt16)
    elseif (flags & 1) == 0 && (flags & 2) == 2
        read(p, UInt32)
    else
        read(p, UInt64)
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

sizeof(::Union(Type{Superblock}, Superblock)) = 
    12+sizeof(Offset)*4+4

function read(io::IO, ::Type{Superblock})
    cio = begin_checksum(io)

    # Signature
    signature = read(cio, UInt64)
    signature == SUPERBLOCK_SIGNATURE || throw(UnsupportedVersionException())

    # Version
    version  = read(cio, UInt8)
    version == 2 || throw(UnsupportedVersionException())

    # Size of offsets and size of lengths
    size_of_offsets = read(cio, Offset)
    size_of_lengths = read(cio, Offset)
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

function write(io::IO, s::Superblock)
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

@enum(HeaderMessageType,
      HM_NIL,
      HM_DATASPACE,
      HM_LINK_INFO,
      HM_DATATYPE,
      HM_FILL_VALUE_OLD,
      HM_FILL_VALUE,
      HM_LINK_MESSAGE,
      HM_EXTERNAL_FILE_LIST,
      HM_DATA_LAYOUT,
      HM_BOGUS,
      HM_GROUP_INFO,
      HM_FILTER_PIPELINE,
      HM_ATTRIBUTE,
      HM_OBJECT_COMMENT,
      HM_SHARED_MESSAGE_TABLE,
      HM_OBJECT_HEADER_CONTINUATION,
      HM_SYMBOL_TABLE,
      HM_MODIFICATION_TIME,
      HM_BTREE_K_VALUES,
      HM_DRIVER_INFO,
      HM_ATTRIBUTE_INFO,
      HM_REFERENCE_COUNT)

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
    fractal_heap_address::Offset
    name_index_btree::Offset
end
LinkInfo() = LinkInfo(UNDEFINED_ADDRESS, UNDEFINED_ADDRESS)

sizeof(::LinkInfo) = 18

function read(io::IO, ::Type{LinkInfo})
    version = read(io, UInt8)
    version == 0 || throw(UnsupportedVersionException())

    flags = read(io, UInt8)
    flags == 0 || throw(UnsupportedFeatureException())

    fractal_heap_address = read(io, Offset)
    name_index_btree = read(io, Offset)

    LinkInfo(fractal_heap_address, name_index_btree)
end

function write(io::IO, lh::LinkInfo)
    write(io, UInt16(0)) # Version and flags
    write(io, lh.fractal_heap_address::Offset)
    write(io, lh.name_index_btree::Offset)
end

@enum(CharacterSet,
      CSET_ASCII,
      CSET_UTF8)

const LM_CREATION_ORDER_PRESENT = 2^2
const LM_LINK_TYPE_FIELD_PRESENT = 2^3
const LM_LINK_NAME_CHARACTER_SET_FIELD_PRESENT = 2^4

immutable Link
    name::ByteString
    target::Offset
end

function sizeof(link::Link)
    sz = 2 + size_size(sizeof(link.name)) + sizeof(link.name) + sizeof(Offset)
    isa(link.name, UTF8String) && (sz += 1)
    sz
end

function read(io::IO, ::Type{Link})
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
        Link(ASCIIString(name), target)
    else
        Link(UTF8String(name), target)
    end
end

function write(io::IO, link::Link)
    # Version
    write(io, UInt8(1))

    # Flags
    flags = size_flag(sizeof(link.name))
    if isa(link.name, UTF8String)
        flags &= LM_LINK_NAME_CHARACTER_SET_FIELD_PRESENT
    end
    write(io, flags::UInt8)

    # Link name character set
    if isa(link.name, UTF8String)
        write(io, UInt8(CSET_UTF8))
    end

    # Length of link name
    write_size(io, sizeof(link.name))

    # Link name
    write(io, pointer(link.name), sizeof(link.name))

    # Link information
    write(io, link.target::Offset)
end

immutable Group
    link_info::LinkInfo
    links::Vector{Link}
end

const OH_ATTRIBUTE_CREATION_ORDER_TRACKED = 2^2
const OH_ATTRIBUTE_CREATION_ORDER_INDEXED = 2^3
const OH_ATTRIBUTE_PHASE_CHANGE_VALUES_STORED = 2^4
const OH_TIMES_STORED = 2^5

function payload_size(group::Group)
    sz = sizeof(group.link_info) + 2 +
                 (length(group.links) + 2) * sizeof(HeaderMessage)
    for link in group.links
        sz += sizeof(link)
    end
    sz
end

function sizeof(group::Group)
    sz = payload_size(group)
    4 + 1 + 1 + size_size(sz) + sz + 4
end

function read(io::IO, ::Type{Group})
    cio = begin_checksum(io)

    # Signature
    signature = read(cio, UInt32)
    signature == OBJECT_HEADER_SIGNATURE || throw(InvalidDataException())

    # Version
    version = read(cio, UInt8)
    version == 2 || throw(UnsupportedVersionException())

    # Flags
    flags = read(cio, UInt8)

    if (flags & OH_TIMES_STORED) != 0
        # Skip access, modification, change and birth times
        skip(cio, 128)
    end
    if (flags & OH_ATTRIBUTE_PHASE_CHANGE_VALUES_STORED) != 0
        # Skip maximum # of attributes fields
        skip(cio, 32)
    end

    # Size
    sz = read_size(cio, flags)
    pmax = position(cio) + sz
    link_info = LinkInfo()
    links = Link[]

    # Messages
    while position(io) < pmax
        msg = read(cio, HeaderMessage)
        endpos = position(io) + msg.size
        if msg.msg_type == HM_LINK_INFO
            link_info = read(cio, LinkInfo)[1]
            link_info.fractal_heap_address == UNDEFINED_ADDRESS || throw(UnsupportedFeatureException())
        elseif msg.msg_type == HM_GROUP_INFO
            # This message doesn't help us much, so we ignore it for now
        elseif msg.msg_type == HM_LINK_MESSAGE
            push!(links, read(cio, Link)[1])
        elseif (msg.flags & 2^3) != 0
            throw(UnsupportedFeatureException())
        elseif msg.msg_types == HM_NIL
            break
        end
        seek(io, endpos)
    end
    seek(io, pmax)

    # Checksum
    cs = unsafe_load!(Ptr{UInt32}(pmax))
    cs == checksum(ptr, pmax) || throw(InvalidDataException())

    Group(link_info, links)
end

function write(io::IO, group::Group)
    group.link_info.fractal_heap_address == UNDEFINED_ADDRESS ||
        throw(UnsupportedFeatureException())
    sz = payload_size(group)

    cio = begin_checksum(io, sizeof(ObjectHeaderP) + size_size(sz) + sz)

    write(cio, OBJECT_HEADER_SIGNATURE::UInt32) # Header
    write(cio, UInt8(2))                        # Version
    write(cio, size_flag(sz)::UInt8)            # Flags
    write_size(cio, sz)                         # Size

    # Link info message
    write(cio, HeaderMessage(HM_LINK_INFO, sizeof(group.link_info), 0))
    write(cio, group.link_info)

    # Group info message
    write(cio, HeaderMessage(HM_GROUP_INFO, 2, 0))
    write(cio, UInt16(0))

    # Links
    for link in group.links
        write(cio, HeaderMessage(HM_LINK_MESSAGE, sizeof(link), 0))
        write(cio, link)
    end

    # Checksum
    write(io, end_checksum(cio))
end

#
# Dataspaces
#

@enum(DataspaceType, DS_SCALAR, DS_SIMPLE, DS_NULL)

immutable Dataspace{N}
    dataspace_type::DataspaceType
    size::NTuple{N,Length}
end

immutable DataspaceHeaderP
    version::UInt8
    dimensionality::UInt8
    flags::UInt8
    dataspace_type::UInt8
end
define_packed(DataspaceHeaderP)

Dataspace(::Any) = Dataspace(DS_SCALAR, ())
Dataspace{T,N}(x::Array{T,N}) = Dataspace(DS_SIMPLE, convert(Tuple{Vararg{Length}}, reverse(size(x))))

sizeof{N}(::Union(Dataspace{N},Type{Dataspace{N}})) = 4 + sizeof(Length)*N

function write{N}(io::IO, dspace::Dataspace{N})
    write(io, DataspaceHeaderP(2, N, 0, dspace.dataspace_type))
    for x in dspace.size
        write(io, x::Length)
    end
end

#
# Datatypes
#

@enum(DatatypeClass,
      DT_FIXED_POINT = 0 | (UInt8(3) << 4),
      DT_FLOATING_POINT = 1 | (UInt8(3) << 4),
      DT_TIME = 2 | (UInt8(3) << 4),
      DT_STRING = 3 | (UInt8(3) << 4),
      DT_BITFIELD = 4 | (UInt8(3) << 4),
      DT_OPAQUE = 5 | (UInt8(3) << 4),
      DT_COMPOUND = 6 | (UInt8(3) << 4),
      DT_REFERENCE = 7 | (UInt8(3) << 4),
      DT_ENUMERATED = 8 | (UInt8(3) << 4),
      DT_VARIABLE_LENGTH = 9 | (UInt8(3) << 4),
      DT_ARRAY = 10 | (UInt8(3) << 4)
)

@enum(ByteOrder,
      BO_LE = 0,
      BO_BE = 1,
      BO_VAX = 3)

abstract H5Datatype

immutable DatatypeHeaderP
    class::UInt8
    bitfield1::UInt8
    bitfield2::UInt8
    bitfield3::UInt8
    size::UInt32
end
define_packed(DatatypeHeaderP)

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

function sizeof(dt::CompoundDatatype)
    sz = 8 + size_size(dt.size)*length(dt.names)
    for i = 1:length(dt.names)
        # Extra byte for null padding of name
        sz += sizeof(dt.names[i]) + 1 + sizeof(dt.members[i])
    end
    sz
end

function write(io::IO, dt::CompoundDatatype)
    n = length(dt.names)
    write(io, DatatypeHeaderP(DT_COMPOUND, n % UInt8, (n >> 8) % UInt8, 0x00, dt.size))
    for i = 1:length(dt.names)
        # Name
        write(io, pointer(dt.names[i]), sizeof(dt.names[i]))
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

immutable ReferenceDatatype <: H5Datatype
    class::UInt8
    bitfield1::UInt8
    bitfield2::UInt8
    bitfield3::UInt8
    size::UInt32
end
define_packed(ReferenceDatatype)

const REFERENCE = ReferenceDatatype(DT_REFERENCE, 0x00, 0x00, 0x00, 0x08)

H5Datatype(x::Union(Type{Int8}, Type{Int16}, Type{Int32}, Type{Int64}, Type{Int128})) =
    FixedPointDatatype(DT_FIXED_POINT, 0x08, 0x00, 0x00, sizeof(x), 0, 8*sizeof(x))
H5Datatype(x::Union(Type{UInt8}, Type{UInt16}, Type{UInt32}, Type{UInt64}, Type{UInt128})) =
    FixedPointDatatype(DT_FIXED_POINT, 0x00, 0x00, 0x00, sizeof(x), 0, 8*sizeof(x))
H5Datatype(x::Type{Float64}) =
    FloatingPointDatatype(DT_FLOATING_POINT, 0x20, 0x3f, 0x00, 8, 0, 64, 52, 11, 0, 52, 0x000003ff)
H5Datatype(x::Type{Float32}) =
    FloatingPointDatatype(DT_FLOATING_POINT, 0x20, 0x1f, 0x00, 4, 0, 32, 23, 8, 0, 23, 0x0000007f)
H5Datatype(x::Type{Float16}) =
    FloatingPointDatatype(DT_FLOATING_POINT, 0x20, 0x0f, 0x00, 2, 0, 16, 10, 5, 0, 10, 0x0000000f)
H5Datatype{T,N}(x::Type{Array{T,N}}) = H5Datatype(T)

#
# Datasets
#

# Use an Enum here when it doesn't make us allocate
const LC_COMPACT_STORAGE = 0x00
const LC_CONTIGUOUS_STORAGE = 0x01
const LC_CHUNKED_STORAGE = 0x02

immutable ObjectHeaderP
    signature::UInt32
    version::UInt8
    flags::UInt8
end
define_packed(ObjectHeaderP)

immutable Dataset{N,P<:H5Datatype,D}
    dataspace::Dataspace{N}
    datatype::P
    data::D
end

layout_class(dset::Dataset) = sizeof(dset.data) <= 8192 ? LC_COMPACT_STORAGE : LC_CONTIGUOUS_STORAGE

function payload_size(dset::Dataset)
    sz = sizeof(dset.dataspace) + sizeof(dset.datatype) + 2 + 4*sizeof(HeaderMessage) + 2
    if layout_class(dset) == LC_COMPACT_STORAGE
        sz + 2 + sizeof(dset.data)
    else
        sz + sizeof(Offset) + sizeof(Length)
    end
end

function write_data(io::MmapIO, dset::Dataset)
    if isa(dset.data, Array)
        write(io, pointer(dset.data::Array), length(dset.data))
    else
        write(io, dset.data)
    end
    nothing
end

function write(io::IO, dset::Dataset)
    startpos = position(io)
    psz = payload_size(dset)
    fullsz = sizeof(ObjectHeaderP) + size_size(psz) + psz

    cio = begin_checksum(io, fullsz)

    write(cio, ObjectHeaderP(OBJECT_HEADER_SIGNATURE, 2, size_flag(psz)))
    write_size(cio, psz)

    # Dataspace
    write(cio, HeaderMessage(HM_DATASPACE, sizeof(dset.dataspace), 0))
    write(cio, dset.dataspace)

    # Datatype
    write(cio, HeaderMessage(HM_DATATYPE, sizeof(dset.datatype), 0))
    write(cio, dset.datatype)

    # Fill value
    write(cio, HeaderMessage(HM_FILL_VALUE, 2, 0))
    write(cio, UInt8(3)) # Version
    write(cio, 0x09)     # Flags

    # Data storage layout
    if layout_class(dset) == LC_COMPACT_STORAGE
        write(cio, HeaderMessage(HM_DATA_LAYOUT, 4+sizeof(dset.data), 0))
        write(cio, UInt8(3))                  # Version
        write(cio, UInt8(0))                  # Layout class
        write(cio, UInt16(sizeof(dset.data))) # Size
        write_data(io, dset)
        write(io, end_checksum(cio))
    else
        write(cio, HeaderMessage(HM_DATA_LAYOUT, 2+sizeof(Offset)+sizeof(Length), 0))
        write(cio, UInt8(3))                  # Version
        write(cio, UInt8(1))                  # Layout class
        write(cio, Offset(startpos+fullsz+4)) # Offset
        write(cio, Length(sizeof(dset.data))) # Length
        write(io, end_checksum(cio))

        write_data(io, dset)
    end
end

function save(fname::String, name::String, obj)
    superblock = Superblock(0, 0, UNDEFINED_ADDRESS, 0, 0)
    dset = Dataset(Dataspace(obj), H5Datatype(typeof(obj)), obj)
    group = Group(LinkInfo(), [Link(name, sizeof(superblock))])

    io = MmapIO(fname, true)

    seek(io, sizeof(superblock))
    write(io, dset)

    superblock.root_group_object_header_address = position(io)
    write(io, group)

    superblock.end_of_file_address = position(io)
    truncate(io, position(io))

    seek(io, 0)
    write(io, superblock)

    close(io)
end

@noinline write_dset(p, x) =
    write(p, Dataset(Dataspace(x), H5Datatype(typeof(x)), x))

function reftest(fname::String, name::String, x)
    superblock = Superblock(0, 0, UNDEFINED_ADDRESS, 0, 0)

    io = MmapIO(fname, true)
    seek(io, sizeof(superblock))

    refs = Array(Reference, length(x))
    for i = 1:length(x)
        refs[i] = Reference(position(io))
        write_dset(io, x[i])
    end

    group = Group(LinkInfo(), [Link(name, position(io))])
    dset = Dataset(Dataspace(x), REFERENCE, refs)
    write(io, dset)

    superblock.root_group_object_header_address = position(io)
    write(io, group)

    superblock.end_of_file_address = position(io)
    truncate(io, position(io))

    seek(io, 0)
    write(io, superblock)

    close(io)
end

end # module
