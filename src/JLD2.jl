module JLD2
include("Lookup3.jl")

const SUPERBLOCK_SIGNATURE = reinterpret(UInt64, UInt8[0o211, 'H', 'D', 'F', '\r', '\n', 0o032, '\n'])[1]
const OBJECT_HEADER_SIGNATURE = reinterpret(UInt32, UInt8['O', 'H', 'D', 'R'])[1]
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

ptrload(p::Ptr{Void}, ty::PlainType) =
    (unsafe_load(Ptr{ty}(p)), p + sizeof(ty))
function ptrstore!(p::Ptr{Void}, x::Plain)
    unsafe_store!(Ptr{typeof(x)}(p), x)
    p + sizeof(x)
end

# Redefine unsafe_load and unsafe_store! so that they pack the type
# Assumes default constructor
packed_sizeof(ty::DataType) = sizeof(ty)

checksum(startptr::Ptr{Void}, endptr::Ptr{Void}) =
    Lookup3.hash(pointer_to_array(Ptr{UInt8}(startptr), endptr - startptr))

# Loads a variable-length size according to flags
# Expects that the first two bits of flags mean:
# 0   The size of the Length of Link Name field is 1 byte.
# 1   The size of the Length of Link Name field is 2 bytes.
# 2   The size of the Length of Link Name field is 4 bytes.
# 3   The size of the Length of Link Name field is 8 bytes.
# Returns the size as a UInt and a new pointer that is offset by the
# size of the size field
function load_size(p::Ptr{Void}, flags::UInt8)
    if (flags & 1) == 0 && (flags & 2) == 0
        ptrload(p, UInt8)
    elseif (flags & 1) == 1 && (flags & 2) == 0
        ptrload(p, UInt16)
    elseif (flags & 1) == 0 && (flags & 2) == 2
        ptrload(p, UInt32)
    else
        ptrload(p, UInt64)
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
function store_size!(p::Ptr{Void}, sz::Integer)
    if sz <= typemax(UInt8)
        ptrstore!(p, UInt8(sz))
    elseif sz <= typemax(UInt16)
        ptrstore!(p, UInt16(sz))
    elseif sz <= typemax(UInt32)
        ptrstore!(p, UInt32(sz))
    else
        ptrstore!(p, UInt64(sz))
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

packed_sizeof(::Union(Type{Superblock}, Superblock)) = 
    12+sizeof(Offset)*4+4

function ptrload(ptr::Ptr{Void}, ::Type{Superblock})
    p = ptr

    # Signature
    signature, p = ptrload(p, UInt64)
    signature == SUPERBLOCK_SIGNATURE || throw(UnsupportedVersionException())

    # Version
    version, p = ptrload(p, UInt8)
    version == 2 || throw(UnsupportedVersionException())

    # Size of offsets and size of lengths
    size_of_offsets, p = ptrload(p, Offset)
    size_of_lengths, p = ptrload(p, Offset)
    (size_of_offsets == 8 && size_of_lengths == 8) || throw(UnsupportedFeatureException())

    # File consistency flags
    file_consistency_flags, p = ptrload(p, UInt8)

    # Addresses
    base_address, p = ptrload(p, Offset)
    superblock_extension_address, p = ptrload(p, Offset)
    end_of_file_address, p = ptrload(p, Offset)
    root_group_object_header_address, p = ptrload(p, Offset)

    # Checksum
    cs, pend = ptrload(p, UInt32)
    cs == checksum(ptr, p) || throw(InvalidDataException())

    (Superblock(file_consistency_flags, base_address, superblock_extension_address,
                end_of_file_address, root_group_object_header_address), pend)
end

function ptrstore!(ptr::Ptr{Void}, s::Superblock)
    p = ptr

    p = ptrstore!(p, SUPERBLOCK_SIGNATURE::UInt64)    # Signature
    p = ptrstore!(p, UInt8(2))                        # Version
    p = ptrstore!(p, UInt8(8))                        # Size of offsets
    p = ptrstore!(p, UInt8(8))                        # Size of lengths
    p = ptrstore!(p, s.file_consistency_flags::UInt8)
    p = ptrstore!(p, s.base_address::Offset)
    p = ptrstore!(p, s.superblock_extension_address::Offset)
    p = ptrstore!(p, s.end_of_file_address::Offset)
    p = ptrstore!(p, s.root_group_object_header_address::Offset)
    ptrstore!(p, checksum(ptr, p))
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
    msg_type::HeaderMessageType
    size::UInt16
    flags::UInt8
end

packed_sizeof(::Union(HeaderMessage, Type{HeaderMessage})) = 4

#
# Groups
#

@inline function ptrload(p::Ptr{Void}, ::Type{HeaderMessage})
    msg_type, p = ptrload(p, UInt8)
    size, p = ptrload(p, UInt16)
    flags, p = ptrload(p, UInt8)
    (HeaderMessage(msg_type, size, flags), p)
end

@inline function ptrstore!(p::Ptr{Void}, msg::HeaderMessage)
    p = ptrstore!(p, UInt8(msg.msg_type))
    p = ptrstore!(p, msg.size::UInt16)
    ptrstore!(p, msg.flags::UInt8)
end

immutable LinkInfo
    fractal_heap_address::Offset
    name_index_btree::Offset
end
LinkInfo() = LinkInfo(UNDEFINED_ADDRESS, UNDEFINED_ADDRESS)

packed_sizeof(::LinkInfo) = 18

function ptrload(p::Ptr{Void}, ::Type{LinkInfo})
    version, p = ptrload(p, UInt8)
    version == 0 || throw(UnsupportedVersionException())

    flags, p = ptrload(p, UInt8)
    flags == 0 || throw(UnsupportedFeatureException())

    fractal_heap_address, p = ptrload(p, Offset)
    name_index_btree, p = ptrload(p, Offset)

    (LinkInfo(fractal_heap_address, name_index_btree), p)
end

function ptrstore!(p::Ptr{Void}, lh::LinkInfo)
    p = ptrstore!(p, UInt16(0)) # Version and flags
    p = ptrstore!(p, lh.fractal_heap_address::Offset)
    ptrstore!(p, lh.name_index_btree::Offset)
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

function packed_sizeof(link::Link)
    sz = 2 + size_size(sizeof(link.name)) + sizeof(link.name) + sizeof(Offset)
    isa(link.name, UTF8String) && (sz += 1)
    sz
end

function ptrload(p::Ptr{Void}, ::Type{Link})
    # Version
    version, p = ptrload(p, UInt8)
    version == 1 || throw(UnsupportedVersionException())

    # Flags
    flags = ptrload(p, UInt8)

    if (flags & LM_LINK_TYPE_FIELD_PRESENT) != 0
        link_type, p = ptrload(p, UInt8)
        link_type == 0 || throw(UnsupportedFeatureException())
    end

    if (flags & LM_CREATION_ORDER_PRESENT) != 0
        p += 8
    end

    # Link name character set
    cset = CSET_ASCII
    if (flags & LM_LINK_NAME_CHARACTER_SET_FIELD_PRESENT) != 0
        cset_byte, p = ptrload(p, UInt8)
        cset = CharacterSet(cset_byte)
    end

    # Size
    sz, p = load_size(p, flags)

    # Link name
    name = cset == CSET_ASCII ? ascii(p, sz) : utf8(p, sz)
    p += sz
    
    # Link information
    target, p = ptrload(p, Offset)

    (Link(name, target), p)
end

function ptrstore!(p::Ptr{Void}, link::Link)
    # Version
    p = ptrstore!(p, UInt8(1))

    # Flags
    flags = size_flag(sizeof(link.name))
    if isa(link.name, UTF8String)
        flags &= LM_LINK_NAME_CHARACTER_SET_FIELD_PRESENT
    end
    p = ptrstore!(p, flags::UInt8)

    # Link name character set
    if isa(link.name, UTF8String)
        p = ptrstore!(p, UInt8(CSET_UTF8))
    end

    # Length of link name
    p = store_size!(p, sizeof(link.name))

    # Link name
    unsafe_copy!(Ptr{UInt8}(p), pointer(link.name), sizeof(link.name))
    p += sizeof(link.name)

    # Link information
    ptrstore!(p, link.target::Offset)
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
    sz = packed_sizeof(group.link_info) + 2 +
                 (length(group.links) + 2) * packed_sizeof(HeaderMessage)
    for link in group.links
        sz += packed_sizeof(link)
    end
    sz
end

function packed_sizeof(group::Group)
    sz = payload_size(group)
    4 + 1 + 1 + size_size(sz) + sz + 4
end

function ptrload(ptr::Ptr{Void}, ::Type{Group})
    p = ptr

    # Signature
    signature = ptrload(p, UInt32)
    signature == OBJECT_HEADER_SIGNATURE || throw(InvalidDataException())

    # Version
    version = ptrload(p, UInt8)
    version == 2 || throw(UnsupportedVersionException())
    p += 1

    # Flags
    flags = ptrload(p, UInt8)
    p += 1

    if (flags & OH_TIMES_STORED) != 0
        # Skip access, modification, change and birth times
        p += 128
    end
    if (flags & OH_ATTRIBUTE_PHASE_CHANGE_VALUES_STORED) != 0
        # Skip maximum # of attributes fields
        p += 32
    end

    # Size
    sz, p = load_size(p, flags)
    pmax = p + sz
    link_info = LinkInfo()
    links = Link[]

    # Messages
    while p < pmax
        msg, p = ptrload(p, HeaderMessage)
        if msg.msg_type == HM_LINK_INFO
            link_info = ptrload(p, LinkInfo)[1]
            link_info.fractal_heap_address == UNDEFINED_ADDRESS || throw(UnsupportedFeatureException())
        elseif msg.msg_type == HM_GROUP_INFO
            # This message doesn't help us much, so we ignore it for now
        elseif msg.msg_type == HM_LINK_MESSAGE
            push!(links, ptrload(p, Link)[1])
        elseif (msg.flags & 2^3) != 0
            throw(UnsupportedFeatureException())
        elseif msg.msg_types == HM_NIL
            break
        end
        p += msg.size
    end

    # Checksum
    cs = unsafe_load!(Ptr{UInt32}(pmax))
    cs == checksum(ptr, pmax) || throw(InvalidDataException())

    Group(link_info, links)
end

function ptrstore!(ptr::Ptr{Void}, group::Group)
    group.link_info.fractal_heap_address == UNDEFINED_ADDRESS ||
        throw(UnsupportedFeatureException())

    p = ptr

    sz = payload_size(group)
    p = ptrstore!(p, OBJECT_HEADER_SIGNATURE::UInt32) # Header
    p = ptrstore!(p, UInt8(2))                        # Version
    p = ptrstore!(p, size_flag(sz)::UInt8)            # Flags
    p = store_size!(p, sz)                            # Size

    # Link info message
    p = ptrstore!(p, HeaderMessage(HM_LINK_INFO, packed_sizeof(group.link_info), 0))
    p = ptrstore!(p, group.link_info)

    # Group info message
    p = ptrstore!(p, HeaderMessage(HM_GROUP_INFO, 2, 0))
    p = ptrstore!(p, UInt16(0))

    # Links
    for link in group.links
        p = ptrstore!(p, HeaderMessage(HM_LINK_MESSAGE, packed_sizeof(link), 0))
        p = ptrstore!(p, link)
    end

    # Checksum
    p = ptrstore!(p, checksum(ptr, p))
    @assert p - ptr == packed_sizeof(group)
    p
end

#
# Datasets
#

@enum(DataspaceType, DS_SCALAR, DS_SIMPLE, DS_NULL)

immutable Dataspace{N}
    dataspace_type::DataspaceType
    size::NTuple{N,Length}
end

Dataspace(::Any) = Dataspace(DS_SCALAR, ())
Dataspace{T,N}(x::Array{T,N}) = Dataspace(DS_SIMPLE, convert(Tuple{Vararg{Length}}, reverse(size(x))))

packed_sizeof{N}(::Union(Dataspace{N},Type{Dataspace{N}})) = 4 + sizeof(Length)*N

function ptrstore!{N}(p::Ptr{Void}, dspace::Dataspace{N})
    p = ptrstore!(p, UInt8(2))                     # Version
    p = ptrstore!(p, UInt8(N))                     # Dimensionality
    p = ptrstore!(p, UInt8(0))                     # Flags
    p = ptrstore!(p, UInt8(dspace.dataspace_type)) # Type
    for x in dspace.size
        p = ptrstore!(p, x::Length)
    end
    p
end

@enum(DatatypeClass,
      DT_FIXED_POINT,
      DT_FLOATING_POINT,
      DT_TIME,
      DT_STRING,
      DT_BITFIELD,
      DT_OPAQUE,
      DT_COMPOUND,
      DT_REFERENCE,
      DT_ENUMERATED,
      DT_VARIABLE_LENGTH,
      DT_ARRAY
)

immutable FixedPointProperties
    bitoffset::UInt16
    bitprecision::UInt16
end

packed_sizeof(::FixedPointProperties) = 4

function ptrstore!(p::Ptr{Void}, props::FixedPointProperties)
    p = ptrstore!(p, props.bitoffset::UInt16)
    ptrstore!(p, props.bitprecision::UInt16)
end

immutable FloatingPointProperties
    bitoffset::UInt16
    bitprecision::UInt16
    exponentlocation::UInt8
    exponentsize::UInt8
    mantissalocation::UInt8
    mantissasize::UInt8
    exponentbias::UInt32
end

packed_sizeof(::FloatingPointProperties) = 12

function ptrstore!(p::Ptr{Void}, props::FloatingPointProperties)
    p = ptrstore!(p, props.bitoffset::UInt16)
    p = ptrstore!(p, props.bitprecision::UInt16)
    p = ptrstore!(p, props.exponentlocation::UInt8)
    p = ptrstore!(p, props.exponentsize::UInt8)
    p = ptrstore!(p, props.mantissalocation::UInt8)
    p = ptrstore!(p, props.mantissasize::UInt8)
    p = ptrstore!(p, props.exponentbias::UInt32)
end

immutable Datatype{P}
    class::DatatypeClass
    classbitfield::NTuple{3,UInt8}
    size::UInt32
    properties::P
end

Datatype(x::Union(Type{Int8}, Type{Int16}, Type{Int32}, Type{Int64}, Type{Int128})) =
    Datatype(DT_FIXED_POINT, (0x08, 0x00, 0x00), UInt32(sizeof(x)), FixedPointProperties(0, 8*sizeof(x)))
Datatype(x::Union(Type{UInt8}, Type{UInt16}, Type{UInt32}, Type{UInt64}, Type{UInt128})) =
    Datatype(DT_FIXED_POINT, (0x00, 0x00, 0x00), UInt32(sizeof(x)), FixedPointProperties(0, 8*sizeof(x)))
Datatype(x::Type{Float64}) =
    Datatype(DT_FLOATING_POINT, (0x20, 0x3f, 0x00), UInt32(8), FloatingPointProperties(0, 64, 52, 11, 0, 52, 0x000003ff))
Datatype(x::Type{Float32}) =
    Datatype(DT_FLOATING_POINT, (0x20, 0x1f, 0x00), UInt32(4), FloatingPointProperties(0, 32, 23, 8, 0, 23, 0x0000007f))
Datatype(x::Type{Float16}) =
    Datatype(DT_FLOATING_POINT, (0x20, 0x0f, 0x00), UInt32(2), FloatingPointProperties(0, 16, 10, 5, 0, 10, 0x0000000f))
Datatype{T,N}(x::Type{Array{T,N}}) = Datatype(T)

packed_sizeof(dtype::Datatype) = 8 + packed_sizeof(dtype.properties)

function ptrstore!(p::Ptr{Void}, dtype::Datatype)
    # Class and version
    p = ptrstore!(p, UInt8(dtype.class) | (UInt8(3) << 4))

    # Class bit field
    for i = 1:3
        p = ptrstore!(p, UInt8(dtype.classbitfield[i]))
    end

    # Size
    p = ptrstore!(p, dtype.size::UInt32)

    # Properties
    if dtype.properties != nothing
        p = ptrstore!(p, dtype.properties)
    end
    p
end

immutable Dataset{N,P,D}
    dataspace::Dataspace{N}
    datatype::Datatype{P}
    data::D
end

function payload_size(dset::Dataset)
    sz = packed_sizeof(dset.dataspace) + packed_sizeof(dset.datatype) + 2 + 4*packed_sizeof(HeaderMessage)
    sz + 4 + sizeof(dset.data)
end

function packed_sizeof(dset::Dataset)
    sz = payload_size(dset)
    6 + size_size(sz) + sz + 4
end

function ptrstore!(ptr::Ptr{Void}, dset::Dataset)
    p = ptr

    sz = payload_size(dset)
    p = ptrstore!(p, OBJECT_HEADER_SIGNATURE::UInt32) # Header
    p = ptrstore!(p, UInt8(2))                        # Version
    p = ptrstore!(p, size_flag(sz)::UInt8)            # Flags
    p = store_size!(p, sz)                            # Size

    # Dataspace
    p = ptrstore!(p, HeaderMessage(HM_DATASPACE, packed_sizeof(dset.dataspace), 0))
    p = ptrstore!(p, dset.dataspace)

    # Datatype
    p = ptrstore!(p, HeaderMessage(HM_DATATYPE, packed_sizeof(dset.datatype), 0))
    p = ptrstore!(p, dset.datatype)

    # Fill value
    p = ptrstore!(p, HeaderMessage(HM_FILL_VALUE, 2, 0))
    p = ptrstore!(p, UInt8(3)) # Version
    p = ptrstore!(p, 0x09)     # Flags

    # Data storage layout
    p = ptrstore!(p, HeaderMessage(HM_DATA_LAYOUT, 4+sizeof(dset.data), 0))
    p = ptrstore!(p, UInt8(3))                  # Version
    p = ptrstore!(p, UInt8(0))                  # Compact storage
    p = ptrstore!(p, UInt16(sizeof(dset.data))) # Size
    if isa(dset.data, Array)
        unsafe_copy!(Ptr{eltype(dset.data)}(p), pointer(dset.data), length(dset.data))
        p += sizeof(dset.data)
    else
        p = ptrstore!(p, dset.data) # Raw data
    end

    # Checksum
    p = ptrstore!(p, checksum(ptr, p))
    @assert p - ptr == packed_sizeof(dset)
    p
end

function save(fname::String, name::String, obj)
    superblock = Superblock(0, 0, UNDEFINED_ADDRESS, 0, 0)
    dset = Dataset(Dataspace(obj), Datatype(typeof(obj)), obj)
    group = Group(LinkInfo(), [Link(name, packed_sizeof(superblock))])
    fsize = superblock.end_of_file_address = packed_sizeof(superblock)+packed_sizeof(dset)+packed_sizeof(group)
    superblock.root_group_object_header_address = packed_sizeof(superblock)+packed_sizeof(dset)

    f = open(fname, "w+")
    truncate(f, fsize)
    arr = mmap_array(UInt8, (fsize,), f)
    ptr = Ptr{Void}(pointer(arr))
    p = ptr
    p = ptrstore!(p, superblock)
    p = ptrstore!(p, dset)
    p = ptrstore!(p, group)
    @assert p == ptr+fsize
    close(f)
    msync(arr)
end

end # module
