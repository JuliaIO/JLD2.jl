## Signatures
const OBJECT_HEADER_SIGNATURE = htol(0x5244484f) # "OHDR"

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
RelOffset(r::RelOffset) = r
Base.:(==)(x::RelOffset, y::RelOffset) = x === y
Base.hash(x::RelOffset) = hash(x.offset)
Base.:(+)(x::RelOffset, y::Integer) = RelOffset(UInt64(x.offset + y))

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