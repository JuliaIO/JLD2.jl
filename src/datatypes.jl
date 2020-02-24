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
#    Opaque datatypes
#    References
struct BasicDatatype <: H5Datatype
    class::UInt8
    bitfield1::UInt8
    bitfield2::UInt8
    bitfield3::UInt8
    size::UInt32
end
define_packed(BasicDatatype)
StringDatatype(::Type{String}, size::Integer) =
    BasicDatatype(DT_STRING, 0x11, 0x00, 0x00, size)
OpaqueDatatype(size::Integer) =
    BasicDatatype(DT_OPAQUE, 0x00, 0x00, 0x00, size) # XXX make sure ignoring the tag is OK
ReferenceDatatype() =
    BasicDatatype(DT_REFERENCE, 0x00, 0x00, 0x00, sizeof(RelOffset))

# Reads a datatype message and returns a (offset::RelOffset, class::UInt8)
# tuple. If the datatype is committed, the offset is the offset of the
# committed datatype and the class is typemax(UInt8). Otherwise, the
# offset is the offset of the datatype in the file, and the class is
# the corresponding datatype class.
function read_datatype_message(io::IO, f::JLDFile, committed)
    if committed
        # Shared datatype
        read(io, UInt8) == 3 || throw(UnsupportedVersionException())
        read(io, UInt8) == 2 || throw(UnsupportedFeatureException())
        (typemax(UInt8), Int64(fileoffset(f, read(io, RelOffset))))
    else
        # Datatype stored here
        (read(io, UInt8), Int64(position(io)-1))
    end
end

# Replace a symbol or expression in an AST with a new one
function replace_expr(x, from, to)
    ex = copy(x)
    for i = 1:length(ex.args)
        x = ex.args[i]
        if x == from
            ex.args[i] = to
        elseif isa(x, Expr)
            ex.args[i] = replace_expr(x, from, to)
        end
    end
    ex
end

# Macro to read an entire datatype from a file, avoiding dynamic
# dispatch for all but variable length types
macro read_datatype(io, datatype_class, datatype, then)
    esc(quote
        if $datatype_class == DT_FIXED_POINT
            $(replace_expr(then, datatype, :(read($io, FixedPointDatatype))))
        elseif $datatype_class == DT_FLOATING_POINT
            $(replace_expr(then, datatype, :(read($io, FloatingPointDatatype))))
        elseif $datatype_class == DT_STRING || $datatype_class == DT_OPAQUE || $datatype_class == DT_REFERENCE
            $(replace_expr(then, datatype, :(read($io, BasicDatatype))))
        elseif $datatype_class == DT_COMPOUND
            $(replace_expr(then, datatype, :(read($io, CompoundDatatype))))
        elseif $datatype_class == DT_VARIABLE_LENGTH
            $(replace_expr(then, datatype, :(read($io, VariableLengthDatatype))))
        else
            throw(UnsupportedFeatureException())
        end
    end)
end

struct FixedPointDatatype <: H5Datatype
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

struct FloatingPointDatatype <: H5Datatype
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

class(dt::Union{BasicDatatype,FixedPointDatatype,FloatingPointDatatype}) = dt.class

struct CompoundDatatype <: H5Datatype
    size::UInt32
    names::Vector{Symbol}
    offsets::Vector{Int}
    members::Vector{H5Datatype}

    function CompoundDatatype(size, names, offsets, members)
        length(names) == length(offsets) == length(members) ||
            throw(ArgumentError("names, offsets, and members must have same length"))
        new(size, names, offsets, members)
    end
end

Base.:(==)(x::CompoundDatatype, y::CompoundDatatype) =
    x.size == y.size && x.names == y.names && x.offsets == y.offsets &&
    x.members == y.members
Base.hash(::CompoundDatatype) = throw(ArgumentError("hash not defined for CompoundDatatype"))

class(dt::CompoundDatatype) = DT_COMPOUND
function Base.sizeof(dt::CompoundDatatype)
    sz = sizeof(BasicDatatype) + size_size(dt.size)*length(dt.names)
    for i = 1:length(dt.names)
        # Extra byte for null padding of name
        sz += symbol_length(dt.names[i]) + 1 + sizeof(dt.members[i])
    end
    sz
end

function Base.write(io::IO, dt::CompoundDatatype)
    n = length(dt.names)
    write(io, BasicDatatype(DT_COMPOUND, n % UInt8, (n >> 8) % UInt8, 0x00, dt.size))
    for i = 1:length(dt.names)
        # Name
        name = dt.names[i]
        name_ptr = Base.unsafe_convert(Ptr{UInt8}, name)
        unsafe_write(io, name_ptr, symbol_length(name)+1)

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

function Base.read(io::IO, ::Type{CompoundDatatype})
    dt = read(io, BasicDatatype)
    nfields = UInt16(dt.bitfield1) | UInt16(dt.bitfield2 << 8)
    dt.bitfield3 == 0 || throw(UnsupportedFeatureException())

    names = Vector{Symbol}(undef, nfields)
    offsets = Vector{Int}(undef, nfields)
    members = Vector{H5Datatype}(undef, nfields)
    for i = 1:nfields
        # Name
        names[i] = Symbol(read_bytestring(io))

        # Byte offset of member
        if dt.size <= typemax(UInt8)
            offsets[i] = read(io, UInt8)
        elseif dt.size <= typemax(UInt16)
            offsets[i] = read(io, UInt16)
        else
            offsets[i] = read(io, UInt32)
        end

        # Member type message
        datatype_class = read(io, UInt8)
        skip(io, -1)
        @read_datatype io datatype_class member begin
            members[i] = member
        end
    end

    CompoundDatatype(dt.size, names, offsets, members)
end

struct VariableLengthDatatype{T<:H5Datatype} <: H5Datatype
    class::UInt8
    bitfield1::UInt8
    bitfield2::UInt8
    bitfield3::UInt8
    size::UInt32
    basetype::T
end
VariableLengthDatatype(basetype::H5Datatype) =
    VariableLengthDatatype{typeof(basetype)}(DT_VARIABLE_LENGTH, 0x00, 0x00, 0x00, 8+sizeof(RelOffset), basetype)
VariableLengthDatatype(class, bitfield1, bitfield2, bitfield3, size, basetype::H5Datatype) =
    VariableLengthDatatype{typeof(basetype)}(class, bitfield1, bitfield2, bitfield3, size, basetype)

Base.:(==)(x::VariableLengthDatatype, y::VariableLengthDatatype) =
    x.class == y.class && x.bitfield1 == y.bitfield1 &&
    x.bitfield2 == y.bitfield2 && x.size == y.size &&
    x.basetype == y.basetype
Base.hash(::VariableLengthDatatype) = throw(ArgumentError("hash not defined for CompoundDatatype"))

class(dt::VariableLengthDatatype) = dt.class
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
    @read_datatype io datatype_class dt begin
        VariableLengthDatatype(dtype.class, dtype.bitfield1, dtype.bitfield2, dtype.bitfield3, dtype.size, dt)
    end
end

Base.sizeof(dt::CommittedDatatype) = 2 + sizeof(RelOffset)

function Base.write(io::IO, dt::CommittedDatatype)
    write(io, UInt8(3))
    write(io, UInt8(2))
    write(io, dt.header_offset)
end

function commit(f::JLDFile, dt::H5Datatype, attrs::Tuple{Vararg{WrittenAttribute}}=())
    psz = sizeof(HeaderMessage) * (length(attrs) + 1) + sizeof(dt)
    for attr in attrs
        psz += sizeof(attr)
    end
    io = f.io

    sz = sizeof(ObjectStart) + size_size(psz) + psz
    offset = f.end_of_data
    seek(io, offset)
    f.end_of_data = offset + sz + 4

    cio = begin_checksum_write(io, sz)
    write(cio, ObjectStart(size_flag(psz)))
    write_size(cio, psz)
    write(cio, HeaderMessage(HM_DATATYPE, sizeof(dt), 64))
    write(cio, dt)
    for attr in attrs
        write(cio, HeaderMessage(HM_ATTRIBUTE, sizeof(attr), 0))
        write_attribute(cio, f, attr, f.datatype_wsession)
    end
    write(io, end_checksum(cio))
end

# Read the actual datatype for a committed datatype
function read_committed_datatype(f::JLDFile, cdt::CommittedDatatype)
    io = f.io
    seek(io, fileoffset(f, cdt.header_offset))
    cio = begin_checksum_read(io)
    sz = read_obj_start(cio)
    pmax = position(cio) + sz

    # Messages
    datatype_class::UInt8 = 0
    datatype_offset::Int = 0
    attrs = ReadAttribute[]
    while position(cio) < pmax
        msg = read(cio, HeaderMessage)
        endpos = position(cio) + msg.size
        if msg.msg_type == HM_DATATYPE
            # Datatype stored here
            datatype_offset = position(cio)
            datatype_class = read(cio, UInt8)
        elseif msg.msg_type == HM_ATTRIBUTE
            push!(attrs, read_attribute(cio, f))
        end
        seek(cio, endpos)
    end
    seek(cio, pmax)

    # Checksum
    end_checksum(cio) == read(io, UInt32) || throw(InvalidDataException())

    seek(io, datatype_offset)
    @read_datatype io datatype_class dt begin
        return (dt, attrs)
    end
end


struct FixedLengthString{T<:AbstractString}
    length::Int
end
