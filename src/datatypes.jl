#
# Datatypes
#

# Datatype is encoded in the lower four bytes (0-10)
# upper four bytes encode variant used. JLD2 always uses variant 3
# detail in the hdf5 format spec
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

const DATATYPES = Dict{UInt8, String}(
    0 => "DT_FIXED_POINT",
    1 => "DT_FLOATING_POINT",
    2 => "DT_TIME",
    3 => "DT_STRING",
    4 => "DT_BITFIELD",
    5 => "DT_OPAQUE",
    6 => "DT_COMPOUND",
    7 => "DT_REFERENCE",
    8 => "DT_ENUMERATED",
    9 => "DT_VARIABLE_LENGTH",
    10=> "DT_ARRAY")

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
    BasicDatatype(DT_REFERENCE, 0x00, 0x00, 0x00, jlsizeof(RelOffset))

function Base.:(==)(dt1::BasicDatatype, dt2::BasicDatatype)
    ret = true
    ret &= (dt1.class << 4) == (dt2.class << 4)
    ret &= dt1.bitfield1 == dt2.bitfield1
    ret &= dt1.bitfield2 == dt2.bitfield2
    ret &= dt1.bitfield3 == dt2.bitfield3
    ret &= dt1.size == dt2.size
    ret
end
# Reads a datatype message and returns a (offset::RelOffset, class::UInt8)
# tuple. If the datatype is committed, the offset is the offset of the
# committed datatype and the class is typemax(UInt8). Otherwise, the
# offset is the offset of the datatype in the file, and the class is
# the corresponding datatype class.
function read_datatype_message(io::IO, f::JLDFile, committed)
    if committed
        # Shared datatype
        version = jlread(io, UInt8)
        msgtype = jlread(io, UInt8)
        # supported combinations are 
        (version == 3 && msgtype == 2) || (version == 2) || throw(UnsupportedVersionException("Unsupported shared message"))

        (typemax(UInt8), Int64(fileoffset(f, jlread(io, RelOffset))))
    else
        # Datatype stored here
        class = jlread(io, UInt8)
        (class, Int64(position(io)-1))
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
        if $datatype_class << 4 == DT_FIXED_POINT << 4
            $(replace_expr(then, datatype, :(jlread($io, FixedPointDatatype))))
        elseif $datatype_class << 4 == DT_FLOATING_POINT << 4
            $(replace_expr(then, datatype, :(jlread($io, FloatingPointDatatype))))
        elseif $datatype_class << 4 == DT_STRING << 4 || $datatype_class << 4 == DT_OPAQUE << 4 || $datatype_class << 4 == DT_REFERENCE << 4
            $(replace_expr(then, datatype, :(jlread($io, BasicDatatype))))
        elseif $datatype_class << 4 == DT_COMPOUND << 4
            $(replace_expr(then, datatype, :(jlread($io, CompoundDatatype))))
        elseif $datatype_class << 4 == DT_VARIABLE_LENGTH << 4
            $(replace_expr(then, datatype, :(jlread($io, VariableLengthDatatype))))
        elseif $datatype_class << 4 == DT_BITFIELD << 4
            $(replace_expr(then, datatype, :(jlread($io, BitFieldDatatype))))
        elseif $datatype_class << 4 == DT_TIME << 4
            $(replace_expr(then, datatype, :(jlread($io, TimeDatatype))))
        elseif $datatype_class << 4 == DT_ARRAY << 4
            $(replace_expr(then, datatype, :(jlread($io, ArrayDatatype))))
        elseif $datatype_class << 4 == DT_ENUMERATED << 4
            $(replace_expr(then, datatype, :(jlread($io, EnumerationDatatype))))
        else
            throw(UnsupportedFeatureException("invalid datatype class $datatype_class"))
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

struct BitFieldDatatype <: H5Datatype
    class::UInt8
    bitfield1::UInt8
    bitfield2::UInt8
    bitfield3::UInt8
    size::UInt32
    bitoffset::UInt16
    bitprecision::UInt16
end
define_packed(BitFieldDatatype)
BitFieldDatatype(size) =
    BitFieldDatatype(DT_BITFIELD, 0x00, 0x00, 0x00, size, 0, 8*size)


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

function Base.:(==)(fp1::FloatingPointDatatype, fp2::FloatingPointDatatype)
    ret = true
    ret &= (fp1.class << 4) == (fp2.class << 4) # compare only class and not version
    ret &= fp1.bitfield1 == fp2.bitfield1
    ret &= fp1.bitfield2 == fp2.bitfield2
    ret &= fp1.bitfield3 == fp2.bitfield3
    ret &= fp1.size == fp2.size
    ret &= fp1.bitoffset == fp2.bitoffset
    ret &= fp1.bitprecision == fp2.bitprecision
    ret &= fp1.exponentlocation == fp2.exponentlocation
    ret &= fp1.exponentsize == fp2.exponentsize
    ret &= fp1.mantissalocation == fp2.mantissalocation
    ret &= fp1.mantissasize == fp2.mantissasize
    ret &= fp1.exponentbias == fp2.exponentbias
    ret
end

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

function jltype(f::JLDFile, dt::CompoundDatatype)
    odr = reconstruct_odr(f, dt, RelOffset[])
    T = NamedTuple{tuple(dt.names...), typeof(odr).parameters[2]}
    return ReadRepresentation{T, odr}()    
end

Base.:(==)(x::CompoundDatatype, y::CompoundDatatype) =
    x.size == y.size && x.names == y.names && x.offsets == y.offsets &&
    x.members == y.members
Base.hash(::CompoundDatatype) = throw(ArgumentError("hash not defined for CompoundDatatype"))

class(dt::CompoundDatatype) = DT_COMPOUND
function jlsizeof(dt::CompoundDatatype)
    sz = jlsizeof(BasicDatatype) + size_size(dt.size)*length(dt.names)
    for i = 1:length(dt.names)
        # Extra byte for null padding of name
        sz += symbol_length(dt.names[i]) + 1 + jlsizeof(dt.members[i])
    end
    sz::Int
end

function jlwrite(io::IO, dt::CompoundDatatype)
    n = length(dt.names)
    jlwrite(io, BasicDatatype(DT_COMPOUND, n % UInt8, (n >> 8) % UInt8, 0x00, dt.size))
    for i = 1:length(dt.names)
        # Name
        name = dt.names[i]
        name_ptr = Base.unsafe_convert(Ptr{UInt8}, name)
        unsafe_write(io, name_ptr, symbol_length(name)+1)

        # Byte offset of member
        if dt.size <= typemax(UInt8)
            jlwrite(io, UInt8(dt.offsets[i]))
        elseif dt.size <= typemax(UInt16)
            jlwrite(io, UInt16(dt.offsets[i]))
        else
            jlwrite(io, UInt32(dt.offsets[i]))
        end

        # Member type message
        jlwrite(io, dt.members[i])
    end
end

function jlread(io::IO, ::Type{CompoundDatatype})
    dt = jlread(io, BasicDatatype)
    version = dt.class >> 4
    nfields = UInt16(dt.bitfield1) | UInt16(dt.bitfield2 << 8)
    dt.bitfield3 == 0 || throw(UnsupportedFeatureException())

    names = Vector{Symbol}(undef, nfields)
    offsets = Vector{Int}(undef, nfields)
    members = Vector{H5Datatype}(undef, nfields)
    for i = 1:nfields
        # Name
        names[i] = Symbol(read_bytestring(io))
        # Byte offset of member
        if version == 2 || version == 1
            skip(io, 8-mod1(sizeof(names[i]),8)-1)
            offsets[i] = jlread(io, UInt32)
        elseif dt.size <= typemax(UInt8)
            offsets[i] = jlread(io, UInt8)
        elseif dt.size <= typemax(UInt16)
            offsets[i] = jlread(io, UInt16)
        else
            offsets[i] = jlread(io, UInt32)
        end

        if version == 1
            # supports array members
            # can encode dimensionality here
            dimensionality = jlread(io, UInt8)
            skip(io, 3)
            skip(io, 4) # dimension permutation
            skip(io, 4)
            skip(io, 16)
        end

        # Member type message
        datatype_class = jlread(io, UInt8)
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
    VariableLengthDatatype{typeof(basetype)}(DT_VARIABLE_LENGTH, 0x00, 0x00, 0x00, 8+jlsizeof(RelOffset), basetype)
VariableLengthDatatype(class, bitfield1, bitfield2, bitfield3, size, basetype::H5Datatype) =
    VariableLengthDatatype{typeof(basetype)}(class, bitfield1, bitfield2, bitfield3, size, basetype)

Base.:(==)(x::VariableLengthDatatype, y::VariableLengthDatatype) =
    x.class == y.class && x.bitfield1 == y.bitfield1 &&
    x.bitfield2 == y.bitfield2 && x.size == y.size &&
    x.basetype == y.basetype
Base.hash(::VariableLengthDatatype) = throw(ArgumentError("hash not defined for CompoundDatatype"))

class(dt::VariableLengthDatatype) = dt.class
jlsizeof(dt::VariableLengthDatatype) =
    jlsizeof(BasicDatatype) + jlsizeof(dt.basetype)

function jlwrite(io::IO, dt::VariableLengthDatatype)
    jlwrite(io, BasicDatatype(DT_VARIABLE_LENGTH, dt.bitfield1, dt.bitfield2, dt.bitfield3, dt.size))
    jlwrite(io, dt.basetype)
end

function jlread(io::IO, ::Type{VariableLengthDatatype})
    dtype = jlread(io, BasicDatatype)
    datatype_class = jlread(io, UInt8)
    skip(io, -1)
    @read_datatype io datatype_class dt begin
        VariableLengthDatatype(dtype.class, dtype.bitfield1, dtype.bitfield2, dtype.bitfield3, dtype.size, dt)
    end
end

jlsizeof(dt::CommittedDatatype) = 2 + jlsizeof(RelOffset)

function jlwrite(io::IO, dt::CommittedDatatype)
    jlwrite(io, UInt8(3))
    jlwrite(io, UInt8(2))
    jlwrite(io, dt.header_offset)
end

function commit(f::JLDFile,
        @nospecialize(dt::H5Datatype), 
        attrs::Tuple{Vararg{WrittenAttribute}}=())
    psz = jlsizeof(HeaderMessage) * (length(attrs) + 1) + jlsizeof(dt)
    for attr in attrs
        psz += jlsizeof(attr)
    end
    io = f.io

    sz = jlsizeof(ObjectStart) + size_size(psz) + psz
    offset = f.end_of_data
    seek(io, offset)
    f.end_of_data = offset + sz + 4

    cio = begin_checksum_write(io, sz)
    jlwrite(cio, ObjectStart(size_flag(psz)))
    write_size(cio, psz)
    jlwrite(cio, HeaderMessage(HM_DATATYPE, jlsizeof(dt), 64))
    jlwrite(cio, dt)
    for attr in attrs
        jlwrite(cio, HeaderMessage(HM_ATTRIBUTE, jlsizeof(attr), 0))
        write_attribute(cio, f, attr, f.datatype_wsession)
    end
    jlwrite(io, end_checksum(cio))
end

# Read the actual datatype for a committed datatype
function read_shared_datatype(f::JLDFile, cdt::Union{SharedDatatype, CommittedDatatype})
    io = f.io
    chunk_start::Int64 = fileoffset(f, cdt.header_offset)
    seek(io, chunk_start)

    header_version = jlread(io, UInt8)
    if header_version == 1
        seek(io, chunk_start)
        cio = io
        sz, = read_obj_start(cio)
        chunk_end = position(cio) + sz
        # Skip to nearest 8byte aligned position
        skip_to_aligned!(cio, chunk_start)
    else
        header_version = 2
        seek(io, chunk_start)
        cio = begin_checksum_read(io)
        sz, = read_obj_start(cio)
        chunk_end = position(cio) + sz
    end
    # Messages
    chunk_end::Int64
    continuation_message_goes_here::Int64 = -1
    chunks = [(; chunk_start, chunk_end)]
    chunk_number = 0

    # Messages
    datatype_class::UInt8 = 0
    datatype_offset::Int = 0
    attrs = ReadAttribute[]


    while !isempty(chunks)
        chunk = popfirst!(chunks)
        chunk_start = chunk.chunk_start
        chunk_end = chunk.chunk_end

        if chunk_number > 0
            seek(io, chunk_start)
            chunk_end -= 4
            if header_version == 2
                cio = begin_checksum_read(io)
                jlread(cio, UInt32) == OBJECT_HEADER_CONTINUATION_SIGNATURE || throw(InvalidDataException())
            end
        end
        chunk_number += 1
        while (curpos = position(cio)) < chunk_end-4
            if header_version == 1
                # Message start 8byte aligned relative to object start
                skip_to_aligned!(cio, chunk_start)
                # Version 1 header message is padded
                msg = HeaderMessage(jlread(cio, UInt16), jlread(cio, UInt16), jlread(cio, UInt8))
                skip(cio, 3)
            else # header_version == 2
                msg = jlread(cio, HeaderMessage)
            end
            endpos = position(cio) + msg.size
            if msg.msg_type == HM_DATATYPE
                # Datatype stored here
                datatype_offset = position(cio)
                datatype_class = jlread(cio, UInt8)
            elseif msg.msg_type == HM_ATTRIBUTE
                push!(attrs, read_attribute(cio, f))
            elseif msg.msg_type == HM_OBJECT_HEADER_CONTINUATION
                    cont_chunk_start = fileoffset(f, jlread(cio, RelOffset))
                    chunk_length = jlread(cio, Length)
                    push!(chunks, (;chunk_start=cont_chunk_start,
                                    chunk_end  =cont_chunk_start+chunk_length))
            elseif (msg.flags & 2^3) != 0
                throw(UnsupportedFeatureException())
            end
            seek(cio, endpos)
        end

        # Checksum
        #seek(cio, chunk_end)
        if header_version == 2
            end_checksum(cio) == jlread(io, UInt32) || throw(InvalidDataException())
        end
        seek(cio, chunk_end)
    end

    seek(io, datatype_offset)
    @read_datatype io datatype_class dt begin
        return (dt, attrs)
    end
end

struct FixedLengthString{T<:AbstractString}
    length::Int
end


struct ArrayDatatype <: H5Datatype
    class::UInt8
    bitfield1::UInt8
    bitfield2::UInt8
    bitfield3::UInt8
    dimensionality::UInt8
    dims::Vector{UInt32}
    base_type::H5Datatype
end

function jlread(io::IO, ::Type{ArrayDatatype})
    dt = jlread(io, BasicDatatype)
    version = dt.class >> 4
    dimensionality = jlread(io, UInt8)
    version == 2 && skip(io, 3)
    dims = jlread(io, UInt32, dimensionality)
    if version == 2
        # unsupported permutation index
        skip(io, 4*dimensionality)
    end
    
    datatype_class = jlread(io, UInt8)
    skip(io, -1)
    @read_datatype io datatype_class base_type begin
        ArrayDatatype(dt.class, 0x0, 0x0, 0x0, dimensionality, dims, base_type)
    end
end

struct ArrayPlaceHolder{T, D} end

odr_sizeof(::Type{ArrayPlaceHolder{T,D}}) where {T,D} = odr_sizeof(T)*prod(D)

function jltype(f::JLDFile, dt::ArrayDatatype)
    rr = jltype(f, dt.base_type)
    T = typeof(rr).parameters[1]
    ReadRepresentation{Array{T, Int(dt.dimensionality)}, ArrayPlaceHolder{rr, tuple(dt.dims...)}}()
end


function jlconvert(::ReadRepresentation{Array{T,D}, ArrayPlaceHolder{RR, DIMS}}, f::JLDFile, ptr::Ptr, 
                    header_offset::RelOffset) where {T, D, RR, DIMS}
    v = Array{T, D}(undef, reverse(DIMS)...)
    for i=1:prod(DIMS)
        v[i] = jlconvert(RR, f, ptr, header_offset)
        ptr += jlsizeof(typeof(RR).parameters[2])
    end
    return v
end

struct EnumerationDatatype <: H5Datatype
    class::UInt8
    bitfield1::UInt8
    bitfield2::UInt8
    bitfield3::UInt8
    base_type::DataType
    names::Vector{String}
    values::Vector{<:Any}
end

function jlread(io::IO, ::Type{EnumerationDatatype})
    dt = jlread(io, BasicDatatype)
    version = dt.class >> 4
    num_members = dt.bitfield1 | UInt16(dt.bitfield2)<<8
    base_type = jlread(io, BasicDatatype)
    T = uintofsize(base_type.size)
    # assume that it is an Integer
    names = String[]
    values = Any[]
    for _=1:num_members
        name = read_bytestring(io)
        push!(names, name)
        version < 3 && skip(io, 8-mod1(sizeof(name), 8))
        push!(values, jlread(io, T))
    end
    return EnumerationDatatype(dt.class, dt.bitfield1, dt.bitfield2, dt.bitfield3,
                                T, names, values)
end

function jltype(f::JLDFile, dt::EnumerationDatatype)
    ReadRepresentation{dt.base_type, dt.base_type}()
end

# Can't read big endian ints 
