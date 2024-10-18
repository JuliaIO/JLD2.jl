#
# Datatypes
#
@enum DatatypeClass::UInt8 begin
    DT_FIXED_POINT = 0x00
    DT_FLOATING_POINT = 0x01
    DT_TIME = 0x02
    DT_STRING = 0x03
    DT_BITFIELD = 0x04
    DT_OPAQUE = 0x05
    DT_COMPOUND = 0x06
    DT_REFERENCE = 0x07
    DT_ENUMERATED = 0x08
    DT_VARIABLE_LENGTH = 0x09
    DT_ARRAY = 0x0a
    DT_SHARED = 0xff # placeholder for shared datatypes
end
Base.convert(::Type{UInt8}, l::DatatypeClass) = UInt8(l)

jlwrite(io::IO, dt::DatatypeClass) = jlwrite(io, UInt8(dt) | UInt8(3) << 4)
Base.:(==)(dt::DatatypeClass, x::Integer) = UInt8(dt) == x%16
Base.:(==)(x::Integer, dt::DatatypeClass) = UInt8(dt) == x%16

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
    BasicDatatype(UInt8(DT_STRING) | 0x3<<4, 0x11, 0x00, 0x00, size)
OpaqueDatatype(size::Integer) =
    BasicDatatype(UInt8(DT_OPAQUE) | 0x3<<4, 0x00, 0x00, 0x00, size) # XXX make sure ignoring the tag is OK
ReferenceDatatype() =
    BasicDatatype(UInt8(DT_REFERENCE) | 0x3<<4, 0x00, 0x00, 0x00, jlsizeof(RelOffset))

function Base.:(==)(dt1::BasicDatatype, dt2::BasicDatatype)
    ret = true
    ret &= (dt1.class << 4) == (dt2.class << 4)
    ret &= dt1.bitfield1 == dt2.bitfield1
    ret &= dt1.bitfield2 == dt2.bitfield2
    ret &= dt1.bitfield3 == dt2.bitfield3
    ret &= dt1.size == dt2.size
    ret
end

function jlread(io::IO, ::Type{H5Datatype})
    dtclass = jlread(io, UInt8) % 16
    seek(io, position(io)-1)
    if dtclass == DT_FIXED_POINT
        jlread(io, FixedPointDatatype)
    elseif dtclass == DT_FLOATING_POINT
        jlread(io, FloatingPointDatatype)
    elseif dtclass in (DT_STRING, DT_OPAQUE, DT_REFERENCE)
        jlread(io, BasicDatatype)
    elseif dtclass == DT_COMPOUND
        jlread(io, CompoundDatatype)
    elseif dtclass == DT_VARIABLE_LENGTH
        jlread(io, VariableLengthDatatype)
    elseif dtclass == DT_BITFIELD
        jlread(io, BitFieldDatatype)
    elseif dtclass == DT_TIME
        throw(UnsupportedFeatureException("Time datatype (rarely used) not supported"))
    elseif dtclass == DT_ARRAY
        jlread(io, ArrayDatatype)
    elseif dtclass == DT_ENUMERATED
        jlread(io, EnumerationDatatype)
    else
        throw(UnsupportedFeatureException("invalid datatype class $dtclass"))
    end
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
    FixedPointDatatype(UInt8(DT_FIXED_POINT) | 3<<4, ifelse(signed, 0x08, 0x00), 0x00, 0x00, size, 0, 8*size)

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
    BitFieldDatatype(UInt8(DT_BITFIELD) | 0x3<<4, 0x00, 0x00, 0x00, size, 0, 8*size)


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
    field_datatypes = OrderedDict{String, RelOffset}(string.(dt.names) .=> fill(NULL_REFERENCE, length(dt.names)))
    odr = reconstruct_odr(f, dt, field_datatypes)
    T = NamedTuple{tuple(dt.names...), odr.parameters[2]}
    return ChangedLayout{T, odr}()    
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
    jlwrite(io, BasicDatatype(UInt8(DT_COMPOUND) | 0x3<<4, n % UInt8, (n >> 8) % UInt8, 0x00, dt.size))
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
    nfields = UInt16(dt.bitfield1) | UInt16(dt.bitfield2) << 8
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
        members[i] = jlread(io, H5Datatype)
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
    VariableLengthDatatype{typeof(basetype)}(UInt8(DT_VARIABLE_LENGTH) | 0x3<<4, 0x00, 0x00, 0x00, 8+jlsizeof(RelOffset), basetype)
VariableLengthDatatype(class, bitfield1, bitfield2, bitfield3, size, basetype::H5Datatype) =
    VariableLengthDatatype{typeof(basetype)}(class, bitfield1, bitfield2, bitfield3, size, basetype)

Base.:(==)(x::VariableLengthDatatype, y::VariableLengthDatatype) =
    x.class<<4 == y.class<<4 && x.bitfield1 == y.bitfield1 &&
    x.bitfield2 == y.bitfield2 && x.size == y.size &&
    x.basetype == y.basetype
Base.hash(::VariableLengthDatatype) = throw(ArgumentError("hash not defined for CompoundDatatype"))

class(dt::VariableLengthDatatype) = dt.class
jlsizeof(dt::VariableLengthDatatype) =
    jlsizeof(BasicDatatype) + jlsizeof(dt.basetype)

function jlwrite(io::IO, dt::VariableLengthDatatype)
    jlwrite(io, BasicDatatype(UInt8(DT_VARIABLE_LENGTH) | 0x3<<4, dt.bitfield1, dt.bitfield2, dt.bitfield3, dt.size))
    jlwrite(io, dt.basetype)
end

function jlread(io::IO, ::Type{VariableLengthDatatype})
    dtype = jlread(io, BasicDatatype)
    dt = jlread(io, H5Datatype)
    VariableLengthDatatype(dtype.class, dtype.bitfield1, dtype.bitfield2, dtype.bitfield3, dtype.size, dt)
end

jlsizeof(::CommittedDatatype) = 2 + jlsizeof(RelOffset)

function jlwrite(io::IO, dt::CommittedDatatype)
    jlwrite(io, UInt8(3))
    jlwrite(io, UInt8(2))
    jlwrite(io, dt.header_offset)
end

@nospecializeinfer function commit(f::JLDFile,
        @nospecialize(dt::H5Datatype), 
        attrs::Tuple{Vararg{WrittenAttribute}}=())
    psz = jlsizeof(Val(HmDatatype), 64; dt)
    psz += jlsizeof(HeaderMessage) * (length(attrs))
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
    write_header_message(cio, Val(HmDatatype), 64;  dt)
    for attr in attrs
        write_header_message(cio, f, attr)
    end
    jlwrite(io, end_checksum(cio))
end

# Read the actual datatype for a committed datatype
function read_shared_datatype(f::JLDFile, cdt::Union{SharedDatatype, CommittedDatatype})
    datatype::H5Datatype = PlaceholderH5Datatype()
    attrs = ReadAttribute[]

    for msg in HeaderMessageIterator(f, cdt.header_offset)
        if msg.type == HmDatatype
            datatype = HmWrap(HmDatatype, msg).dt
        elseif msg.type == HmAttribute
            push!(attrs, read_attribute(f, msg))
        elseif (msg.hflags & 2^3) != 0
            throw(UnsupportedFeatureException())
        end
    end
    if datatype isa PlaceholderH5Datatype
        throw(InvalidDataException("Did not find datatype message"))
    end
    return datatype, attrs
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
    base_type = jlread(io, H5Datatype)
    ArrayDatatype(dt.class, 0x0, 0x0, 0x0, dimensionality, dims, base_type)
end

struct ArrayPlaceHolder{T, D} end

odr_sizeof(::Type{ArrayPlaceHolder{T,D}}) where {T,D} = Int(odr_sizeof(T)*prod(D))

function jltype(f::JLDFile, dt::ArrayDatatype)
    rr = jltype(f, dt.base_type)
    T = julia_type(rr)
    ChangedLayout{Array{T, Int(dt.dimensionality)}, ArrayPlaceHolder{rr, tuple(dt.dims...)}}()
end


function jlconvert(::ChangedLayout{Array{T,D}, ArrayPlaceHolder{RR, DIMS}}, f::JLDFile, ptr::Ptr, 
                    header_offset::RelOffset) where {T, D, RR, DIMS}
    v = Array{T, D}(undef, reverse(DIMS)...)
    for i=1:prod(DIMS)
        v[i] = jlconvert(RR, f, ptr, header_offset)
        ptr += jlsizeof(file_repr(RR))
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
    SameLayout{dt.base_type}()
end

# Can't read big endian ints 
