
#
# Dataspaces
#

const DS_SCALAR = 0x00
const DS_SIMPLE = 0x01
const DS_NULL = 0x02
const DS_V1 = 0xff

struct WriteDataspace{N,A<:Tuple}
    dataspace_type::UInt8
    size::NTuple{N,Length}
    attributes::A
end

struct ReadDataspace
    dataspace_type::UInt8
    dimensionality::UInt8
    dimensions_offset::Int64
end
ReadDataspace() = ReadDataspace(DS_SCALAR, 0, -1)

ReadDataspace(f, msg_::Union{Hmessage, Message}) = 
    ReadDataspace(f,  HmWrap(HmDataspace, msg_))
ReadDataspace(f, msg::HmWrap{HmDataspace}) =
    ReadDataspace(msg.dataspace_type, msg.dimensionality, fileoffset(f, msg.dim_offset))

struct DataspaceStart
    version::UInt8
    dimensionality::UInt8
    flags::UInt8
    dataspace_type::UInt8
end
define_packed(DataspaceStart)

const EMPTY_DIMENSIONS = Int64[]

# Pass-through for custom serializations
# Need a bunch of methods to avoid ambiguity
WriteDataspace(f::JLDFile, x, ::Type{CustomSerialization{S,ODR}}) where {S,ODR} =
    WriteDataspace(f, x, ODR)
WriteDataspace(f::JLDFile, x::Array, ::Type{CustomSerialization{S,ODR}}) where {S,ODR} =
    WriteDataspace(f, x, ODR)
WriteDataspace(f::JLDFile, x::Array{T,0}, ::Type{CustomSerialization{S,ODR}}) where {T,S,ODR} =
    WriteDataspace(f, x, ODR)

WriteDataspace() = WriteDataspace(DS_NULL, (), ())
WriteDataspace(::JLDFile, ::Any, odr::Nothing) = WriteDataspace()
WriteDataspace(::JLDFile, ::Any, ::Any) = WriteDataspace(DS_SCALAR, (), ())

# Ghost type array
WriteDataspace(f::JLDFile, x::Array{T}, ::Nothing) where {T} =
   WriteDataspace(DS_NULL, (),
             (WrittenAttribute(f, :dimensions, collect(Int64, reverse(size(x)))),))

# Reference array
WriteDataspace(f::JLDFile, x::Array{T,N}, ::Type{RelOffset}) where {T,N} =
    WriteDataspace(DS_SIMPLE, convert(Tuple{Vararg{Length}}, reverse(size(x))),
              (WrittenAttribute(f, :julia_type, write_ref(f, T, f.datatype_wsession)),))

# isbitstype array
WriteDataspace(f::JLDFile, x::Array, ::Any) =
    WriteDataspace(DS_SIMPLE, convert(Tuple{Vararg{Length}}, reverse(size(x))), ())

# Zero-dimensional arrays need an empty dimensions attribute
WriteDataspace(f::JLDFile, x::Array{T,0}, ::Nothing) where {T} =
    WriteDataspace(DS_NULL, (Length(1),),
              (WrittenAttribute(f, :dimensions, EMPTY_DIMENSIONS)))
WriteDataspace(f::JLDFile, x::Array{T,0}, ::Type{RelOffset}) where {T} =
    WriteDataspace(DS_SIMPLE, (Length(1),),
              (WrittenAttribute(f, :julia_type, write_ref(f, T, f.datatype_wsession)),
               WrittenAttribute(f, :dimensions, EMPTY_DIMENSIONS)))
WriteDataspace(f::JLDFile, x::Array{T,0}, ::Any) where {T} =
    WriteDataspace(DS_SIMPLE, (Length(1),),
              (WrittenAttribute(f, :dimensions, EMPTY_DIMENSIONS),))

@static if VERSION >= v"1.11"
    # Memory is basically a vector but needs an additional attribute
    # Ghost type array
    WriteDataspace(f::JLDFile, x::Memory{T}, ::Nothing) where {T} =
    WriteDataspace(DS_NULL, (),
                (WrittenAttribute(f, :dimensions, collect(Int64, reverse(size(x)))),
                WrittenAttribute(f, :Memory, true)))

    # Reference array
    WriteDataspace(f::JLDFile, x::Memory{T}, ::Type{RelOffset}) where {T} =
        WriteDataspace(DS_SIMPLE, convert(Tuple{Vararg{Length}}, reverse(size(x))),
                (WrittenAttribute(f, :julia_type, write_ref(f, T, f.datatype_wsession)),
                WrittenAttribute(f, :Memory, true)))

    # isbitstype array
    WriteDataspace(f::JLDFile, x::Memory, ::Any) =
        WriteDataspace(DS_SIMPLE, convert(Tuple{Vararg{Length}}, reverse(size(x))),
        (WrittenAttribute(f, :Memory, true),))
end


jlsizeof(::Union{WriteDataspace{N},Type{WriteDataspace{N}}}) where {N} = 4 + jlsizeof(Length)*N
numel(x::WriteDataspace{0}) = x.dataspace_type == DS_SCALAR ? 1 : 0
numel(x::WriteDataspace) = Int(prod(x.size))

function jlwrite(io::IO, dspace::WriteDataspace{N}) where N
    jlwrite(io, DataspaceStart(2, N, 0, dspace.dataspace_type))
    for x in dspace.size
        jlwrite(io, x::Length)
    end
end