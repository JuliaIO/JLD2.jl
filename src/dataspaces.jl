
#
# Dataspaces
#

const DS_SCALAR = 0x00
const DS_SIMPLE = 0x01
const DS_NULL = 0x02

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

struct DataspaceStart
    version::UInt8
    dimensionality::UInt8
    flags::UInt8
    dataspace_type::UInt8
end
define_packed(DataspaceStart)

const EMPTY_DIMENSIONS = Int[]

# Pass-through for custom serializations
# Need a bunch of methods to avoid ambiguity
WriteDataspace{S,ODR}(f::JLDFile, x, ::Type{CustomSerialization{S,ODR}}) =
    WriteDataspace(f, x, ODR)
WriteDataspace{S,ODR}(f::JLDFile, x::Array, ::Type{CustomSerialization{S,ODR}}) =
    WriteDataspace(f, x, ODR)
WriteDataspace{T,S,ODR}(f::JLDFile, x::Array{T,0}, ::Type{CustomSerialization{S,ODR}}) =
    WriteDataspace(f, x, ODR)

WriteDataspace() = WriteDataspace(DS_NULL, (), ())
WriteDataspace(::JLDFile, ::Any, odr::Void) = WriteDataspace()
WriteDataspace(::JLDFile, ::Any, ::Any) = WriteDataspace(DS_SCALAR, (), ())

# Ghost type array
WriteDataspace{T}(f::JLDFile, x::Array{T}, ::Void) =
   WriteDataspace(DS_NULL, (),
             (WrittenAttribute(f, :dimensions, Int64[x for x in reverse(size(x))]),))

# Reference array
WriteDataspace{T,N}(f::JLDFile, x::Array{T,N}, ::Type{RelOffset}) =
    WriteDataspace(DS_SIMPLE, convert(Tuple{Vararg{Length}}, reverse(size(x))),
              (WrittenAttribute(f, :julia_type, write_ref(f, T, f.datatype_wsession)),))

# isbits array
WriteDataspace(f::JLDFile, x::Array, ::Any) =
    WriteDataspace(DS_SIMPLE, convert(Tuple{Vararg{Length}}, reverse(size(x))), ())

# Zero-dimensional arrays need an empty dimensions attribute
WriteDataspace{T}(f::JLDFile, x::Array{T,0}, ::Void) =
    WriteDataspace(DS_NULL, (Length(1),),
              (WrittenAttribute(f, :dimensions, EMPTY_DIMENSIONS)))
WriteDataspace{T}(f::JLDFile, x::Array{T,0}, ::Type{RelOffset}) =
    WriteDataspace(DS_SIMPLE, (Length(1),),
              (WrittenAttribute(f, :julia_type, write_ref(f, T, f.datatype_wsession)),
               WrittenAttribute(f, :dimensions, EMPTY_DIMENSIONS)))
WriteDataspace{T}(f::JLDFile, x::Array{T,0}, ::Any) =
    WriteDataspace(DS_SIMPLE, (Length(1),),
              (WrittenAttribute(f, :dimensions, EMPTY_DIMENSIONS),))

sizeof{N}(::Union{WriteDataspace{N},Type{WriteDataspace{N}}}) = 4 + sizeof(Length)*N
numel(x::WriteDataspace{0}) = x.dataspace_type == DS_SCALAR ? 1 : 0
numel(x::WriteDataspace) = Int(prod(x.size))

function Base.write{N}(io::IO, dspace::WriteDataspace{N})
    write(io, DataspaceStart(2, N, 0, dspace.dataspace_type))
    for x in dspace.size
        write(io, x::Length)
    end
end

# Reads a dataspace from the file and returns a
# (dataspace_type::UInt8, dataspace_dimensions:G:Vector{Length})
# tuple, where dataspace_type is one of the DS_* constants and
# dataspace_dimensions are the corresponding dimensions
function read_dataspace_message(io::IO)
    dspace_start = read(io, DataspaceStart)
    dspace_start.version == 2 || throw(UnsupportedVersionException())
    dataspace_type = dspace_start.dataspace_type
    ReadDataspace(dataspace_type, dspace_start.dimensionality, position(io))
end
