
#
# Dataspaces
#

const DS_SCALAR = 0x00
const DS_SIMPLE = 0x01
const DS_NULL = 0x02

immutable WriteDataspace{N,A<:Tuple}
    dataspace_type::UInt8
    size::NTuple{N,Length}
    attributes::A
end

immutable ReadDataspace
    dataspace_type::UInt8
    dimensionality::Int
    dimensions_offset::Int
end
ReadDataspace() = ReadDataspace(DS_SCALAR, 0, -1)

immutable DataspaceStart
    version::UInt8
    dimensionality::UInt8
    flags::UInt8
    dataspace_type::UInt8
end
define_packed(DataspaceStart)

const EMPTY_DIMENSIONS = Int[]

WriteDataspace() = WriteDataspace(DS_NULL, (), ())
WriteDataspace(::JLDFile, ::Any, odr::Void) = WriteDataspace()
WriteDataspace(::JLDFile, ::Any, ::Any) = WriteDataspace(DS_SCALAR, (), ())

nulldataspace{T}(f::JLDFile, x::Array{T}, ::Union(Void, Type{Reference})) =
    WriteDataspace(DS_NULL, (),
              (WrittenAttribute(f, :dimensions, Int[x for x in reverse(size(x))]),
               WrittenAttribute(f, :julia_type, T)))
nulldataspace(f::JLDFile, x::Array, ::Any) =
    WriteDataspace(DS_NULL, (),
              (WrittenAttribute(f, :dimensions, Int[x for x in reverse(size(x))]),))

WriteDataspace(f::JLDFile, x::Array, ::Void) = nulldataspace(f, x, nothing)
WriteDataspace{T,N}(f::JLDFile, x::Array{T,N}, ::Type{Reference}) =
    WriteDataspace(DS_SIMPLE, convert(Tuple{Vararg{Length}}, reverse(size(x))),
              (WrittenAttribute(f, :julia_type, T),))
WriteDataspace(f::JLDFile, x::Array, ::Any) =
    WriteDataspace(DS_SIMPLE, convert(Tuple{Vararg{Length}}, reverse(size(x))), ())

sizeof{N}(::Union(WriteDataspace{N},Type{WriteDataspace{N}})) = 4 + sizeof(Length)*N
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
