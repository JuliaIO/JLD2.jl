module JLD2
using ArrayViews, DataStructures
import Base.sizeof
export jldopen

const SUPERBLOCK_SIGNATURE = reinterpret(UInt64, UInt8[0o211, 'H', 'D', 'F', '\r', '\n', 0o032, '\n'])[1]
const OBJECT_HEADER_SIGNATURE = reinterpret(UInt32, UInt8['O', 'H', 'D', 'R'])[1]
const GLOBAL_HEAP_SIGNATURE = reinterpret(UInt32, UInt8['G', 'C', 'O', 'L'])[1]

# Currently we specify that all offsets and lengths are 8 bytes
typealias Length UInt64

# Currently we specify a 512 byte header
const FILE_HEADER_LENGTH = 512

immutable UnsupportedVersionException <: Exception end
immutable UnsupportedFeatureException <: Exception end
immutable InvalidDataException <: Exception end

typealias Plain     Union(Int8,Int16,Int32,Int64,Int128,UInt8,UInt16,UInt32,UInt64,UInt128,
                          Float16,Float32,Float64)
typealias PlainType Union(Type{Int8},Type{Int16},Type{Int32},Type{Int64},Type{Int128},
                          Type{UInt8},Type{UInt16},Type{UInt32},Type{UInt64},Type{UInt128},
                          Type{Float16},Type{Float32},Type{Float64})

include("Lookup3.jl")
include("mmapio.jl")
include("misc.jl")

# Offset represents an HDF5 relative offset. It differs from FileOffset (used
# elsewhere) in that it is relative to the superblock base address. In practice,
# this means that FILE_HEADER_LENGTH has been subtracted. `fileoffset` and
# `h5offset` convert between Offsets and FileOffsets
immutable Offset
    offset::UInt64
end
define_packed(Offset)
const UNDEFINED_ADDRESS = Offset(0xffffffffffffffff)

immutable JLDWriteSession{T<:Union(Dict{UInt,Offset},None)}
    h5offset::T
    objects::Vector{Any}

    JLDWriteSession() = new()
    JLDWriteSession(h5offset, objects) = new(h5offset, objects)
end
JLDWriteSession() = JLDWriteSession{Dict{UInt,Offset}}(Dict{UInt,Offset}(), Any[])

immutable Reference
    offset::Offset
end
define_packed(Reference)
const NULL_REFERENCE = Reference(Offset(0))

type GlobalHeap
    offset::FileOffset
    length::Length
    free::Length
    objects::Vector{FileOffset}
end

abstract H5Datatype

immutable CommittedDatatype <: H5Datatype
    header_offset::Offset
    index::Int
end

immutable OnDiskRepresentation{Offsets,Types,ODRs} end
immutable ReadRepresentation{T,ODR} end

symbol_length(x::Symbol) = ccall(:strlen, Int, (Cstring,), x)

type JLDFile{T<:IO}
    io::T
    writable::Bool
    written::Bool
    datatype_locations::OrderedDict{Offset,CommittedDatatype}
    datatypes::Vector{H5Datatype}
    datatype_wsession::JLDWriteSession
    datasets::OrderedDict{ByteString,Offset}
    jlh5type::Dict{Type,CommittedDatatype}
    h5jltype::ObjectIdDict
    jloffset::Dict{Offset,WeakRef}
    end_of_data::FileOffset
    global_heaps::Dict{Offset,GlobalHeap}
    global_heap::GlobalHeap
end
JLDFile(io::IO, writable::Bool, written::Bool) =
    JLDFile(io, writable, written, OrderedDict{Offset,CommittedDatatype}(), H5Datatype[],
            JLDWriteSession(), OrderedDict{ByteString,Offset}(), Dict{Type,CommittedDatatype}(),
            ObjectIdDict(), Dict{Offset,WeakRef}(),
            FileOffset(FILE_HEADER_LENGTH + sizeof(Superblock)), Dict{Offset,GlobalHeap}(),
            GlobalHeap(0, 0, 0, FileOffset[]))

fileoffset(f::JLDFile, x::Offset) = FileOffset(x.offset + FILE_HEADER_LENGTH)
h5offset(f::JLDFile, x::FileOffset) = Offset(x - FILE_HEADER_LENGTH)

#
# File
#

function jldopen(fname::AbstractString, write::Bool, create::Bool, truncate::Bool)
    io = MmapIO(fname, write, create, truncate)
    f = JLDFile(io, write, truncate)

    if !truncate
        seek(io, FILE_HEADER_LENGTH)
        superblock = read(io, Superblock)
        f.end_of_data = superblock.end_of_file_address
        seek(io, fileoffset(f, superblock.root_group_object_header_address))
        root_group = read(io, Group)
        for i = 1:length(root_group.names)
            name = root_group.names[i]
            offset = root_group.offsets[i]
            if name == "_types"
                seek(io, fileoffset(f, offset))
                types_group = read(io, Group)
                for i = 1:length(types_group.offsets)
                    f.datatype_locations[types_group.offsets[i]] = CommittedDatatype(types_group.offsets[i], i)
                end
                resize!(f.datatypes, length(types_group.offsets))
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
    f.end_of_data == 0 && throw(ArgumentError("file is closed"))
    haskey(f.datasets, name) || throw(ArgumentError("file has no dataset $name"))
    read_dataset(f, f.datasets[name])
end

function Base.write(f::JLDFile, name::String, obj, wsession::JLDWriteSession=JLDWriteSession())
    f.end_of_data == 0 && throw(ArgumentError("file is closed"))
    !f.writable && throw(ArgumentError("file was opened read-only"))
    f.written = true

    io = f.io
    seek(io, f.end_of_data)
    header_offset = write_dataset(f, obj, wsession)
    f.datasets[name] = header_offset
    nothing
end

function Base.close(f::JLDFile)
    io = f.io
    if f.written
        seek(io, f.end_of_data)

        names = ByteString[]
        sizehint!(names, length(f.datasets)+1)
        offsets = Offset[]
        sizehint!(offsets, length(f.datasets)+1)

        # Write types group
        if !isempty(f.datatypes)
            push!(names, "_types")
            push!(offsets, h5offset(f, position(io)))
            write(io, Group(ASCIIString[@sprintf("%08d", i) for i = 1:length(f.datatypes)],
                            collect(keys(f.datatype_locations))))
        end

        # Write root group
        root_group_object_header_address = h5offset(f, position(io))
        for (k, v) in f.datasets
            push!(names, k)
            push!(offsets, v)
        end
        write(io, Group(names, offsets))

        eof_position = position(io)
        truncate(io, eof_position)
        seek(io, FILE_HEADER_LENGTH)
        write(io, Superblock(0, FILE_HEADER_LENGTH, UNDEFINED_ADDRESS,
              eof_position, root_group_object_header_address))
    end
    f.end_of_data = 0
    close(io)
    nothing
end

include("superblock.jl")
include("object_headers.jl")
include("groups.jl")
include("dataspaces.jl")
include("attributes.jl")
include("datatypes.jl")
include("datasets.jl")
include("global_heaps.jl")
include("data.jl")

end # module
