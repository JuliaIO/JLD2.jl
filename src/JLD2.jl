module JLD2
using DataStructures, Libz
import Base.sizeof
export jldopen

const OBJECT_HEADER_SIGNATURE = htol(0x5244484f) # "OHDR"

# Currently we specify that all offsets and lengths are 8 bytes
const Length = UInt64

# Currently we specify a 512 byte header
const FILE_HEADER_LENGTH = 512
const FILE_HEADER = "Julia data file (HDF5), version "
const CURRENT_VERSION = v"0.2"

struct UnsupportedVersionException <: Exception end
struct UnsupportedFeatureException <: Exception end
struct InvalidDataException <: Exception end

include("Lookup3.jl")
include("mmapio.jl")
include("misc.jl")

"""
    RelOffset

Represents an HDF5 relative offset. This differs from a file offset (used elsewhere) in
that it is relative to the superblock base address. In practice, this means that
`FILE_HEADER_LENGTH `has been subtracted. `fileoffset` and `h5offset` convert between
`RelOffsets` and file offsets.
"""
struct RelOffset
    offset::UInt64
end
define_packed(RelOffset)
Base.:(==)(x::RelOffset, y::RelOffset) = x === y
Base.hash(x::RelOffset) = hash(x.offset)

const UNDEFINED_ADDRESS = RelOffset(0xffffffffffffffff)
const NULL_REFERENCE = RelOffset(0)

"""
    JLDWriteSession{T}

A JLDWriteSession keeps track of references to serialized objects. If `T` is a Dict,
`h5offset` maps an object ID (returned by calling `object_id`) to th `RelOffset` of the
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
    H5Datatype

Supertype of all HDF5 datatypes.
"""
abstract type H5Datatype end

struct CommittedDatatype <: H5Datatype
    header_offset::RelOffset
    index::Int
end

"""
    ReadRepresentation{T,ODR}

A type encoding both the Julia type `T` and the on-disk (HDF5) representation `ODR`.
"""
struct ReadRepresentation{T,ODR} end

"""
    CustomSerialization{T,S}

On-disk representation for data that is written as if it were of Julia type `T`, but is
read as type `S`.
"""
struct CustomSerialization{T,S} end

"""
    Group{T}

JLD group object.
"""
mutable struct Group{T}
    f::T
    last_chunk_start_offset::Int64
    continuation_message_goes_here::Int64
    last_chunk_checksum_offset::Int64
    unwritten_links::OrderedDict{String,RelOffset}
    unwritten_child_groups::OrderedDict{String,Group}
    written_links::OrderedDict{String,RelOffset}

    Group{T}(f) where T =
        new(f, -1, -1, -1, OrderedDict{String,RelOffset}(), OrderedDict{String,Group}())

    Group{T}(f, last_chunk_start_offset, continuation_message_goes_here,
             last_chunk_checksum_offset, unwritten_links, unwritten_child_groups,
             written_links) where T =
        new(f, last_chunk_start_offset, continuation_message_goes_here,
            last_chunk_checksum_offset, unwritten_links, unwritten_child_groups,
            written_links)
end

"""
    JLDFile{T<:IO}

JLD file object.
"""
mutable struct JLDFile{T<:IO}
    io::T
    path::String
    writable::Bool
    written::Bool
    created::Bool
    compress::Bool
    mmaparrays::Bool
    datatype_locations::OrderedDict{RelOffset,CommittedDatatype}
    datatypes::Vector{H5Datatype}
    datatype_wsession::JLDWriteSession{Dict{UInt,RelOffset}}
    jlh5type::ObjectIdDict
    h5jltype::ObjectIdDict
    jloffset::Dict{RelOffset,WeakRef}
    end_of_data::Int64
    global_heaps::Dict{RelOffset,GlobalHeap}
    global_heap::GlobalHeap
    loaded_groups::Dict{RelOffset,Group}
    root_group_offset::RelOffset
    root_group::Group
    types_group::Group

    function JLDFile{T}(io::IO, path::AbstractString, writable::Bool, written::Bool,
                        created::Bool, compress::Bool, mmaparrays::Bool) where T
        f = new(io, path, writable, written, created, compress, mmaparrays,
            OrderedDict{RelOffset,CommittedDatatype}(), H5Datatype[],
            JLDWriteSession(), ObjectIdDict(), ObjectIdDict(), Dict{RelOffset,WeakRef}(),
            Int64(FILE_HEADER_LENGTH + sizeof(Superblock)), Dict{RelOffset,GlobalHeap}(),
            GlobalHeap(0, 0, 0, Int64[]), Dict{RelOffset,Group}(), UNDEFINED_ADDRESS)
        finalizer(f, safe_close)
        f
    end
end
JLDFile(io::IO, path::AbstractString, writable::Bool, written::Bool, created::Bool,
        compress::Bool, mmaparrays::Bool) =
    JLDFile{typeof(io)}(io, path, writable, written, created, compress, mmaparrays)

"""
    fileoffset(f::JLDFile, x::RelOffset)

Converts an offset `x` relative to the superblock of file `f` to an absolute offset.
"""
fileoffset(f::JLDFile, x::RelOffset) = Int64(x.offset + FILE_HEADER_LENGTH)

"""
    h5offset(f::JLDFile, x::RelOffset)

Converts an absolute file offset `x` to an offset relative to the superblock of file `f`.
"""
h5offset(f::JLDFile, x::Int64) = RelOffset(x - FILE_HEADER_LENGTH)

#
# File
#

function jldopen(fname::AbstractString, wr::Bool, create::Bool, truncate::Bool;
                 compress::Bool=false, mmaparrays::Bool=false)
    path = realpath(fname)
    exists = isfile(path)
    io = MmapIO(path, wr, create, truncate)
    created = !exists || truncate
    f = JLDFile(io, path, wr, truncate, created, compress, mmaparrays)

    if !truncate
        if String(read(io, UInt8, length(FILE_HEADER))) != FILE_HEADER
            throw(ArgumentError(string('"', fname, "\" is not a JLD file")))
        end

        ver = convert(VersionNumber, read_bytestring(io))
        if ver < v"0.2"
            throw(ArgumentError("only JLD2 files are presently supported"))
        elseif ver > CURRENT_VERSION
            warn('"', fname, "\" was written in JLD file format version ", ver,
                 ", but this version of JLD supports only JLD file format ", CURRENT_VERSION,
                 ". Some or all data in the file may not be readable")
        end
    end

    if wr
        seek(io, 0)
        # Write JLD header
        write(io, FILE_HEADER)
        print(io, CURRENT_VERSION)
    end

    if created
        f.root_group = Group{typeof(f)}(f)
        f.types_group = Group{typeof(f)}(f)
    else
        seek(io, FILE_HEADER_LENGTH)
        superblock = read(io, Superblock)
        f.end_of_data = superblock.end_of_file_address
        f.root_group_offset = superblock.root_group_object_header_address
        f.root_group = load_group(f, superblock.root_group_object_header_address)

        if haskey(f.root_group.written_links, "_types")
            types_group_offset = f.root_group.written_links["_types"]
            f.types_group = f.loaded_groups[types_group_offset] = load_group(f, types_group_offset)
            i = 0
            for offset in values(f.types_group.written_links)
                f.datatype_locations[offset] = CommittedDatatype(offset, i += 1)
            end
            resize!(f.datatypes, length(f.datatype_locations))
        else
            f.types_group = Group{typeof(f)}(f)
        end
    end

    f
end

"""
    jldopen(fname::AbstractString, mode::AbstractString)

Opens a JLD file at path `fname`.

`"r"`: Open for reading only, failing if no file exists
`"r+"`: Open for reading and writing, failing if no file exists
`"w"`/`"w+"`: Open for reading and writing, overwriting the file if it already exists
`"a"`/`"a+"`: Open for reading and writing, creating a new file if none exists, but
              preserving the existing file if one is present
"""
function jldopen(fname::AbstractString, mode::AbstractString="r"; kwargs...)
    (wr, create, truncate) = mode == "r"  ? (false, false, false) :
                             mode == "r+" ? (true, false, false) :
                             mode == "a" || mode == "a+" ? (true, true, false) :
                             mode == "w" || mode == "w+" ? (true, true, true) :
                             throw(ArgumentError("invalid open mode: $mode"))
    jldopen(fname, wr, create, truncate; kwargs...)
end

"""
    load_datatypes(f::JLDFile)

Populate f.datatypes and f.jlh5types with all of the committed datatypes from a file. We
need to do this before writing to make sure we reuse written datatypes.
"""
function load_datatypes(f::JLDFile)
    dts = f.datatypes
    cdts = f.datatype_locations
    @assert length(dts) == length(cdts)
    i = 1
    for cdt in values(cdts)
        !isassigned(dts, i) && jltype(f, cdt)
        i += 1
    end
end

"""
    prewrite(f::JLDFile)

Check that a JLD file is actually writable, and throw an error if not. Sets the `written`
flag on the file.
"""
function prewrite(f::JLDFile)
    f.end_of_data == 0 && throw(ArgumentError("file is closed"))
    !f.writable && throw(ArgumentError("file was opened read-only"))
    !f.written && !f.created && load_datatypes(f)
    f.written = true
end

Base.read(f::JLDFile, name::AbstractString) = f.root_group[name]
Base.write(f::JLDFile, name::AbstractString, obj, wsession::JLDWriteSession=JLDWriteSession()) =
    write(f.root_group, name, obj, wsession)

function Base.close(f::JLDFile)
    f.end_of_data == 0 && return
    io = f.io
    if f.written
        # Save any groups we know of that have been modified
        for group in values(f.loaded_groups)
            save_group(group)
        end
        if !isempty(f.types_group) && !haskey(f.root_group, "_types")
            f.root_group["_types"] = f.types_group
        end
        res = save_group(f.root_group)
        if f.root_group_offset == UNDEFINED_ADDRESS
            f.root_group_offset = res
        end

        eof_position = f.end_of_data
        truncate(io, eof_position)
        seek(io, FILE_HEADER_LENGTH)
        write(io, Superblock(0, FILE_HEADER_LENGTH, UNDEFINED_ADDRESS,
              eof_position, f.root_group_offset))
    end
    f.end_of_data = 0
    close(io)
    nothing
end

"""
    safe_close(f::JLDFile)

When a JLDFile is finalized, it is possible that the `MmapIO` has been munmapped, since
Julia does not guarantee finalizer order. This means that we can't write to the `MmapIO` or
Julia will segfault. We avoid the segfault by first finalizing the underlying parts of the
`MmapIO`, and then constructing a new mapping and writing to it.
"""
function safe_close(f::JLDFile{MmapIO})
    f.end_of_data == 0 && return
    if f.written
        finalize(f.io.f)
        finalize(f.io.arr)
        f.io = MmapIO(f.path, true, false, false)
    end
    close(f)
end

function Base.show(io::IO, f::JLDFile)
    println(io, "JLDFile $(f.path) ", f.writable ? "(read/write)" : "(read-only)")
    show_group(io, f.root_group)
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
