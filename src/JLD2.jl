module JLD2
using OrderedCollections: OrderedDict
using Reexport
import Base.sizeof
using MacroTools
using Printf
using Mmap
using TranscodingStreams
@reexport using FileIO: load, save

export jldopen, @load, @save, save_object, load_object, printtoc
export jldsave

const OBJECT_HEADER_SIGNATURE = htol(0x5244484f) # "OHDR"

# Currently we specify that all offsets and lengths are 8 bytes
const Length = UInt64

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

# Due to custom overrides we do not use Base functions directly
# but define our own to avoid type piracy
jlwrite(io, x) = Base.write(io, x)
jlread(io, x) = Base.read(io, x)
jlsizeof(x) = Base.sizeof(x)
jlunsafe_store!(p, x) = Base.unsafe_store!(p, x)
jlunsafe_load(p) = Base.unsafe_load(p)

include("julia_compat.jl")
include("file_header.jl")
include("Lookup3.jl")
include("mmapio.jl")
include("bufferedio.jl")
include("misc.jl")
include("superblock.jl")

"""
    RelOffset

Represents an HDF5 relative offset. This differs from a file offset (used elsewhere) in
that it is relative to the superblock base address. `fileoffset` and `h5offset` convert between
`RelOffsets` and file offsets.
"""
struct RelOffset
    offset::UInt64
end
define_packed(RelOffset)
Base.:(==)(x::RelOffset, y::RelOffset) = x === y
Base.hash(x::RelOffset) = hash(x.offset)
Base.:(+)(x::RelOffset, y::Integer) = RelOffset(UInt64(x.offset + y))

const UNDEFINED_ADDRESS = RelOffset(0xffffffffffffffff)
const NULL_REFERENCE = RelOffset(0)

"""
    JLDWriteSession{T}

A JLDWriteSession keeps track of references to serialized objects. If `T` is a Dict,
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
    H5Datatype

Supertype of all HDF5 datatypes.
"""
abstract type H5Datatype end

"""
    SharedDatatype

Reference to a shared datatype message (stored elsewhere in a file).
"""
struct SharedDatatype <: H5Datatype
    header_offset::RelOffset
end

"""
    CommittedDatatype

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

"""
    Group(file)

JLD2 group object. 

## Advanced Usage
Takes two optional keyword arguments:
    est_num_entries::Int=4
    est_link_name_len::Int=8
These determine how much (additional) empty space should be allocated for the group description. (list of entries)
This can be useful for performance when one expects to append many additional datasets after first writing the file.
"""
mutable struct Group{T}
    f::T
    last_chunk_start_offset::Int64
    continuation_message_goes_here::Int64
    last_chunk_checksum_offset::Int64
    next_link_offset::Int64
    est_num_entries::Int
    est_link_name_len::Int
    unwritten_links::OrderedDict{String,RelOffset}
    unwritten_child_groups::OrderedDict{String,Group{T}}
    written_links::OrderedDict{String,RelOffset}

    Group{T}(f; est_num_entries::Int=4, est_link_name_len::Int=8) where T =
        new(f, -1, -1, -1, -1, est_num_entries, est_link_name_len,
        OrderedDict{String,RelOffset}(), OrderedDict{String,Group{T}}())

    Group{T}(f, last_chunk_start_offset, continuation_message_goes_here,
             last_chunk_checksum_offset, next_link_offset,
             est_num_entries, est_link_name_len,
             unwritten_links, unwritten_child_groups,
             written_links) where T =
        new(f, last_chunk_start_offset, continuation_message_goes_here,
            last_chunk_checksum_offset, next_link_offset,
            est_num_entries, est_link_name_len,
            unwritten_links, unwritten_child_groups,
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
    compress#::Union{Bool,Symbol}
    mmaparrays::Bool
    n_times_opened::Int
    datatype_locations::OrderedDict{RelOffset,CommittedDatatype}
    datatypes::Vector{H5Datatype}
    datatype_wsession::JLDWriteSession{Dict{UInt,RelOffset}}
    typemap::Dict{String, Any}
    jlh5type::IdDict{Any,Any}
    h5jltype::IdDict{Any,Any}
    jloffset::Dict{RelOffset,WeakRef}
    end_of_data::Int64
    global_heaps::Dict{RelOffset,GlobalHeap}
    global_heap::GlobalHeap
    loaded_groups::Dict{RelOffset,Group{JLDFile{T}}}
    root_group_offset::RelOffset
    root_group::Group{JLDFile{T}}
    types_group::Group{JLDFile{T}}
    base_address::UInt64

    function JLDFile{T}(io::IO, path::AbstractString, writable::Bool, written::Bool,
                        compress,#::Union{Bool,Symbol},
                        mmaparrays::Bool) where T
        f = new(io, path, writable, written, compress, mmaparrays, 1,
            OrderedDict{RelOffset,CommittedDatatype}(), H5Datatype[],
            JLDWriteSession(), Dict{String,Any}(), IdDict(), IdDict(), Dict{RelOffset,WeakRef}(),
            DATA_START, Dict{RelOffset,GlobalHeap}(),
            GlobalHeap(0, 0, 0, Int64[]), Dict{RelOffset,Group{JLDFile{T}}}(), UNDEFINED_ADDRESS)
        finalizer(jld_finalizer, f)
        f
    end
end
JLDFile(io::IO, path::AbstractString, writable::Bool, written::Bool, compress, mmaparrays::Bool) =
    JLDFile{typeof(io)}(io, path, writable, written, compress, mmaparrays)

"""
    fileoffset(f::JLDFile, x::RelOffset)

Converts an offset `x` relative to the superblock of file `f` to an absolute offset.
"""
fileoffset(f::JLDFile, x::RelOffset) = Int64(x.offset + f.base_address)

"""
    h5offset(f::JLDFile, x::Integer)

Converts an absolute file offset `x` to an offset relative to the superblock of file `f`.
"""
h5offset(f::JLDFile, x::Integer) = RelOffset(UInt64(x - f.base_address))

#
# File
#

openfile(::Type{IOStream}, fname, wr, create, truncate, fallback::Nothing = nothing) =
    open(fname, read = true, write = wr, create = create,
         truncate = truncate, append = false)
openfile(::Type{MmapIO}, fname, wr, create, truncate, fallback::Nothing = nothing) =
    MmapIO(fname, wr, create, truncate)

function openfile(T::Type, fname, wr, create, truncate, fallback::Type)
    try
         openfile(T, fname, wr, create, truncate, nothing)
    catch
        @warn "Opening file with $T failed, falling back to $fallback"
        openfile(fallback, fname, wr, create, truncate, nothing)
    end
end

# default fallback behaviour : MmapIO -> IOStream -> failure
FallbackType(::Type{MmapIO}) = IOStream
FallbackType(::Type{IOStream}) = nothing

# The delimiter is excluded by default
read_bytestring(io::IOStream) = String(readuntil(io, 0x00))

const OPEN_FILES = Dict{String,WeakRef}()
const OPEN_FILES_LOCK = ReentrantLock()
function jldopen(fname::AbstractString, wr::Bool, create::Bool, truncate::Bool, iotype::T=MmapIO;
                 fallback::Union{Type, Nothing} = FallbackType(iotype),
                 compress=false,
                 mmaparrays::Bool=false,
                 typemap::Dict{String}=Dict{String,Any}(),
                 ) where T<:Union{Type{IOStream},Type{MmapIO}}
    mmaparrays && @warn "mmaparrays keyword is currently ignored" maxlog=1
    verify_compressor(compress)
    exists = ispath(fname)

    lock(OPEN_FILES_LOCK)
    f = try
        if exists
            rname = realpath(fname)
            # catch existing file system entities that are not regular files
            if !isfile(rname)
                throw(ArgumentError("not a regular file: $fname"))
            end
            if haskey(OPEN_FILES, rname)
                ref = OPEN_FILES[rname]
                f = ref.value
                if !isnothing(f)
                    if truncate
                        throw(ArgumentError("attempted to truncate a file that was already open"))
                    elseif !isa(f, JLDFile{iotype})
                        throw(ArgumentError("attempted to open file with $iotype backend, but already open with a different backend"))
                    elseif f.writable != wr
                        current = wr ? "read/write" : "read-only"
                        previous = f.writable ? "read/write" : "read-only"
                        throw(ArgumentError("attempted to open file $(current), but file was already open $(previous)"))
                    elseif f.compress != compress
                        throw(ArgumentError("attempted to open file with compress=$(compress), but file was already open with compress=$(f.compress)"))
                    elseif f.mmaparrays != mmaparrays
                        throw(ArgumentError("attempted to open file with mmaparrays=$(mmaparrays), but file was already open with mmaparrays=$(f.mmaparrays)"))
                    end

                    f = f::JLDFile{iotype}
                    f.n_times_opened += 1
                    return f
                end
            end
        end

        io = openfile(iotype, fname, wr, create, truncate, fallback)
        created = !exists || truncate
        rname = realpath(fname)
        f = JLDFile(io, rname, wr, created, compress, mmaparrays)
        OPEN_FILES[rname] = WeakRef(f)
        f
    catch e
        rethrow(e)
    finally
        unlock(OPEN_FILES_LOCK)
    end
    if f.written
        f.base_address = 512
        if f isa JLDFile{MmapIO}
            f.root_group = Group{JLDFile{MmapIO}}(f)
            f.types_group =  Group{JLDFile{MmapIO}}(f)
        elseif f isa JLDFile{IOStream}
            f.root_group =  Group{JLDFile{IOStream}}(f)
            f.types_group = Group{JLDFile{IOStream}}(f)
        end
    else
        load_file_metadata!(f)
    end
    merge!(f.typemap, typemap)
    return f
end

function load_file_metadata!(f)
    superblock = find_superblock(f)
    f.end_of_data = superblock.end_of_file_address
    f.base_address = superblock.base_address
    f.root_group_offset = superblock.root_group_object_header_address
    if superblock.version >= 2
        verify_file_header(f)
    else
        @warn "This file was not written with JLD2. Some things may not work."
        if f.writable
            close(f)
            throw(UnsupportedVersionException("This file can not be edited by JLD2. Please open in read-only mode."))
        end
    end
    try
        f.root_group = load_group(f, f.root_group_offset)

    if haskey(f.root_group.written_links, "_types")
        types_group_offset = f.root_group.written_links["_types"]::RelOffset
        f.types_group = f.loaded_groups[types_group_offset] = load_group(f, types_group_offset)
        i = 0
        for (offset::RelOffset) in values(f.types_group.written_links)
            f.datatype_locations[offset] = CommittedDatatype(offset, i += 1)
        end
        resize!(f.datatypes, length(f.datatype_locations))
    else
        f.types_group = Group{typeof(f)}(f)
    end
    catch e
        show(e)
        f.types_group = Group{typeof(f)}(f)

    end
    nothing
end

"""
    jldopen(fname::AbstractString, mode::AbstractString; iotype=MmapIO, compress=false, typemap=Dict())

Opens a JLD2 file at path `fname`.

`"r"`: Open for reading only, failing if no file exists
`"r+"`: Open for reading and writing, failing if no file exists
`"w"`/`"w+"`: Open for reading and writing, overwriting the file if it already exists
`"a"`/`"a+"`: Open for reading and writing, creating a new file if none exists, but
              preserving the existing file if one is present
"""
function jldopen(fname::AbstractString, mode::AbstractString="r"; iotype=MmapIO, kwargs...)
    (wr, create, truncate) = mode == "r"  ? (false, false, false) :
                             mode == "r+" ? (true, false, false) :
                             mode == "a" || mode == "a+" ? (true, true, false) :
                             mode == "w" || mode == "w+" ? (true, true, true) :
                             throw(ArgumentError("invalid open mode: $mode"))
    jldopen(fname, wr, create, truncate, iotype; kwargs...)
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
    f.n_times_opened == 0 && throw(ArgumentError("file is closed"))
    !f.writable && throw(ArgumentError("file was opened read-only"))
    !f.written && load_datatypes(f)
    f.written = true
end

Base.read(f::JLDFile, name::AbstractString) = f.root_group[name]
#Base.write(f::JLDFile, name::AbstractString, obj, wsession::JLDWriteSession=JLDWriteSession()) =
#    write(f.root_group, name, obj, wsession)

Base.getindex(f::JLDFile, name::AbstractString) = f.root_group[name]
Base.setindex!(f::JLDFile, obj, name::AbstractString) = (f.root_group[name] = obj; f)
Base.haskey(f::JLDFile, name::AbstractString) = haskey(f.root_group, name)
Base.isempty(f::JLDFile) = isempty(f.root_group)
Base.keys(f::JLDFile) = filter!(x->x != "_types", keys(f.root_group))
Base.keytype(f::JLDFile) = String
Base.length(f::Union{JLDFile, Group}) = length(keys(f))

Base.get(default::Function, f::Union{JLDFile, Group}, name::AbstractString) =
    haskey(f, name) ? f[name] : default()
Base.get(f::Union{JLDFile, Group}, name::AbstractString, default) =
    haskey(f, name) ? f[name] : default
Base.get!(f::Union{JLDFile, Group}, name::AbstractString, default) =
    get!(() -> default, f, name)
function Base.get!(default::Function, f::Union{JLDFile, Group}, name::AbstractString)
    if haskey(f, name)
        return f[name]
    else
        default_value = default()
        f[name] = default_value
        return default_value
    end
end

function Base.close(f::JLDFile)
    if f.n_times_opened != 1
        f.n_times_opened == 0 && return
        f.n_times_opened -= 1
        return
    end

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

        write_file_header(f)
        truncate_and_close(io, f.end_of_data)
    else
        close(io)
    end
    lock(OPEN_FILES_LOCK) do
        delete!(OPEN_FILES, f.path)
        f.n_times_opened = 0
    end
    nothing
end

"""
    jld_finalizer(f::JLDFile)

When a JLDFile is finalized, it is possible that the `MmapIO` has been munmapped, since
Julia does not guarantee finalizer order. This means that the underlying file may be closed
before we get a chance to write to it.
"""
function jld_finalizer(f::JLDFile{MmapIO})
    f.n_times_opened == 0 && return
    if f.written && !isopen(f.io.f)
        f.io.f = open(f.path, "r+")
    end
    f.n_times_opened = 1
    close(f)
end

function jld_finalizer(f::JLDFile{IOStream})
    f.n_times_opened == 0 && return
    if f.written && !isopen(f.io)
        f.io = openfile(IOStream, f.path, true, false, false)
    end
    f.n_times_opened = 1
    close(f)
end

# Display functions

# simple one-line display (without trailing line break)
function Base.show(io::IO, f::JLDFile)
    print(io, "JLDFile $(f.path)")
    if get(io, :compact, false)
        return
    else
        print(io, f.writable ? " (read/write)" : " (read-only)")
    end
end

# fancy multi-line display (unless an IOContext requests compactness)
function Base.show(io::IO, ::MIME"text/plain", f::JLDFile)
    show(io, f)
    if !get(io, :compact, false)
        print(io, "\n")
        printtoc(io, f; numlines = get(io, :jld2_numlines, 10))
    end
end

"""
    printtoc([io::IO,] f::JLDFile [; numlines])
Prints an overview of the contents of `f` to the `IO`.

Use the optional `numlines` parameter to restrict the amount of items listed.
"""
printtoc(f::JLDFile; kwargs...) = printtoc(Base.stdout, f; kwargs...)
printtoc(io::IO, f::JLDFile; numlines = typemax(Int64)) =
    show_group(io, f.root_group, numlines, " ", true)



include("object_headers.jl")
include("groups.jl")
include("dataspaces.jl")
include("attributes.jl")
include("datatypes.jl")
include("datasets.jl")
include("global_heaps.jl")
include("fractal_heaps.jl")

include("data/type_defs.jl")
include("data/specialcased_types.jl")
include("data/number_types.jl")
include("data/custom_serialization.jl")
include("data/writing_datatypes.jl")
include("data/reconstructing_datatypes.jl")

include("dataio.jl")
include("loadsave.jl")
include("stdlib.jl")
include("backwards_compatibility.jl")
include("inlineunion.jl")
include("fileio.jl")
include("compression.jl")

if ccall(:jl_generating_output, Cint, ()) == 1   # if we're precompiling the package
    include("precompile.jl")
end

end