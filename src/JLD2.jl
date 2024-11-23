module JLD2
using OrderedCollections: OrderedDict
using MacroTools: MacroTools, @capture
using Mmap: Mmap
using TranscodingStreams: TranscodingStreams
using FileIO: load, save
export load, save
using Requires: @require
using PrecompileTools: @setup_workload, @compile_workload
export jldopen, @load, @save, save_object, load_object, jldsave

include("types.jl")




include("macros_utils.jl")
include("io/mmapio.jl")
include("io/bufferedio.jl")
include("julia_compat.jl")
include("file_header.jl")
include("Lookup3.jl")
include("superblock.jl")
include("misc.jl")


is_win7() = Sys.iswindows() && Sys.windows_version().major <= 6 && Sys.windows_version().minor <= 1
# Windows 7 doesn't support mmap, falls back to IOStream
const DEFAULT_IOTYPE = is_win7() ? IOStream : MmapIO

"""
    Group(file::JLDFile, name::String)
    Group(file::Group, name::String)

Construct a `Group` in `file` with name `name`.
`Group`s are JLD2s equivalent of folders and may be nested, so `file` itself may alread be a `Group` or a `JLDFile` file handle.

## Example usage
```
jldopen("example.jld2", "w") do f
    g = Group(f, "subgroup")
    g["data"] = 42
end

jldopen("example.jld2") do f
    g = f["subgroup"]
    f["subgroup/data"] == g["data"]
end
```

## Keyword arguments:

- `est_num_entries::Int` = 4
- `est_link_name_len::Int` = 8

Determine how much (additional) empty space should be allocated for the group description. (list of entries)
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
    plain::Bool
    compress#::Union{Bool,Symbol}
    mmaparrays::Bool
    n_times_opened::Int
    # Experimental feature: disable committing structs
    disable_commit::Bool
    datatype_locations::OrderedDict{RelOffset,CommittedDatatype}
    datatypes::Vector{H5Datatype}
    datatype_wsession::JLDWriteSession{Dict{UInt,Tuple{RelOffset,WeakRef}}}
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
                        plain::Bool,
                        compress,#::Union{Bool,Symbol},
                        mmaparrays::Bool) where T
        f = new(io, path, writable, written, plain, compress, mmaparrays, 1, false,
            OrderedDict{RelOffset,CommittedDatatype}(), H5Datatype[],
            JLDWriteSession(), Dict{String,Any}(), IdDict(), IdDict(), Dict{RelOffset,WeakRef}(),
            DATA_START, Dict{RelOffset,GlobalHeap}(),
            GlobalHeap(0, 0, 0, Int64[]), Dict{RelOffset,Group{JLDFile{T}}}(), UNDEFINED_ADDRESS)
        finalizer(jld_finalizer, f)
        f
    end
end
JLDFile(io::IO, path::AbstractString, writable::Bool, written::Bool, plain::Bool, compress, mmaparrays::Bool) =
    JLDFile{typeof(io)}(io, path, writable, written, plain, compress, mmaparrays)

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

const OPEN_FILES = Dict{String,WeakRef}()
const OPEN_FILES_LOCK = ReentrantLock()
function jldopen(fname::AbstractString, wr::Bool, create::Bool, truncate::Bool, iotype::T=DEFAULT_IOTYPE;
                 fallback::Union{Type, Nothing} = FallbackType(iotype),
                 compress=false,
                 mmaparrays::Bool=false,
                 typemap::Dict{String}=Dict{String,Any}(),
                 parallel_read::Bool=false,
                 plain::Bool=false
                 ) where T<:Union{Type{IOStream},Type{MmapIO}}
    mmaparrays && @warn "mmaparrays keyword is currently ignored" maxlog=1
    verify_compressor(compress)

    # Can only open multiple in parallel if mode is "r"
    if parallel_read && (wr, create, truncate)  != (false, false, false)
        throw(ArgumentError("Cannot open file in a parallel context unless mode is \"r\""))
    end

    lock(OPEN_FILES_LOCK)

    f = try
        exists = ispath(fname)
        if exists
            rname = realpath(fname)
            # catch existing file system entities that are not regular files
            !isfile(rname) && throw(ArgumentError("not a regular file: $fname"))

            f = get(OPEN_FILES, rname, (;value=nothing)).value
            # If in serial, return existing handle. In parallel always generate a new handle
            if !isnothing(f)
                if parallel_read 
                    f.writable && throw(ArgumentError("Tried to open file in a parallel context but it is open in write-mode elsewhere in a serial context."))
                else
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
        f = JLDFile(io, rname, wr, created, plain, compress, mmaparrays)

        !parallel_read && (OPEN_FILES[rname] = WeakRef(f))

        f
    finally
        unlock(OPEN_FILES_LOCK)
    end
    try
        initialize_fileobject!(f)
    catch e
        close(f)
        throw(e)
    end
    merge!(f.typemap, typemap)
    return f
end

function initialize_fileobject!(f::JLDFile)
    if f.written
        f.base_address = 512
        f.root_group = Group{typeof(f)}(f)
        f.types_group =  Group{typeof(f)}(f)
        return
    end
    superblock = find_superblock(f)
    f.end_of_data = superblock.end_of_file_address
    f.base_address = superblock.base_address
    f.root_group_offset = superblock.root_group_object_header_address
    if superblock.version >= 2
        verify_file_header(f)
    elseif f.writable
        close(f)
        throw(UnsupportedVersionException("This file can not be edited by JLD2. Please open in read-only mode."))
    end
    f.root_group = load_group(f, f.root_group_offset)

    types_offset = get(f.root_group.written_links, "_types", UNDEFINED_ADDRESS)
    if types_offset != UNDEFINED_ADDRESS
        f.types_group = f.loaded_groups[types_offset] = load_group(f, types_offset)
        for (i, offset::RelOffset) in enumerate(values(f.types_group.written_links))
            f.datatype_locations[offset] = CommittedDatatype(offset, i)
        end
        resize!(f.datatypes, length(f.datatype_locations))
    else
        f.types_group = Group{typeof(f)}(f)
    end
end

"""
    jldopen(file, mode::AbstractString; iotype=MmapIO, compress=false, typemap=Dict())

Opens a JLD2 file at path `file`. Alternatively `file` may be a suitable IO object.

Options for `mode`:
- `"r"`: Open for reading only, failing if no file exists
- `"r+"`: Open for reading and writing, failing if no file exists
- `"w"`/`"w+"`: Open for reading and writing, overwriting the file if it already exists
- `"a"`/`"a+"`: Open for reading and writing, creating a new file if none exists, but
                preserving the existing file if one is present
"""
function jldopen(fname::Union{AbstractString, IO}, mode::AbstractString="r"; iotype=DEFAULT_IOTYPE, kwargs...)
    (wr, create, truncate) = mode == "r"  ? (false, false, false) :
                             mode == "r+" ? (true, false, false) :
                             mode == "a" || mode == "a+" ? (true, true, false) :
                             mode == "w" || mode == "w+" ? (true, true, true) :
                             throw(ArgumentError("invalid open mode: $mode"))
    if fname isa AbstractString
        jldopen(fname, wr, create, truncate, iotype; kwargs...)
    else
        jldopen(fname, wr, create, truncate; kwargs...)
    end
end


function jldopen(io::IO, writable::Bool, create::Bool, truncate::Bool;
                plain::Bool=false,
                compress=false,
                typemap::Dict{String}=Dict{String,Any}(),
                )
    verify_compressor(compress)
    # figure out what kind of io object this is 
    # for now assume it is
    !io.readable && throw("IO object is not readable")
    if io.seekable && writable && iswritable(io)
        # Here could have a more lightweight wrapper
        # that just ensures API is defined
        created = truncate
        io = RWBuffer(io)
        f = JLDFile(io, "RWBuffer", writable, created, plain, compress, false)
    elseif (false == writable == create == truncate)
        # Were trying to read, so let's hope `io` implements `read` and bytesavailable
        io = ReadOnlyBuffer(io)
        f = JLDFile(io, "ReadOnlyBuffer", false, false, plain, compress, false)
    end
    initialize_fileobject!(f)
    merge!(f.typemap, typemap)
    return f
end

"""
    load_datatypes(f::JLDFile)

Populate `f.datatypes` and `f.jlh5types` with all of the committed datatypes from a file.
We need to do this before writing to make sure we reuse written datatypes.
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

Base.read(f::JLDFile, name::AbstractString) = Base.inferencebarrier(f.root_group[name])
Base.getindex(f::JLDFile, name::AbstractString) = Base.inferencebarrier(f.root_group[name])
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

function jld_finalizer(f::JLDFile)
    f.n_times_opened == 0 && return
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
    return nothing
end

"""
    printtoc([io::IO,] f::JLDFile [; numlines])

Prints an overview of the contents of `f` to the `IO`.

Use the optional `numlines` parameter to restrict the amount of items listed.
"""
printtoc(f::JLDFile; kwargs...) = printtoc(Base.stdout, f; kwargs...)
function printtoc(io::IO, f::JLDFile; numlines = typemax(Int64))
    show_group(io, f.root_group, numlines, " ", true)
    return nothing
end



include("object_headers.jl")
include("headermessages.jl")
include("groups.jl")
include("dataspaces.jl")
include("attributes.jl")
include("datatypes.jl")
include("datalayouts.jl")
include("datasets.jl")
include("global_heaps.jl")
include("fractal_heaps.jl")

include("data/type_defs.jl")
include("data/specialcased_types.jl")
include("data/number_types.jl")
include("data/custom_serialization.jl")
include("data/writing_datatypes.jl")
include("data/reconstructing_datatypes.jl")

include("io/dataio.jl")
include("io/io_wrappers.jl")
include("loadsave.jl")
include("backwards_compatibility.jl")
include("inlineunion.jl")
include("fileio.jl")
include("compression.jl")
include("explicit_datasets.jl")
include("committed_datatype_introspection.jl")


if ccall(:jl_generating_output, Cint, ()) == 1   # if we're precompiling the package
    include("precompile.jl")
end

function __init__()
    # If UnPack.jl package is loaded, extend @unpack, @pack! for file-like interface
    @require UnPack="3a884ed6-31ef-47d7-9d2a-63182c4928ed" begin
        include("../ext/UnPackExt.jl")
    end
end

end
