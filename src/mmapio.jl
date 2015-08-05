#
# MmapIO
#
# An IO built on top of mmap to avoid the overhead of ordinary disk IO
const MMAP_GROW_SIZE = 2^24
const FILE_GROW_SIZE = 2^18

type MmapIO <: IO
    f::IOStream
    arr::Vector{UInt8}
    curptr::Ptr{Void}
    endptr::Ptr{Void}
end

function MmapIO(fname::String, write::Bool, create::Bool, truncate::Bool)
    truncate && !write && throw(ArgumentError("cannot truncate file that is not writable"))

    f = open(fname, true, write, create, truncate, false)
    initialsz = truncate ? 0 : filesize(fname)
    arr = Mmap.mmap(f, Vector{UInt8}, (initialsz + MMAP_GROW_SIZE,); grow=false)
    ptr = Ptr{Void}(pointer(arr))
    io = MmapIO(f, arr, ptr, ptr + initialsz)
end

Base.show(io::IO, ::MmapIO) = print(io, "MmapIO")

if OS_NAME === :Linux
    # This is substantially faster than truncate on Linux, but slower on OS X.
    # TODO: Benchmark on Windows
    grow(io::IOStream, sz::Integer) =
        systemerror("pwrite", ccall(:jl_pwrite, Cssize_t,
                                    (Cint, Ptr{UInt8}, Uint, FileOffset),
                                    fd(io), &UInt8(0), 1, sz - 1) < 1)
else
    grow(io::IOStream, sz::Integer) = truncate(io, sz)
end

function Base.resize!(io::MmapIO, newend::Ptr{Void})
    # Resize file
    ptr = pointer(io.arr)
    newsz = Int(max(newend - ptr, io.curptr - ptr + FILE_GROW_SIZE))
    grow(io.f, newsz)

    if newsz > length(io.arr)
        # If we have not mapped enough memory, map more
        io.arr = Mmap.mmap(io.f, Vector{UInt8}, (newsz + MMAP_GROW_SIZE,); grow=false)
        newptr = pointer(io.arr)
        io.curptr += newptr - ptr
        ptr = newptr
    end

    # Set new end
    io.endptr = ptr + newsz
    io
end

@inline function ensureroom(io::MmapIO, n::Int)
    ep = io.curptr + n
    if ep > io.endptr
        resize!(io, ep)
    end
    nothing
end

Base.truncate(io::MmapIO, pos) = truncate(io.f, pos)

function Base.close(io::MmapIO)
    Mmap.sync!(io.arr)
    close(io.f)
end

@inline function _write(io::MmapIO, x)
    cp = io.curptr
    ep = cp + sizeof(x)
    if ep > io.endptr
        resize!(io, ep)
        cp = io.curptr
        ep = cp + sizeof(x)
    end
    unsafe_store!(Ptr{typeof(x)}(cp), x)
    io.curptr = ep
    nothing
end
@inline Base.write(io::MmapIO, x::UInt8) = _write(io, x)
@inline Base.write(io::MmapIO, x::Plain)  = _write(io, x)

function Base.write{T}(io::MmapIO, x::Ptr{T}, n::Integer)
    cp = io.curptr
    ep = cp + sizeof(T)*n
    if ep > io.endptr
        resize!(io, ep)
        cp = io.curptr
        ep = cp + sizeof(T)*n
    end
    unsafe_copy!(Ptr{T}(cp), x, n)
    io.curptr = ep
    nothing
end

Base.write(io::MmapIO, x::ASCIIString) = write(io, pointer(x), sizeof(x))
Base.write(io::MmapIO, x::UTF8String) = write(io, pointer(x), sizeof(x))
Base.write(io::MmapIO, x::Array) = write(io, pointer(x), sizeof(x))

@inline function _read(io::MmapIO, T::DataType)
    cp = io.curptr
    ep = cp + sizeof(T)
    ep > io.endptr && throw(EOFError())
    v = unsafe_load(Ptr{T}(cp))
    io.curptr = ep
    v
end
@inline Base.read(io::MmapIO, T::Type{UInt8}) = _read(io, T)
@inline Base.read(io::MmapIO, T::Type{Int8}) = _read(io, T)
@inline Base.read(io::MmapIO, T::PlainType) = _read(io, T)

function Base.read{T}(io::MmapIO, ::Type{T}, n::Int)
    cp = io.curptr
    ep = cp + sizeof(T)*n
    ep > io.endptr && throw(EOFError())
    arr = Array(T, n)
    unsafe_copy!(pointer(arr), Ptr{T}(cp), n)
    io.curptr = ep
    arr
end
Base.read{T}(io::MmapIO, ::Type{T}, n::Integer) =
    read(io, T, Int(n))

# Read a null-terminated string
function read_bytestring(io::MmapIO)
    cp = io.curptr
    str = bytestring(convert(Ptr{UInt8}, cp))
    io.curptr = cp + sizeof(str) + 1
    str
end

function Base.seek(io::MmapIO, offset::Integer)
    io.curptr = pointer(io.arr) + offset
    nothing
end

function Base.skip(io::MmapIO, offset::Integer)
    io.curptr += offset
    nothing
end

Base.position(io::MmapIO) = FileOffset(io.curptr - pointer(io.arr))

# We sometimes need to compute checksums. We do this by first calling
# begin_checksum when starting to handle whatever needs checksumming,
# and calling end_checksum afterwards. Note that we never compute
# nested checksums.
# XXX not thread-safe!

const CHECKSUM_POS = FileOffset[]
const NCHECKSUM = Ref{Int}(0)
function begin_checksum(io::MmapIO)
    idx = NCHECKSUM[] += 1
    if idx > length(CHECKSUM_POS)
        push!(CHECKSUM_POS, position(io))
    else
        @inbounds CHECKSUM_POS[idx] = position(io)
    end
    io
end
function begin_checksum(io::MmapIO, sz::Integer)
    ensureroom(io, sz)
    begin_checksum(io)
end
function end_checksum(io::MmapIO)
    @inbounds v = CHECKSUM_POS[NCHECKSUM[]]
    NCHECKSUM[] -= 1
    Lookup3.hash(UnsafeContiguousView(pointer(io.arr) + v, (position(io) - v,)))
end
