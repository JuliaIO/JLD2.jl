#
# MmapIO
#
# An IO built on top of mmap to avoid the overhead of ordinary disk IO
if is_windows()
    const MMAP_GROW_SIZE = 2^19
    const FILE_GROW_SIZE = 2^19
else
    const MMAP_GROW_SIZE = 2^24
    const FILE_GROW_SIZE = 2^18
end

const Plain = Union{Int16,Int32,Int64,Int128,UInt16,UInt32,UInt64,UInt128,Float16,Float32,
                    Float64}
const PlainType = Union{Type{Int16},Type{Int32},Type{Int64},Type{Int128},Type{UInt16},
                        Type{UInt32},Type{UInt64},Type{UInt128},Type{Float16},
                        Type{Float32},Type{Float64}}

mutable struct MmapIO <: IO
    f::IOStream
    write::Bool
    n::Int
    startptr::Ptr{Void}
    curptr::Ptr{Void}
    endptr::Ptr{Void}
    @static if is_windows()
        mapping::Ptr{Void}
    end
end

if is_unix()
    function mmap!(io::MmapIO, n::Int)
        oldptr = io.startptr
        newptr = ccall(:jl_mmap, Ptr{Void}, (Ptr{Void}, Csize_t, Cint, Cint, Cint, Int64),
                       C_NULL, n, Mmap.PROT_READ | (io.write*Mmap.PROT_WRITE),
                       Mmap.MAP_SHARED, fd(io.f), 0)
        io.n = n
        io.curptr += newptr - oldptr
        io.startptr = newptr
    end

    munmap(io::MmapIO) =
        systemerror("munmap",
                    ccall(:munmap, Cint, (Ptr{Void}, Int), io.startptr, io.n) != 0)

    function msync(io::MmapIO, offset::Integer=0, len::Integer=UInt(io.endptr - io.startptr),
                   invalidate::Bool=false)
        # shift `offset` to start of page boundary
        offset_page::Int64 = div(offset, Mmap.PAGESIZE) * Mmap.PAGESIZE
        # add (offset - offset_page) to `len` to get total length of memory-mapped region
        mmaplen::Int64 = (offset - offset_page) + len
        systemerror("msync",
                    ccall(:msync, Cint, (Ptr{Void}, Csize_t, Cint),
                          io.startptr + offset_page, mmaplen,
                          invalidate ? Mmap.MS_INVALIDATE : Mmap.MS_SYNC) != 0)
    end
elseif is_windows()
    const DWORD = Culong
    function mmap!(io::MmapIO, n::Int)
        oldptr = io.startptr
        mapping = ccall(:CreateFileMappingW, stdcall, Ptr{Void},
                        (Cptrdiff_t, Ptr{Void}, DWORD, DWORD, DWORD, Ptr{Void}),
                        Mmap.gethandle(io.f), C_NULL,
                        io.write ? Mmap.PAGE_READWRITE : Mmap.PAGE_READONLY, n >> 32,
                        n % UInt32, C_NULL)
        systemerror("CreateFileMappingW", mapping == C_NULL)
        newptr = ccall(:MapViewOfFile, stdcall, Ptr{Void},
                       (Ptr{Void}, DWORD, DWORD, DWORD, Csize_t),
                        mapping, io.write ? Mmap.FILE_MAP_WRITE : Mmap.FILE_MAP_READ, 0, 0,
                        n)
        systemerror("MapViewOfFile", newptr == C_NULL)
        io.n = n
        io.curptr += newptr - oldptr
        io.mapping = mapping
        io.startptr = newptr
    end

    function munmap(io::MmapIO)
        systemerror("UnmapViewOfFile",
                    ccall(:UnmapViewOfFile, stdcall, Cint, (Ptr{Void},), io.startptr) == 0)
        systemerror("CloseHandle",
                    ccall(:CloseHandle, stdcall, Cint, (Ptr{Void},), io.mapping) == 0)
    end

    function msync(io::MmapIO, offset::Integer=0, len::Integer=UInt(io.endptr - io.startptr),
                   invalidate::Bool=false)
        # shift `offset` to start of page boundary
        offset_page::Int64 = div(offset, Mmap.PAGESIZE) * Mmap.PAGESIZE
        # add (offset - offset_page) to `len` to get total length of memory-mapped region
        mmaplen::Int64 = (offset - offset_page) + len
        systemerror("FlushViewOfFile",
                    ccall(:FlushViewOfFile, stdcall, Cint,
                          (Ptr{Void}, Csize_t), io.startptr + offset_page, mmaplen) == 0)
    end
end

function MmapIO(fname::AbstractString, write::Bool, create::Bool, truncate::Bool)
    truncate && !write && throw(ArgumentError("cannot truncate file that is not writable"))

    f = open(fname, true, write, create, truncate, false)
    initialsz = truncate ? Int64(0) : filesize(fname)
    @static if Int == Int32
        initialsz > typemax(Int) && error("cannot read a file greater than 2GB on a 32-bit system")
    end
    n = (initialsz % Int) + (write ? MMAP_GROW_SIZE : 0)
    n < 0 && (n = typemax(Int))

    @static if is_windows()
        io = MmapIO(f, write, 0, C_NULL, C_NULL, C_NULL, C_NULL)
    else
        io = MmapIO(f, write, 0, C_NULL, C_NULL, C_NULL)
    end
    mmap!(io, n)
    io.endptr = io.startptr + (initialsz % Int)

    io
end

Base.show(io::IO, ::MmapIO) = print(io, "MmapIO")

if is_linux()
    # This is substantially faster than truncate on Linux, but slower on OS X.
    # TODO: Benchmark on Windows
    grow(io::IOStream, sz::Integer) =
        systemerror("pwrite", ccall(:jl_pwrite, Cssize_t,
                                    (Cint, Ptr{UInt8}, UInt, Int64),
                                    fd(io), &UInt8(0), 1, sz - 1) < 1)
else
    grow(io::IOStream, sz::Integer) = truncate(io, sz)
end

function Base.resize!(io::MmapIO, newend::Ptr{Void})
    io.write || throw(EOFError())

    # Resize file
    ptr = io.startptr
    newsz = Int(max(newend - ptr, io.curptr - ptr + FILE_GROW_SIZE))
    @static if !is_windows()
        grow(io.f, newsz)
    end

    if newsz > io.n
        # If we have not mapped enough memory, map more
        munmap(io)
        ptr = mmap!(io, max(newsz, io.n + MMAP_GROW_SIZE))
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

function truncate_and_close(io::MmapIO, endpos::Integer)
    io.write && msync(io)
    munmap(io)
    truncate(io.f, endpos)
    close(io.f)
end

function Base.close(io::MmapIO)
    io.write && msync(io)
    munmap(io)
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
    return sizeof(x)
end
@inline Base.write(io::MmapIO, x::UInt8) = _write(io, x)
@inline Base.write(io::MmapIO, x::Int8) = _write(io, x)
@inline Base.write(io::MmapIO, x::Plain)  = _write(io, x)

function Base.unsafe_write(io::MmapIO, x::Ptr{UInt8}, n::UInt)
    cp = io.curptr
    ep = cp + n
    if ep > io.endptr
        resize!(io, ep)
        cp = io.curptr
        ep = cp + n
    end
    unsafe_copy!(Ptr{UInt8}(cp), x, n)
    io.curptr = ep
    return n
end

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

function Base.read(io::MmapIO, ::Type{T}, n::Int) where T
    cp = io.curptr
    ep = cp + sizeof(T)*n
    ep > io.endptr && throw(EOFError())
    arr = Vector{T}(n)
    unsafe_copy!(pointer(arr), Ptr{T}(cp), n)
    io.curptr = ep
    arr
end
Base.read(io::MmapIO, ::Type{T}, n::Integer) where {T} =
    read(io, T, Int(n))

# Read a null-terminated string
function read_bytestring(io::MmapIO)
    # TODO do not try to read outside the buffer
    cp = io.curptr
    str = unsafe_string(convert(Ptr{UInt8}, cp))
    io.curptr = cp + sizeof(str) + 1
    str
end

@inline function Base.seek(io::MmapIO, offset::Integer)
    if io.startptr + offset > io.endptr
        resize!(io, io.startptr + offset)
    end
    io.curptr = io.startptr + offset
    nothing
end

@inline function Base.skip(io::MmapIO, offset::Integer)
    if io.curptr + offset > io.endptr
        resize!(io, io.curptr + offset)
    end
    io.curptr += offset
    nothing
end

Base.position(io::MmapIO) = Int64(io.curptr - io.startptr)

"""
    IndirectPointer

When writing data, we may need to enlarge the memory mapping, which would invalidate any
memory addresses arising from the old `mmap` pointer. `IndirectPointer` holds a pointer to
the `startptr` field of an MmapIO, and the offset relative to that pointer. It defers
computing a memory address until converted to a Ptr{T}, so the memory mapping can be
enlarged and addresses will remain valid.
"""
struct IndirectPointer
    ptr::Ptr{Ptr{Void}}
    offset::Int
end

function IndirectPointer(io::MmapIO, offset::Integer=position(io))
    IndirectPointer(pointer_from_objref(io) + fieldoffset(MmapIO, 4), offset)
end
Base.:+(x::IndirectPointer, y::Integer) = IndirectPointer(x.ptr, x.offset+y)
Base.convert{T}(::Type{Ptr{T}}, x::IndirectPointer) = Ptr{T}(unsafe_load(x.ptr) + x.offset)

# We sometimes need to compute checksums. We do this by first calling begin_checksum when
# starting to handle whatever needs checksumming, and calling end_checksum afterwards. Note
# that we never compute nested checksums, but we may compute multiple checksums
# simultaneously. This strategy is not thread-safe.

const CHECKSUM_POS = Int64[]
const NCHECKSUM = Ref{Int}(0)
function begin_checksum_read(io::MmapIO)
    idx = NCHECKSUM[] += 1
    if idx > length(CHECKSUM_POS)
        push!(CHECKSUM_POS, position(io))
    else
        @inbounds CHECKSUM_POS[idx] = position(io)
    end
    io
end
function begin_checksum_write(io::MmapIO, sz::Integer)
    ensureroom(io, sz)
    begin_checksum_read(io)
end
function end_checksum(io::MmapIO)
    @inbounds v = CHECKSUM_POS[NCHECKSUM[]]
    NCHECKSUM[] -= 1
    Lookup3.hash(Ptr{UInt8}(io.startptr + v), position(io) - v)
end
