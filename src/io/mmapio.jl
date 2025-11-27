#
# MmapIO
#
# An IO built on top of mmap to avoid the overhead of ordinary disk IO
if Sys.iswindows()
    const MMAP_GROW_SIZE = 2^19
    const FILE_GROW_SIZE = 2^19
else
    const MMAP_GROW_SIZE = 2^24
    const FILE_GROW_SIZE = 2^18
end

mutable struct MmapIO <: MemoryBackedIO
    f::IOStream
    write::Bool
    n::Int
    startptr::Ptr{Cvoid}
    curptr::Ptr{Cvoid}
    endptr::Ptr{Cvoid}
    @static if Sys.iswindows()
        mapping::Ptr{Cvoid}
    end
    # The following two fields used to be global constants but
    # that breaks concurrent writes on separate threads
    checksum_pos::Vector{Int64}
    nchecksum::Int64
end

if Sys.isunix()
    function mmap!(io::MmapIO, n::Int)
        oldptr = io.startptr
        newptr = ccall(:jl_mmap, Ptr{Cvoid}, (Ptr{Cvoid}, Csize_t, Cint, Cint, Cint, Int64),
                       C_NULL, n, Mmap.PROT_READ | (io.write*Mmap.PROT_WRITE),
                       Mmap.MAP_SHARED, fd(io.f), 0)
        systemerror("mmap", newptr == Ptr{Nothing}(-1))
        io.n = n
        io.curptr += newptr - oldptr
        io.startptr = newptr
    end

    munmap(io::MmapIO) =
        systemerror("munmap",
                    ccall(:munmap, Cint, (Ptr{Cvoid}, Int), io.startptr, io.n) != 0)

    function msync(io::MmapIO, offset::Integer=0, len::Integer=UInt(io.endptr - io.startptr),
                   invalidate::Bool=false)
        # shift `offset` to start of page boundary
        offset_page::Int64 = div(offset, Mmap.PAGESIZE) * Mmap.PAGESIZE
        # add (offset - offset_page) to `len` to get total length of memory-mapped region
        mmaplen::Int64 = (offset - offset_page) + len
        # Edge case: calling msync with 0 mmaplen fails on mac
        mmaplen == 0 && return nothing
        systemerror("msync",
                    ccall(:msync, Cint, (Ptr{Cvoid}, Csize_t, Cint),
                          io.startptr + offset_page, mmaplen,
                          invalidate ? Mmap.MS_INVALIDATE : Mmap.MS_SYNC) != 0)
    end
elseif Sys.iswindows()
    const DWORD = Culong
    function mmap!(io::MmapIO, n::Int)
        oldptr = io.startptr
        mapping = ccall(:CreateFileMappingW, stdcall, Ptr{Cvoid},
                        (Base.Libc.WindowsRawSocket, Ptr{Cvoid}, DWORD, DWORD, DWORD, Ptr{Cvoid}),
                        Mmap.gethandle(io.f), C_NULL,
                        io.write ? Mmap.PAGE_READWRITE : Mmap.PAGE_READONLY, n >> 32,
                        n % UInt32, C_NULL)
        systemerror("CreateFileMappingW", mapping == C_NULL)
        newptr = ccall(:MapViewOfFile, stdcall, Ptr{Cvoid},
                       (Ptr{Cvoid}, DWORD, DWORD, DWORD, Csize_t),
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
                    ccall(:UnmapViewOfFile, stdcall, Cint, (Ptr{Cvoid},), io.startptr) == 0)
        systemerror("CloseHandle",
                    ccall(:CloseHandle, stdcall, Cint, (Ptr{Cvoid},), io.mapping) == 0)
    end

    function msync(io::MmapIO, offset::Integer=0, len::Integer=UInt(io.endptr - io.startptr),
                   invalidate::Bool=false)
        # shift `offset` to start of page boundary
        offset_page::Int64 = div(offset, Mmap.PAGESIZE) * Mmap.PAGESIZE
        # add (offset - offset_page) to `len` to get total length of memory-mapped region
        mmaplen::Int64 = (offset - offset_page) + len
        systemerror("FlushViewOfFile",
                    ccall(:FlushViewOfFile, stdcall, Cint,
                          (Ptr{Cvoid}, Csize_t), io.startptr + offset_page, mmaplen) == 0)
    end
end

function MmapIO(fname::AbstractString, write::Bool, create::Bool, truncate::Bool)
    truncate && !write && throw(ArgumentError("cannot truncate file that is not writable"))

    f = open(fname, read = true, write = write,
             create = create, truncate = truncate, append = false)
    initialsz = truncate ? Int64(0) : filesize(fname)
    @static if Int == Int32
        initialsz > typemax(Int) && error("cannot read a file greater than 2GB on a 32-bit system")
    end
    n = (initialsz % Int) + (write ? MMAP_GROW_SIZE : 0)
    n < 0 && (n = typemax(Int))

    @static if Sys.iswindows()
        io = MmapIO(f, write, 0, C_NULL, C_NULL, C_NULL, C_NULL, Int64[], 0)
    else
        io = MmapIO(f, write, 0, C_NULL, C_NULL, C_NULL, Int64[], 0)
    end
    mmap!(io, n)
    io.endptr = io.startptr + (initialsz % Int)

    io
end

Base.show(io::IO, ::MmapIO) = print(io, "MmapIO")

function grow(io::IOStream, sz::Integer)
    @static if Sys.islinux()
        systemerror("ftruncate",
            ccall(:ftruncate, Cint, (Cint, Int64), fd(io), sz) != 0)
    else
        truncate(io, sz)
    end
end


function Base.resize!(io::MmapIO, newend::Ptr{Cvoid})
    io.write || throw(EOFError())

    # Resize file
    ptr = io.startptr
    newsz = Int(max(newend - ptr, io.curptr - ptr + FILE_GROW_SIZE))
    @static if !Sys.iswindows()
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

function ensureroom(io::MmapIO, n::Integer)
    ep = io.curptr + n
    ep > io.endptr && resize!(io, ep)
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

@static if Sys.isunix()
    function Base.unsafe_write(io::MmapIO, ptr::Ptr{UInt8}, nb::UInt)
        if nb > MMAP_CUTOFF
            pos = position(io)

            # Ensure that the current page has been flushed to disk
            msync(io, pos, min(io.endptr - io.curptr, nb))

            # Write to the underlying IOStream
            regulario = io.f
            seek(regulario, pos)
            unsafe_write(regulario, ptr, nb)

            # Invalidate cache of any pages that were just written to
            msync(io, pos, min(io.n - pos, nb), true)

            # Make sure the mapping encompasses the written data
            ensureroom(io, nb + 1)

            # Seek to the place we just wrote
            seek(io, pos + nb)
        else
            ensureroom(io, nb)
            unsafe_copyto!(Ptr{UInt8}(io.curptr), ptr, nb)
            io.curptr += nb
        end
        nb
    end
end

# Read a null-terminated string
function read_bytestring(io::MmapIO)
    ptr = pconvert(Ptr{UInt8}, io.curptr)
    maxbytes = io.endptr - io.curptr + 1
    q = @ccall memchr(ptr::Ptr{UInt8}, 0::Int32, maxbytes::Csize_t)::Ptr{UInt8}
    q == C_NULL && throw(EOFError())

    str = unsafe_string(ptr, (q - ptr) % Int)
    io.curptr += (q - ptr) % Int + 1
    str
end

Base.position(io::MmapIO) = Int64(io.curptr - io.startptr)
bufferpos(io::MmapIO) = Int64(io.curptr - io.startptr)

# We sometimes need to compute checksums. We do this by first calling begin_checksum when
# starting to handle whatever needs checksumming, and calling end_checksum afterwards. Note
# that we never compute nested checksums, but we may compute multiple checksums
# simultaneously. This strategy is not thread-safe.

function begin_checksum_read(io::MmapIO)
    idx = io.nchecksum += 1
    if idx > length(io.checksum_pos)
        push!(io.checksum_pos, position(io))
    else
        @inbounds io.checksum_pos[idx] = position(io)
    end
    io
end

function begin_checksum_write(io::MmapIO, sz::Integer)
    ensureroom(io, sz)
    begin_checksum_read(io)
end

function end_checksum(io::MmapIO)
    @inbounds v = io.checksum_pos[io.nchecksum]
    io.nchecksum -= 1
    Lookup3.hash(Ptr{UInt8}(io.startptr + v), position(io) - v)
end
