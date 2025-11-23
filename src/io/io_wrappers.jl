## Requirements for an IO object
#=

Implement for Reading:
Base.position(io)::Int
Base.bufferpos(io)::Int (if applicable)
Base.close(io)
Base.seek(io, pos::Int)
read_bytestring(io)::String # read a null-terminated byte string
Base.skip(io, n::Int)
_read(io, ::Type{T}) for [U]Int8 and `PlainType`
_write(io, x::T) for [U]Int8 and `PlainType`
begin_checksum_read(io) # see existing implementations
end_checksum(io)
=#

## Create an IO object which wraps a non-seekable read-only buffer
const MINBUFFERSIZE = 2^9 # should maybe be 2^16 

mutable struct ReadOnlyBuffer{B <: IO} <: MemoryBackedIO
    _buf::B
    offset::UInt64 # position of file start in wrapped stream
    data::Vector{UInt8}
    startptr::Ptr{Cvoid}
    curptr::Ptr{Cvoid}
    size::UInt64
    checksum_pos::Vector{Int64}
    nchecksum::Int64
    function ReadOnlyBuffer(_buf::IO) 
        offset = position(_buf)
        nb = min(MINBUFFERSIZE, bytesavailable(_buf))
        data = read(_buf, nb)
        curptr = startptr = pointer(data)
        new{typeof(_buf)}(_buf, offset, data, 
            startptr, curptr, length(data), Int[], 0)
    end
end

Base.position(io::ReadOnlyBuffer) = Int(io.curptr-io.startptr)
bufferpos(io::ReadOnlyBuffer) = Int(io.curptr-io.startptr)
Base.close(::ReadOnlyBuffer) = nothing
Base.isreadable(::ReadOnlyBuffer) = true
Base.iswritable(::ReadOnlyBuffer) = false

function Base.resize!(io::ReadOnlyBuffer, newend::Integer)
    newend < io.size && return
    nb = min(bytesavailable(io._buf), max(newend-io.size, MINBUFFERSIZE))
    bts = read(io._buf, nb)
    append!(io.data, bts)
    io.size += length(bts)
    newend ≤ io.size || throw(EOFError())
    # update pointers
    oldstart = io.startptr
    io.startptr = pointer(io.data)
    io.curptr = io.curptr - oldstart + io.startptr
    nothing
end
Base.resize!(io::ReadOnlyBuffer, p::Ptr) = resize!(io, p - io.startptr)

ensureroom(io::ReadOnlyBuffer, n::Integer) = resize!(io, bufferpos(io) + n)

# Read a null-terminated string
function read_bytestring(io::ReadOnlyBuffer)
    nb = 0
    while true
        idx = position(io)+1+nb
        idx > io.size && resize!(io, idx)
        io.data[idx] == 0x00 && break
        nb += 1
    end
    pos = position(io)
    v = io.data[pos+1 : pos+nb]
    skip(io, nb+1)
    return String(v)
end

function begin_checksum_read(io::ReadOnlyBuffer)
    idx = io.nchecksum += 1
    if idx > length(io.checksum_pos)
        push!(io.checksum_pos, position(io))
    else
        io.checksum_pos[idx] = position(io)
    end
    io
end

function end_checksum(io::ReadOnlyBuffer)
    v = io.checksum_pos[io.nchecksum]
    io.nchecksum -= 1
    Lookup3.hash(Ptr{UInt8}(io.startptr + v), position(io) - v)
end

###########################################################################################
## RWBuffer
###########################################################################################

mutable struct RWBuffer{B <: IO} <: IO
    _buf::B
    offset::UInt64 # position of file start in wrapped stream
    pos::UInt64
    size::UInt64
    RWBuffer(_buf::IO) = new{typeof(_buf)}(_buf, position(_buf), 0, _getsize(_buf))
end

Base.position(io::RWBuffer) = Int(io.pos)
Base.close(::RWBuffer) = nothing
function truncate_and_close(io::RWBuffer, endpos::Integer)
    #truncate(io, endpos)
    close(io)
end

function Base.seek(io::RWBuffer, n::Integer)
    n > io.size && resize!(io, n)
    seek(io._buf, n + io.offset)
    @assert position(io._buf) == n+io.offset
    io.pos = n
end

function Base.resize!(io::RWBuffer, newend::Integer)
    newend < io.size && return
    buf = io._buf
    pos = position(buf)
    seek(buf, io.size+io.offset)
    write(buf, zeros(UInt8, newend-io.size))
    seek(buf, pos)
    io.size = newend
end
ensureroom(io::RWBuffer, n::Integer) = resize!(io, position(io) + n)

# Read a null-terminated string
function read_bytestring(io::RWBuffer)
    v = readuntil(io._buf, 0x00)
    io.pos = position(io._buf) - io.offset
    return String(v)
end

Base.skip(io::RWBuffer, offset::Integer) = seek(io, position(io)+offset)

function _read(io::RWBuffer, T::DataType)
    n = jlsizeof(T)
    ensureroom(io, n)
    io.pos += jlsizeof(T)
    read(io._buf, T)
end
# needed explicitly for avoiding ambiguities
Base.read(io::RWBuffer, T::Type{UInt8}) = _read(io, T)
#Base.read(io::RWBuffer, T::PlainType) = _read(io, T)
Base.write(io::RWBuffer, x::UInt8) = _write(io, x)
jlwrite(io::RWBuffer, x::String) = _write(io, x)

function jlwrite(io::RWBuffer, x::Array{T}) where T
    for y in x
        jlwrite(io, y)
    end
    return length(x) * jlsizeof(T)
end

function _write(io::RWBuffer, x)
    posprev = position(io)
    jlwrite(io._buf, x)    
    io.pos = position(io._buf) - io.offset
    io.size = max(io.size, io.pos)
    return io.pos-posprev
end

Base.bytesavailable(io::RWBuffer) = io.size-io.pos
Base.isreadable(::RWBuffer) = true
Base.iswritable(::RWBuffer) = true

function _getsize(io::IO)
    try
        return filesize(io)
    catch
        # fallback
        pos = position(io)
        seekend(io)
        sz = position(io)
        seek(io, pos)
        return sz
    end
end

###########################################################################################
## ByteVectorIO - Optimized wrapper for Vector{UInt8}
###########################################################################################

"""
    ByteVectorIO

High-performance memory-backed IO for reading and writing JLD2 files to/from `Vector{UInt8}`.
This provides direct memory access similar to `MmapIO` but for in-memory byte vectors.

# Examples
```julia
# Writing to a byte vector
data = UInt8[]
f = jldopen(data, "w")
f["mydata"] = [1, 2, 3]
close(f)

# Reading from a byte vector
f = jldopen(data, "r")
recovered = f["mydata"]
close(f)
```
"""
mutable struct ByteVectorIO <: MemoryBackedIO
    data::Vector{UInt8}
    writable::Bool
    curptr::Ptr{Nothing}
    startptr::Ptr{Nothing}
    # For checksum computation
    checksum_pos::Vector{Int64}
    nchecksum::Int64
end

function ByteVectorIO(data::Vector{UInt8}, writable::Bool)
    startptr = isempty(data) ? Ptr{Nothing}(0) : pointer(data)
    ByteVectorIO(data, writable, startptr, startptr, Int64[], 0)
end

Base.show(io::IO, ::ByteVectorIO) = print(io, "ByteVectorIO")
Base.isreadable(::ByteVectorIO) = true
Base.iswritable(io::ByteVectorIO) = io.writable

Base.position(io::ByteVectorIO) = Int64(io.curptr - io.startptr)
bufferpos(io::ByteVectorIO) = Int64(io.curptr - io.startptr)

function Base.seek(io::ByteVectorIO, offset::Integer)
    offset < 0 && throw(ArgumentError("cannot seek to negative position"))
    offset > length(io.data) && ensureroom(io, offset - position(io))
    io.curptr = io.startptr + offset
    nothing
end

function Base.resize!(io::ByteVectorIO, newend::Ptr{Nothing})
    io.writable || throw(EOFError())

    # Calculate new size needed
    newsz = Int(max(newend - io.startptr, io.curptr - io.startptr))
    oldlen = length(io.data)

    if newsz > oldlen
        # Resize the vector
        resize!(io.data, newsz)
        # Update pointer (resize! may have moved the data)
        oldstartptr = io.startptr
        io.startptr = pointer(io.data)
        io.curptr = io.curptr - oldstartptr + io.startptr
    end

    io
end

function ensureroom(io::ByteVectorIO, n::Integer)
    ep = io.curptr + n
    endptr = io.startptr + length(io.data)
    ep > endptr && resize!(io, ep)
end

function truncate_and_close(io::ByteVectorIO, endpos::Integer)
    if io.writable && endpos < length(io.data)
        resize!(io.data, endpos)
    end
    close(io)
end

Base.close(::ByteVectorIO) = nothing

# Read a null-terminated string
function read_bytestring(io::ByteVectorIO)
    pos = position(io)
    idx = findfirst(==(0x00), view(io.data, pos+1:length(io.data)))
    idx === nothing && throw(EOFError())
    str = String(io.data[pos+1:pos+idx-1])
    io.curptr += idx
    str
end

# Checksum support
function begin_checksum_read(io::ByteVectorIO)
    idx = io.nchecksum += 1
    if idx > length(io.checksum_pos)
        push!(io.checksum_pos, position(io))
    else
        @inbounds io.checksum_pos[idx] = position(io)
    end
    io
end

function begin_checksum_write(io::ByteVectorIO, sz::Integer)
    ensureroom(io, sz)
    begin_checksum_read(io)
end

function end_checksum(io::ByteVectorIO)
    @inbounds v = io.checksum_pos[io.nchecksum]
    io.nchecksum -= 1
    Lookup3.hash(Ptr{UInt8}(io.startptr + v), position(io) - v)
end