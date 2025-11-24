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
    # Find the null terminator position
    nb = 0
    while true
        idx = position(io)+1+nb
        idx > io.size && resize!(io, idx)
        io.data[idx] == 0x00 && break
        nb += 1
    end

    pos = position(io)
    data = io.data
    str = GC.@preserve data unsafe_string(pointer(data, pos+1), nb)
    skip(io, nb+1)
    return str
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
    RWBuffer(_buf::IO) = new{typeof(_buf)}(_buf, position(_buf), 0, _buf.size)
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