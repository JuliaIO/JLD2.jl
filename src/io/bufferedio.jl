#
# BufferedIO
#

mutable struct BufferedWriter{io} <: MemoryBackedIO
    f::io
    buffer::Vector{UInt8}
    file_position::Int64
    curptr::Ptr{Nothing}
    extensible::Bool
end

function BufferedWriter(io, buffer_size::Integer = 0; extensible::Bool=false)
    pos = position(io)
    skip(io, buffer_size)
    buf = Vector{UInt8}(undef, buffer_size)
    BufferedWriter(io, buf, Int64(pos), Ptr{Nothing}(pointer(buf)), extensible)
end
Base.show(io::IO, ::BufferedWriter) = print(io, "BufferedWriter")

function ensureroom(io::BufferedWriter, n::Integer)
    if bufferpos(io) + n > length(io.buffer)
        if io.extensible
            pos = bufferpos(io)
            resize!(io.buffer, length(io.buffer) + n)
            io.curptr = pointer(io.buffer, pos+1)
        else
            throw(InternalError("BufferedWriter: not enough room"))
        end
    end
end

Base.position(io::BufferedWriter) = io.file_position + bufferpos(io)

function Base.seek(io::BufferedWriter, offset::Integer)
    buffer_offset = offset - io.file_position
    buffer_offset < 0 && throw(ArgumentError("cannot seek before start of buffer"))
    ensureroom(io, buffer_offset - bufferpos(io))
    io.curptr = pointer(io.buffer) + buffer_offset
end

function finish!(io::BufferedWriter)
    bufferpos(io) == length(io.buffer) ||
        throw(InternalError("BufferedWriter: buffer not written to end; position is $(bufferpos(io)) but length is $(length(io.buffer))"))
    seek(io.f, io.file_position)
    jlwrite(io.f, io.buffer)
    nothing
end

mutable struct BufferedReader{io} <: MemoryBackedIO
    f::io
    buffer::Vector{UInt8}
    file_position::Int64
    curptr::Ptr{Nothing}
end

function BufferedReader(io)
    buf = Vector{UInt8}()
    BufferedReader(io, buf, Int64(position(io)), Ptr{Nothing}(pointer(buf)))
end

Base.show(io::IO, ::BufferedReader) = print(io, "BufferedReader")

function readmore!(io::BufferedReader, n::Integer)
    f = io.f
    sz = _getsize(f)
    amount = min(max(n, 2^14), sz-position(f))
    amount < n && throw(EOFError())
    buffer = io.buffer
    pos = bufferpos(io)
    resize!(buffer, length(buffer) + amount)
    io.curptr = pointer(buffer, pos+1)
    unsafe_read(f, io.curptr, amount)
end

ensureroom(io::BufferedReader, n::Integer) = 
    (bufferpos(io) + n >= length(io.buffer)) && readmore!(io, n)


Base.position(io::BufferedReader) = io.file_position + bufferpos(io)

"""
    bufferpos(io::Union{BufferedReader, BufferedWriter})

Get the current position in the buffer.
"""
bufferpos(io::Union{BufferedReader, BufferedWriter}) = Int(io.curptr - pointer(io.buffer))

function Base.seek(io::BufferedReader, offset::Integer)
    pos =  offset - io.file_position
    pos < 0 && throw(ArgumentError("cannot seek before start of buffer"))
    ensureroom(io, offset - position(io))
    io.curptr = pointer(io.buffer) + pos
    nothing
end
finish!(io::BufferedReader) = seek(io.f, io.file_position + bufferpos(io))

function truncate_and_close(io::IOStream, endpos::Integer)
    truncate(io, endpos)
    close(io)
end

Base.close(::BufferedReader) = nothing

begin_checksum_read(io::IO) = BufferedReader(io)

function begin_checksum_write(io::IO, sz::Integer)
    BufferedWriter(io, sz)
end
function end_checksum(io::Union{BufferedReader,BufferedWriter})
    ret = Lookup3.hash(io.buffer, 1, bufferpos(io))
    finish!(io)
    ret
end

function update_checksum(io, chunk_start, chunk_end)
    seek(io, chunk_start)
    cio = begin_checksum_read(io)
    seek(cio, chunk_end)
    seek(io, chunk_end)
    jlwrite(io, end_checksum(cio))
end