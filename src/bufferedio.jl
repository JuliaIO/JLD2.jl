#
# BufferedIO
#

const DEFAULT_BUFFER_SIZE = 1024

struct BufferedWriter <: IO
    f::IOStream
    buffer::Vector{UInt8}
    file_position::Int64
    position::Base.RefValue{Int}
end

function BufferedWriter(io::IOStream, buffer_size::Int)
    pos = position(io)
    skip(io, buffer_size)
    BufferedWriter(io, Vector{UInt8}(undef, buffer_size), pos, Ref{Int}(0))
end
Base.show(io::IO, ::BufferedWriter) = print(io, "BufferedWriter")

function finish!(io::BufferedWriter)
    f = io.f
    buffer = io.buffer
    io.position[] == length(buffer) ||
        error("buffer not written to end; position is $(io.position[]) but length is $(length(buffer))")
    seek(f, io.file_position)
    jlwrite(f, buffer)
    io.position[] = 0
    nothing
end

@inline function _write(io::BufferedWriter, x)
    position = io.position[]
    buffer = io.buffer
    n = jlsizeof(x)
    n + position <= length(buffer) || throw(EOFError())
    io.position[] = position + n
    jlunsafe_store!(Ptr{typeof(x)}(pointer(buffer, position+1)), x)
    # Base.show_backtrace(STDOUT, backtrace())
    # gc()
    return n
end
@inline jlwrite(io::BufferedWriter, x::UInt8) = _write(io, x)
@inline jlwrite(io::BufferedWriter, x::Int8) = _write(io, x)
@inline jlwrite(io::BufferedWriter, x::Plain)  = _write(io, x)
@inline Base.write(io::BufferedWriter, x::UInt8) = _write(io, x)
@inline Base.write(io::BufferedWriter, x::Int8) = _write(io, x)
@inline Base.write(io::BufferedWriter, x::Plain)  = _write(io, x)

function Base.unsafe_write(io::BufferedWriter, x::Ptr{UInt8}, n::UInt64)
    buffer = io.buffer
    position = io.position[]
    n + position <= length(buffer) || throw(EOFError())
    unsafe_copyto!(pointer(buffer, position+1), x, n)
    io.position[] = position + n
    return n
end

Base.position(io::BufferedWriter) = io.file_position + io.position[]

struct BufferedReader <: IO
    f::IOStream
    buffer::Vector{UInt8}
    file_position::Int64
    position::Base.RefValue{Int}
end

BufferedReader(io::IOStream) =
    BufferedReader(io, Vector{UInt8}(), position(io), Ref{Int}(0))
Base.show(io::IO, ::BufferedReader) = print(io, "BufferedReader")

function readmore!(io::BufferedReader, n::Integer)
    f = io.f
    amount = max(bytesavailable(f), n)
    buffer = io.buffer
    oldlen = length(buffer)
    resize!(buffer, oldlen + amount)
    unsafe_read(f, pointer(buffer, oldlen+1), amount)
end

@inline function _read(io::BufferedReader, T::DataType)
    position = io.position[]
    buffer = io.buffer
    if length(buffer) - position < jlsizeof(T)
        readmore!(io, jlsizeof(T))
    end
    io.position[] = position + jlsizeof(T)
    jlunsafe_load(Ptr{T}(pointer(buffer, position+1)))
end
@inline jlread(io::BufferedReader, T::Type{UInt8}) = _read(io, T)
@inline jlread(io::BufferedReader, T::Type{Int8}) = _read(io, T)
@inline jlread(io::BufferedReader, T::PlainType) = _read(io, T)

function jlread(io::BufferedReader, ::Type{T}, n::Int) where T
    position = io.position[]
    buffer = io.buffer
    m = jlsizeof(T) * n
    if length(buffer) - position < m
        readmore!(io, m)
    end
    io.position[] = position + m
    arr = Vector{T}(undef, n)
    unsafe_copyto!(pointer(arr), Ptr{T}(pointer(buffer, position+1)), n)
    arr
end
jlread(io::BufferedReader, ::Type{T}, n::Integer) where {T} =
    jlread(io, T, Int(n))

Base.position(io::BufferedReader) = io.file_position + io.position[]

function adjust_position!(io::BufferedReader, position::Integer)
    if position < 0
        throw(ArgumentError("cannot seek before start of buffer"))
    elseif position > length(io.buffer)
        readmore!(io, position - length(io.buffer))
    end
    io.position[] = position
end

Base.seek(io::BufferedReader, offset::Integer) =
    adjust_position!(io, offset - io.file_position)

Base.skip(io::BufferedReader, offset::Integer) =
    adjust_position!(io, io.position[] + offset)

finish!(io::BufferedReader) =
    seek(io.f, io.file_position + io.position[])

function truncate_and_close(io::IOStream, endpos::Integer)
    truncate(io, endpos)
    close(io)
end


# We sometimes need to compute checksums. We do this by first calling begin_checksum when
# starting to handle whatever needs checksumming, and calling end_checksum afterwards. Note
# that we never compute nested checksums, but we may compute multiple checksums
# simultaneously.

function begin_checksum_read(io::IOStream)
    BufferedReader(io)
end
function begin_checksum_write(io::IOStream, sz::Integer)
    BufferedWriter(io, sz)
end
function end_checksum(io::Union{BufferedReader,BufferedWriter})
    ret = Lookup3.hash(io.buffer, 1, io.position[])
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