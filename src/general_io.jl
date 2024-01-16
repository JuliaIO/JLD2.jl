## Requirements for an IO object
#=

Implement for Reading:
Base.position(io)::Int
Base.close(io)
Base.seek(io, pos::Int)
read_bytestring(io)::String # read a null-terminated byte string
Base.skip(io, n::Int)
jlread(io, ::Type{T}) for [U]Int8 and `PlainType`
jlread(io, ::Type{T}, n::Integer) read n elements of type T into a vector
Base.read!(io, vec::Vector{UInt8})
begin_checksum_read(io) # see existing implementations
end_checksum(io)

read_scalar(f::JLDFile{<:CustomIOType}, rr, header_offset)
read_array!(v::Array{T}, f::JLDFile{<:CustomIOType}, rr::ReadRepresentation{T,T}) where T
read_array!(v::Array{T}, f::JLDFile{<:CustomIOType}, rr::ReadRepresentation{T,RR}) where {T,RR}

=#



## Create an IO object which wraps a non-seekable read-only buffer
const MINBUFFERSIZE = 2^9 # should maybe be 2^16 

mutable struct ReadOnlyBuffer{B <: IO} <: IO
    _buf::B
    offset::UInt64 # position of file start in wrapped stream
    data::Vector{UInt8}
    startptr::Ptr{Cvoid}
    curptr::Ptr{Cvoid}
    endptr::Ptr{Cvoid}
    size::UInt64
    checksum_pos::Vector{Int64}
    nchecksum::Int64
    function ReadOnlyBuffer(_buf::IO) 
        offset = position(_buf)
        nb = min(MINBUFFERSIZE, bytesavailable(_buf))
        data = read(_buf, nb)
        startptr = pointer(data)
        curptr = startptr
        endptr = startptr + nb-1
        new{typeof(_buf)}(_buf, offset, data, 
        startptr, curptr, endptr,
        length(data), Int[], 0)
    end
end

Base.position(io::ReadOnlyBuffer) = Int(io.curptr-io.startptr)

Base.close(::ReadOnlyBuffer) = nothing

function Base.seek(io::ReadOnlyBuffer, n::Integer)
    n > io.size && resize!(io, n)
    io.curptr = io.startptr+n
end

function Base.resize!(io::ReadOnlyBuffer, newend::Integer)
    newend < io.size && return
    readmore!(io, newend-io.size)
    if !(newend ≤ io.size)
        throw(EOFError())
    end
end
Base.resize!(io::ReadOnlyBuffer, p::Ptr) = resize!(io, p - io.startptr)

function readmore!(io::ReadOnlyBuffer, nb::Integer=MINBUFFERSIZE)
    nb = min(bytesavailable(io._buf), max(nb, MINBUFFERSIZE))
    bts = read(io._buf, nb)
    append!(io.data, bts)
    io.size += length(bts)
    _updatepointers!(io)
end

function _updatepointers!(io::ReadOnlyBuffer)
    oldstart = io.startptr
    io.startptr = pointer(io.data)
    io.curptr = io.curptr - oldstart + io.startptr
    io.endptr = io.startptr + length(io.data)-1
    nothing
end
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

function Base.skip(io::ReadOnlyBuffer, offset::Integer)
    if  io.curptr+offset > io.endptr
        resize!(io, position(io)+offset)
    end
    io.curptr += offset
    nothing
end

function _read(io::ReadOnlyBuffer, T::DataType)
    ep = io.curptr + jlsizeof(T)
    ep > io.endptr && resize!(io, ep)
    v = jlunsafe_load(Ptr{T}(io.curptr))
    io.curptr += jlsizeof(T)
    v
end
jlread(io::ReadOnlyBuffer, T::Type{UInt8}) = _read(io, T)
jlread(io::ReadOnlyBuffer, T::Type{Int8}) = _read(io, T)
jlread(io::ReadOnlyBuffer, T::PlainType) = _read(io, T)

function jlread(io::ReadOnlyBuffer, ::Type{T}, n::Int) where T
    if io.endptr < io.curptr + jlsizeof(T)*n 
        readmore!(io, jlsizeof(T)*n)
    end
    arr = Vector{T}(undef, n)
    unsafe_copyto!(pointer(arr), Ptr{T}(io.curptr), n)
    io.curptr += jlsizeof(T)*n
    arr
end
jlread(io::ReadOnlyBuffer, ::Type{T}, n::Integer) where {T} = jlread(io, T, Int(n))

function Base.read!(io::ReadOnlyBuffer, vec::Vector{UInt8})
    nb = length(vec) 
    position(io)+nb > io.size && resize!(io, newpos)
    unsafe_copyto!(pointer(vec), Ptr{UInt8}(io.curptr), nb)
    io.curptr += nb
    vec
end

# We sometimes need to compute checksums. We do this by first calling begin_checksum when
# starting to handle whatever needs checksumming, and calling end_checksum afterwards. Note
# that we never compute nested checksums, but we may compute multiple checksums
# simultaneously. This strategy is not thread-safe.

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
## API

function jldopen(io, writable::Bool, create::Bool, truncate::Bool;
                compress=false,
                typemap::Dict{String}=Dict{String,Any}(),
                )

    verify_compressor(compress)

    # figure out what kind of io object this is 
    # for now assume it is
    if !io.readable 
        throw("IO object is not readable")
    end
    if io.seekable && writable && iswritable(io)
        # Here could have a more lightweight wrapper
        # that just ensures API is defined
        created = truncate
        io = RWBuffer(io)
        f = JLDFile(io, "RWBuffer", writable, created, compress, false)
        if created
            f.base_address = 512
            f.root_group = Group{typeof(f)}(f)
            f.types_group =  Group{typeof(f)}(f)
        else
            try
                load_file_metadata!(f)
            catch e
                close(f)
                rethrow(e)
            end
        end
        merge!(f.typemap, typemap)
        return f
    end
    if (false == writable == create == truncate)
        # Were trying to read, so let's hope `io` implements `read`
        # and bytesavailable
        io = ReadOnlyBuffer(io)
        f = JLDFile(io, "ReadOnlyBuffer", false, false, compress, false)
        load_file_metadata!(f)
        merge!(f.typemap, typemap)
        return f
    end
end


function read_scalar(f::JLDFile{<:ReadOnlyBuffer}, @nospecialize(rr), header_offset::RelOffset)::Any
    io = f.io
    inptr = io.curptr
    obj = jlconvert(rr, f, inptr, header_offset)
    io.curptr = inptr + odr_sizeof(rr)
    obj
end


function read_array!(v::Array{T}, f::JLDFile{<:ReadOnlyBuffer},
                             rr::ReadRepresentation{T,T}) where T
    io = f.io
    inptr = io.curptr
    n = length(v)
    unsafe_copyto!(pointer(v), pconvert(Ptr{T}, inptr), n)
    io.curptr = inptr + odr_sizeof(T) * n
    v
end

###########################################################################################
## RWBuffer
###########################################################################################




mutable struct RWBuffer{B <: IO} <: IO
    _buf::B
    offset::UInt64 # position of file start in wrapped stream
    pos::UInt64
    size::UInt64
    
    function RWBuffer(_buf::IO) 
        offset = position(_buf)
        pos = 0
        size = _buf.size
        new{typeof(_buf)}(_buf, offset, pos, size)
    end
end

Base.position(io::RWBuffer) = Int(io.pos)

Base.close(::RWBuffer) = nothing

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

# Read a null-terminated string
function read_bytestring(io::RWBuffer)
    v = readuntil(io._buf, 0x00)
    io.pos = position(io._buf) - io.offset
    return String(v)
end

function Base.skip(io::RWBuffer, offset::Integer)
    seek(io, position(io)+offset)
    nothing
end

function _read(io::RWBuffer, T::DataType)
    if io.pos + jlsizeof(T) > io.size
        throw(EOFError())
    end
    io.pos += jlsizeof(T)
    read(io._buf, T)
end
Base.read(io::RWBuffer, T::Type{UInt8}) = _read(io, T)
Base.read(io::RWBuffer, T::PlainType) = _read(io, T)
Base.write(io::RWBuffer, x::UInt8) = _write(io, x)
Base.write(io::RWBuffer, x::Int8) = _write(io, x)
Base.write(io::RWBuffer, x::String) = _write(io, x)
Base.write(io::RWBuffer, x::Plain) = _write(io, x)
Base.write(io::RWBuffer, x) = _write(io, x)
function Base.write(io::RWBuffer, x::Array{T}) where T
    for y in x
        jlwrite(io, y)
    end
    return length(x) *jlsizeof(T)
end

function _write(io::RWBuffer, x)
    posprev = position(io)
    jlwrite(io._buf, x)    
    io.pos = position(io._buf) - io.offset #+1
    io.size = max(io.size, io.pos)
    return posprev-io.pos
end

begin_checksum_write(io::RWBuffer, sz::Integer) = BufferedWriter(io, sz)
begin_checksum_read(io::RWBuffer) = BufferedReader(io)
Base.bytesavailable(io::RWBuffer) = io.size-io.pos

function read_scalar(f::JLDFile{<:RWBuffer}, rr, header_offset::RelOffset)
    r = Vector{UInt8}(undef, odr_sizeof(rr))
    @GC.preserve r begin
        unsafe_read(f.io, pointer(r), odr_sizeof(rr))
        jlconvert(rr, f, pointer(r), header_offset)
    end
end

function truncate_and_close(io::RWBuffer, endpos::Integer)
    #truncate(io, endpos)
    close(io)
end

#adapted from dataio.jl 246 
function write_data(io::RWBuffer, f::JLDFile, data::Array{T}, odr::S, wm::DataMode,
                    wsession::JLDWriteSession) where {T,S}
    nb = odr_sizeof(odr) * length(data)
    buf = Vector{UInt8}(undef, nb)
    pos = position(io)
    cp = Ptr{Cvoid}(pointer(buf))
    @simd for i = 1:length(data)
        if isassigned(data, i)
            @inbounds h5convert!(cp, odr, f, data[i], wsession)
        else
            @inbounds h5convert_uninitialized!(cp, odr)
        end
        cp += odr_sizeof(odr)
    end
    !isa(wm, ReferenceFree) && seek(io, pos)
    jlwrite(io, buf)
    nothing
end
