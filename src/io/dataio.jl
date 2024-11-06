const Plain = Union{Int8, Int16,Int32,Int64,Int128,UInt8, UInt16,UInt32,UInt64,UInt128,Float16,Float32,
                    Float64}
const PlainType = Union{Type{Int8}, Type{Int16},Type{Int32}, Type{Int64}, Type{Int128},
                        Type{UInt8}, Type{UInt16}, Type{UInt32}, Type{UInt64}, Type{UInt128}, Type{Float16},
                        Type{Float32}, Type{Float64}}
# JLD2 requires two levels of read / write customization
# These need to use separate function names to avoid ambiguities
# Non-trivial structs get a custom jlread/jlwrite method for generic ::IO
# Different IO types should implement `_read` and `_write` for plain types.
jlwrite(io, x) = Base.write(io, x)
jlwrite(io, x::Plain) = _write(io, x)
_write(io, x) = Base.write(io, x)

jlread(io, T) = Base.read(io, T)
jlread(io, x::PlainType) = _read(io, x)
_read(io, T) = Base.read(io, T)

jlread(io::IO, ::Type{T}, n::Integer) where {T} = T[jlread(io, T) for _=1:n]

Base.read(io::MemoryBackedIO, T::Type{UInt8}) = _read(io, T)

function _read(io::MemoryBackedIO, T::DataType)
    n = jlsizeof(T)
    ensureroom(io, n)
    v = jlunsafe_load(Ptr{T}(io.curptr))
    io.curptr += n
    v
end

function Base.read(io::MemoryBackedIO, ::Type{T}, n::Int) where T
    m = jlsizeof(T) * n
    ensureroom(io, m)
    arr = Vector{T}(undef, n)
    unsafe_copyto!(pointer(arr), Ptr{T}(io.curptr), n)
    io.curptr += m
    arr
end

Base.read(io::MemoryBackedIO, ::Type{T}, n::Integer) where {T} = read(io, T, Int(n))
jlread(io::MemoryBackedIO, ::Type{T}, n::Integer) where {T} = read(io, T, Int(n))


function _write(io::MemoryBackedIO, x)
    n = jlsizeof(x)
    ensureroom(io, n)
    jlunsafe_store!(Ptr{typeof(x)}(io.curptr), x)
    io.curptr += n
    return n
end

function Base.unsafe_write(io::MemoryBackedIO, ptr::Ptr{UInt8}, n::UInt)
    ensureroom(io, n)
    unsafe_copyto!(Ptr{UInt8}(io.curptr), ptr, n)
    io.curptr += n
    n
end

function Base.seek(io::MemoryBackedIO, offset::Integer)
    offset < 0 && throw(ArgumentError("cannot seek before start of file"))
    ensureroom(io, offset-position(io))
    io.curptr = io.startptr + offset
    nothing
end

function Base.skip(io::MemoryBackedIO, offset::Integer)
    ensureroom(io, offset)
    io.curptr += offset
    nothing
end

"""
    read_scalar(f::JLDFile, rr, header_offset::RelOffset)

Read raw data representing a scalar with read representation `rr` from the current position
of JLDFile `f`. `header_offset` is the [`RelOffset`](@ref) of the object header, used to resolve
cycles.
"""
function read_scalar end

"""
    read_array!(v::Array, f::JLDFile, rr)

Fill the array `v` with the contents of JLDFile `f` at the current position, assuming a
[`ReadRepresentation`](@ref) `rr`.
"""
function read_array! end


"""
    read_compressed_array!(v::Array, f::JLDFile, rr, data_length::Int, Val(filter_id))

Fill the array `v` with the compressed contents of JLDFile `f` at the current position,
assuming a [`ReadRepresentation`](@ref) `rr` and that the compressed data has length `data_length`.
"""
function read_compressed_array! end

#
# MmapIO
#

# Cutoff for using ordinary IO instead of copying into mmapped region
const MMAP_CUTOFF = 1048576

@nospecializeinfer function read_scalar(f::JLDFile{<:MemoryBackedIO}, @nospecialize(rr), header_offset::RelOffset)
    io = f.io
    inptr = io.curptr
    obj = jlconvert(rr, f, inptr, header_offset)
    io.curptr = inptr + odr_sizeof(rr)::Int
    obj
end

function read_array!(v::Array{T}, f::JLDFile{<:MemoryBackedIO}, ::SameRepr{T}) where T
    inptr = f.io.curptr
    n = length(v)
    unsafe_copyto!(pointer(v), pconvert(Ptr{T}, inptr), n)
    f.io.curptr = inptr + odr_sizeof(T) * n
    v
end

function read_array!(v::Array{T}, f::JLDFile{<:MemoryBackedIO}, rr::ReadRepresentation{T}) where T
    cp0 = f.io.curptr
    @simd for i in eachindex(v)
        cp = cp0 + (i-1)*odr_sizeof(rr)
        if !jlconvert_canbeuninitialized(rr) || jlconvert_isinitialized(rr, cp)
            v[i] = jlconvert(rr, f, cp, NULL_REFERENCE)
        end
    end
    f.io.curptr = cp0 + odr_sizeof(rr) * length(v)
    v
end

function write_data(io::MemoryBackedIO, f::JLDFile, data, odr, ::ReferenceFree, wsession::JLDWriteSession)
    ensureroom(io, odr_sizeof(odr))
    cp = io.curptr
    h5convert!(cp, odr, f, data, wsession)
    io.curptr == cp || throw(InternalError())
    io.curptr = cp + odr_sizeof(odr)
    nothing
end

function write_data(io::MemoryBackedIO, f::JLDFile, data, odr, ::HasReferences, wsession::JLDWriteSession)
    ensureroom(io, odr_sizeof(odr))
    cp = IndirectPointer(io)
    h5convert!(cp, odr, f, data, wsession)
    io.curptr = pconvert(Ptr{Nothing}, cp) + odr_sizeof(odr)
    nothing
end

function write_data(io::MemoryBackedIO, f::JLDFile, data::Array, odr, ::ReferenceFree, wsession::JLDWriteSession)
    ensureroom(io, odr_sizeof(odr) * length(data))
    cp0 = io.curptr
    @simd for i = 1:length(data)
        cp = cp0 + (i-1)*odr_sizeof(odr)
        @inbounds h5convert!(cp, odr, f, data[i], wsession)
    end
    io.curptr == cp0 || throw(InternalError())
    io.curptr = cp0 + length(data)*odr_sizeof(odr)
    nothing
end

function write_data(io::MemoryBackedIO, f::JLDFile, data::Array, odr::ODR, ::HasReferences, wsession::JLDWriteSession) where ODR
    ensureroom(io, odr_sizeof(odr) * length(data))
    cp = IndirectPointer(io)

    for i = eachindex(data)
        if isassigned(data, i)
            h5convert!(cp, odr, f, data[i], wsession)
        else
            h5convert_uninitialized!(cp, odr)
        end
        cp += odr_sizeof(odr)
    end
    io.curptr = pconvert(Ptr{Nothing}, cp)
    nothing
end

write_data(io::MemoryBackedIO, ::JLDFile, data::Array{T}, odr::Type{T}, ::ReferenceFree, ::JLDWriteSession) where {T} =
    unsafe_write(io, Ptr{UInt8}(pointer(data)), odr_sizeof(odr) * length(data))


#
# Fallback for non-memory-backed IO
#

function read_scalar(f::JLDFile, rr, header_offset::RelOffset)
    r = Vector{UInt8}(undef, odr_sizeof(rr))
    @GC.preserve r begin
        unsafe_read(f.io, pointer(r), odr_sizeof(rr))
        jlconvert(rr, f, pointer(r), header_offset)
    end
end


function read_array!(v::Array{T}, f::JLDFile, ::SameRepr{T}) where {T}
    unsafe_read(f.io, pointer(v), odr_sizeof(T)*length(v))
    v
end

function read_array!(v::Array{T}, f::JLDFile, rr::ReadRepresentation{T}) where {T}
    n = length(v)
    nb = odr_sizeof(rr)*n
    data = read!(f.io, Vector{UInt8}(undef, nb))
    @GC.preserve data begin
        p0 = Ptr{Cvoid}(pointer(data))
        @simd for i = eachindex(v)
            dataptr = p0 + odr_sizeof(rr)*(i-1)
            if !jlconvert_canbeuninitialized(rr) || jlconvert_isinitialized(rr, dataptr)
                v[i] = jlconvert(rr, f, dataptr, NULL_REFERENCE)
            end
        end
    end
    v
end


function write_data(io::IO, f::JLDFile, data::Array{T}, odr::Type{T}, ::ReferenceFree,
                    wsession::JLDWriteSession) where T
    unsafe_write(io, Ptr{UInt8}(pointer(data)), odr_sizeof(odr) * length(data))
    nothing
end

function write_data(io::IO, f::JLDFile, data, odr, _, wsession::JLDWriteSession)
    buf = Vector{UInt8}(undef, odr_sizeof(odr))
    GC.@preserve buf begin
        cp = Ptr{Cvoid}(pointer(buf))
        h5convert!(cp, odr, f, data, wsession)
        unsafe_write(io, Ptr{UInt8}(pointer(buf)), odr_sizeof(odr))
    end
    nothing
end

function write_data(io::IO, f::JLDFile, data::Array, odr, wm::DataMode, wsession::JLDWriteSession)
    nb = odr_sizeof(odr) * length(data)
    buf = Vector{UInt8}(undef, nb)
    pos = position(io)
    cp0 = Ptr{Cvoid}(pointer(buf))
    for i = eachindex(data)
        cp = cp0 + (i-1)*odr_sizeof(odr)
        if isassigned(data, i)
            h5convert!(cp, odr, f, data[i], wsession)
        else
            h5convert_uninitialized!(cp, odr)
        end
    end
    # We might seek around in the file as a consequence of writing stuff, so seek back. We
    # don't need to worry about this for a BufferedWriter, since it will seek back before
    # writing.
    !isa(wm, ReferenceFree) && seek(io, pos)
    jlwrite(io, buf)
    nothing
end


# The delimiter is excluded by default
read_bytestring(io::Union{IOStream, IOBuffer}) = String(readuntil(io, 0x00))

# Late addition for MmapIO that can't be defined in mmapio.jl due to include ordering
function read_array!(v::Array{T}, f::JLDFile{MmapIO}, ::SameRepr{T}) where T
    io = f.io
    inptr = io.curptr
    n = length(v)
    nb = odr_sizeof(T)*n
    if nb > MMAP_CUTOFF && (!Sys.iswindows() || !f.written)
        # It turns out that regular IO is faster here (at least on OS X), but on Windows,
        # we shouldn't use ordinary IO to read, since coherency with the memory map is not
        # guaranteed
        mmapio = f.io
        regulario = mmapio.f
        seek(regulario, inptr - io.startptr)
        unsafe_read(regulario, pointer(v), nb)
    else
        unsafe_copyto!(pointer(v), pconvert(Ptr{T}, inptr), n)
    end
    io.curptr = inptr + nb
    v
end
