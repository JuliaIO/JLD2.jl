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

function read_scalar(f::JLDFile{<:MemoryBackedIO}, @nospecialize(rr), header_offset::RelOffset)
    io = f.io
    inptr = io.curptr
    obj = jlconvert(rr, f, inptr, header_offset)
    io.curptr = inptr + odr_sizeof(rr)
    obj
end

function read_array!(v::Array{T}, f::JLDFile{<:MemoryBackedIO}, rr::ReadRepresentation{T,T}) where T
    io = f.io
    inptr = io.curptr
    n = length(v)
    unsafe_copyto!(pointer(v), pconvert(Ptr{T}, inptr), n)
    io.curptr = inptr + odr_sizeof(T) * n
    v
end

function read_array!(v::Array{T}, f::JLDFile{MmapIO}, rr::ReadRepresentation{T,T}) where T
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
    io.curptr = inptr + odr_sizeof(T) * n
    v
end

function read_array!(v::Array{T}, f::JLDFile{IO},
                             rr::ReadRepresentation{T,RR}) where {T,RR, IO<:MemoryBackedIO}
    io = f.io
    inptr = io.curptr
    n = length(v)
    @simd for i = 1:n
        if !jlconvert_canbeuninitialized(rr) || jlconvert_isinitialized(rr, inptr)
            @inbounds v[i] = jlconvert(rr, f, inptr, NULL_REFERENCE)
        end
        inptr += odr_sizeof(RR)
    end
    io.curptr = inptr + odr_sizeof(RR) * n
    v
end

function write_data(io::MemoryBackedIO, f::JLDFile, data, odr::S, ::ReferenceFree,
                    wsession::JLDWriteSession) where S
    ensureroom(io, odr_sizeof(odr))
    cp = io.curptr
    h5convert!(cp, odr, f, data, wsession)
    io.curptr == cp || throw(InternalError())
    io.curptr = cp + odr_sizeof(odr)
    nothing
end

function write_data(io::MemoryBackedIO, f::JLDFile, data, odr::S, ::HasReferences,
                    wsession::JLDWriteSession) where S
    ensureroom(io, odr_sizeof(odr))
    cp = IndirectPointer(io)
    h5convert!(cp, odr, f, data, wsession)
    io.curptr = pconvert(Ptr{Nothing}, cp) + odr_sizeof(odr)
    nothing
end

function write_data(io::MemoryBackedIO, f::JLDFile, data::Array{T}, odr::S, ::ReferenceFree,
                    wsession::JLDWriteSession) where {T,S}
    ensureroom(io, odr_sizeof(odr) * length(data))
    cp = cporig = io.curptr
    @simd for i = 1:length(data)
        @inbounds h5convert!(cp, odr, f, data[i], wsession)
        cp += odr_sizeof(odr)
    end
    io.curptr == cporig || throw(InternalError())
    io.curptr = cp
    nothing
end

function write_data(io::MemoryBackedIO, f::JLDFile, data::Array{T}, odr::S, ::HasReferences,
                    wsession::JLDWriteSession) where {T,S}
    ensureroom(io, odr_sizeof(odr) * length(data))
    cp = IndirectPointer(io)
    
    for i = 1:length(data)
        if isassigned(data, i)
            @inbounds h5convert!(cp, odr, f, data[i], wsession)
        else
            @inbounds h5convert_uninitialized!(cp, odr)
        end
        cp += odr_sizeof(odr)
    end
    io.curptr = pconvert(Ptr{Nothing}, cp)
    nothing
end

#
# IOStream/BufferedWriter
#

function read_scalar(f::JLDFile, rr, header_offset::RelOffset)
    r = Vector{UInt8}(undef, odr_sizeof(rr))
    @GC.preserve r begin
        unsafe_read(f.io, pointer(r), odr_sizeof(rr))
        jlconvert(rr, f, pointer(r), header_offset)
    end
end


function read_array!(v::Array{T}, f::JLDFile, rr::ReadRepresentation{T,T}) where {T}
    unsafe_read(f.io, pointer(v), odr_sizeof(T)*length(v))
    v
end

function read_array!(v::Array{T}, f::JLDFile, rr::ReadRepresentation{T,RR}) where {T,RR}
    n = length(v)
    nb = odr_sizeof(RR)*n
    io = f.io
    data = read!(io, Vector{UInt8}(undef, nb))
    @GC.preserve data begin
        @simd for i = 1:n
            dataptr = Ptr{Cvoid}(pointer(data, odr_sizeof(RR)*(i-1)+1))
            if !jlconvert_canbeuninitialized(rr) || jlconvert_isinitialized(rr, dataptr)
                @inbounds v[i] = jlconvert(rr, f, dataptr, NULL_REFERENCE)
            end
        end
    end
    v
end

write_data(io::MemoryBackedIO, ::JLDFile, data::Array{T}, odr::Type{T}, ::ReferenceFree, ::JLDWriteSession) where {T} =
    unsafe_write(io, Ptr{UInt8}(pointer(data)), odr_sizeof(odr) * length(data))

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

function write_data(io::IO, f::JLDFile, data::Array{T}, odr::S, wm::DataMode,
                    wsession::JLDWriteSession) where {T,S}
    nb = odr_sizeof(odr) * length(data)
    buf = Vector{UInt8}(undef, nb)
    pos = position(io)
    cp0 = Ptr{Cvoid}(pointer(buf))
    for i = 1:length(data)
        cp = cp0 + (i-1)*odr_sizeof(odr)
        if isassigned(data, i)
            @inbounds h5convert!(cp, odr, f, data[i], wsession)
        else
            @inbounds h5convert_uninitialized!(cp, odr)
        end
    end
    # We might seek around in the file as a consequence of writing stuff, so seek back. We
    # don't need to worry about this for a BufferedWriter, since it will seek back before
    # writing.
    !isa(wm, ReferenceFree) && seek(io, pos)
    jlwrite(io, buf)
    nothing
end
