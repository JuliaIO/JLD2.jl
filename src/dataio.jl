"""
    read_scalar(f::JLDFile, rr, header_offset::RelOffset)

Read raw data representing a scalar with read representation `rr` from the current position
of JLDFile `f`. `header_offset` is the RelOffset of the object header, used to resolve
cycles.
"""
function read_scalar end

"""
    read_array!(v::Array, f::JLDFile, rr)

Fill the array `v` with the contents of JLDFile `f` at the current position, assuming a
ReadRepresentation `rr`.
"""
function read_array! end


"""
    read_compressed_array!(v::Array, f::JLDFile, rr, data_length::Int)

Fill the array `v` with the compressed contents of JLDFile `f` at the current position,
assuming a ReadRepresentation `rr` and that the compressed data has length `data_length`.
"""
function read_compressed_array! end

#
# MmapIO
#

# Cutoff for using ordinary IO instead of copying into mmapped region
const MMAP_CUTOFF = 1048576

@inline function read_scalar(f::JLDFile{MmapIO}, rr, header_offset::RelOffset)
    io = f.io
    inptr = io.curptr
    obj = jlconvert(rr, f, inptr, header_offset)
    io.curptr = inptr + odr_sizeof(rr)
    obj
end

@inline function read_array!(v::Array{T}, f::JLDFile{MmapIO},
                             rr::ReadRepresentation{T,T}) where T
    io = f.io
    inptr = io.curptr
    n = length(v)
    nb = odr_sizeof(T)*n
    if nb > MMAP_CUTOFF && (!is_windows() || !f.written)
        # It turns out that regular IO is faster here (at least on OS X), but on Windows,
        # we shouldn't use ordinary IO to read, since coherency with the memory map is not
        # guaranteed
        mmapio = f.io
        regulario = mmapio.f
        seek(regulario, inptr - io.startptr)
        unsafe_read(regulario, pointer(v), nb)
    else
        unsafe_copy!(pointer(v), convert(Ptr{T}, inptr), n)
    end
    io.curptr = inptr + odr_sizeof(T) * n
    v
end

@inline function read_array!(v::Array{T}, f::JLDFile{MmapIO},
                             rr::ReadRepresentation{T,RR}) where {T,RR}
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

@inline function read_compressed_array!(v::Array{T}, f::JLDFile{MmapIO},
                                        rr::ReadRepresentation{T,RR},
                                        data_length::Int) where {T,RR}
    io = f.io
    inptr = io.curptr
    data = read(ZlibInflateInputStream(unsafe_wrap(Array, Ptr{UInt8}(inptr), data_length); gzip=false))
    @simd for i = 1:length(v)
        dataptr = Ptr{Void}(pointer(data, odr_sizeof(RR)*(i-1)+1))
        if !jlconvert_canbeuninitialized(rr) || jlconvert_isinitialized(rr, dataptr)
            @inbounds v[i] = jlconvert(rr, f, dataptr, NULL_REFERENCE)
        end
    end
    io.curptr = inptr + data_length
    v
end

function write_data(io::MmapIO, f::JLDFile, data, odr::S, ::ReferenceFree,
                    wsession::JLDWriteSession) where S
    io = f.io
    ensureroom(io, odr_sizeof(odr))
    cp = io.curptr
    h5convert!(cp, odr, f, data, wsession)
    io.curptr == cp || throw(InternalError())
    io.curptr = cp + odr_sizeof(odr)
    nothing
end

function write_data(io::MmapIO, f::JLDFile, data, odr::S, ::HasReferences,
                    wsession::JLDWriteSession) where S
    io = f.io
    ensureroom(io, odr_sizeof(odr))
    p = position(io)
    cp = IndirectPointer(io, p)
    h5convert!(cp, odr, f, data, wsession)
    seek(io, p + odr_sizeof(odr))
    nothing
end

@static if is_unix()
    function raw_write(io::MmapIO, ptr::Ptr{UInt8}, nb::Int)
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

            # Make sure the mapping is encompasses the written data
            ensureroom(io, nb + 1)

            # Seek to the place we just wrote
            seek(io, pos + nb)
        else
            unsafe_write(io, ptr, nb)
        end
        nothing
    end
else
    # Don't use ordinary IO to write files on Windows, since coherency with memory map is
    # not guaranteed
    function raw_write(io::MmapIO, ptr::Ptr{UInt8}, nb::Int)
        unsafe_write(io, ptr, nb)
        nothing
    end
end

write_data(io::MmapIO, f::JLDFile, data::Array{T}, odr::Type{T}, ::ReferenceFree,
           wsession::JLDWriteSession) where {T} =
    raw_write(io, Ptr{UInt8}(pointer(data)), odr_sizeof(odr) * length(data))

function write_data(io::MmapIO, f::JLDFile, data::Array{T}, odr::S, ::ReferenceFree,
                    wsession::JLDWriteSession) where {T,S}
    io = f.io
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

function write_data(io::MmapIO, f::JLDFile, data::Array{T}, odr::S, ::HasReferences,
                    wsession::JLDWriteSession) where {T,S}
    io = f.io
    ensureroom(io, odr_sizeof(odr) * length(data))
    p = position(io)
    cp = IndirectPointer(io, p)

    for i = 1:length(data)
        if (isleaftype(T) && isbits(T)) || unsafe_isdefined(data, i)
            @inbounds h5convert!(cp, odr, f, data[i], wsession)
        else
            @inbounds h5convert_uninitialized!(cp, odr)
        end
        cp += odr_sizeof(odr)
    end

    seek(io, cp.offset)
    nothing
end

#
# IOStream/BufferedWriter
#

@inline function read_scalar(f::JLDFile{IOStream}, rr, header_offset::RelOffset)
    r = Vector{UInt8}(odr_sizeof(rr))
    unsafe_read(f.io, pointer(r), odr_sizeof(rr))
    jlconvert(rr, f, pointer(r), header_offset)
end

@inline function read_compressed_array!(v::Array{T}, f::JLDFile{IOStream},
                                        rr::ReadRepresentation{T,RR},
                                        data_length::Int) where {T,RR}
    io = f.io
    data_offset = position(io)
    n = length(v)
    data = read(ZlibInflateInputStream(io; gzip=false), UInt8, odr_sizeof(RR)*n)
    @simd for i = 1:n
        dataptr = Ptr{Void}(pointer(data, odr_sizeof(RR)*(i-1)+1))
        if !jlconvert_canbeuninitialized(rr) || jlconvert_isinitialized(rr, dataptr)
            @inbounds v[i] = jlconvert(rr, f, dataptr, NULL_REFERENCE)
        end
    end
    seek(io, data_offset + data_length)
    v
end

@inline function read_array!(v::Array{T}, f::JLDFile{IOStream},
                             rr::ReadRepresentation{T,T}) where T
    unsafe_read(f.io, pointer(v), odr_sizeof(T)*length(v))
    v
end

@inline function read_array!(v::Array{T}, f::JLDFile{IOStream},
                             rr::ReadRepresentation{T,RR}) where {T,RR}
    n = length(v)
    nb = odr_sizeof(RR)*n
    io = f.io
    data = read(io, UInt8, nb)
    @simd for i = 1:n
        dataptr = Ptr{Void}(pointer(data, odr_sizeof(RR)*(i-1)+1))
        if !jlconvert_canbeuninitialized(rr) || jlconvert_isinitialized(rr, dataptr)
            @inbounds v[i] = jlconvert(rr, f, dataptr, NULL_REFERENCE)
        end
    end
    v
end

function write_data(io::BufferedWriter, f::JLDFile, data, odr::S, ::DataMode,
                    wsession::JLDWriteSession) where S
    position = io.position[]
    h5convert!(Ptr{Void}(pointer(io.buffer, position+1)), odr, f, data, wsession)
    io.position[] = position + odr_sizeof(odr)
    nothing
end

function write_data(io::BufferedWriter, f::JLDFile, data::Array{T}, odr::Type{T}, ::ReferenceFree,
                    wsession::JLDWriteSession) where T
    unsafe_write(io, Ptr{UInt8}(pointer(data)), odr_sizeof(odr) * length(data))
    nothing
end

function write_data(io::IOStream, f::JLDFile, data::Array{T}, odr::Type{T}, ::ReferenceFree,
                    wsession::JLDWriteSession) where T
    unsafe_write(io, Ptr{UInt8}(pointer(data)), odr_sizeof(odr) * length(data))
    nothing
end

function write_data(io::BufferedWriter, f::JLDFile, data::Array{T}, odr::S,
                    ::DataMode, wsession::JLDWriteSession) where {T,S}
    position = io.position[]
    cp = Ptr{Void}(pointer(io.buffer, position+1))
    @simd for i = 1:length(data)
        if (isleaftype(T) && isbits(T)) || unsafe_isdefined(data, i)
            @inbounds h5convert!(cp, odr, f, data[i], wsession)
        else
            @inbounds h5convert_uninitialized!(cp, odr)
        end
        cp += odr_sizeof(odr)
    end
    io.position[] = position + odr_sizeof(odr) * length(data)
    nothing
end

function write_data(io::IOStream, f::JLDFile, data::Array{T}, odr::S, wm::DataMode,
                    wsession::JLDWriteSession) where {T,S}
    nb = odr_sizeof(odr) * length(data)
    buf = Vector{UInt8}(nb)
    pos = position(io)
    cp = Ptr{Void}(pointer(buf))
    @simd for i = 1:length(data)
        if (isleaftype(T) && isbits(T)) || unsafe_isdefined(data, i)
            @inbounds h5convert!(cp, odr, f, data[i], wsession)
        else
            @inbounds h5convert_uninitialized!(cp, odr)
        end
        cp += odr_sizeof(odr)
    end
    # We might seek around in the file as a consequence of writing stuff, so seek back. We
    # don't need to worry about this for a BufferedWriter, since it will seek back before
    # writing.
    !isa(wm, ReferenceFree) && seek(io, pos)
    write(io, buf)
    nothing
end
