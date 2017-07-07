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
    io.curptr = inptr + sizeof(rr)
    obj
end

@inline function read_array!{T}(v::Array{T}, f::JLDFile{MmapIO},
                                rr::ReadRepresentation{T,T})
    io = f.io
    inptr = io.curptr
    n = length(v)
    nb = sizeof(T)*n
    if nb > MMAP_CUTOFF
        # It turns out that regular IO is faster here (at least on OS X)
        mmapio = f.io
        regulario = mmapio.f
        seek(regulario, inptr - io.startptr)
        unsafe_read(regulario, pointer(v), nb)
    else
        unsafe_copy!(pointer(v), convert(Ptr{T}, inptr), n)
    end
    io.curptr = inptr + sizeof(T) * n
    v
end

@inline function read_array!{T,RR}(v::Array{T}, f::JLDFile{MmapIO},
                                   rr::ReadRepresentation{T,RR})
    io = f.io
    inptr = io.curptr
    n = length(v)
    @simd for i = 1:n
        if !jlconvert_canbeuninitialized(rr) || jlconvert_isinitialized(rr, inptr)
            @inbounds v[i] = jlconvert(rr, f, inptr, NULL_REFERENCE)
        end
        inptr += sizeof(RR)
    end
    io.curptr = inptr + sizeof(RR) * n
    v
end

@inline function read_compressed_array!{T,RR}(v::Array{T}, f::JLDFile{MmapIO},
                                              rr::ReadRepresentation{T,RR},
                                              data_length::Int)
    io = f.io
    inptr = io.curptr
    data = read(ZlibInflateInputStream(unsafe_wrap(Array, Ptr{UInt8}(inptr), data_length); gzip=false))
    @simd for i = 1:length(v)
        dataptr = Ptr{Void}(pointer(data, sizeof(RR)*(i-1)+1))
        if !jlconvert_canbeuninitialized(rr) || jlconvert_isinitialized(rr, dataptr)
            @inbounds v[i] = jlconvert(rr, f, dataptr, NULL_REFERENCE)
        end
    end
    io.curptr = inptr + data_length
    v
end

function write_data{S}(io::MmapIO, f::JLDFile, data, odr::S, ::ReferenceFree,
                       wsession::JLDWriteSession)
    io = f.io
    ensureroom(io, sizeof(odr))
    cp = io.curptr
    h5convert!(cp, odr, f, data, wsession)
    io.curptr = cp + sizeof(odr)
    nothing
end

function write_data{S}(io::MmapIO, f::JLDFile, data, odr::S, ::HasReferences,
                       wsession::JLDWriteSession)
    io = f.io
    ensureroom(io, sizeof(odr))
    p = position(io)
    cp = IndirectPointer(io, p)
    h5convert!(cp, odr, f, data, wsession)
    seek(io, p + sizeof(odr))
    nothing
end

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

write_data{T}(io::MmapIO, f::JLDFile, data::Array{T}, odr::Type{T}, ::ReferenceFree,
              wsession::JLDWriteSession) =
    raw_write(io, Ptr{UInt8}(pointer(data)), sizeof(odr) * length(data))

function write_data{T,S}(io::MmapIO, f::JLDFile, data::Array{T}, odr::S, ::ReferenceFree,
                         wsession::JLDWriteSession)
    io = f.io
    ensureroom(io, sizeof(odr) * length(data))
    cp = io.curptr
    @simd for i = 1:length(data)
        @inbounds h5convert!(cp, odr, f, data[i], wsession)
        cp += sizeof(odr)
    end
    io.curptr = cp
    nothing
end

function write_data{T,S}(io::MmapIO, f::JLDFile, data::Array{T}, odr::S, ::HasReferences,
                         wsession::JLDWriteSession)
    io = f.io
    ensureroom(io, sizeof(odr) * length(data))
    p = position(io)
    cp = IndirectPointer(io, p)

    for i = 1:length(data)
        if (isleaftype(T) && isbits(T)) || unsafe_isdefined(data, i)
            @inbounds h5convert!(cp, odr, f, data[i], wsession)
        else
            @inbounds h5convert_uninitialized!(cp, odr)
        end
        cp += sizeof(odr)
    end

    seek(io, cp.offset)
    nothing
end

#
# IOStream/BufferedWriter
#

@inline function read_scalar(f::JLDFile{IOStream}, rr, header_offset::RelOffset)
    r = Vector{UInt8}(sizeof(rr))
    unsafe_read(f.io, pointer(r), sizeof(rr))
    jlconvert(rr, f, pointer(r), header_offset)
end

@inline function read_compressed_array!{T,RR}(v::Array{T}, f::JLDFile{IOStream},
                                              rr::ReadRepresentation{T,RR},
                                              data_length::Int)
    io = f.io
    data_offset = position(io)
    n = length(v)
    data = read(ZlibInflateInputStream(io; gzip=false), UInt8, sizeof(RR)*n)
    @simd for i = 1:n
        dataptr = Ptr{Void}(pointer(data, sizeof(RR)*(i-1)+1))
        if !jlconvert_canbeuninitialized(rr) || jlconvert_isinitialized(rr, dataptr)
            @inbounds v[i] = jlconvert(rr, f, dataptr, NULL_REFERENCE)
        end
    end
    seek(io, data_offset + data_length)
    v
end

@inline function read_array!{T}(v::Array{T}, f::JLDFile{IOStream},
                                rr::ReadRepresentation{T,T})
    unsafe_read(f.io, pointer(v), sizeof(T)*length(v))
    v
end

@inline function read_array!{T,RR}(v::Array{T}, f::JLDFile{IOStream},
                                   rr::ReadRepresentation{T,RR})
    n = length(v)
    nb = sizeof(RR)*n
    io = f.io
    data = read(io, UInt8, nb)
    @simd for i = 1:n
        dataptr = Ptr{Void}(pointer(data, sizeof(RR)*(i-1)+1))
        if !jlconvert_canbeuninitialized(rr) || jlconvert_isinitialized(rr, dataptr)
            @inbounds v[i] = jlconvert(rr, f, dataptr, NULL_REFERENCE)
        end
    end
    v
end

function write_data{S}(io::BufferedWriter, f::JLDFile, data, odr::S, ::DataMode,
                       wsession::JLDWriteSession)
    position = io.position[]
    h5convert!(Ptr{Void}(pointer(io.buffer, position+1)), odr, f, data, wsession)
    io.position[] = position + sizeof(odr)
    nothing
end

function write_data{T}(io::BufferedWriter, f::JLDFile, data::Array{T}, odr::Type{T}, ::ReferenceFree,
                       wsession::JLDWriteSession)
    unsafe_write(io, Ptr{UInt8}(pointer(data)), sizeof(odr) * length(data))
    nothing
end

function write_data{T}(io::IOStream, f::JLDFile, data::Array{T}, odr::Type{T}, ::ReferenceFree,
                       wsession::JLDWriteSession)
    unsafe_write(io, Ptr{UInt8}(pointer(data)), sizeof(odr) * length(data))
    nothing
end

function write_data{T,S}(io::BufferedWriter, f::JLDFile, data::Array{T}, odr::S,
                         ::DataMode, wsession::JLDWriteSession)
    position = io.position[]
    cp = Ptr{Void}(pointer(io.buffer, position+1))
    @simd for i = 1:length(data)
        if (isleaftype(T) && isbits(T)) || unsafe_isdefined(data, i)
            @inbounds h5convert!(cp, odr, f, data[i], wsession)
        else
            @inbounds h5convert_uninitialized!(cp, odr)
        end
        cp += sizeof(odr)
    end
    io.position[] = position + sizeof(odr) * length(data)
    nothing
end

function write_data{T,S}(io::IOStream, f::JLDFile, data::Array{T}, odr::S, wm::DataMode,
                         wsession::JLDWriteSession)
    nb = sizeof(odr) * length(data)
    buf = Vector{UInt8}(nb)
    pos = position(io)
    cp = Ptr{Void}(pointer(buf))
    @simd for i = 1:length(data)
        if (isleaftype(T) && isbits(T)) || unsafe_isdefined(data, i)
            @inbounds h5convert!(cp, odr, f, data[i], wsession)
        else
            @inbounds h5convert_uninitialized!(cp, odr)
        end
        cp += sizeof(odr)
    end
    # We might seek around in the file as a consequence of writing stuff, so seek back. We
    # don't need to worry about this for a BufferedWriter, since it will seek back before
    # writing.
    !isa(wm, ReferenceFree) && seek(io, pos)
    write(io, buf)
    nothing
end
