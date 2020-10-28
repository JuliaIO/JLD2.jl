const CODECZLIB_ID = UInt16(1)
const BLOSC_ID = UInt16(32001)

function issupported_filter(filter_id)
    filter_id == CODECZLIB_ID && return true #CodecZlib
    filter_id == BLOSC_ID && return true #Blosc
    return false
end

const DEFLATE_PIPELINE_MESSAGE = let
    io = IOBuffer()
    write(io, HeaderMessage(HM_FILTER_PIPELINE, 12, 0))
    write(io, UInt8(2))                 # Version
    write(io, UInt8(1))                 # Number of Filters
    #write(io, UInt16(1))               # Filter Identification Value (= deflate)
    write(io, BLOSC_ID)                 # Use Blosc as default
    write(io, UInt16(0))                # Flags
    write(io, UInt16(1))                # Number of Client Data Values
    write(io, UInt32(5))                # Client Data (Compression Level)
    take!(io)
end

function deflate_data(f::JLDFile, data::Array{T}, odr::S, wsession::JLDWriteSession) where {T,S}
    buf = Vector{UInt8}(undef, odr_sizeof(odr) * length(data))
    cp = Ptr{Cvoid}(pointer(buf))
    @simd for i = 1:length(data)
        @inbounds h5convert!(cp, odr, f, data[i], wsession)
        cp += odr_sizeof(odr)
    end
    Blosc.compress(buf; itemsize=odr_sizeof(odr))
end


@inline function read_compressed_array!(v::Array{T}, f::JLDFile{MmapIO},
                                        rr::ReadRepresentation{T,RR},
                                        data_length::Int,
                                        ::Val{CODECZLIB_ID}) where {T,RR}
    io = f.io
    inptr = io.curptr
    data = transcode(ZlibDecompressor, unsafe_wrap(Array, Ptr{UInt8}(inptr), data_length))
    @simd for i = 1:length(v)
        dataptr = Ptr{Cvoid}(pointer(data, odr_sizeof(RR)*(i-1)+1))
        if !jlconvert_canbeuninitialized(rr) || jlconvert_isinitialized(rr, dataptr)
            @inbounds v[i] = jlconvert(rr, f, dataptr, NULL_REFERENCE)
        end
    end
    io.curptr = inptr + data_length
    v
end

@inline function read_compressed_array!(v::Array{T}, f::JLDFile{IOStream},
                                        rr::ReadRepresentation{T,RR},
                                        data_length::Int,
                                        ::Val{CODECZLIB_ID}) where {T,RR}
    io = f.io
    data_offset = position(io)
    n = length(v)
    data = read!(ZlibDecompressorStream(io), Vector{UInt8}(undef, odr_sizeof(RR)*n))
    @simd for i = 1:n
        dataptr = Ptr{Cvoid}(pointer(data, odr_sizeof(RR)*(i-1)+1))
        if !jlconvert_canbeuninitialized(rr) || jlconvert_isinitialized(rr, dataptr)
            @inbounds v[i] = jlconvert(rr, f, dataptr, NULL_REFERENCE)
        end
    end
    seek(io, data_offset + data_length)
    v
end


@inline function read_compressed_array!(v::Array{T}, f::JLDFile{MmapIO},
                                        rr::ReadRepresentation{T,RR},
                                        data_length::Int,
                                        ::Val{BLOSC_ID}) where {T,RR}
    io = f.io
    inptr = io.curptr
    data = decompress(UInt8, unsafe_wrap(Array, Ptr{UInt8}(inptr), data_length))
    @simd for i = 1:length(v)
        dataptr = Ptr{Cvoid}(pointer(data, odr_sizeof(RR)*(i-1)+1))
        if !jlconvert_canbeuninitialized(rr) || jlconvert_isinitialized(rr, dataptr)
            @inbounds v[i] = jlconvert(rr, f, dataptr, NULL_REFERENCE)
        end
    end
    io.curptr = inptr + data_length
    v
end



@inline function read_compressed_array!(v::Array{T}, f::JLDFile{IOStream},
                                        rr::ReadRepresentation{T,RR},
                                        data_length::Int,
                                        ::Val{BLOSC_ID}) where {T,RR}
    io = f.io
    data_offset = position(io)
    n = length(v)
    buf = read(io, data_length)
    data = decompress(UInt8, buf)
    @simd for i = 1:n
        dataptr = Ptr{Cvoid}(pointer(data, odr_sizeof(RR)*(i-1)+1))
        if !jlconvert_canbeuninitialized(rr) || jlconvert_isinitialized(rr, dataptr)
            @inbounds v[i] = jlconvert(rr, f, dataptr, NULL_REFERENCE)
        end
    end
    seek(io, data_offset + data_length)
    v
end


# Optimizations for primitive types


@inline function read_compressed_array!(v::Array{T}, f::JLDFile{MmapIO},
                                        rr::ReadRepresentation{T,T},
                                        data_length::Int,
                                        ::Val{BLOSC_ID}) where {T,RR}
    io = f.io
    inptr = io.curptr
    decompress!(
        unsafe_wrap(Array, Ptr{T}(pointer(v)), length(v)),
        unsafe_wrap(Array, Ptr{UInt8}(inptr), data_length))
    io.curptr = inptr + data_length
    v
end

function deflate_data(f::JLDFile, data::Array{T}, odr::Type{T}, wsession::JLDWriteSession) where {T,S}
    Blosc.compress(data)
end

function write_compressed_data(cio, f, data, odr, wsession)
    write(cio, DEFLATE_PIPELINE_MESSAGE)
    deflated = deflate_data(f, data, odr, wsession)
    write_chunked_storage_message(cio, odr_sizeof(odr), size(data), length(deflated), h5offset(f, f.end_of_data))
    write(f.io, end_checksum(cio))

    f.end_of_data += length(deflated)
    write(f.io, deflated)
end
