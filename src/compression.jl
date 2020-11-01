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

const MAX_CHUNK_SIZE = 2^30

function deflate_data(f::JLDFile, data::Array{T}, odr::S, wsession::JLDWriteSession) where {T,S}
    @show el_size= odr_sizeof(odr)
    @show num_el = length(data)
    #num_buffers = (num_el*el_size) ÷ MAX_CHUNK_SIZE + 1 # Approximately
    buffers = Vector{UInt8}[]
    idx = 1
    while idx <= num_el
        num_el_buffer = min(num_el-idx+1, MAX_CHUNK_SIZE ÷ el_size)
        buf = Vector{UInt8}(undef, el_size * num_el_buffer)
        @info "$(el_size*num_el_buffer) bytes in buffer indices $(extrema(range(idx, length=num_el_buffer)))"
        cp = Ptr{Cvoid}(pointer(buf))
        @simd for i = range(idx, length=num_el_buffer)
            @inbounds h5convert!(cp, odr, f, data[i], wsession)
            cp += el_size
        end
        bufc = Blosc.compress(buf; itemsize=el_size)
        @info "Size after compression $(length(bufc))"
        push!(buffers, bufc)
        idx += num_el_buffer
    end
    buffers
end

#= function deflate_data(f::JLDFile, data::Array{T}, odr::Type{T}, wsession::JLDWriteSession) where {T,S}
    Blosc.compress(data)
end =#


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
    @show data_length
    @show size(v)
    io = f.io
    if data_length != -1
        inptr = io.curptr
        decompress!(
            unsafe_wrap(Array, Ptr{T}(pointer(v)), length(v)),
            unsafe_wrap(Array, Ptr{UInt8}(inptr), data_length))
        io.curptr = inptr + data_length
        return v
    end
    header_offset = position(io)
    # If we got to here then curptr should point to fixed array header
    cio = begin_checksum_read(io)
    @show String(read!(cio, Vector{UInt8}(undef, 4))) == "FAHD"
    read(cio, UInt8) == 0 # Version
    read(cio, UInt8) == 1 # Client ID
    read(cio, UInt8)
    read(cio, UInt8)
    @show numchunks = read(cio, Length)
    datablockaddress = read(cio, RelOffset)
    @show end_checksum(cio) == read(io, UInt32)

    seek(io, fileoffset(f, datablockaddress))

    cio = begin_checksum_read(io)
    @show String(read!(cio, Vector{UInt8}(undef, 4))) == "FADB"
    read(cio, UInt8)
    read(cio, UInt8)
    header_address = read(cio, RelOffset)
    @show fileoffset(f, header_address) == header_offset
    entries = map(1:numchunks) do _
        read(cio, FixedArrayFilteredChunk)
    end
    @show end_checksum(cio) == read(io, UInt32)    
    @show entries
    idx = 1
    for entry in entries
        seek(io, fileoffset(f, entry.address))
        buf = decompress(T, unsafe_wrap(Array, Ptr{UInt8}(io.curptr), entry.chunksize))
        @show size(buf)
        v[idx:(idx+length(buf)-1)] .= buf
        idx += length(buf)
    end
    v
end

#= function deflate_data(f::JLDFile, data::Array{T}, odr::Type{T}, wsession::JLDWriteSession) where {T,S}
    Blosc.compress(data)
end =#



@inline chunked_storage_message_size(ndims::Int) =
    #sizeof(HeaderMessage) + 5 + (ndims+1)*sizeof(Length) + 1 + sizeof(Length) + 4 + sizeof(RelOffset)
    sizeof(HeaderMessage) + 5 + (ndims+1)*sizeof(Length) + 1 + 1 + sizeof(RelOffset)


function write_chunked_storage_message(
    io::IO,
    elsize::Int,
    dims::NTuple{N,Int},
    #filtered_size::Int,
    offset::RelOffset) where N
    write(io, HeaderMessage(HM_DATA_LAYOUT, chunked_storage_message_size(N) - sizeof(HeaderMessage), 0))
    write(io, UInt8(4))                     # Version
    write(io, UInt8(LC_CHUNKED_STORAGE))    # Layout Class
    write(io, UInt8(2))                     # Flags (= SINGLE_INDEX_WITH_FILTER)
    write(io, UInt8(N+1))                   # Dimensionality
    write(io, UInt8(sizeof(Length)))        # Dimensionality Size
    for i = N:-1:1
        write(io, Length(dims[i]))          # Dimensions 1...N
    end
    write(io, Length(elsize))               # Element size (last dimension)
    #write(io, UInt8(1))   #3                # Chunk Indexing Type (= Single Chunk)
    write(io, UInt8(3))                     # Fixed Array Index
    write(io, UInt8(8))                     # Number of bits needed for storing number of entries in data block page
    #write(io, Length(filtered_size))        # Size of filtered chunk
    #write(io, UInt32(0))                    # Filters for chunk
    write(io, offset)                       # Address
end

fixed_array_header_size() = 
    4+1+1+1+1+sizeof(Length)+odr_sizeof(RelOffset)+4

function write_fixed_array_header(io::IO, numchunks::Int, datablockaddress::RelOffset, entrysize)
    cio = begin_checksum_write(io, 4+1+1+1+1+sizeof(Length)+odr_sizeof(RelOffset)+4)
    write(cio, "FAHD")   # Signature
    write(cio, UInt8(0)) # Version
    write(cio, UInt8(1)) # Client ID (0 for unfiltered, 1 for compressed)
    # Entry Size (sizeof(address) + sizeof(chunksize) + filter mask)
    # Entry Size (sizeof(address)) # unfiltered
    write(cio, UInt8(entrysize)) 
    write(cio, UInt8(0)) # Page bits Maybe try writing UInt8(0) to deactivate ?
    write(cio, Length(numchunks))   # Max Num entries 
    write(cio, datablockaddress)    # Data Block Address
    write(io, end_checksum(cio))    # Checksum
end

struct FixedArrayFilteredChunk
    address::RelOffset # Adress of chunk in file
    chunksize::UInt64 # Number of bytes in chunk
    filter_mask::UInt32 # Indicates the filter to skip for the dataset chunk. Each filter has an index number in the pipeline; if that filter is skipped, the bit corresponding to its index is set. 
    # No idea what the last thing means... also size not clear
end
odr_sizeof(::Type{FixedArrayFilteredChunk}) = odr_sizeof(RelOffset) + 8 + 4
function Base.write(io::IO, x::FixedArrayFilteredChunk)
    write(io, x.address)
    write(io, x.chunksize)
    write(io, x.filter_mask)
end

function Base.read(io::IO, ::Type{FixedArrayFilteredChunk})
    FixedArrayFilteredChunk(read(io,RelOffset), read(io, UInt64), read(io, UInt32))
end
struct FixedArrayChunk
    address::RelOffset
end

fixed_array_data_block_size(numentries::Int, filter::Bool) = 
    4 + 1 + 1 + odr_sizeof(RelOffset) + numentries*(odr_sizeof(RelOffset) + filter*(8+4)) + 4


function write_fixed_array_data_block(io::IO, header_address, entries)
    cio = begin_checksum_write(io, 4+1+1+odr_sizeof(RelOffset)+length(entries)*odr_sizeof(eltype(entries))+4)
    write(cio, "FADB")      # Signature
    write(cio, UInt8(0))    # Version
    write(cio, UInt8(1))    # Client ID (0 for unfiltered, 1 for compressed)
    write(cio, header_address) # Header Address
    # Page Bitmap (only if data block is paged) so don't do this
    # Elements - so maybe list of 
    for entry in entries
        write(cio, entry)
    end
    write(io, end_checksum(cio))    # Checksum
end

function write_compressed_data(cio, f, data, odr, wsession)
    write(cio, DEFLATE_PIPELINE_MESSAGE)
    #write_chunked_storage_message(cio, odr_sizeof(odr), size(data), length(deflated), h5offset(f, f.end_of_data))
    # No more than 256 chunks maybe?
    write_chunked_storage_message(cio, odr_sizeof(odr), size(data), h5offset(f, f.end_of_data))
    write(f.io, end_checksum(cio))
    header_address = h5offset(f, f.end_of_data)
    println("Writing at $(Int(f.io.curptr-f.io.startptr)) and end of data is $(f.end_of_data)")
    f.end_of_data += fixed_array_header_size()
    io = f.io
    deflated_buffers = deflate_data(f, data, odr, wsession)
    #deflated = deflate_data(f, data, odr, wsession)
    numchunks = length(deflated_buffers)

    cp = io.curptr
    write_fixed_array_header(io, numchunks, h5offset(f, f.end_of_data), odr_sizeof(FixedArrayFilteredChunk))
    println("True size fixed array header $(Int(io.curptr - cp)), $(fixed_array_header_size())")

    println("Writing at $(Int(f.io.curptr-f.io.startptr)) and end of data is $(f.end_of_data)")

    f.end_of_data += fixed_array_data_block_size(numchunks, true)

    #entries = [FixedArrayFilteredChunk(h5offset(f, f.end_of_data), length(deflated), UInt64(0))]
    entries = [FixedArrayFilteredChunk(h5offset(f, f.end_of_data + sum(length.(deflated_buffers[1:(i-1)]))), length(deflated_buffers[i]), UInt32(32001)) for i=1:length(deflated_buffers)]
    @show sum(length.(deflated_buffers[1:(1-1)]))
    @show entries
    cp = io.curptr
    write_fixed_array_data_block(io::IO, header_address, entries)
    println("True size fixed array datablock $(Int(io.curptr - cp)), $(fixed_array_data_block_size(numchunks, true))")
    println("Writing at $(Int(f.io.curptr-f.io.startptr)) and end of data is $(f.end_of_data)")
    f.end_of_data += sum(length.(deflated_buffers))
    #f.end_of_data += length(deflated)
    for buf in deflated_buffers
        write(f.io, buf)
    end
    #write(f.io, deflated)
    print("End at $(Int(f.io.curptr-f.io.startptr)) with end of data at $(f.end_of_data)")
end



