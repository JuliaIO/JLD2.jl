const COMPRESSOR_TO_ID = Dict(
    :ZlibCompressor => UInt16(1),
    :ShuffleFilter => UInt16(2),
    :Bzip2Compressor => UInt16(307),
    #:BloscCompressor => UInt16(32001),
    :LZ4FrameCompressor => UInt16(32004),
    :ZstdCompressor => UInt16(32015),
    )

# For loading need filter_ids as keys
const ID_TO_DECOMPRESSOR = Dict(
    UInt16(1) => (:CodecZlib, :ZlibCompressor, :ZlibDecompressor, ""),
    UInt16(2) => (:JLD2, :ShuffleFilter, :ShuffleFilter, ""),
    UInt16(307) => (:CodecBzip2, :Bzip2Compressor, :Bzip2Decompressor, "BZIP2"),
    #UInt16(32001) => (:Blosc, :BloscCompressor, :BloscDecompressor, "BLOSC"),
    UInt16(32004) => (:CodecLz4, :LZ4FrameCompressor, :LZ4FrameDecompressor, "LZ4"),
    UInt16(32015) => (:CodecZstd, :ZstdFrameCompressor, :ZstdDecompressor, "ZSTD"),
)

const compressor_list_string = map(values(ID_TO_DECOMPRESSOR)) do val
    "\n\t"*string(val[1])*"."*string(val[2])
end

issupported_filter(filter_id) = filter_id ∈ keys(ID_TO_DECOMPRESSOR)

function verify_compressor(compressor)
    (compressor isa Bool || haskey(COMPRESSOR_TO_ID, nameof(typeof(compressor)))) && return

    throw(ArgumentError("""Unsupported Compressor
    Supported Compressors are $(compressor_list_string...)"""))
end
#############################################################################################################
# Dynamic Package Loading Logic copied from FileIO
const load_locker = Base.ReentrantLock()


function checked_import(pkg::Symbol)
    lock(load_locker) do
        for m in Base.loaded_modules_array()
            (Symbol(m) == pkg) && return false, m
        end
        @info "Attempting to dynamically load $pkg"
        @eval Base.__toplevel__  import $pkg
        for m in Base.loaded_modules_array()
            (Symbol(m) == pkg) && return true, m
        end
        throw(InternalError("Module $pkg could not be loaded."))
    end
end

#############################################################################################################

# jld2.jl 341
@nospecializeinfer Base.write(f::JLDFile, name::AbstractString, @nospecialize(obj), wsession::JLDWriteSession=JLDWriteSession(); compress=nothing) =
    write(f.root_group, name, obj, wsession; compress=compress)

# groups.jl 112
@nospecializeinfer function Base.write(g::Group, name::AbstractString, @nospecialize(obj), wsession::JLDWriteSession=JLDWriteSession(); compress=nothing)
    f = g.f
    prewrite(f)
    (g, name) = pathize(g, name, true)
    if !(compress === nothing)
        verify_compressor(compress)
        if obj isa Array
            g[name] = write_dataset(f, obj, wsession, compress)
            return nothing
        end
        @warn "Only arrays can be compressed."
    end
    g[name] = write_dataset(f, obj, wsession)
    nothing
end


get_compressor(compressor) = false, COMPRESSOR_TO_ID[nameof(typeof(compressor))], compressor

function get_compressor(::Bool)
   call_again, m = checked_import(:CodecZlib)
    if call_again || !applicable(m.ZlibCompressor)
        # Reinvoke with latest world age if
        # - we just loaded the CodecZlib
        # - we didn't just load it but the constructor is still not `applicable`.
        #   This happens when a save call wants to compress multiple datasets
        #   and loaded CodecZlib for the first one.
        _, filter_id, compressor = Base.invokelatest(get_compressor, true)
        filter_id::UInt16
        return true, filter_id, compressor
    end
    false, COMPRESSOR_TO_ID[:ZlibCompressor], m.ZlibCompressor()
end

function get_compressor(filter_id::UInt16)
    modname, compressorname, decompressorname, = ID_TO_DECOMPRESSOR[filter_id]
    invoke_again, m = checked_import(modname)
    if invoke_again || !applicable(getproperty(m,compressorname))
        _, compressor = Base.invokelatest(get_compressor, filter_id)
        return true, compressor
    end
    return invoke_again, getproperty(m,compressorname)()
end
function get_decompressor(filter_id::UInt16)
    modname, compressorname, decompressorname, = ID_TO_DECOMPRESSOR[filter_id]
    invoke_again, m = checked_import(modname)
    return invoke_again, getproperty(m,decompressorname)()
end
function get_decompressor(filters::FilterPipeline)
    decompressors = Any[]
    invoke_again = false
    for filter in filters.filters
        modname, compressorname, decompressorname, = ID_TO_DECOMPRESSOR[filter.id]
        invoke_again, m = checked_import(modname)
        push!(decompressors, getproperty(m,decompressorname)())
    end
    return invoke_again, decompressors
end

function pipeline_message_size(filter_id) 
    sz = 4 + 12
    if (filter_id > 255)
        sz += 2
        filter_name = ID_TO_DECOMPRESSOR[filter_id][4]
        fnamelen = length(filter_name)+1
        fnamelen += 8-mod1(fnamelen, 8)
        sz += fnamelen
    end
    sz
end
function write_filter_pipeline_message(io, filter_id::UInt16)
    hmsize = 12
    if filter_id > 255
        filter_name = ID_TO_DECOMPRESSOR[filter_id][4]
        fnamelen = length(filter_name)+1
        fnamelen += 8-mod1(fnamelen, 8)
        padding = fnamelen - length(filter_name)
        hmsize += 2 + fnamelen
    end
    jlwrite(io, HeaderMessage(HmFilterPipeline, hmsize, 0))
    jlwrite(io, UInt8(2))                 # Version
    jlwrite(io, UInt8(1))                 # Number of Filters
    jlwrite(io, filter_id)                # Filter Identification Value
    filter_id > 255 && jlwrite(io, UInt16(fnamelen))
                                        # Length of Filter Name
    jlwrite(io, UInt16(0))                # Flags
    jlwrite(io, UInt16(1))                # Number of Client Data Values
    filter_id > 255 && jlwrite(io, filter_name) # Filter Name
    filter_id > 255 && (padding > 0) && jlwrite(io, zeros(UInt8, padding))
    jlwrite(io, UInt32(5))                # Client Data (Compression Level)
    nothing
end


function deflate_data(f::JLDFile, data::Array{T}, odr::S, wsession::JLDWriteSession,
                      compressor) where {T,S}
    buf = Vector{UInt8}(undef, odr_sizeof(odr) * length(data))
    cp = Ptr{Cvoid}(pointer(buf))
    @simd for i = 1:length(data)
        @inbounds h5convert!(cp, odr, f, data[i], wsession)
        cp += odr_sizeof(odr)
    end
    TranscodingStreams.initialize(compressor)
    res = transcode(compressor, buf)
    TranscodingStreams.finalize(compressor)
    res
end


chunked_storage_message_size(ndims::Int) =
    jlsizeof(HeaderMessage) + 5 + (ndims+1)*jlsizeof(Length) + 1 + jlsizeof(Length) + 4 + jlsizeof(RelOffset)


function write_chunked_storage_message( io::IO,
                                        elsize::Int,
                                        dims::NTuple{N,Int},
                                        filtered_size::Int,
                                        data_address::RelOffset) where N
    write_header_message(io, Val(HmDataLayout);
        layout_class = LcChunked,
        flags = 2,  # (= SINGLE_INDEX_WITH_FILTER)
        dimensions = UInt64.((reverse(dims)..., elsize)), # Reversed dimensions with element size as last dim
        chunk_indexing_type = 1,  # (= Single Chunk)
        data_size = filtered_size,
        filters = 0, # Filters for chunk
        data_address)  
end

function decompress!(io::MemoryBackedIO, data_length, element_size, n, decompressor::TranscodingStreams.Codec)
    ensureroom(io, data_length)
    inptr = io.curptr
    TranscodingStreams.initialize(decompressor)
    data = transcode(decompressor, unsafe_wrap(Array, Ptr{UInt8}(inptr), data_length))::Array{UInt8, 1}
    TranscodingStreams.finalize(decompressor)
    return data
end

struct ShuffleFilter end

function decompress!(data::Vector{UInt8}, data_length, element_size, num_elements, decompressor::ShuffleFilter)
    # Start with all least significant bytes, then work your way up
    # I'll leave this for someone else to make performant
    @assert data_length == length(data)
    @assert data_length % element_size == 0
    @assert data_length÷element_size == num_elements
    data_new = similar(data)
    for n = eachindex(data_new)
        j = 1 + (n-1)*num_elements
        i = mod1(j , data_length) + (j-1)÷data_length
        data_new[n] = data[i]
    end
    return data_new
end

function decompress!(io::IO, data_length, element_size, n, decompressor)
    read!(TranscodingStreams.TranscodingStream(decompressor, io), Vector{UInt8}(undef, element_size*n))
end

function read_compressed_array!(v::Array{T}, f::JLDFile,
                                rr::ReadRepresentation{T,RR},
                                data_length::Integer,
                                filters,
                                ) where {T,RR}
    invoke_again, decompressors = get_decompressor(filters)
    if invoke_again
        return Base.invokelatest(read_compressed_array!, v, f, rr, data_length, filters)::typeof(v)
    end

    io = f.io
    data_offset = position(io)
    n = length(v)
    element_size = odr_sizeof(RR)
    data = decompress!(io, data_length, element_size, n, decompressors[end])
    if length(decompressors) > 1 
        for decompressor in decompressors[end-1:-1:1]
            data = decompress!(data, length(data), element_size, n, decompressor)
        end
    end
    cp0 = Ptr{Cvoid}(pointer(data))
    @simd for i = eachindex(v)
        dataptr = cp0 + element_size*(i-1)
        if !jlconvert_canbeuninitialized(rr) || jlconvert_isinitialized(rr, dataptr)
            v[i] = jlconvert(rr, f, dataptr, NULL_REFERENCE)
        end
    end
    seek(io, data_offset + data_length)
    v
end