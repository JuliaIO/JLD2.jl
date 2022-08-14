function jlread(io, ::Type{FilterPipeline})
    version = jlread(io, UInt8)
    nfilters = jlread(io, UInt8)
    if version == 1
        skip(io, 6)
        filters = map(1:nfilters) do _
            id = jlread(io, UInt16)
            name_length = jlread(io, UInt16)
            flags = jlread(io, UInt16)
            nclient_vals = jlread(io, UInt16)
            if iszero(name_length) 
                name = ""
            else
                name = read_bytestring(io)
                skip(io, 8-mod1(sizeof(name), 8)-1)
            end
            client_data = jlread(io, UInt32, nclient_vals)
            isodd(nclient_vals) && skip(io, 4)
            Filter(id, flags, name, client_data)
        end
        return FilterPipeline(filters)
    elseif version == 2
        filters = map(1:nfilters) do _
            id = jlread(io, UInt16)
            if id > 255
                name_length = jlread(io, UInt16)
                flags = jlread(io, UInt16)
                nclient_vals = jlread(io, UInt16)
                if iszero(name_length) 
                    name = ""
                else
                    name = read_bytestring(io)
                    skip(io, 8-mod1(sizeof(name), 8)-1)
                end
            else
                name = ""
                flags = jlread(io, UInt16)
                nclient_vals = jlread(io, UInt16)
            end
            client_data = jlread(io, UInt32, nclient_vals)
            Filter(id, flags, name, client_data)
        end
        return FilterPipeline(filters)
    else
        throw(UnsupportedVersionException("Filter Pipeline Message version $version is not implemented"))
    end


end

const COMPRESSOR_TO_ID = Dict(
    :ZlibCompressor => UInt16(1),
    :ShuffleFilter => UInt16(2),
    :Bzip2Compressor => UInt16(307),
    #:BloscCompressor => UInt16(32001),
    :LZ4FrameCompressor => UInt16(32004),
    )

# For loading need filter_ids as keys
const ID_TO_DECOMPRESSOR = Dict(
    UInt16(1) => (:CodecZlib, :ZlibCompressor, :ZlibDecompressor, ""),
    UInt16(2) => (:JLD2, :ShuffleFilter, :ShuffleFilter, ""),
    UInt16(307) => (:CodecBzip2, :Bzip2Compressor, :Bzip2Decompressor, "BZIP2"),
    #UInt16(32001) => (:Blosc, :BloscCompressor, :BloscDecompressor, "BLOSC"),
    UInt16(32004) => (:CodecLz4, :LZ4FrameCompressor, :LZ4FrameDecompressor, "LZ4"),
)

issupported_filter(filter_id) = filter_id ∈ keys(ID_TO_DECOMPRESSOR)

function verify_compressor(compressor)
    (compressor isa Bool || haskey(COMPRESSOR_TO_ID, nameof(typeof(compressor)))) && return

    crs = map(values(ID_TO_DECOMPRESSOR)) do val
        "\n\t"*string(val[1])*"."*string(val[2])
    end


    throw(ArgumentError("""Unsupported Compressor
    Supported Compressors are $(crs...)"""))
end
#############################################################################################################
# Dynamic Package Loading Logic copied from FileIO
const load_locker = Base.ReentrantLock()

is_installed(pkg::Symbol) = get(Pkg.installed(), string(pkg), nothing) != nothing

function _findmod(f::Symbol)
    for (u,v) in Base.loaded_modules
        (Symbol(v) == f) && return u
    end
    nothing
end

function topimport(modname)
    @info "Attempting to dynamically load $modname"
    @eval Base.__toplevel__  import $modname
    u = _findmod(modname)
    @eval $modname = Base.loaded_modules[$u]
end

function checked_import(pkg::Symbol)
    lock(load_locker) do
        # kludge for test suite
        if isdefined(Main, pkg)
            m1 = getfield(Main, pkg)
            isa(m1, Module) && return false, m1
        end
        if isdefined(JLD2, pkg)
            m1 = getfield(JLD2, pkg)
            isa(m1, Module) && return false, m1
        end
        m = _findmod(pkg)
        (m === nothing) || return false, Base.loaded_modules[m]
        topimport(pkg)
        return true, Base.loaded_modules[_findmod(pkg)]
    end
end

#############################################################################################################

# jld2.jl 341
Base.write(f::JLDFile, name::AbstractString, obj, wsession::JLDWriteSession=JLDWriteSession(); compress=nothing) =
    write(f.root_group, name, obj, wsession; compress=compress)

# groups.jl 112
function Base.write(g::Group, name::AbstractString, obj, wsession::JLDWriteSession=JLDWriteSession(); compress=nothing)
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

function get_decompressor(filter_id::UInt16)
    modname, compressorname, decompressorname, = ID_TO_DECOMPRESSOR[filter_id]
    invoke_again, m = checked_import(modname)
    return invoke_again, @eval $m.$decompressorname()
end
function get_decompressor(filters::FilterPipeline)
    decompressors = Any[]
    invoke_again = false
    for filter in filters.filters
        modname, compressorname, decompressorname, = ID_TO_DECOMPRESSOR[filter.id]
        invoke_again, m = checked_import(modname)
        push!(decompressors, @eval $m.$decompressorname())
    end
    return invoke_again, decompressors
end

pipeline_message_size(filter_id) = 4 + 12 + (filter_id > 255)*(2 + length(ID_TO_DECOMPRESSOR[filter_id][4]))

function write_filter_pipeline_message(io, filter_id::UInt16)
    hmsize = 12
    if filter_id > 255
        filter_name = ID_TO_DECOMPRESSOR[filter_id][4]
        hmsize += 2 + length(filter_name)
    end
    jlwrite(io, HeaderMessage(HM_FILTER_PIPELINE, hmsize, 0))
    jlwrite(io, UInt8(2))                 # Version
    jlwrite(io, UInt8(1))                 # Number of Filters
    jlwrite(io, filter_id)                # Filter Identification Value
    filter_id > 255 && jlwrite(io, UInt16(length(filter_name)))
                                        # Length of Filter Name
    jlwrite(io, UInt16(0))                # Flags
    jlwrite(io, UInt16(1))                # Number of Client Data Values
    filter_id > 255 && jlwrite(io, filter_name) # Filter Name
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


@inline chunked_storage_message_size(ndims::Int) =
    jlsizeof(HeaderMessage) + 5 + (ndims+1)*jlsizeof(Length) + 1 + jlsizeof(Length) + 4 + jlsizeof(RelOffset)


function write_chunked_storage_message( io::IO,
                                        elsize::Int,
                                        dims::NTuple{N,Int},
                                        filtered_size::Int,
                                        offset::RelOffset) where N
    jlwrite(io, HeaderMessage(HM_DATA_LAYOUT, chunked_storage_message_size(N) - jlsizeof(HeaderMessage), 0))
    jlwrite(io, UInt8(4))                     # Version
    jlwrite(io, UInt8(LC_CHUNKED_STORAGE))    # Layout Class
    jlwrite(io, UInt8(2))                     # Flags (= SINGLE_INDEX_WITH_FILTER)
    jlwrite(io, UInt8(N+1))                   # Dimensionality
    jlwrite(io, UInt8(jlsizeof(Length)))        # Dimensionality Size
    for i = N:-1:1
        jlwrite(io, Length(dims[i]))          # Dimensions 1...N
    end
    jlwrite(io, Length(elsize))               # Element size (last dimension)
    jlwrite(io, UInt8(1))                     # Chunk Indexing Type (= Single Chunk)
    jlwrite(io, Length(filtered_size))        # Size of filtered chunk
    jlwrite(io, UInt32(0))                    # Filters for chunk
    jlwrite(io, offset)                       # Address
end


function write_compressed_data(cio, f, data, odr, wsession, filter_id, compressor)
    write_filter_pipeline_message(cio, filter_id)

    # deflate first
    deflated = deflate_data(f, data, odr, wsession, compressor)

    write_chunked_storage_message(cio, odr_sizeof(odr), size(data), length(deflated), h5offset(f, f.end_of_data))
    jlwrite(f.io, end_checksum(cio))

    f.end_of_data += length(deflated)
    jlwrite(f.io, deflated)
end

function decompress!(inptr::Ptr, data_length, element_size, n, decompressor::TranscodingStreams.Codec)
    TranscodingStreams.initialize(decompressor)
    data = transcode(decompressor, unsafe_wrap(Array, Ptr{UInt8}(inptr), data_length))::Array{UInt8, 1}
    TranscodingStreams.finalize(decompressor)
    return data
end

struct ShuffleFilter end

function decompress!(data::Vector{UInt8}, data_length, element_size,n, decompressor::ShuffleFilter)
    # Start with all least significant bytes, then work your way up
    # I'll leave this for somenone else to make performant
    @assert data_length == length(data)
    @assert data_length % element_size == 0
    data_new = similar(data)
    numelements = length(data)÷element_size
    for n = eachindex(data_new)
        j = 1 + (n-1)*numelements
        i = mod1(j , length(data)) + j÷length(data)
        data_new[n] = data[i]
    end
    return data_new
end
function decompress!(io::IOStream, data_length, element_size, n, decompressor)
    read!(TranscodingStream(decompressor, io), Vector{UInt8}(undef, element_size*n))
end

function read_compressed_array!(v::Array{T}, f::JLDFile{MmapIO},
                                rr::ReadRepresentation{T,RR},
                                data_length::Int64,
                                filters
                                ) where {T,RR}

    invoke_again, decompressors = get_decompressor(filters)
    if invoke_again
        return Base.invokelatest(read_compressed_array!, v, f, rr, data_length, filters)::typeof(v)
    end
    io = f.io
    inptr = io.curptr
    element_size = odr_sizeof(RR)
    n = length(v)
    data = decompress!(inptr, data_length, element_size, n, decompressors[end])
    if length(decompressors) > 1 
        for decompressor in decompressors[end-1:-1:1]
            data = decompress!(data, length(data), element_size, n, decompressor)
        end
    end
    @simd for i = 1:length(v)
        dataptr = Ptr{Cvoid}(pointer(data, odr_sizeof(RR)*(i-1)+1))
        if !jlconvert_canbeuninitialized(rr) || jlconvert_isinitialized(rr, dataptr)
            @inbounds v[i] = jlconvert(rr, f, dataptr, NULL_REFERENCE)
        end
    end
    io.curptr = inptr + data_length
    v
end

function read_compressed_array!(v::Array{T}, f::JLDFile{IOStream},
                                rr::ReadRepresentation{T,RR},
                                data_length::Int64,
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
    @simd for i = 1:n
        dataptr = Ptr{Cvoid}(pointer(data, odr_sizeof(RR)*(i-1)+1))
        if !jlconvert_canbeuninitialized(rr) || jlconvert_isinitialized(rr, dataptr)
            @inbounds v[i] = jlconvert(rr, f, dataptr, NULL_REFERENCE)
        end
    end
    seek(io, data_offset + data_length)
    v
end
