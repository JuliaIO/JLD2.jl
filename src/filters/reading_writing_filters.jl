using JLD2.Filters: Filter, FilterPipeline, filterid, filtername, normalize_filters, iscompressed

struct WrittenFilter
    id::UInt16
    flags::UInt16
    name::String
    client_data::Vector{UInt32}
end

struct WrittenFilterPipeline
    filters::Vector{WrittenFilter}
end

WrittenFilterPipeline() = WrittenFilterPipeline(WrittenFilter[])
Filters.iscompressed(fp::WrittenFilterPipeline) = !isempty(fp.filters)

function WrittenFilterPipeline(msg_::Hmessage)
    msg = HmWrap(HmFilterPipeline, msg_)
    version = msg.version
    nfilters = msg.nfilters
    io = msg.m.io
    seek(io, msg.m.address+2)
    version == 1 && skip(io, 6)
    filters = map(1:nfilters) do _
        id = jlread(io, UInt16)
        name_length = (version == 2 && id < 255) ? zero(UInt16) : jlread(io, UInt16)
        flags = jlread(io, UInt16)
        nclient_vals = jlread(io, UInt16)
        name = iszero(name_length) ? "" : read_bytestring(io)
        skip(io, max(0, 8-mod1(name_length, 8)-1))
        client_data = jlread(io, UInt32, nclient_vals)
        (version == 1 && isodd(nclient_vals)) && skip(io, 4)
        WrittenFilter(id, flags, name, client_data)
    end
    return WrittenFilterPipeline(filters)
end

FilterPipeline(hm::Hmessage) =
    FilterPipeline(WrittenFilterPipeline(hm))

FilterPipeline(fp::WrittenFilterPipeline) =
    FilterPipeline([Filter(fil.id, fil.client_data...) for fil in fp.filters])


function pipeline_message_size(fp::FilterPipeline)
    sz = 4 + 2
    for filter in fp
        sz += 6 + 4*length(Filters.client_values(filter))
        if filterid(filter) > 255
            sz += 2
            fnamelen = length(filtername(filter))+1
            fnamelen += 8-mod1(fnamelen, 8)
            sz += fnamelen
        end
    end
    sz
end

function write_filter_pipeline_message(io, fp::FilterPipeline)
    hmsize = pipeline_message_size(fp) - 4
    jlwrite(io, HeaderMessage(HmFilterPipeline, hmsize, 0))
    jlwrite(io, UInt8(2))                   # Version
    jlwrite(io, UInt8(length(fp.filters)))  # Number of Filters

    for filter in fp.filters
        filter_id = filterid(filter)
        client_data = Filters.client_values(filter)
        if filter_id > 255
            filter_name = filtername(filter)
            fnamelen = length(filter_name)+1
            fnamelen += 8-mod1(fnamelen, 8)
            padding = fnamelen - length(filter_name)
        end
        jlwrite(io, filter_id)                # Filter Identification Value
        filter_id > 255 && jlwrite(io, UInt16(fnamelen))
                                            # Length of Filter Name
        jlwrite(io, UInt16(0))                # Flags
        jlwrite(io, UInt16(length(client_data))) # Number of Client Data Values
        filter_id > 255 && jlwrite(io, filter_name) # Filter Name
        filter_id > 255 && (padding > 0) && jlwrite(io, zeros(UInt8, padding))
        for v in client_data
            jlwrite(io, UInt32(v))     # Client Data (Compression Level)
        end
    end
    nothing
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

function deflate_data(f::JLDFile, data::Array{T}, odr::S, wsession::JLDWriteSession,
                      filter) where {T,S}
    buf = Vector{UInt8}(undef, odr_sizeof(odr) * length(data))

    cp = Ptr{Cvoid}(pointer(buf))
    @simd for i = 1:length(data)
        @inbounds h5convert!(cp, odr, f, data[i], wsession)
        cp += odr_sizeof(odr)
    end

    Filters.compress(filter, buf, odr_sizeof(odr))
end

function read_compressed_array!(v::Array{T}, f::JLDFile,
                                rr::ReadRepresentation{T,RR},
                                data_length::Integer,
                                filter,
                                ) where {T,RR}
    io = f.io
    data_offset = position(io)
    element_size = odr_sizeof(RR)
    data = Filters.decompress(filter, io, data_length, element_size)
    @GC.preserve data begin
        cp0 = Ptr{Cvoid}(pointer(data))
        @simd for i = eachindex(v)
            dataptr = cp0 + element_size*(i-1)
            if !jlconvert_canbeuninitialized(rr) || jlconvert_isinitialized(rr, dataptr)
                v[i] = jlconvert(rr, f, dataptr, NULL_REFERENCE)
            end
        end
    end
    seek(io, data_offset + data_length)
    v
end

#############################################################################################################

# jld2.jl 341
@nospecializeinfer Base.write(f::JLDFile, name::AbstractString, @nospecialize(obj), wsession::JLDWriteSession=JLDWriteSession(); kwargs...) =
    write(f.root_group, name, obj, wsession; kwargs...)

# groups.jl 112
@nospecializeinfer function Base.write(g::Group, name::AbstractString, @nospecialize(obj), wsession::JLDWriteSession=JLDWriteSession(); compress=nothing)
    f = g.f
    prewrite(f)
    (g, name) = pathize(g, name, true)
    if !isnothing(compress)
        filters = normalize_filters(compress)
        if obj isa Array
            g[name] = write_dataset(f, obj, wsession, filters)
            return nothing
        end
        @warn "Only arrays can be compressed."
    end
    g[name] = write_dataset(f, obj, wsession)
    nothing
end
