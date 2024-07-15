struct Hmessage
    type::HeaderMessageTypes
    size::UInt16
    hflags::UInt8
    body::Vector{UInt8}
    offset::RelOffset
end

function Base.getproperty(hm::Hmessage, s::Symbol)
    s in fieldnames(Hmessage) && return getfield(hm, s)
    getprop(Val(hm.type), hm, s)
end

function Hmessage(type::HeaderMessageTypes, hflags=0x00, size=0; kwargs...)
    payload = construct_hm_payload(Val(type), hflags, size; kwargs...)
    Hmessage(type, length(payload), hflags, payload, UNDEFINED_ADDRESS)
end

function read_msg(io, header_version, flags)

end

mutable struct Dataset
    parent::Group #param..
    name::String
    offset::RelOffset
    datatype
    dataspace
    layout
    attributes
    chunk
    filters#::Vector{Filter}
    #external
    header_chunk_info # chunk_start, chunk_end, next_msg_offset
end

create_dataset(f::JLDFile, args...; kwargs...) = create_dataset(f.root_group, args...; kwargs...)
function create_dataset(
    parent::Group,
    name::Union{Nothing,String},
    datatype=nothing,
    dataspace=nothing;
    layout = nothing,
    chunk=nothing,
    filters=Filter[],
    attributes=[],
)  
    if !isnothing(name)
        (parent, name) = pathize(parent, name, true)
    end

    return Dataset(parent, name, UNDEFINED_ADDRESS, datatype, dataspace,
            layout, attributes, chunk, filters, nothing)
end

iswritten(dset::Dataset) = (dset.offset != UNDEFINED_ADDRESS)

function write_dataset(dataset::Dataset, data)
    f = dataset.parent.f
    if dataset.offset != UNDEFINED_ADDRESS
        throw(ArgumentError("Dataset has already been written to file"))
    end
    wsession = JLDWriteSession()
    # first need to figure out if data type and dataspace are defined / correct
    if isnothing(dataset.datatype)
        dataset.datatype = h5type(f, data)
    end
    datatype = dataset.datatype
    odr = objodr(data)
    if isnothing(dataset.dataspace)
        dataset.dataspace = WriteDataspace(f, data, odr)
    end
    dataspace = dataset.dataspace
    # Attributes
    attributes = map(dataset.attributes) do a
        a isa WrittenAttribute && return a
        typeof(a) <: (Pair{Symbol,T}where T) && return WrittenAttribute(dataset.parent.f, a.first, a.second)
        throw(ArgumentError("Invalid attribute: $a"))
    end
    io = f.io
    datasz = odr_sizeof(odr)::Int * numel(dataspace)::Int

    psz = payload_size_without_storage_message(dataspace, datatype)::Int

    psz += sum(message_size.(attributes), init=0)

    # minimum extra space for continuation message
    psz += jlsizeof(HeaderMessage) + jlsizeof(RelOffset) + jlsizeof(Length)


    # determine layout class
    # DataLayout object is only available after the data is written
    if datasz < 8192
        layout_class = LC_COMPACT_STORAGE
        psz += jlsizeof(CompactStorageMessage) + datasz

    elseif !isnothing(dataset.chunk) || !isempty(dataset.filters)
        # Do some additional checks on the data here
        layout_class = LC_CHUNKED_STORAGE
        # improve filter support here
        psz += chunked_storage_message_size(ndims(data)) + pipeline_message_size(filter_id::UInt16)
    else
        layout_class = LC_CONTIGUOUS_STORAGE
        psz += jlsizeof(ContiguousStorageMessage)
    end
    fullsz = jlsizeof(ObjectStart) + size_size(psz) + psz + 4 # why do I need to correct here?

    header_offset = f.end_of_data
    seek(io, header_offset)
    f.end_of_data = header_offset + fullsz
    
    if ismutabletype(typeof(data)) && !isa(wsession, JLDWriteSession{Union{}})
        wsession.h5offset[objectid(data)] = h5offset(f, header_offset)
        push!(wsession.objects, data)
    end
    
    cio = begin_checksum_write(io, fullsz - 4)
    write_object_header_and_dataspace_message(cio, f, psz, dataspace)
    write_datatype_message(cio, datatype)
    for a in attributes
        write_message(cio, f, a, wsession)
    end
    # Data storage layout
    if layout_class == LC_COMPACT_STORAGE
        jlwrite(cio, CompactStorageMessage(datasz))
        if datasz != 0
            write_data(cio, f, data, odr, datamode(odr), wsession)
        end
        dataset.header_chunk_info = (header_offset, position(cio)+20, position(cio))
        # Add NIL message replacable by continuation message
        jlwrite(cio, HeaderMessage(HM_NIL, 16, 0))
        jlwrite(cio, zeros(UInt8, 16))
        jlwrite(io, end_checksum(cio))
    elseif layout_class == LC_CHUNKED_STORAGE
        # this thing is a bit weird
        write_compressed_data(cio, f, data, odr, wsession, filter_id, compressor)
        write_filter_pipeline_message(cio, filter_id)

        # deflate first
        deflated = deflate_data(f, data, odr, wsession, compressor)
        seek(f.io, h5offset(f, f.end_of_data))
        f.end_of_data += length(deflated)
        jlwrite(f.io, deflated)


        write_chunked_storage_message(cio, odr_sizeof(odr), size(data), length(deflated), h5offset(f, f.end_of_data))
        
        dataset.header_chunk_info = (header_offset, position(cio)+20, position(cio))
        # Add NIL message replacable by continuation message
        jlwrite(cio, HeaderMessage(HM_NIL, 16, 0))
        jlwrite(cio, zeros(UInt8, 16))
        jlwrite(f.io, end_checksum(cio))
    
    else
        jlwrite(cio, ContiguousStorageMessage(datasz, h5offset(f, f.end_of_data)))

        dataset.header_chunk_info = (header_offset, position(cio)+20, position(cio))
        # Add NIL message replacable by continuation message
        jlwrite(io, HeaderMessage(HM_NIL, 16, 0))
        jlwrite(io, zeros(UInt8, 16))
        jlwrite(io, end_checksum(cio))

        f.end_of_data += datasz
        write_data(io, f, data, odr, datamode(odr), wsession)
    end

    offset = h5offset(f, header_offset)
    !isempty(dataset.name) && (dataset.parent[dataset.name] = offset)
    return offset
end

function read_dataset(dset::Dataset)
    read_data(dset.parent.f, 
        dset.dataspace, 
        dset.datatype[1], #datatype_class
        dset.datatype[2], #datatype_offset,
        dset.layout,
        isnothing(dset.filters) ? FilterPipeline() : FilterPipeline(dset.filters), 
        dset.offset, 
        dset.attributes)
end

get_dataset(f::JLDFile, args...; kwargs...) = get_dataset(f.root_group, args...; kwargs...)

function get_dataset(g::Group, name::String)
    f = g.f
    f.n_times_opened == 0 && throw(ArgumentError("file is closed"))

    (g, name) = pathize(g, name, false)

    roffset = lookup_offset(g, name)
    if roffset == UNDEFINED_ADDRESS
        throw(KeyError(name))
    end

    if isgroup(f, roffset)
        let loaded_groups = f.loaded_groups
            get!(()->load_group(f, roffset), loaded_groups, roffset)
        end
    else
        get_dataset(f, roffset, g, name)
    end
end

function get_dataset(f::JLDFile, offset::RelOffset, g, name)
    msgs, chunks = read_header(f, offset)
    dset = Dataset(g, name, offset, nothing, nothing, nothing, ReadAttribute[], false, nothing, nothing)
    
    chunk = chunks[end]
    dset.header_chunk_info = (; 
        chunk.chunk_start,
        chunk.chunk_end, 
        next_msg_offset = chunk.chunk_end-20)

    for msg in msgs
        if msg.type == HM_DATASPACE
            dset.dataspace = ReadDataspace(f, msg)
        elseif msg.type == HM_DATATYPE
            dset.datatype = datatype_from_message(f, msg)
        elseif msg.type == HM_DATA_LAYOUT
            dset.layout = DataLayout(f, msg)
        elseif msg.type == HM_FILTER_PIPELINE
            dset.filters = FilterPipeline(msg)
        elseif msg.type == HM_ATTRIBUTE
            push!(dset.attributes, read_attribute(f, msg))
        elseif msg.type == HM_OBJECT_HEADER_CONTINUATION
            continue
        else
            @warn "encountered unhandeled message type: $(msg.type)"
        end
    end
    return dset
end

function add_attribute(dset::Dataset, name::String, data::Dataset)
    # link an existing dataset as attribute
    throw(UnsupportedFeatureException("Not implemented"))
end

# Attributes
message_size(msg::WrittenAttribute) = jlsizeof(HeaderMessage) + jlsizeof(msg)
function write_message(io,f::JLDFile, msg::WrittenAttribute, wsession=JLDWriteSession())
    jlwrite(io, HeaderMessage(HM_ATTRIBUTE, jlsizeof(msg), 0))
    write_attribute(io, f, msg, wsession)
    return nothing
end

# Links
message_size(msg::Pair{String, RelOffset}) = jlsizeof(HeaderMessage) + link_size(msg.first)
write_message(io, f, msg::Pair{String, RelOffset}, _=nothing) = write_link(io, msg...)

function attach_message(f::JLDFile, offset, messages, wsession=JLDWriteSession();
    chunk_start,# =UNDEFINED_ADDRESS,
    chunk_end,#::RelOffset=UNDEFINED_ADDRESS,
    next_msg_offset,#::RelOffset=UNDEFINED_ADDRESS,
    minimum_continuation_size::Int = 0
    )
    isempty(messages) && return nothing
    if chunk_start == UNDEFINED_ADDRESS || chunk_end == UNDEFINED_ADDRESS ||
            next_msg_offset == UNDEFINED_ADDRESS
        throw(UnsupportedFeatureException("Not implemented. pass all info"))
    end

    io = f.io
    seek(io, next_msg_offset)
    remaining_space = chunk_end  - 20 - next_msg_offset
    while !isempty(messages)

        msg = first(messages)
        sz = message_size(msg)
        if remaining_space ≥ sz + 4 || remaining_space == sz 
            write_message(io, f, msg)
            next_msg_offset += sz
            remaining_space -= sz
            popfirst!(messages)
        else
            break
        end
    end

    if isempty(messages)
        # Managed to add all messages
        # Cleanup and return
        # Mark remaining free space with a NIL message
        jlwrite(io, HeaderMessage(HM_NIL, chunk_end - (position(io)+4), 0))
        jlwrite(io, zeros(UInt8, chunk_end - (position(io)+4)))
        # Re-calculate checksum
        update_checksum(io, chunk_start, chunk_end)

        return nothing
    end
    if !iszero(remaining_space)
        # Mark remaining free space with a NIL message
        jlwrite(io, HeaderMessage(HM_NIL, remaining_space, 0))
        jlwrite(io, zeros(UInt8, remaining_space))
    end
    # If we got to here then a new continuation needs to be created
    continuation_start = f.end_of_data
    continuation_size = jlsizeof(OBJECT_HEADER_CONTINUATION_SIGNATURE)
    continuation_size += sum(message_size(msg) for msg in messages)
    continuation_size += jlsizeof(HeaderMessage) + jlsizeof(RelOffset) + jlsizeof(Length)
    continuation_size += 4 # checksum
    tmp = max(continuation_size, minimum_continuation_size)
    # only replace if the gap is larger than 4 bytes
    tmp - continuation_size > 4 && (continuation_size = tmp)
    
    # Object continuation message
    # could re-use next_msg_offset for this
    #seek(io, continuation_message_goes_here)
    jlwrite(io, HeaderMessage(HM_OBJECT_HEADER_CONTINUATION, jlsizeof(RelOffset) + jlsizeof(Length), 0))
    jlwrite(io, h5offset(f, continuation_start))
    jlwrite(io, Length(continuation_size))

    # Re-calculate checksum
    update_checksum(io, chunk_start, chunk_end)

    # Object continuation
    seek(io, continuation_start)
    chunk_start = continuation_start
    chunk_end = continuation_start + continuation_size -4
    cio = begin_checksum_write(io, continuation_size - 4)
    jlwrite(cio, OBJECT_HEADER_CONTINUATION_SIGNATURE)
    remaining_space = continuation_size - jlsizeof(OBJECT_HEADER_CONTINUATION_SIGNATURE)
    remaining_space -= (jlsizeof(HeaderMessage) + jlsizeof(RelOffset) + jlsizeof(Length) +4)
    while !isempty(messages)
        msg = popfirst!(messages)
        sz = message_size(msg)
        write_message(io, f, msg, wsession)
        next_msg_offset += sz
        remaining_space -= sz
    end
    if remaining_space > 0
        @assert remaining_space ≥ 4 "Gaps smaller than 4 bytes should not occur"
        jlwrite(cio, HeaderMessage(HM_NIL, remaining_space, 0))
        jlwrite(cio, zeros(UInt8, remaining_space))
    end
    # Extra space for object continuation
    #g.continuation_message_goes_here = position(cio)
    jlwrite(cio, HeaderMessage(HM_NIL, 16, 0))
    jlwrite(cio, RelOffset(0))
    jlwrite(cio, Length(0))
    # Checksum
    jlwrite(io, end_checksum(cio))
    f.end_of_data = position(io)
    #g.last_chunk_checksum_offset = f.end_of_data - 4
    # TODO: last_chunk_checksum_offset is not updated
    return header_chunk_info = (
        chunk_start, 
        chunk_end, 
        position(io)-24)
end

function add_attribute(dset::Dataset, name::Symbol, data, wsession=JLDWriteSession())
    f = dset.parent.f
    prewrite(f) # assert writability

    if !iswritten(dset)
        # Dataset only lives in memory so far 
        push!(dset.attributes, name=>data)
        return nothing
    else
        dset.header_chunk_info = 
            attach_message(f, dset.offset, [WrittenAttribute(f,name,data)], wsession;
                chunk_start=dset.header_chunk_info[1],
                chunk_end=dset.header_chunk_info[2],
                next_msg_offset=dset.header_chunk_info[3],
                )
        #TODO  add attr to dset.attributes
        return nothing
    end
end

function attributes(dset::Dataset; plain::Bool=false)
    plain && return dset.attributes
    map(dset.attributes) do attr
        attr.name => read_attr_data(dset.parent.f, attr)
    end
end