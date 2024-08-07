mutable struct Dataset
    parent::Group #param..
    name::String
    offset::RelOffset
    datatype
    dataspace
    layout
    attributes::OrderedDict{String, Any}
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
)  
    if !isnothing(name)
        (parent, name) = pathize(parent, name, true)
    end

    return Dataset(parent, name, UNDEFINED_ADDRESS, datatype, dataspace,
            layout, OrderedDict{String,Any}(), chunk, filters, nothing)
end

iswritten(dset::Dataset) = (dset.offset != UNDEFINED_ADDRESS)

function Base.show(io::IO, ::MIME"text/plain", dset::Dataset)
    f = dset.parent.f
    print(io, "┌─ Dataset:")
    print(io, isempty(dset.name) ? " (unnamed)" : " \"$(dset.name)\"")
    print(io, iswritten(dset) ? " at $(dset.offset)" : " (unwritten)", "\n")
    prefix = "│  "
    #println(io, prefix*"parent: $(dset.parent)")
    if !isnothing(dset.datatype)
        dt = dset.datatype
        iscommitted = dt isa SharedDatatype && haskey(f.datatype_locations, dt.header_offset)
        print(io, prefix*"datatype: $(typeof(dt))", iscommitted ? " (committed)\n" : "\n")
        iscommitted && println(io, prefix*"\tcommitted at: $(dt.header_offset)")
        rr = jltype(dset.parent.f, dt)
        jt = typeof(rr).parameters[1]
        println(io, prefix*"\twritten structure: $jt")
        if iscommitted
            juliatype, writtentype, fields = stringify_committed_datatype(f, f.datatype_locations[dt.header_offset], showfields=true)
            println(io, prefix*"\ttype name: $(juliatype)")
            if !isempty(writtentype)
                println(io, prefix*"\twritten type name: $(writtentype)")
            end
            for field in fields
                println(io, prefix*"\t\t$(field)")
            end
        end
    end
    if !isnothing(dset.dataspace)
        ds = dset.dataspace
        if ds isa HmWrap{HmDataspace}#Hmessage
            println(io, prefix*"dataspace:")
            spacetype = ("Scalar", "Simple", "Null", "V1")[Int(ds.dataspace_type)+1]
            println(io, prefix*"\ttype: $(spacetype)")
            println(io, prefix*"\tdimensions: $(ds.dimensions)")            
        else
            println(io, prefix*"dataspace: $(dset.dataspace)")
        end
    end
    if !isnothing(dset.layout)
        layout = dset.layout
        if layout isa HmWrap{HmDataLayout}
            println(io, prefix*"layout:")
            println(io, prefix*"\tclass: $(layout.layout_class)")
        else
            println(io, prefix*"layout: $(dset.layout)")
        end
    end
    if !isnothing(dset.filters) && !isempty(dset.filters.filters)
        println(io, prefix*"filters: $(dset.filters)")
    end
    if !isempty(dset.attributes)
        println(io, prefix*"Attributes:")
        for (k, attr) in pairs(dset.attributes)
            if attr isa ReadAttribute
                data = read_attr_data(dset.parent.f, attr)
                println(io, prefix*"\t$(attr.name) = ",
                    data isa String ? "\"$data\"" : data)
            else
                println(io, prefix*"\t$(k) = $(attr)")
            end
        end
    end
    println(io, "└─")
end

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
    attributes = map(collect(dataset.attributes)) do (name, attr)
        attr isa WrittenAttribute && return attr
        return WrittenAttribute(dataset.parent.f, name, attr)
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
        layout_class = LcCompact
        psz += jlsizeof(CompactStorageMessage) + datasz

    elseif !isnothing(dataset.chunk) || !isempty(dataset.filters)
        # Do some additional checks on the data here
        layout_class = LcChunked
        # improve filter support here
        psz += chunked_storage_message_size(ndims(data)) + pipeline_message_size(filter_id::UInt16)
    else
        layout_class = LcContiguous
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
    if layout_class == LcCompact
        jlwrite(cio, CompactStorageMessage(datasz))
        if datasz != 0
            write_data(cio, f, data, odr, datamode(odr), wsession)
        end
        dataset.header_chunk_info = (header_offset, position(cio)+20, position(cio))
        # Add NIL message replacable by continuation message
        jlwrite(cio, CONTINUATION_PLACEHOLDER)
        jlwrite(io, end_checksum(cio))
    elseif layout_class == LcChunked
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
        jlwrite(cio, CONTINUATION_PLACEHOLDER)
        jlwrite(f.io, end_checksum(cio))
    
    else
        jlwrite(cio, ContiguousStorageMessage(datasz, h5offset(f, f.end_of_data)))

        dataset.header_chunk_info = (header_offset, position(cio)+20, position(cio))
        # Add NIL message replacable by continuation message
        jlwrite(io, CONTINUATION_PLACEHOLDER)
        jlwrite(io, end_checksum(cio))

        f.end_of_data += datasz
        write_data(io, f, data, odr, datamode(odr), wsession)
    end

    offset = h5offset(f, header_offset)
    !isempty(dataset.name) && (dataset.parent[dataset.name] = offset)
    return offset
end

function read_dataset(dset::Dataset)
    f = dset.parent.f
    read_data(f,
        ReadDataspace(f, dset.dataspace), 
        dset.datatype,
        DataLayout(f, dset.layout),
        isnothing(dset.filters) ? FilterPipeline() : dset.filters, 
        dset.offset, 
        collect(values(dset.attributes)))
end

get_dataset(f::JLDFile, args...; kwargs...) = 
    get_dataset(f.root_group, args...; kwargs...)

function get_dataset(g::Group, name::String)
    f = g.f
    f.n_times_opened == 0 && throw(ArgumentError("file is closed"))

    (g, name) = pathize(g, name, false)
    roffset = lookup_offset(g, name)
    if roffset == UNDEFINED_ADDRESS
        if isempty(name)
            # this is a group
            return get_dataset(f, group_offset(g), g, name)
        end
        throw(KeyError(name))
    end
    get_dataset(f, roffset, g, name)
end

function get_dataset(f::JLDFile, offset::RelOffset, g=f.root_group, name="")
    dset = Dataset(g, name, offset, nothing, nothing, nothing, OrderedDict{String,Any}(), false, FilterPipeline(), nothing)
    
    hmitr = HeaderMessageIterator(f, offset)
    for msg in hmitr
        if msg.type == HmDataspace
            dset.dataspace = HmWrap(HmDataspace, msg)#ReadDataspace(f, msg)
        elseif msg.type == HmDatatype
            dset.datatype = HmWrap(HmDatatype, msg).dt
        elseif msg.type == HmDataLayout
            dset.layout = HmWrap(HmDataLayout, msg)
        elseif msg.type == HmFilterPipeline
            dset.filters = FilterPipeline(msg)
        elseif msg.type == HmAttribute
            attr = read_attribute(f, msg)
            dset.attributes[string(attr.name)] = attr
        end
    end

    dset.header_chunk_info = (; 
                hmitr.chunk.chunk_start,
                hmitr.chunk.chunk_end, 
                next_msg_offset = hmitr.chunk.chunk_end-CONTINUATION_MSG_SIZE)
    return dset
end

function add_attribute(dset::Dataset, name::String, data::Dataset)
    # link an existing dataset as attribute
    throw(UnsupportedFeatureException("Not implemented"))
end

# Attributes
message_size(msg::WrittenAttribute) = jlsizeof(HeaderMessage) + jlsizeof(msg)
function write_message(io,f::JLDFile, msg::WrittenAttribute, wsession=JLDWriteSession())
    jlwrite(io, HeaderMessage(HmAttribute, jlsizeof(msg), 0))
    write_attribute(io, f, msg, wsession)
    return nothing
end

# Links
message_size(msg::Pair{String, RelOffset}) = jlsizeof(HeaderMessage) + link_size(msg.first)
write_message(io, f, msg::Pair{String, RelOffset}, _=nothing) = 
    jlwrite(io, Hmessage(HmLinkMessage; link_name = msg.first, target = msg.second))


function attach_message(f::JLDFile, offset, messages, wsession=JLDWriteSession();
    chunk_start,
    chunk_end,
    next_msg_offset,
    minimum_continuation_size::Int = 0
    )
    if chunk_start == UNDEFINED_ADDRESS || chunk_end == UNDEFINED_ADDRESS ||
            next_msg_offset == UNDEFINED_ADDRESS
        throw(UnsupportedFeatureException("Not implemented. pass all info"))
    end
    next_msg_offset == -1 && throw(InternalError("next_msg_offset should not be -1"))

    io = f.io
    seek(io, next_msg_offset)
    remaining_space = chunk_end  - 20 - next_msg_offset
    while !isempty(messages)

        msg = first(messages)
        sz = message_size(msg)
        if remaining_space ≥ sz + 4 || remaining_space == sz 
            pos = position(io)
            write_message(io, f, msg)
            rsz = position(io) - pos
            if rsz != sz
                throw(InternalError("Message size mismatch. Expected $sz, got $rsz for message $msg"))
            end
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
        empty_space = chunk_end-position(io)-4 - 20
        if empty_space != -4
            empty_space < 0 && throw(InternalError("Negative empty space. This should not happen"))
            write_message(io, f, Hmessage(HmNil, 0, empty_space))
        end
        # continuation space
        write_message(io, f, Hmessage(HmNil, 0, 16))

        # Re-calculate checksum
        update_checksum(io, chunk_start, chunk_end)

        return nothing
    end
    if !iszero(remaining_space)
        # Mark remaining free space with a NIL message
        write_message(io, f, Hmessage(HmNil, 0, remaining_space-4))
    end
    # If we got to here then a new continuation needs to be created
    continuation_start = f.end_of_data
    continuation_size = jlsizeof(OBJECT_HEADER_CONTINUATION_SIGNATURE)
    continuation_size += sum(message_size(msg) for msg in messages)
    continuation_size += CONTINUATION_MSG_SIZE + 4 # Checksum
    tmp = max(continuation_size, minimum_continuation_size)
    # only replace if the gap is larger than 4 bytes
    tmp - continuation_size > 4 && (continuation_size = tmp)
    
    # Object continuation message
    jlwrite(io, Hmessage(HmObjectHeaderContinuation; 
        continuation_offset=h5offset(f, continuation_start),
        continuation_length=Length(continuation_size)))

    # Re-calculate checksum
    update_checksum(io, chunk_start, chunk_end)

    # Object continuation
    seek(io, continuation_start)
    chunk_start = continuation_start
    chunk_end = continuation_start + continuation_size -4
    cio = begin_checksum_write(io, continuation_size - 4)
    jlwrite(cio, OBJECT_HEADER_CONTINUATION_SIGNATURE)
    remaining_space = continuation_size - jlsizeof(OBJECT_HEADER_CONTINUATION_SIGNATURE) - CONTINUATION_MSG_SIZE -4
    while !isempty(messages)
        msg = popfirst!(messages)
        sz = message_size(msg)
        write_message(io, f, msg, wsession)
        next_msg_offset += sz
        remaining_space -= sz
    end
    if remaining_space > 0
        @assert remaining_space ≥ 4 "Gaps smaller than 4 bytes should not occur"
        jlwrite(cio, Hmessage(HmNil, 0, remaining_space))
    end
    # Extra space for object continuation
    jlwrite(cio, CONTINUATION_PLACEHOLDER)
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

function add_attribute(dset::Dataset, name::String, data, wsession=JLDWriteSession())
    f = dset.parent.f
    prewrite(f) # assert writability

    for attr in dset.attributes
        if (attr isa ReadAttribute && attr.name == name) || (attr isa Pair && attr.first == name)
            throw(ArgumentError("Attribute $name already exists. Attribute names must be unique."))
        end
    end
    dset.attributes[name] = data
    if iswritten(dset)
        dset.header_chunk_info = 
        attach_message(f, dset.offset, [WrittenAttribute(f,name,data)], wsession;
            chunk_start=dset.header_chunk_info[1],
            chunk_end=dset.header_chunk_info[2],
            next_msg_offset=dset.header_chunk_info[3],
            )
        return nothing
    end
end

function attributes(dset::Dataset; plain::Bool=false)
    plain && return dset.attributes
    map(values(dset.attributes)) do attr
        read_attr_data(dset.parent.f, attr)
    end
end