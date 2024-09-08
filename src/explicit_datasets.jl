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


"""
    create_dataset(parent, path, datatype, dataspace; kwargs...)

Arguments:
    - `parent::Union{JLDfile, Group}`: Containing group of new dataset
    - `path`: Path to new dataset relative to `parent`. If `path` is `nothing`, the dataset is unnamed.
    - `datatype`: Datatype of new dataset (element type in case of arrays)
    - `dataspace`: Dimensions or `Dataspace` of new dataset

Keyword arguments:
    - `layout`: `DataLayout` of new dataset
    - `filters`: `FilterPipeline` for describing the compression pipeline
"""
create_dataset(f::JLDFile, args...; kwargs...) = create_dataset(f.root_group, args...; kwargs...)
function create_dataset(
    g::Group,
    path::Union{Nothing,String},
    datatype=nothing,
    dataspace=nothing;
    layout = nothing,
    chunk=nothing,
    filters=FilterPipeline(),
)  
    if !isnothing(path)
        (parent, name) = pathize(g, path, true)
    else
        name = ""
        parent = g.f
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

"""
    write_dataset(dataset::Dataset, data)

Write data to file using metadata prepared in the `dataset`.
"""
function write_dataset(dataset::Dataset, data, wsession::JLDWriteSession=JLDWriteSession())
    f = dataset.parent.f
    if dataset.offset != UNDEFINED_ADDRESS
        throw(ArgumentError("Dataset has already been written to file"))
    end
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
    if !isempty(dataset.filters.filters)
        filter_id = dataset.filters.filters[1].id
        invoke_again, compressor = get_compressor(filter_id)
        if invoke_again
            return Base.invokelatest(write_dataset, dset, data)::RelOffset
        end
    else
        compressor = nothing
    end
    offset = write_dataset(f, dataspace, datatype, odr, data, wsession, compressor)
    !isempty(dataset.name) && (dataset.parent[dataset.name] = offset)
    # Attributes
    attrs = map(collect(keys(pairs(dataset.attributes)))) do name
       WrittenAttribute(f, name, dataset.attributes[name]) 
    end
    dataset = get_dataset(f, offset, dataset.parent, dataset.name)
    dataset.header_chunk_info = 
        attach_message(f, dataset.offset, attrs, wsession;
            chunk_start=dataset.header_chunk_info[1],
            chunk_end=dataset.header_chunk_info[2],
            next_msg_offset=dataset.header_chunk_info[3],
            )

    return offset
end

"""
    read_dataset(dset::Dataset)

Read the data referenced by a dataset.
"""
function read_dataset(dset::Dataset)
    f = dset.parent.f
    read_data(f,
        ReadDataspace(f, dset.dataspace), 
        dset.datatype,
        DataLayout(f, dset.layout),
        isnothing(dset.filters) ? FilterPipeline() : dset.filters, 
        dset.offset, 
        collect(ReadAttribute, values(dset.attributes)))
end

"""
    get_dataset(parent::Union{JLDFile, Group}, name::String)

Get a stored dataset from a file by name or path as a `Dataset` object.
This may be useful for inspecting the metadata incl. types of a dataset.
"""
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
            dset.dataspace = HmWrap(HmDataspace, msg)
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

# Attributes
message_size(msg::WrittenAttribute) = jlsizeof(HeaderMessage) + jlsizeof(msg)
function write_header_message(io,f::JLDFile, msg::WrittenAttribute, wsession=JLDWriteSession())
    jlwrite(io, HeaderMessage(HmAttribute, jlsizeof(msg), 0))
    write_attribute(io, f, msg, wsession)
    return nothing
end

# Links
message_size(msg::Pair{String, RelOffset}) = jlsizeof(Val(HmLinkMessage); link_name=msg.first)
write_header_message(io, f, msg::Pair{String, RelOffset}, _=nothing) = 
    write_header_message(io, Val(HmLinkMessage); link_name=msg.first, target=msg.second)

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
            write_header_message(io, f, msg)
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
            write_header_message(io, Val(HmNil), 0, empty_space)
        end
        # continuation space
        write_continuation_placeholder(io)

        # Re-calculate checksum
        update_checksum(io, chunk_start, chunk_end)

        return nothing
    end
    if !iszero(remaining_space)
        # Mark remaining free space with a NIL message
        write_header_message(io, Val(HmNil), 0, remaining_space-4)
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
    write_header_message(io, Val(HmObjectHeaderContinuation);
        continuation_offset=h5offset(f, continuation_start),
        continuation_length=Length(continuation_size))

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
        write_header_message(io, f, msg, wsession)
        next_msg_offset += sz
        remaining_space -= sz
    end
    if remaining_space > 0
        @assert remaining_space ≥ 4 "Gaps smaller than 4 bytes should not occur"
        write_header_message(cio, Val(HmNil), 0, remaining_space)
    end
    # Extra space for object continuation
    write_continuation_placeholder(cio)
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

    for attrname in keys(dset.attributes)
        if name == attrname
            throw(ArgumentError("Attribute \"$name\" already exists. Attribute names must be unique."))
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

"""
    attributes(dset::Dataset; plain::Bool=false)

Return the attributes of a dataset as an `OrderedDict`.
If `plain` is set to `true` then the values are returned as stored in the dataset object.  
"""
function attributes(dset::Dataset; plain::Bool=false)
    plain && return dset.attributes
    OrderedDict(keys(dset.attributes) .=> map(values(dset.attributes)) do attr
        read_attr_data(dset.parent.f, attr)
    end)
end

"""
    ismmappable(dset::Dataset)

Check if a dataset can be memory-mapped. This can be useful for large arrays and for editing written arrays.

An Array dataset may be mmapped if:
    - `JLD2.samelayout(T) == true`: The element type is `isbits` and has a size that is a multiple of 8 bytes.
    - Uncompressed: Compressed arrays cannot be memory-mapped
    - Uses a contiguous layout: This is true for all array datasets written by JLD2 with version ≥ v0.4.52
    - Offset in file is a multiple of 8 bytes: This is a requirement for Mmap.
    - Windows: The file must be opened in read-only mode. This is a limitation of Mmap on Windows. 
"""
function ismmappable(dset::Dataset)
    iswritten(dset) || return false
    f = dset.parent.f
    dt = dset.datatype
    rr = jltype(f, dt)
    T = typeof(rr).parameters[1]
    !(samelayout(T)) && return false
    !isempty(dset.filters.filters) && return false
    ret = false
    if (layout = dset.layout) isa HmWrap{HmDataLayout}
        ret = (layout.layout_class == LcContiguous && layout.data_address != UNDEFINED_ADDRESS)
    end
    if ret == true && Sys.iswindows() && dset.parent.f.writable
        @warn "On Windows memory-mapping is only possible for files in read-only mode."
        ret = false
    end
    return ret
end


"""
    readmmap(dset::Dataset)

Memory-map a dataset. This can be useful for large arrays and for editing written arrays.
See [`ismmappable`](@ref) for requirements.
"""
function readmmap(dset::Dataset)
    ismmappable(dset) || throw(ArgumentError("Dataset is not mmappable"))
    f = dset.parent.f

    # figure out the element type
    dt = dset.datatype
    rr = jltype(f, dt)
    T = typeof(rr).parameters[1]
    ndims, offset = get_ndims_offset(f, ReadDataspace(f, dset.dataspace), collect(values(dset.attributes)))
    
    io = f.io
    seek(io, offset)
    dims = [jlread(io, Int64) for i in 1:ndims]
    iobackend = io isa IOStream ? io : io.f
    seek(iobackend, DataLayout(f, dset.layout).data_offset)
    return Mmap.mmap(iobackend, Array{T, Int(ndims)}, (reverse(dims)..., ))
end

@static if !Sys.iswindows()
"""
    allocate_early(dset::Dataset, T::DataType)

Write a dataset to file without any actual data. Reserve space according to element type and dimensions.
This may be useful in conjunction with [`readmmap`](@ref).

Note: Not available on Windows.
"""
function allocate_early(dset::Dataset, T::DataType)
    iswritten(dset) && throw(ArgumentError("Dataset has already been written to file"))
    # for this to work, require all information to be provided
    isnothing(dset.datatype) && throw(ArgumentError("datatype must be provided"))
    isnothing(dset.dataspace) && throw(ArgumentError("dataspace must be provided"))
    datatype = dset.datatype
    dataspace = dset.dataspace

    f = dset.parent.f
    attributes = map(collect(dset.attributes)) do (name, attr)
        attr isa WrittenAttribute && return attr
        return WrittenAttribute(f, name, attr)
        throw(ArgumentError("Invalid attribute: $a"))
    end
    writtenas = writeas(T)
    odr_ = _odr(writtenas, T, odr(writtenas))
    datasz = odr_sizeof(odr_)::Int * numel(dataspace)::Int
    psz = payload_size_without_storage_message(dataspace, datatype)::Int
    psz += sum(message_size.(attributes), init=0)
    # minimum extra space for continuation message
    psz += jlsizeof(HeaderMessage) + jlsizeof(RelOffset) + jlsizeof(Length)

    # Layout class: Use contiguous for now
    layout_class = LcContiguous
    psz += jlsizeof(Val(HmDataLayout); layout_class)
    fullsz = jlsizeof(ObjectStart) + size_size(psz) + psz + 4

    header_offset = f.end_of_data
    io = f.io
    seek(io, header_offset)
    f.end_of_data = header_offset + fullsz

    cio = begin_checksum_write(io, fullsz - 4)
    jlwrite(cio, ObjectStart(size_flag(psz)))
    write_size(cio, psz)
    write_header_message(cio, Val(HmFillValue); flags=0x09)
    write_header_message(cio, Val(HmDataspace); dataspace.dataspace_type, dimensions=dataspace.size)
    for attr in dataspace.attributes
        write_header_message(cio, f, attr)
    end
    write_header_message(cio, Val(HmDatatype), 1 | (2*isa(datatype, CommittedDatatype)); dt=datatype)
    for a in attributes
        write_header_message(cio, f, a, wsession)
    end
    # Align contiguous chunk to 8 bytes in the file
    address = f.end_of_data + 8 - mod1(f.end_of_data, 8)
    data_address = h5offset(f, address)
    write_header_message(cio, Val(HmDataLayout); 
        layout_class, data_address, data_size=datasz)

    dset.header_chunk_info = (header_offset, position(cio)+20, position(cio))
    # Add NIL message replacable by continuation message
    write_continuation_placeholder(cio)
    jlwrite(io, end_checksum(cio))

    f.end_of_data = address + datasz
    seek(io, f.end_of_data)

    offset = h5offset(f, header_offset)
    !isempty(dset.name) && (dset.parent[dset.name] = offset)
    #dset.offset = offset

    # load current dataset as new dataset
    ddset = get_dataset(f, offset, dset.parent, dset.name)
    for field in fieldnames(Dataset)
        setproperty!(dset, field, getfield(ddset, field))
    end
    return offset
end
end

struct ArrayDataset{T, N, ODR, io} <: AbstractArray{T, N}
    f::JLDFile{io}
    dset::Dataset
    dims::NTuple{N, Int}
    data_address::Int64
    rr::ReadRepresentation{T, ODR}
    writable::Bool
end
function ArrayDataset(dset::Dataset)
    isarraydataset(dset) || throw(ArgumentError("Dataset is not an array"))
    iscompressed(dset.filters) && throw(UnsupportedFeatureException("Compressed datasets are not supported."))
    f = dset.parent.f
    dt = dset.datatype
    writable = f.writable && (dset.layout.layout_class == LcContiguous)
    return ArrayDataset(
        f, dset, 
        Int.(reverse(dset.dataspace.dimensions)), 
        fileoffset(f, dset.layout.data_address), 
        jltype(f, !(f.plain) && dt isa SharedDatatype ? get(f.datatype_locations, dt.header_offset, dt) : dt),
        writable
        )
end

function isarraydataset(dset::Dataset)
    isnothing(dset.dataspace) && return false
    ds = dset.dataspace
    if ds isa HmWrap{HmDataspace}
        return ds.dataspace_type == DS_SIMPLE || ds.dataspace_type == DS_V1
    end
    return false
end

Base.IndexStyle(::Type{<:ArrayDataset}) = IndexLinear()
Base.size(A::ArrayDataset) = A.dims
Base.getindex(dset::Dataset, I...) = ArrayDataset(dset)[I...]
Base.getindex(dset::Dataset) = read_dataset(dset)
Base.setindex!(dset::Dataset, v, i, I...) = Base.setindex!(ArrayDataset(dset), v, i, I...)

function Base.getindex(A::ArrayDataset, i::Integer)
    @boundscheck checkbounds(A, i)
    seek(A.f.io, A.data_address + (i-1)*odr_sizeof(A.rr))
    return read_scalar(A.f, A.rr, UNDEFINED_ADDRESS)
end

function Base.setindex!(A::ArrayDataset{T,N,ODR}, v, i::Integer) where {T,N,ODR}
    @boundscheck checkbounds(A, i)
    A.f.writable || throw(ArgumentError("Cannot edit in read-only mode"))
    A.writable || throw(ArgumentError("Dataset cannot be edited"))
    seek(A.f.io, A.data_address + (i-1)*odr_sizeof(A.rr))
    write_data(A.f.io, A.f, v, T, datamode(ODR), JLDWriteSession())
    return v
end
