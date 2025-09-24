"""
    Dataset

A mutable struct representing an HDF5/JLD2 dataset with explicit control over metadata.

The `Dataset` type allows low-level access to dataset metadata and provides fine-grained
control over how data is stored, including compression, chunking, and layout options.
This is useful for advanced use cases where you need more control than the standard
`jldsave`/`load` interface provides.
# Usage

Datasets are created using [`create_dataset`](@ref), written with [`write_dataset`](@ref),
and read with [`read_dataset`](@ref) or retrieved with [`get_dataset`](@ref).

# Example
```julia
jldopen("data.jld2", "w") do f
    # Create a dataset with compression
    dset = JLD2.create_dataset(f, "compressed_data")
    dset.filters = Deflate()
    JLD2.write_dataset(dset, rand(1000, 1000))

    # Add attributes
    JLD2.add_attribute(dset, "description", "Random data with compression")
end
```

See also: [`create_dataset`](@ref), [`write_dataset`](@ref), [`read_dataset`](@ref), [`get_dataset`](@ref)
"""
mutable struct Dataset
    parent::Group
    name::String
    offset::RelOffset
    datatype
    dataspace
    layout
    attributes::OrderedDict{String, Any}
    chunk
    filters
    header_chunk_info # chunk_start, chunk_end, next_msg_offset
end


"""
    create_dataset(parent, path, datatype=nothing, dataspace=nothing; layout=nothing, chunk=nothing, filters=FilterPipeline(), allocate=false)

Create a new [`Dataset`](@ref) object with specified metadata, ready for writing data.

This function creates a dataset specification but does not write any data to the file.
The dataset must be written using [`write_dataset`](@ref) to actually store data.
This two-step process allows you to configure compression, attributes, and other
metadata before writing.

# Arguments
- `parent::Union{JLDFile, Group}`: The containing file or group for the new dataset
- `path::Union{String, Nothing}`: Path to the dataset relative to `parent`.
  If `nothing`, creates an unnamed dataset
- `datatype`: HDF5 datatype specification OR a Julia `Type`.
  If `nothing`, will be inferred from data during writing.
- `dataspace`: Dataspace describing dimensions OR a `Tuple`/`Vector` of integers representing dimensions.
  If `nothing`, will be inferred from data during writing.
- `allocate::Bool`: (Experimental) If `true`, allocates space for the dataset in the file immediately.
  Requires `datatype` and `dataspace` to be provided.
  Returns an `ArrayDataset` wrapper that allows writing to the dataset via array indexing (e.g. `dset[i] = val`).

## Returns
- `Dataset` (or `ArrayDataset` if `allocate=true`): A mutable dataset object ready for configuration and writing

## Notes
- The dataset is not written to the file until [`write_dataset`](@ref) is called (unless `allocate=true`)
- Datatype and dataspace are usually inferred automatically from the data
- Compression filters can be added after creation but before writing
- Attributes can be added before or after writing (see [`add_attribute`](@ref))

See also: [`write_dataset`](@ref), [`Dataset`](@ref), [`add_attribute`](@ref), [`Deflate`](@ref)

## Usage Example

```julia
jldopen("data.jld2", "w") do f
    dset = JLD2.create_dataset(f, "my_data")
    JLD2.write_dataset(dset, [1, 2, 3, 4, 5])
end

jldopen("data.jld2", "w") do f
    dset = JLD2.create_dataset(f, "compressed_array")
    dset.filters = Deflate()  # Add gzip compression
    JLD2.add_attribute(dset, "experiment_id", "exp_001")
    JLD2.write_dataset(dset, rand(10000))
end

# Streaming write (pre-allocate space)
jldopen("data.jld2", "w") do f
    dset = JLD2.create_dataset(f, "streamed", Float64, (100,); allocate=true)
    for i in 1:100
        dset[i] = rand()
    end
end
```
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
    allocate::Bool=false,
)
    if !isnothing(path)
        (parent, name) = pathize(g, path, true)
    else
        name = ""
        parent = g
    end

    T = nothing
    if datatype isa Type
        T = datatype
        datatype = h5fieldtype(g.f, T, T, Val{false})
    end

    if dataspace isa NTuple{N, Integer} where N || dataspace isa Vector{<:Integer}
         dataspace = WriteDataspace(DS_SIMPLE, UInt64.(reverse(Tuple(dataspace))), ())
    end

    dset = Dataset(parent, name, UNDEFINED_ADDRESS, datatype, dataspace,
            layout, OrderedDict{String,Any}(), chunk, filters, nothing)

    if allocate
        allocate_early(dset, T)
        return ArrayDataset(dset)
    end

    return dset
end

iswritten(dset::Dataset) = (dset.offset != UNDEFINED_ADDRESS)

function Base.show(io::IO, ::MIME"text/plain", dset::Dataset)
    print_dataset_header(io, dset)
    prefix = "│  "
    print_datatype_info(io, dset, prefix)
    print_dataspace_info(io, dset, prefix)
    print_layout_info(io, dset, prefix)
    print_filters_info(io, dset, prefix)
    print_attributes_info(io, dset, prefix)
    println(io, "└─")
end

function print_dataset_header(io::IO, dset::Dataset)
    print(io, "┌─ Dataset:")
    print(io, isempty(dset.name) ? " (unnamed)" : " \"$(dset.name)\"")
    println(io, iswritten(dset) ? " at $(dset.offset)" : " (unwritten)")
end

function print_datatype_info(io::IO, dset::Dataset, prefix::String)
    isnothing(dset.datatype) && return

    dt = dset.datatype
    f = dset.parent.f
    iscommitted = isshared(dt)
    tpstr = replace(string(typeof(dt)), "JLD2."=>"")
    print(io, prefix, "datatype: ", tpstr)
    if iscommitted
        print(io, " (committed at ", dt.header_offset, ")\n")
    else
        print(io, "\n")
    end
    # print_datatype_structure
    if dt isa BasicDatatype && dt.class%16 == DT_REFERENCE
        println(io, prefix, "\ttype name: RelOffset")
        return
    end
    julia_type_str, _, field_strs = stringify_h5datatype(f, dt, showfields=true)

    println(io, prefix, "\ttype name: ", julia_type_str)
    if !isempty(field_strs)
        println(io, prefix, "\tstored fields:")
        foreach(field_str -> println(io, prefix, "\t\t", field_str), field_strs)
    end
end

function print_dataspace_info(io::IO, dset::Dataset, prefix::String)
    isnothing(dset.dataspace) && return

    ds = dset.dataspace
    if ds isa HmWrap{HmDataspace}
        spacetype_map = Dict(0x00=>"Scalar", 0x01=>"Simple", 0x02=>"Null", 0xff=>"V1")
        println(io, prefix, "dataspace:")
        println(io, prefix, "\ttype: ", spacetype_map[ds.dataspace_type])
        println(io, prefix, "\tdimensions: ", ds.dimensions)
    else
        println(io, prefix, "dataspace: ", ds)
    end
end

function print_layout_info(io::IO, dset::Dataset, prefix::String)
    isnothing(dset.layout) && return

    layout = dset.layout
    if layout isa HmWrap{HmDataLayout}
        println(io, prefix, "layout:")
        println(io, prefix, "\tclass: ", layout.layout_class)
    else
        println(io, prefix, "layout: ", layout)
    end
end

function print_filters_info(io::IO, dset::Dataset, prefix::String)
    (!isnothing(dset.filters) && !isempty(dset.filters.filters)) && println(io, prefix, "filters: ", dset.filters)
end

function print_attributes_info(io::IO, dset::Dataset, prefix::String)
    isempty(dset.attributes) && return

    println(io, prefix, "Attributes:")
    for (k, attr) in pairs(dset.attributes)
        if attr isa ReadAttribute
            attr_info = safe_read_attribute_info(dset.parent.f, attr)
            println(io, prefix, "\t", attr.name, " = ", attr_info)
        else
            println(io, prefix, "\t", k, " = ", attr)
        end
    end
end

"""
    write_dataset(dataset::Dataset, data, wsession=JLDWriteSession())

Write data to file using the metadata and configuration stored in the [`Dataset`](@ref) object.

This function performs the actual data writing operation after a dataset has been created
with [`create_dataset`](@ref). The dataset must not have been written before (each dataset
can only be written once). The data type and dataspace will be automatically inferred
from the provided data if they weren't specified during dataset creation.

## Arguments
- `dataset::Dataset`: The dataset object created with [`create_dataset`](@ref)
- `data`: The data to write. Can be any Julia object that JLD2 can serialize
- `wsession::JLDWriteSession`: Optional write session for advanced usage (default: new session)

## Returns
- `RelOffset`: The file offset where the dataset was written
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
    compressor = Filters.normalize_filters(dataset.filters)
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

Read and return the complete dataset from the file and reconstructs the original
Julia object.
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

Retrieve a [`Dataset`](@ref) object from a file without reading the actual data.
This is useful for inspecting dataset properties, accessing attributes, or preparing for selective data reading.
The returned `Dataset` object can be used with [`read_dataset`](@ref), [`readmmap`](@ref),
or array indexing operations.

## Arguments
- `parent`: The file or group containing the dataset
- `name::String`: Name or path of the dataset relative to the parent
"""
get_dataset(f::JLDFile, args...; kwargs...) =
    get_dataset(f.root_group, args...; kwargs...)

function get_dataset(g::Group, name::String)
    f = g.f
    f.n_times_opened == 0 && throw(ArgumentError("file is closed"))

    (g, name) = pathize(g, name, false)

    if isempty(name)
        # this is a group
        return get_dataset(f, group_offset(g), g, name)
    end

    link = lookup_link(g, name)
    offset = getoffset(g, link)
    return get_dataset(f, offset, g, name)
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
message_size(msg::Pair{String, Link}) = message_size_for_link(msg.first, msg.second)

write_header_message(io, f, msg::Pair{String, RelOffset}, _=nothing) =
    write_header_message(io, Val(HmLinkMessage); link_name=msg.first, target=msg.second)
write_header_message(io, f, msg::Pair{String, Link}, _=nothing) =
    write_link_message(io, msg.first, msg.second)

"""
    message_size_for_link(name::String, link::Link) -> Int

Calculate the size of a link message for the given link type.
"""
function message_size_for_link(link_name::String, link::Link)
    is_hard_link(link) && return jlsizeof(Val(HmLinkMessage); link_name)

    flags = UInt8(0x10 | 0x08 | size_flag(sizeof(link_name)))
    if is_soft_link(link)
        jlsizeof(Val(HmLinkMessage); link_name, flags, link_type=UInt8(1),
                 link_info_size=sizeof(link.path), soft_link=UInt8[])
    else  # external link
        jlsizeof(Val(HmLinkMessage); link_name, flags, link_type=UInt8(64),
                 link_info_size=3+sizeof(link.external_file)+sizeof(link.path),
                 external_link=UInt8[])
    end
end

"""
    write_link_message(io, name::String, link::Link)

Write a link message for the given link type to the I/O stream.
"""
function write_link_message(io, link_name::String, link::Link)
    if is_hard_link(link)
        return write_header_message(io, Val(HmLinkMessage); link_name, target=link.offset)
    end
    flags = UInt8(0x10 | 0x08 | size_flag(sizeof(link_name)))
    if is_soft_link(link)
        soft_link = Vector{UInt8}(link.path)
        write_header_message(io, Val(HmLinkMessage); link_name, flags, link_type=1, soft_link)
    elseif is_external_link(link)
        # External link data: two null-terminated strings
        external_link = vcat(0x00, Vector{UInt8}(link.external_file), 0x00,
                            Vector{UInt8}(link.path), 0x00)
        write_header_message(io, Val(HmLinkMessage); link_name, flags, link_type=64,
                           external_link)
    end
end

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

"""
    add_attribute(dset::Dataset, name::String, data, wsession=JLDWriteSession())

Add an attribute with the specified name and data to a [`Dataset`](@ref).

Attributes are metadata key-value pairs associated with datasets. They can store
additional information about the data such as units, descriptions, creation dates,
or any other relevant metadata. Attributes can be added before or after the dataset
has been written to the file.

## Arguments
- `dset::Dataset`: The dataset to add the attribute to
- `name::String`: The attribute name (must be unique within the dataset)
- `data`: The attribute value (can be any JLD2-serializable Julia object)
- `wsession::JLDWriteSession`: Optional write session for advanced usage (default: new session)

"""
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
    - `JLD2.samelayout(T) == true`: The element type is `isbits` and has a size that either 1, 2, 4, or a multiple of 8 bytes.
    - Uncompressed: Compressed arrays cannot be memory-mapped
    - Uses a contiguous layout: This is true for all array datasets written by JLD2 with version ≥ v0.4.52
    - Windows: The file must be opened in read-only mode. This is a limitation of Mmap on Windows.
"""
function ismmappable(dset::Dataset)
    iswritten(dset) || return false
    f = dset.parent.f
    dt = dset.datatype
    rr = jltype(f, dt)
    T = julia_repr(rr)
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
    T = julia_repr(rr)
    ndims, offset = get_ndims_offset(f, ReadDataspace(f, dset.dataspace), collect(values(dset.attributes)))

    io = f.io
    seek(io, offset)
    dims = reverse([jlread(io, Int64) for i in 1:ndims])
    iobackend = io isa IOStream ? io : io.f
    seek(iobackend, DataLayout(f, dset.layout).data_offset)
    # May be pos != data_offset for files with custom offset
    pos = position(iobackend)
    if (pos % 8 == 0) ||
       (pos % 2 == 0 && sizeof(T) == 2) ||
       (pos % 4 == 0 && sizeof(T) == 4) ||
       (sizeof(T) == 1)
        # These are cases where we can directly mmap the data.
        Mmap.mmap(iobackend, Array{T,Int(ndims)}, (dims...,))
    else
        dims[1] *= sizeof(T)
        # A fallback for all other cases is to mmap the data as UInt8 and reinterpret it.
        reinterpret(T,
            Mmap.mmap(iobackend, Array{UInt8,Int(ndims)}, (dims...);)
        )
    end
end

"""
    allocate_early(dset::Dataset, T::DataType)

Write a dataset to file without any actual data. Reserve space according to element type and dimensions.
This may be useful in conjunction with [`readmmap`](@ref) or by explicitly writing elements afterwards using [`ArrayDataset`](@ref).
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

struct ArrayDataset{T, N, RR<:ReadRepresentation{T}, io} <: AbstractArray{T, N}
    f::JLDFile{io}
    dset::Dataset
    dims::NTuple{N, Int}
    data_address::Int64
    rr::RR
    writable::Bool
end
function ArrayDataset(dset::Dataset)
    isarraydataset(dset) || throw(ArgumentError("Dataset is not an array"))
    iscompressed(dset.filters) && throw(UnsupportedFeatureException("Compressed datasets are not supported."))
    f = dset.parent.f
    dt = dset.datatype
    writable = f.writable && (dset.layout.layout_class == LcContiguous)
    rr = jltype(f, !(f.plain) && dt isa SharedDatatype ? get(f.datatype_locations, dt.header_offset, dt) : dt)
    return ArrayDataset(
        f, dset,
        Int.(reverse(dset.dataspace.dimensions)),
        fileoffset(f, dset.layout.data_address),
        rr,
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
    write_data(A.f.io, A.f, v, odr(T), datamode(ODR), JLDWriteSession())
    return v
end
