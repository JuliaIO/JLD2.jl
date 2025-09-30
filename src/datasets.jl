#
# Datasets
#
function load_dataset(f::JLDFile{IO}, offset::RelOffset) where IO
    if haskey(f.jloffset, offset)
        # There is a known (loaded) dataset at offset
        # Stored as WeakRefs and may no longer exist
        val = f.jloffset[offset].value
        val !== nothing && return val
    elseif haskey(f.loaded_groups, offset)
        # There is a known (loaded) group at offset
        return f.loaded_groups[offset]
    end

    dataspace = ReadDataspace()
    attrs = EMPTY_READ_ATTRIBUTES
    dt::H5Datatype = PlaceholderH5Datatype()
    layout::DataLayout = DataLayout(0,LcCompact,0,-1)
    filter_pipeline::FilterPipeline = FilterPipeline()

    for msg in HeaderMessageIterator(f, offset)
        if msg.type == HmDataspace
            dataspace = ReadDataspace(f, msg)
        elseif msg.type == HmDatatype
            dt = HmWrap(HmDatatype, msg).dt::H5Datatype
        elseif msg.type == HmDataLayout
            layout = DataLayout(f, msg)
        elseif msg.type == HmFilterPipeline
            filter_pipeline = FilterPipeline(msg)
        elseif msg.type == HmAttribute
            isempty(attrs) && (attrs = ReadAttribute[])
            push!(attrs, read_attribute(f, msg))
        elseif (msg.hflags & 2^3) != 0
            throw(UnsupportedFeatureException())
        end
    end
    if dt isa PlaceholderH5Datatype
        # checking whether something is a group is not cheap.
        # Do it only when it is not a dataset
        if isgroup(f, offset)
            # There is a not-yet loaded group at offset
            return get!(()->load_group(f, offset), f.loaded_groups, offset)
        end

        throw(InvalidDataException("No datatype message found"))
    end
    iscompressed(filter_pipeline) && !ischunked(layout) && throw(InvalidDataException("Compressed data must be chunked"))
    Base.inferencebarrier(read_data(f, dataspace, dt, layout, filter_pipeline, offset, attrs))
end


"""
    read_data(f::JLDFile, dataspace::ReadDataspace, datatype_class::UInt8,
              datatype_offset::Int64, data_offset::Int64[, filters::FilterPipeline,
              header_offset::RelOffset, attributes::Vector{ReadAttribute}])

Read data from a file. If `datatype_class` is `typemax(UInt8)`, the datatype is assumed to be
committed, and `datatype_offset` points to the offset of the committed datatype's header.
Otherwise, `datatype_offset` points to the offset of the datatype attribute.
"""
@nospecializeinfer function read_data(f::JLDFile, dataspace::ReadDataspace,
                   @nospecialize(dt::H5Datatype),
                   layout::DataLayout,
                   filters::FilterPipeline=FilterPipeline(),
                   header_offset::RelOffset=NULL_REFERENCE,
                   attributes::Union{Vector{ReadAttribute},Nothing}=nothing)
    rr = jltype(f, dt)
    if layout.data_offset == -1
        # There was no layout message.
        # That means, this dataset is just a datatype
        return julia_repr(rr)
    elseif layout.data_offset == typemax(Int64)
        T = julia_repr(rr)
        if layout.data_length > -1
            # TODO: this could use the fill value message to populate the array
            @warn "This array should be populated by a fill value. This is not (yet) implemented."
        end
        v = Array{T, 1}()
        track_weakref!(f, header_offset, v)
        return v
    end
    seek(f.io, layout.data_offset)
    read_dataspace = (dataspace, header_offset, layout, filters)
    read_data(f, rr, read_dataspace, attributes)
end

# Most types can only be scalars or arrays
@nospecializeinfer function read_data(f::JLDFile,
     @nospecialize(rr),
     read_dataspace::Tuple{ReadDataspace,RelOffset,DataLayout,FilterPipeline},
     attributes::Union{Vector{ReadAttribute},Nothing}=nothing)

    dataspace, header_offset, layout, filters = read_dataspace
    if dataspace.dataspace_type == DS_SCALAR
        iscompressed(filters) && throw(UnsupportedFeatureException())
        read_scalar(f, rr, header_offset)
    elseif dataspace.dataspace_type == DS_SIMPLE
        read_array(f, dataspace, rr, layout, filters, header_offset, attributes)
    elseif dataspace.dataspace_type == DS_V1 && dataspace.dimensionality == 0
        read_scalar(f, rr, header_offset)
    elseif dataspace.dataspace_type == DS_V1
        read_array(f, dataspace, rr, layout, filters, header_offset, attributes)
    elseif dataspace.dataspace_type == DS_NULL
        read_empty(f, rr, dataspace, attributes, header_offset)
    else
        throw(UnsupportedFeatureException())
    end
end

# Reference arrays can only be arrays or null dataspace (for Union{} case)
function read_data(f::JLDFile,
    ::MappedRepr{Any,RelOffset},
    read_dataspace::Tuple{ReadDataspace,RelOffset,DataLayout,FilterPipeline},
    attributes::Vector{ReadAttribute})

    dataspace, header_offset, layout, filters = read_dataspace
    iscompressed(filters) && throw(UnsupportedFeatureException())
    if dataspace.dataspace_type == DS_SIMPLE
        # Since this is an array of references, there should be an attribute
        # informing us of the type
        io = f.io
        startpos = position(io)
        for x in attributes
            if x.name == :julia_type
                T = read_attr_data(f, x)
                if isunknowntype(T)
                    str = typestring(T)
                    @warn "type $(str) does not exist in workspace; interpreting Array{$str} as Array{Any}" maxlog=1
                    rr = MappedRepr{Any,RelOffset}()
                elseif T isa Upgrade
                    rr = MappedRepr{T.target, RelOffset}()
                else
                    rr = ReadRepresentation(T, RelOffset)
                end
                seek(io, startpos)
                return read_array(f, dataspace, rr,
                                  layout, filters, header_offset, attributes)
            end
        end
    elseif dataspace.dataspace_type == DS_NULL
        return read_empty(f, MappedRepr{Union{},nothing}(), dataspace, attributes, header_offset)
    elseif dataspace.dataspace_type == DS_V1
        return read_array(f, dataspace, MappedRepr{Any,RelOffset}(),
                                  layout, filters, header_offset, attributes)
    end
    throw(UnsupportedFeatureException("Dataspace type $(dataspace.dataspace_type) not implemented"))
end

# Types with no payload can only be null dataspace
function read_data(f::JLDFile,
                   @nospecialize(rr::Union{ReadRepresentation{T,nothing} where T,
                             ReadRepresentation{T,CustomSerialization{S,nothing}} where {S,T}}),
                   read_dataspace::Tuple{ReadDataspace,RelOffset,DataLayout,FilterPipeline},
                   attributes::Vector{ReadAttribute})
    dataspace, header_offset, layout, filters = read_dataspace
    iscompressed(filters) && throw(UnsupportedFeatureException())
    dataspace.dataspace_type == DS_NULL || throw(UnsupportedFeatureException())
    if !any(a->a.name==:dimensions, attributes)
        jlconvert(rr, f, Ptr{Cvoid}(0), header_offset)
    else
        # dimensions attribute => array of empty type
        read_empty(f, rr, dataspace, attributes, header_offset)
    end
end

function read_empty(f::JLDFile, rr::ReadRepresentation{T}, dataspace, attributes, header_offset::RelOffset) where T
    ndims, offset = get_ndims_offset(f, dataspace, attributes)
    seek(f.io, offset)
    v = construct_array(f.io, T, Int(ndims))
    if isconcretetype(T)
        for i = 1:length(v)
            @inbounds v[i] = jlconvert(rr, f, Ptr{Cvoid}(0), header_offset)
        end
    end
    track_weakref!(f, header_offset, v)
    v
end

get_ndims_offset(f::JLDFile, dataspace::ReadDataspace, attributes::Nothing) =
    (dataspace.dimensionality, dataspace.dimensions_offset)

function get_ndims_offset(f::JLDFile, dataspace::ReadDataspace, attributes::AbstractVector)
    ndims = dataspace.dimensionality
    offset = dataspace.dimensions_offset
    if !isempty(attributes)
        for x in attributes
            if x.name == :dimensions
                (x.dataspace.dataspace_type == DS_SIMPLE &&
                 x.dataspace.dimensionality == 1) || throw(InvalidDataException())
                x.datatype == h5fieldtype(f, Int64, Int64, Val{true}) || throw(UnsupportedFeatureException())
                seek(f.io, x.dataspace.dimensions_offset)
                ndims = UInt8(jlread(f.io, Length))
                offset = x.data_offset
            end
        end
    end
    (ndims, offset)
end

"""
    construct_array(io::IO, eltype, ndims::Int)

Construct array by reading `ndims` dimensions from `io`. Assumes `io` has already been
seeked to the correct position.
"""
function construct_array(io::IO, ::Type{T}, N::Int) where {T}
    ds = reverse(ntuple(i->jlread(io, Int64), Val(N)))
    Array{T,N}(undef, ds...)
end

@nospecializeinfer function read_array(f::JLDFile, dataspace::ReadDataspace,
                    @nospecialize(rr::ReadRepresentation), layout::DataLayout,
                    filters::FilterPipeline, header_offset::RelOffset,
                    attributes::Union{Vector{ReadAttribute},Nothing})
    T = julia_repr(rr)
    io = f.io
    ndims, offset = get_ndims_offset(f, dataspace, attributes)
    seek(io, offset)
    # figure out whether to construct an Array or a Memory
    ismem = !isnothing(attributes) && any(attr->attr.name==:Memory, attributes)
    v = @static if VERSION >= v"1.11"
        if ismem
            Memory{T}(undef, Int(jlread(io, Int64)))
        else
            construct_array(io, T, Int(ndims))
        end
    else
        construct_array(io, T, Int(ndims))
    end

    if !ischunked(layout) || (layout.chunk_indexing_type == 1)
        # Contiguous or compact storage - read directly
        n = length(v)
        seek(io, layout.data_offset)
        if iscompressed(filters)
            read_compressed_array!(v, f, rr, layout.data_length, filters)
        else
            read_array!(v, f, rr)
        end
        track_weakref!(f, header_offset, v)
        v
    else
        # Chunked storage - dispatch to specialized handler
        read_chunked_array(f, v, dataspace, rr, layout, filters, header_offset, Int(ndims))
    end
end


function payload_size_without_storage_message(dataspace::WriteDataspace, datatype::H5Datatype)
    sz = 6 + 4 + jlsizeof(dataspace)::Int + 4 + jlsizeof(datatype) + length(dataspace.attributes)*jlsizeof(HeaderMessage)
    for attr in dataspace.attributes
        sz += jlsizeof(attr)::Int
    end
    sz
end

@nospecializeinfer function write_dataset(f::JLDFile,
        dataspace::WriteDataspace,
        datatype::H5Datatype,
        @nospecialize(odr),
        @nospecialize(data),
        wsession::JLDWriteSession,
        @nospecialize(filters = f.compress),)
    io = f.io
    datasz = (odr_sizeof(odr)::Int * numel(dataspace))::Int
    psz = payload_size_without_storage_message(dataspace, datatype)
    psz += CONTINUATION_MSG_SIZE

    local_filters = FilterPipeline(map(filters) do filter
        Filters.set_local(filter, odr, dataspace, ())
    end)

    # Figure out the layout
    if datasz == 0 || (!(data isa ArrayMemory) && datasz < 8192)
        layout_class = LcCompact
        psz += jlsizeof(Val(HmDataLayout); layout_class, data_size=datasz)
    elseif data isa ArrayMemory && isconcretetype(eltype(data)) && isbitstype(eltype(data)) &&
           iscompressed(local_filters)
        layout_class = LcChunked
        psz += jlsizeof(Val(HmDataLayout);
            version = 3,
            layout_class,
            dimensions = fill(1, ndims(data)+1),
            # Dummy values for message size computation
            data_address = 0,
        )
        psz += Filters.pipeline_message_size(local_filters)
    else
        layout_class = LcContiguous
        psz += jlsizeof(Val(HmDataLayout); layout_class)
    end

    fullsz = jlsizeof(ObjectStart) + size_size(psz) + psz + 4

    header_offset = f.end_of_data
    seek(io, header_offset)
    f.end_of_data = header_offset + fullsz

    track!(wsession, data, h5offset(f, header_offset))
    cio = begin_checksum_write(io, fullsz - 4)
    jlwrite(cio, ObjectStart(size_flag(psz)))
    write_size(cio, psz)
    write_header_message(cio, Val(HmFillValue); flags=0x09)
    write_header_message(cio, Val(HmDataspace); dataspace.dataspace_type, dimensions=dataspace.size)
    for attr in dataspace.attributes
        write_header_message(cio, f, attr)
    end
    write_header_message(cio, Val(HmDatatype), 1 | (2*isa(datatype, CommittedDatatype)); dt=datatype)

    # Data storage layout
    if layout_class == LcCompact
        write_header_message(cio, Val(HmDataLayout); layout_class, data_size=datasz)
        if datasz != 0
            write_data(cio, f, data, odr, datamode(odr), wsession)
        end
        write_continuation_placeholder(cio)
        jlwrite(io, end_checksum(cio))
    elseif layout_class == LcChunked
        # Fall back to current simple compression approach
        compressed, retcodes = Filters.compress(local_filters, data, odr, f, wsession)
        Filters.write_filter_pipeline_message(cio, local_filters)

        write_header_message(cio, Val(HmDataLayout);
            layout_class,
            # Reversed dimensions with element size as last dim
            dimensions=UInt64.((reverse(size(data))..., odr_sizeof(odr))),
            data_size=length(compressed),
            data_address=h5offset(f, f.end_of_data)
        )

        write_continuation_placeholder(cio)
        jlwrite(f.io, end_checksum(cio))

        seek(f.io, f.end_of_data)
        f.end_of_data += length(compressed)
        jlwrite(f.io, compressed)
    else
        data_address = f.end_of_data + 8 - mod1(f.end_of_data, 8)
        write_header_message(cio, Val(HmDataLayout);
            layout_class, data_address=h5offset(f, data_address), data_size=datasz)
        write_continuation_placeholder(cio)
        jlwrite(io, end_checksum(cio))

        f.end_of_data = data_address + datasz
        seek(io, data_address)
        write_data(io, f, data, odr, datamode(odr), wsession)
    end

    h5offset(f, header_offset)
end


@nospecializeinfer function write_dataset(f::JLDFile, @nospecialize(x), wsession::JLDWriteSession)::RelOffset
    offset = get_tracked(wsession, x)
    offset != UNDEFINED_ADDRESS && return offset
    odr = objodr(x)
    write_dataset(f, WriteDataspace(f, x, odr), h5type(f, x), odr, x, wsession)::RelOffset
end

write_ref(f::JLDFile, @nospecialize(x), wsession::JLDWriteSession) = write_dataset(f, x, wsession)::RelOffset
write_ref(f::JLDFile, x::RelOffset, wsession::JLDWriteSession) = x

Base.delete!(f::JLDFile, x::AbstractString) = delete!(f.root_group, x)

function Base.delete!(g::Group, name::AbstractString)
    g.f.writable || throw(ArgumentError("Cannot delete in read-only mode"))
    if !haskey(g, name)
        @warn "No entry named $name was found"
        return
    end
    (g, name) = pathize(g, name, false)

    # Simple case first. If it hasn't been written yet,
    # the file doesn't need to be altered.
    if haskey(g.unwritten_links, name)
        delete!(g.unwritten_links, name)
        return
    elseif haskey(g.unwritten_child_groups, name)
        delete!(g.unwritten_child_groups, name)
        return
    end

    # Dataset must already exist in the file
    # Retrieve offset of group in file
    offset = group_offset(g)
    offset == NULL_REFERENCE && throw(InternalError("Group could not be found."))
    delete_written_link!(g.f, offset, name)
    delete!(g.written_links, name)
    return
end


function group_offset(g::Group)
    # Given a group, retrieve its offset inside the parent file
    # Return a null reference when it can't be found. However, that
    # should never be the case as the calling functions checks for this
    if g === g.f.root_group
        return g.f.root_group_offset
    end
    for (offset, lg) in pairs(g.f.loaded_groups)
        if lg === g
            return offset
        end
    end
    return NULL_REFERENCE
end

function delete_written_link!(f::JLDFile, roffset::RelOffset, name::AbstractString)
    # Location of written link in group structure is not known a priory
    # The deletion is done by replacing that message by a placeholder nill message
    iter = HeaderMessageIterator(f, roffset)
    for msg in iter
        if msg.type == HmLinkMessage && HmWrap(HmLinkMessage, msg).link_name == name
            # delete link
            seek(f.io, fileoffset(f, msg.offset))
            write_header_message(f.io, Val(HmNil), 0, msg.size)
            update_checksum(f.io, iter.chunk.chunk_start, iter.chunk.chunk_end)
        end
    end
    return
end
