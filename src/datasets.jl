#
# Datasets
#



function load_dataset(f::JLDFile, offset::RelOffset)
    if haskey(f.jloffset, offset)
        # There is a known (loaded) dataset at offset
        # Stored as WeakRefs and may no longer exist
        val = f.jloffset[offset].value
        val !== nothing && return val
    elseif haskey(f.loaded_groups, offset)
        # There is a known (loaded) group at offset
        return f.loaded_groups[offset]
    elseif isgroup(f, offset)
        # There is a not-yet loaded group at offset
        return get!(()->load_group(f, offset), f.loaded_groups, offset)
    end

    dataspace = ReadDataspace()
    attrs = EMPTY_READ_ATTRIBUTES
    dt::H5Datatype = PlaceholderH5Datatype()
    layout::DataLayout = DataLayout(0,LC_COMPACT_STORAGE,0,-1)
    filter_pipeline::FilterPipeline = FilterPipeline(Filter[])

    for msg in HeaderMessageIterator(f, offset)
        if msg.type == HM_DATASPACE
            dataspace = ReadDataspace(f, msg)
        elseif msg.type == HM_DATATYPE
            dt = msg.dt::H5Datatype
        elseif msg.type == HM_DATA_LAYOUT
            layout = DataLayout(f, msg)
        elseif msg.type == HM_FILTER_PIPELINE
            filter_pipeline = FilterPipeline(msg)
        elseif msg.type == HM_ATTRIBUTE
            isempty(attrs) && (attrs = ReadAttribute[]) 
            push!(attrs, read_attribute(f, msg))
        elseif (msg.hflags & 2^3) != 0
            throw(UnsupportedFeatureException())
        end
    end
    if dt isa PlaceholderH5Datatype
        throw(InvalidDataException("No datatype message found"))
    end
    iscompressed(filter_pipeline) && !ischunked(layout) && throw(InvalidDataException("Compressed data must be chunked"))

    # TODO verify that data length matches
    read_data(f, dataspace, dt, layout,
                    filter_pipeline, offset, attrs)
end


"""
    read_attr_data(f::JLDFile, attr::ReadAttribute)

[`jlread`](@ref) data from an attribute.
"""
read_attr_data(f::JLDFile, attr::ReadAttribute) =
    read_data(f, attr.dataspace, attr.datatype,
              DataLayout(0,LC_COMPACT_STORAGE,-1,attr.data_offset))

"""
    read_attr_data(f::JLDFile, attr::ReadAttribute, expected_datatype::H5Datatype,
                   rr::ReadRepresentation)

[`jlread`](@ref) data from an attribute, assuming a specific HDF5 datatype and
[`ReadRepresentation`](@ref). If the HDF5 datatype does not match, throws an
`UnsupportedFeatureException`. This allows better type stability while
simultaneously validating the data.
"""
function read_attr_data(f::JLDFile, attr::ReadAttribute, expected_datatype::H5Datatype,
                        rr::ReadRepresentation)
    if attr.datatype == expected_datatype
        seek(f.io, attr.data_offset)
        read_dataspace = (attr.dataspace, NULL_REFERENCE, DataLayout(0,LC_COMPACT_STORAGE,-1,attr.data_offset), FilterPipeline())
        return read_data(f, rr, read_dataspace)
    end
    throw(UnsupportedFeatureException())
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
    # See if there is a julia type attribute
    io = f.io
    if dt isa SharedDatatype
        # this means that it is "committed" to `_types` if the file was written by JLD2  
        rr = jltype(f, get(f.datatype_locations, dt.header_offset, dt))

        if layout.data_offset == -1
            # There was no layout message.
            # That means, this dataset is just a datatype
            # return the Datatype
            return typeof(rr).parameters[1]
        end

        seek(io, layout.data_offset)
        read_dataspace = (dataspace, header_offset, layout, filters)
        read_data(f, rr, read_dataspace, attributes)
        
    elseif layout.data_offset == typemax(Int64)
        rr = jltype(f, dt)
        T,S = typeof(rr).parameters
        if layout.data_length > -1
            # TODO: this could use the fill value message to populate the array
            @warn "This array should be populated by a fill value. This is not (yet) implemented."
        end
        v = Array{T, 1}()
        header_offset !== NULL_REFERENCE && (f.jloffset[header_offset] = WeakRef(v))
        return v
    else
        dtt = dt
        rr = jltype(f, dtt)

        if layout.data_offset == -1
            # There was no layout message.
            # That means, this dataset is just a datatype
            # return the Datatype
            return typeof(rr).parameters[1]
        end

        seek(io, layout.data_offset)
        read_dataspace = (dataspace, header_offset, layout, filters)
        read_data(f, rr, read_dataspace, attributes)
    end
end

# Most types can only be scalars or arrays
function read_data(f::JLDFile,
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
    else
        throw(UnsupportedFeatureException())
    end
end

# Reference arrays can only be arrays or null dataspace (for Union{} case)
function read_data(f::JLDFile,
    rr::ReadRepresentation{Any,RelOffset},
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
                    rr = ReadRepresentation{Any,RelOffset}()
                elseif T isa Upgrade
                    rr = ReadRepresentation{T.target, RelOffset}()
                else
                    rr = ReadRepresentation{T, RelOffset}()
                end
                seek(io, startpos)
                return read_array(f, dataspace, rr,
                                  layout, FilterPipeline(), header_offset, attributes)
            end
        end
    elseif dataspace.dataspace_type == DS_NULL
        return read_empty(ReadRepresentation{Union{},nothing}(), f,
                          attributes[find_dimensions_attr(attributes)],
                          header_offset)
    elseif dataspace.dataspace_type == DS_V1
        return read_array(f, dataspace, ReadRepresentation{Any,RelOffset}(),
                                  layout, FilterPipeline(), header_offset, attributes)
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

    dimensions_attr_index = find_dimensions_attr(attributes)
    if dimensions_attr_index == 0
        jlconvert(rr, f, Ptr{Cvoid}(0), header_offset)
    else
        # dimensions attribute => array of empty type
        read_empty(rr, f, attributes[dimensions_attr_index], header_offset)
    end
end

function find_dimensions_attr(attributes::Vector{ReadAttribute})
    dimensions_attr_index = 0
    julia_type_attr_index = 0
    for i = 1:length(attributes)
        x = attributes[i]
        if x.name == :dimensions
            dimensions_attr_index = i
        end
    end
    dimensions_attr_index
end

function read_empty(rr::ReadRepresentation{T}, f::JLDFile,
                 dimensions_attr::ReadAttribute, header_offset::RelOffset) where T
    dimensions_attr.datatype.class == DT_FIXED_POINT || throw(UnsupportedFeatureException())

    io = f.io
    seek(io, dimensions_attr.dataspace.dimensions_offset)
    ndims = Int(jlread(io, Length))

    dimensions_attr.datatype == h5fieldtype(f, Int64, Int64, Val{true}) || throw(UnsupportedFeatureException())

    seek(io, dimensions_attr.data_offset)
    v = construct_array(io, T, Val(ndims))
    if isconcretetype(T)
        for i = 1:length(v)
            @inbounds v[i] = jlconvert(rr, f, Ptr{Cvoid}(0), header_offset)
        end
    end
    header_offset !== NULL_REFERENCE && (f.jloffset[header_offset] = WeakRef(v))
    v
end

get_ndims_offset(f::JLDFile, dataspace::ReadDataspace, attributes::Nothing) =
    (dataspace.dimensionality, dataspace.dimensions_offset)

function get_ndims_offset(f::JLDFile, dataspace::ReadDataspace, attributes::Vector{ReadAttribute})
    ndims = dataspace.dimensionality
    offset = dataspace.dimensions_offset
    if !isempty(attributes)
        for x in attributes
            if x.name == :dimensions
                (x.dataspace.dataspace_type == DS_SIMPLE &&
                 x.dataspace.dimensionality == 1) || throw(InvalidDataException())
                seek(f.io, x.dataspace.dimensions_offset)
                ndims = UInt8(jlread(f.io, Length))
                offset = x.data_offset
            end
        end
    end
    (ndims, offset)
end

"""
    construct_array{T}(io::IO, ::Type{T}, ::Val{ndims})

Construct array by reading `ndims` dimensions from `io`. Assumes `io` has already been
seeked to the correct position.
"""
function construct_array(io::IO, ::Type{T}, ::Val{1}) where {T}
    n = jlread(io, Int64)
    Vector{T}(undef, n)
end

function construct_array(io::IO, ::Type{T}, ::Val{2}) where {T}
    d2 = jlread(io, Int64)
    d1 = jlread(io, Int64)
    Matrix{T}(undef, d1, d2)
end

function construct_array(io::IO, ::Type{T}, ::Val{3}) where {T}
    d3 = jlread(io, Int64)
    d2 = jlread(io, Int64)
    d1 = jlread(io, Int64)
    Array{T,3}(undef, d1, d2, d3)
end        

function construct_array(io::IO, ::Type{T}, ::Val{N})::Array{T,N} where {T,N}
    ds = reverse(ntuple(i->jlread(io, Int64), Val(N)))
    Array{T,N}(undef, ds...)
end

function read_array(f::JLDFile, dataspace::ReadDataspace,
                    @nospecialize(rr::ReadRepresentation), layout::DataLayout,
                    filters::FilterPipeline, header_offset::RelOffset,
                    attributes::Union{Vector{ReadAttribute},Nothing})
    T = eltype(rr)
    io = f.io
    data_offset = layout.data_offset
    if !ischunked(layout) || (layout.chunk_indexing_type == 1)
        ndims, offset = get_ndims_offset(f, dataspace, attributes)

        seek(io, offset)
        v = construct_array(io, T, Val(Int(ndims)))
        n = length(v)
        seek(io, data_offset)
        if iscompressed(filters)
            read_compressed_array!(v, f, rr, layout.data_length, filters)
        else
            read_array!(v, f, rr)
        end
        header_offset !== NULL_REFERENCE && (f.jloffset[header_offset] = WeakRef(v))
        v
    else
        ndims, offset = get_ndims_offset(f, dataspace, attributes)
        seek(io, offset)
        v = construct_array(io, T, Val(Int(ndims)))
        if layout.version == 3 
            # version 1 B-tree
            # This version appears to be padding incomplete chunks
            chunks = read_v1btree_dataset_chunks(f, h5offset(f, layout.data_offset), layout.dimensionality)                
            vchunk = Array{T, Int(ndims)}(undef, reverse(layout.chunk_dimensions)...)
            for chunk in chunks
                cidx = chunk.idx::NTuple{Int(ndims+1), Int}
                idx = reverse(cidx[1:end-1])
                seek(io, fileoffset(f, chunk.offset))
                indexview =  (:).(idx .+1, min.(idx .+ reverse(layout.chunk_dimensions), size(v)))
                indexview2 = (:).(1, length.(indexview))
        
                if iscompressed(filters)
                    if chunk.filter_mask == 0
                        read_compressed_array!(vchunk, f, rr, chunk.chunk_size, filters)
                        v[indexview...] = @view vchunk[indexview2...]    
                    else
                        if length(filters.filters) == 1
                            read_array!(vchunk, f, rr)
                            v[indexview...] = @view vchunk[indexview2...]
                        else
                            mask = Bool[chunk.filter_mask & 2^(n-1) == 0 for n=eachindex(filters.filters)]
                            if any(mask)
                                rf = FilterPipeline(filters.filters[mask])
                                read_compressed_array!(vchunk, f, rr, chunk.chunk_size, rf)
                                v[indexview...] = @view vchunk[indexview2...]
                            else
                                read_array!(vchunk, f, rr)
                                v[indexview...] = @view vchunk[indexview2...]
                            end

                        end
                    end
                else
                    read_array!(vchunk, f, rr)
                    v[indexview...] = @view vchunk[indexview2...]
                end
            end
            return v
        end
        throw(UnsupportedVersionException("Encountered a chunked array ($layout) that is not implemented."))
    end
end


function payload_size_without_storage_message(dataspace::WriteDataspace, datatype::H5Datatype)
    sz = 6 + 4 + jlsizeof(dataspace)::Int + 4 + jlsizeof(datatype) + length(dataspace.attributes)*jlsizeof(HeaderMessage)
    for attr in dataspace.attributes
        sz += jlsizeof(attr)::Int
    end
    sz
end


@nospecializeinfer function write_dataset(
        f::JLDFile,
        dataspace::WriteDataspace,
        datatype::H5Datatype,
        @nospecialize(odr),
        @nospecialize(data::Array),
        wsession::JLDWriteSession,
        @nospecialize(compress = f.compress),
        )
    T = eltype(data)
    io = f.io
    datasz = odr_sizeof(odr)::Int * numel(dataspace)::Int
    #layout_class
    if datasz < 8192
        layout_class = LC_COMPACT_STORAGE
    elseif compress != false
        layout_class = LC_CHUNKED_STORAGE
    else
        layout_class = LC_CONTIGUOUS_STORAGE
    end
    psz = payload_size_without_storage_message(dataspace, datatype)::Int
    if datasz < 8192
        layout_class = LC_COMPACT_STORAGE
        psz += jlsizeof(CompactStorageMessage) + datasz
    elseif compress != false && isconcretetype(T) && isbitstype(T)
        # Only now figure out if the compression argument is valid
        invoke_again, filter_id, compressor = get_compressor(compress)
        if invoke_again
            return Base.invokelatest(write_dataset, f, dataspace, datatype, odr, data, wsession, compress)::RelOffset
        end
        layout_class = LC_CHUNKED_STORAGE
        psz += chunked_storage_message_size(ndims(data)) + pipeline_message_size(filter_id::UInt16)
    else
        layout_class = LC_CONTIGUOUS_STORAGE
        psz += jlsizeof(ContiguousStorageMessage)
    end
    fullsz = jlsizeof(ObjectStart) + size_size(psz) + psz + 4

    header_offset = f.end_of_data
    seek(io, header_offset)
    f.end_of_data = header_offset + fullsz

    if !isa(wsession, JLDWriteSession{Union{}})
        wsession.h5offset[objectid(data)] = h5offset(f, header_offset)
        push!(wsession.objects, data)
    end

    cio = begin_checksum_write(io, fullsz - 4)
    write_object_header_and_dataspace_message(cio, f, psz, dataspace)
    write_datatype_message(cio, datatype)

    # Data storage layout
    if layout_class == LC_COMPACT_STORAGE
        jlwrite(cio, CompactStorageMessage(datasz))
        if datasz != 0
            write_data(cio, f, data, odr, datamode(odr), wsession)
        end
        jlwrite(io, end_checksum(cio))
    elseif layout_class == LC_CHUNKED_STORAGE

        write_compressed_data(cio, f, data, odr, wsession, filter_id, compressor)

    else
        jlwrite(cio, ContiguousStorageMessage(datasz, h5offset(f, f.end_of_data)))
        jlwrite(io, end_checksum(cio))

        f.end_of_data += datasz
        write_data(io, f, data, odr, datamode(odr), wsession)
    end

    h5offset(f, header_offset)
end

@nospecializeinfer function write_dataset(f::JLDFile, dataspace::WriteDataspace, datatype::H5Datatype, @nospecialize(odr), @nospecialize(data), wsession::JLDWriteSession)
    io = f.io
    datasz = (odr_sizeof(odr)::Int * numel(dataspace))
    psz = payload_size_without_storage_message(dataspace, datatype)

    # The simplest CompactStorageMessage only supports data sets < 2^16
    if datasz < typemax(UInt16)
        layout_class = LC_COMPACT_STORAGE
        storage_message = CompactStorageMessage
        psz += jlsizeof(CompactStorageMessage) + datasz
    else
        layout_class = LC_CONTIGUOUS_STORAGE
        storage_message = ContiguousStorageMessage
        psz += jlsizeof(ContiguousStorageMessage)
    end

    fullsz = jlsizeof(ObjectStart) + size_size(psz) + psz + 4

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

    # Data storage layout
    if layout_class == LC_COMPACT_STORAGE
        jlwrite(cio, CompactStorageMessage(datasz))
        if datasz != 0
            write_data(cio, f, data, odr, datamode(odr), wsession)
        end
        jlwrite(io, end_checksum(cio))
    else
        jlwrite(cio, ContiguousStorageMessage(datasz, h5offset(f, f.end_of_data)))
        jlwrite(io, end_checksum(cio))

        f.end_of_data += datasz
        write_data(io, f, data, odr, datamode(odr), wsession)
    end

    h5offset(f, header_offset)
end

function write_object_header_and_dataspace_message(cio::IO, f::JLDFile, psz::Int, dataspace::WriteDataspace)
    jlwrite(cio, ObjectStart(size_flag(psz)))
    write_size(cio, psz)

    # Fill value
    jlwrite(cio, HeaderMessage(HM_FILL_VALUE, 2, 0))
    jlwrite(cio, UInt8(3)) # Version
    jlwrite(cio, 0x09)     # Flags

    # Dataspace
    jlwrite(cio, HeaderMessage(HM_DATASPACE, jlsizeof(dataspace), 0))
    jlwrite(cio, dataspace)

    # Attributes
    for attr in dataspace.attributes
        jlwrite(cio, HeaderMessage(HM_ATTRIBUTE, jlsizeof(attr), 0))
        write_attribute(cio, f, attr, f.datatype_wsession)
    end
end

function write_datatype_message(cio::IO, datatype::H5Datatype)
    jlwrite(cio, HeaderMessage(HM_DATATYPE, jlsizeof(datatype), 1 | (2*isa(datatype, CommittedDatatype))))
    jlwrite(cio, datatype)
end

struct CompactStorageMessage
    hm::HeaderMessage
    version::UInt8
    layout_class::LayoutClass
    data_size::UInt16
end
define_packed(CompactStorageMessage)
CompactStorageMessage(datasz::Int) =
    CompactStorageMessage(
            HeaderMessage(HM_DATA_LAYOUT, jlsizeof(CompactStorageMessage) - jlsizeof(HeaderMessage) + datasz, 0),
            4, LC_COMPACT_STORAGE, datasz
    )
    
struct ContiguousStorageMessage
    hm::HeaderMessage
    version::UInt8
    layout_class::LayoutClass
    address::RelOffset
    data_size::Length
end
define_packed(ContiguousStorageMessage)
ContiguousStorageMessage(datasz::Int, offset::RelOffset) =
    ContiguousStorageMessage(
        HeaderMessage(HM_DATA_LAYOUT, jlsizeof(ContiguousStorageMessage) - jlsizeof(HeaderMessage), 0),
        4, LC_CONTIGUOUS_STORAGE, offset, datasz
    )

@nospecializeinfer function write_dataset(f::JLDFile, @nospecialize(x), wsession::JLDWriteSession)::RelOffset
    if ismutabletype(typeof(x)) && !isa(wsession, JLDWriteSession{Union{}})
        offset = get(wsession.h5offset, objectid(x), UNDEFINED_ADDRESS)
        offset != UNDEFINED_ADDRESS && return offset
    end
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
        if msg.type == HM_LINK_MESSAGE && msg.link_name == name
            # delete link
            seek(f.io, fileoffset(f, msg.offset))
            jlwrite(f.io, HeaderMessage(HM_NIL, msg.size, 0))
            update_checksum(f.io, iter.chunk.chunk_start, iter.chunk.chunk_end)
        end
    end
    return
end
