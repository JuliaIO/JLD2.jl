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
    elseif isvirtual(layout)
        # Handle virtual dataset
        return read_virtual_data(f, dataspace, dt, layout, filters, header_offset, attributes)
    end
    seek(f.io, layout.data_offset)
    read_dataspace = (dataspace, header_offset, layout, filters)
    read_data(f, rr, read_dataspace, attributes)
end

function read_virtual_data(f::JLDFile, dataspace::ReadDataspace,
                          @nospecialize(dt::H5Datatype),
                          layout::DataLayout,
                          filters::FilterPipeline,
                          header_offset::RelOffset,
                          attributes::Union{Vector{ReadAttribute},Nothing})
    # Parse virtual dataset layout from global heap
    global_heap_address = layout.data_offset  # This contains the global heap address
    heap_index = layout.chunk_indexing_type  # This contains the global heap index

    try
        # Read the virtual dataset layout from the global heap
        virtual_layout = read_virtual_dataset_layout(f, global_heap_address, heap_index)

        # Process the virtual dataset mappings to create the combined dataset
        return combine_virtual_mappings(f, virtual_layout, dataspace, dt)

    catch e
        # Fall back to checking for legacy metadata-based virtual datasets
        if !isnothing(attributes)
            source_file = nothing
            source_dataset = nothing

            for attr in attributes
                if attr.name == :_virtual_source || attr.name == Symbol("_virtual_source")
                    source_file = read_attr_data(f, attr)
                elseif attr.name == :_virtual_dataset || attr.name == Symbol("_virtual_dataset")
                    source_dataset = read_attr_data(f, attr)
                end
            end

            if !isnothing(source_file) && !isnothing(source_dataset)
                return load_virtual_source_data(source_file, source_dataset, dt, dataspace, header_offset)
            end
        end

        # If we can't parse the virtual dataset, throw an error
        throw(UnsupportedFeatureException("Cannot parse virtual dataset layout: $e"))
    end
end

function load_virtual_source_data(source_file::AbstractString, source_dataset::AbstractString,
                                 dt::H5Datatype, dataspace::ReadDataspace, header_offset::RelOffset)
    # Attempt to load data from the source file
    try
        # Use the current directory as base path if source_file is relative
        if !isabs(source_file)
            # Try relative to current working directory first
            source_path = joinpath(pwd(), source_file)
            if !isfile(source_path)
                # If not found, try relative to the virtual file's directory
                # This is a simplified approach - proper VDS should handle paths more robustly
                source_path = source_file
            end
        else
            source_path = source_file
        end

        # Load using JLD2 first, fall back to HDF5.jl if needed
        if endswith(source_path, ".jld2") || endswith(source_path, ".jld")
            return jldopen(source_path, "r") do src_f
                src_f[source_dataset]
            end
        else
            # Try to load with HDF5.jl if available
            try
                # Try to use HDF5.jl if it's available
                if isdefined(Main, :HDF5)
                    return Main.HDF5.h5open(source_path, "r") do src_f
                        read(src_f[source_dataset])
                    end
                else
                    # If HDF5.jl is not loaded, try to load it
                    @eval Main import HDF5
                    return Main.HDF5.h5open(source_path, "r") do src_f
                        read(src_f[source_dataset])
                    end
                end
            catch e
                # If HDF5.jl is not available or fails, try JLD2 anyway
                return jldopen(source_path, "r") do src_f
                    src_f[source_dataset]
                end
            end
        end

    catch e
        @warn "Failed to load virtual dataset source: $source_file:/$source_dataset" exception=e
        throw(InvalidDataException("Could not load virtual dataset from source file: $source_file"))
    end
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

function find_dimensions_attr(attributes::Vector{ReadAttribute})
    dimensions_attr_index = 0
    for i = 1:length(attributes)
        x = attributes[i]
        if x.name == :dimensions
            dimensions_attr_index = i
        end
    end
    dimensions_attr_index
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
    elseif data isa ArrayMemory && iscompressed(local_filters) && isconcretetype(eltype(data)) && isbitstype(eltype(data))
        layout_class = LcChunked
        psz += jlsizeof(Val(HmDataLayout);
            layout_class,
            dimensions = UInt64.((reverse(size(data))..., odr_sizeof(odr))),
            # Dummy values for message size computation
            data_size = 0, data_address = 0,
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
