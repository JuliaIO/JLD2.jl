#
# Datasets
#

# Use an Enum here when it doesn't make us allocate
const LC_COMPACT_STORAGE = 0x00
const LC_CONTIGUOUS_STORAGE = 0x01
const LC_CHUNKED_STORAGE = 0x02

function load_dataset(f::JLDFile, offset::RelOffset)
    if haskey(f.jloffset, offset)
        # Stored as WeakRefs and may no longer exist
        val = f.jloffset[offset].value
        val !== nothing && return val
    end
    if isgroup(f, offset)
        return let loaded_groups = f.loaded_groups
            get!(()->load_group(f, offset), loaded_groups, offset)
        end
    end

    io = f.io
    chunk_start::Int64 = fileoffset(f, offset)
    seek(io, chunk_start)
    # Version 1 object header have no signature and start with version
    header_version = jlread(io, UInt8)


    if header_version == 1
        seek(io, chunk_start)
        cio = io
        sz,_,groupflags = read_obj_start(cio)
        chunk_end = position(cio) + sz
        # Skip to nearest 8byte aligned position
        skip_to_aligned!(cio, fileoffset(f, offset))
    else
        header_version = 2
        seek(io, chunk_start)
        cio = begin_checksum_read(io)
        sz,_,groupflags = read_obj_start(cio)
        chunk_end = position(cio) + sz #- 4
    end

    # Messages
    chunks = [(; chunk_start, chunk_end)]
    chunk_number = 0

    dataspace = ReadDataspace()
    attrs = EMPTY_READ_ATTRIBUTES
    datatype_class::UInt8 = 0
    datatype_offset::Int64 = 0
    layout::DataLayout = DataLayout(0,0,0,-1)
    filter_pipeline::FilterPipeline = FilterPipeline(Filter[])
    while !isempty(chunks)
        chunk = popfirst!(chunks)
        chunk_start = chunk.chunk_start
        chunk_end = chunk.chunk_end

        if chunk_number > 0 # Don't do this the first time around
            seek(io, chunk_start)
            if header_version == 2
                chunk_end -= 4
                cio = begin_checksum_read(io)
                jlread(cio, UInt32) == OBJECT_HEADER_CONTINUATION_SIGNATURE || throw(InvalidDataException())
            end
        end
        chunk_number += 1

        while position(cio) <= chunk_end-4
            if header_version == 1
                # Message start 8byte aligned relative to object start
                skip_to_aligned!(cio, chunk_start)
                # Version 1 header message is padded
                msg = HeaderMessage(jlread(cio, UInt16), jlread(cio, UInt16), jlread(cio, UInt8))
                skip(cio, 3)
            else # header_version == 2
                msg = jlread(cio, HeaderMessage)
                (groupflags & 4) == 4 && skip(cio, 2) 
            end
            endpos = position(cio) + msg.size
            if msg.msg_type == HM_DATASPACE
                dataspace = read_dataspace_message(cio)
            elseif msg.msg_type == HM_DATATYPE
                datatype_class, datatype_offset = read_datatype_message(cio, f, (msg.flags & 2) == 2)
            elseif msg.msg_type == HM_FILL_VALUE_OLD
                # don't know what to do with these
                # ignore for now
            elseif msg.msg_type == HM_FILL_VALUE
                # don't know what to do with these
                # ignore for now
                version = jlread(cio, UInt8)
                flags = jlread(cio, UInt8)
                
            elseif msg.msg_type == HM_DATA_LAYOUT
                layout = jlread(cio, DataLayout, f)
            elseif msg.msg_type == HM_FILTER_PIPELINE
               filter_pipeline = jlread(cio, FilterPipeline)
            elseif msg.msg_type == HM_ATTRIBUTE
                if attrs === EMPTY_READ_ATTRIBUTES
                    attrs = ReadAttribute[read_attribute(cio, f)]
                else
                    push!(attrs, read_attribute(cio, f))
                end
            elseif msg.msg_type == HM_OBJECT_HEADER_CONTINUATION
                continuation_offset = fileoffset(f, jlread(cio, RelOffset))
                continuation_length = jlread(cio, Length)
                push!(chunks, (; chunk_start = continuation_offset,
                                 chunk_end = continuation_offset + continuation_length))

            elseif (msg.flags & 2^3) != 0
                throw(UnsupportedFeatureException())
            end
            seek(cio, endpos)
        end
        seek(cio, chunk_end)

        if header_version == 2
            # Checksum
            end_checksum(cio) == jlread(io, UInt32) || throw(InvalidDataException("Invalid Checksum"))
        end
    end
    iscompressed(filter_pipeline) && !ischunked(layout) && throw(InvalidDataException("Compressed data must be chunked"))

    # TODO verify that data length matches
    val = read_data(f, dataspace, datatype_class, datatype_offset, layout,
                    filter_pipeline, offset, attrs)
    val
end


"""
    read_attr_data(f::JLDFile, attr::ReadAttribute)

jlread data from an attribute.
"""
read_attr_data(f::JLDFile, attr::ReadAttribute) =
    read_data(f, attr.dataspace, attr.datatype_class, attr.datatype_offset,
              DataLayout(0,0,-1,attr.data_offset))

"""
    read_attr_data(f::JLDFile, attr::ReadAttribute, expected_datatype::H5Datatype,
                   rr::ReadRepresentation)

jlread data from an attribute, assuming a specific HDF5 datatype and ReadRepresentation. If
the HDF5 datatype does not match, throws an `UnsupportedFeatureException`. This allows
better type stability while simultaneously validating the data.
"""
function read_attr_data(f::JLDFile, attr::ReadAttribute, expected_datatype::H5Datatype,
                        rr::ReadRepresentation)
    io = f.io
    if (attr.datatype_class << 4) == (class(expected_datatype) << 4)
        seek(io, attr.datatype_offset)
        dt = jlread(io, typeof(expected_datatype))
        if dt == expected_datatype
            seek(f.io, attr.data_offset)
            read_dataspace = (attr.dataspace, NULL_REFERENCE, DataLayout(0,0,-1,attr.data_offset), FilterPipeline())
            return read_data(f, rr, read_dataspace)
        end
    end
    throw(UnsupportedFeatureException())
end

"""
    read_data(f::JLDFile, dataspace::ReadDataspace, datatype_class::UInt8,
              datatype_offset::Int64, data_offset::Int64[, filters::FilterPipeline,
              header_offset::RelOffset, attributes::Vector{ReadAttribute}])

Read data from a file. If `datatype_class` is typemax(UInt8), the datatype is assumed to be
committed, and `datatype_offset` points to the offset of the committed datatype's header.
Otherwise, datatype_offset points to the offset of the datatype attribute.
"""
function read_data(f::JLDFile, dataspace::ReadDataspace,
                   datatype_class::UInt8, datatype_offset::Int64,
                   layout::DataLayout, 
                   filters::FilterPipeline=FilterPipeline(),
                   header_offset::RelOffset=NULL_REFERENCE,
                   attributes::Union{Vector{ReadAttribute},Nothing}=nothing)
    # See if there is a julia type attribute
    io = f.io
    if datatype_class == typemax(UInt8) # shared datatype message
        # this means that it is "committed" to `_types` if the file was written by JLD2  
        offset = h5offset(f, datatype_offset)
        rr = jltype(f, get(f.datatype_locations, offset, SharedDatatype(offset)))

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
        seek(io, datatype_offset)
        @read_datatype io datatype_class dt begin
            rr = jltype(f, dt)
            T,S = typeof(rr).parameters
            if layout.data_length > -1
                # TODO: this could use the fill value message to populate the array
                @warn "This array should be populated by a fill value. This is not (yet) implemented."
            end
            v = Array{T, 1}()
            header_offset !== NULL_REFERENCE && (f.jloffset[header_offset] = WeakRef(v))
            return v
        end
    else
        seek(io, datatype_offset)
        @read_datatype io datatype_class dt begin
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
                if isa(T, UnknownType)
                    str = typestring(T)
                    @warn("type $(str) does not exist in workspace; interpreting Array{$str} as Array{Any}")
                    T = Any
                end
                seek(io, startpos)
                return read_array(f, dataspace, ReadRepresentation{T,RelOffset}(),
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
                   rr::Union{ReadRepresentation{T,nothing} where T,
                             ReadRepresentation{T,CustomSerialization{S,nothing}} where {S,T}},
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
    dimensions_attr.datatype_class == DT_FIXED_POINT || throw(UnsupportedFeatureException())

    io = f.io
    seek(io, dimensions_attr.dataspace.dimensions_offset)
    ndims = Int(jlread(io, Length))

    seek(io, dimensions_attr.datatype_offset)
    jlread(io, FixedPointDatatype) == h5fieldtype(f, Int64, Int64, Val{true}) || throw(UnsupportedFeatureException())

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
                    rr::ReadRepresentation{T,RR}, layout::DataLayout,
                    filters::FilterPipeline, header_offset::RelOffset,
                    attributes::Union{Vector{ReadAttribute},Nothing}) where {T,RR}
    io = f.io
    data_offset = layout.data_offset
    if !ischunked(layout) || (layout.chunk_indexing_type == 1)
        #data_offset = position(io)
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
                idx = reverse(chunk.idx[1:end-1])
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
                            mask = [chunk.filter_mask & 2^(n-1) == 0 for n=eachindex(filters.filters)]
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


function write_dataset(
        f::JLDFile,
        dataspace::WriteDataspace,
        datatype::H5Datatype,
        odr::S,
        data::Array{T},
        wsession::JLDWriteSession,
        compress = f.compress,
        ) where {T,S}
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

function write_dataset(f::JLDFile, dataspace::WriteDataspace, datatype::H5Datatype, odr::S, data, wsession::JLDWriteSession) where S
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
    layout_class::UInt8
    data_size::UInt16
end
define_packed(CompactStorageMessage)
@inline CompactStorageMessage(datasz::Int) =
    CompactStorageMessage(
            HeaderMessage(HM_DATA_LAYOUT, jlsizeof(CompactStorageMessage) - jlsizeof(HeaderMessage) + datasz, 0),
            4, LC_COMPACT_STORAGE, datasz
    )
    
struct ContiguousStorageMessage
    hm::HeaderMessage
    version::UInt8
    layout_class::UInt8
    address::RelOffset
    data_size::Length
end
define_packed(ContiguousStorageMessage)
@inline ContiguousStorageMessage(datasz::Int, offset::RelOffset) =
    ContiguousStorageMessage(
        HeaderMessage(HM_DATA_LAYOUT, jlsizeof(ContiguousStorageMessage) - jlsizeof(HeaderMessage), 0),
        4, LC_CONTIGUOUS_STORAGE, offset, datasz
    )

function write_dataset(f::JLDFile, x, wsession::JLDWriteSession)
    odr = objodr(x)
    write_dataset(f, WriteDataspace(f, x, odr), h5type(f, x), odr, x, wsession)::RelOffset
end

function write_ref_mutable(f::JLDFile, x, wsession::JLDWriteSession)
    offset = get(wsession.h5offset, objectid(x), RelOffset(0))
    offset != RelOffset(0) ? offset : write_dataset(f, x, wsession)::RelOffset
end

write_ref_mutable(f::JLDFile, x, wsession::JLDWriteSession{Union{}}) =
    write_dataset(f, x, wsession)

function write_ref(f::JLDFile, x, wsession::JLDWriteSession)
    if ismutabletype(typeof(x))
        write_ref_mutable(f, x, wsession)::RelOffset
    else
        write_dataset(f, x, wsession)::RelOffset
    end
end
write_ref(f::JLDFile, x::RelOffset, wsession::JLDWriteSession) = x

Base.delete!(f::JLDFile, x::AbstractString) = delete!(f.root_group, x)

function Base.delete!(g::Group, name::AbstractString)
    g.f.writable || throw(ArgumentError("Cannot delete in read-only mode"))
    if !haskey(g, name)
        @warn "No entry named $name was found"
        return
    end

    if '/' in name
        dir, dname = rsplit(name, '/'; limit=2)
        if isempty(dir)
            g = g.f.root_group
        else
            g = g[dir]
        end
        name = string(dname)
    end

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
    # Instead, we walk through the structure similar to `load_group`
    # until the correct link message is found
    # The deletion is done by replacing that message by a placeholder nill message
    io = f.io
    chunk_start_offset::Int64 = fileoffset(f, roffset)
    seek(io, chunk_start_offset)

    sz, = read_obj_start(io)
    chunk_checksum_offset::Int64 = position(io) + sz

    continuation_offset::Int64 = -1
    continuation_length::Length = 0
    link_deleted = false
    while !link_deleted
        if continuation_offset != -1
            seek(io, continuation_offset)
            chunk_start_offset = continuation_offset
            chunk_checksum_offset = continuation_offset + continuation_length - 4
            continuation_offset = -1
            jlread(io, UInt32) == OBJECT_HEADER_CONTINUATION_SIGNATURE || throw(InvalidDataException())
        end

        while (curpos = position(io)) <= chunk_checksum_offset - 4
            msg = jlread(io, HeaderMessage)
            endpos = curpos + jlsizeof(HeaderMessage) + msg.size

            if msg.msg_type == HM_LINK_MESSAGE
                dataset_name, loffset = read_link(io)
                if dataset_name == name
                    # delete link
                    seek(io, curpos)
                    jlwrite(io, HeaderMessage(HM_NIL, msg.size, 0))
                    link_deleted = true
                    break
                end
            elseif msg.msg_type == HM_OBJECT_HEADER_CONTINUATION
                continuation_offset = chunk_start_offset = fileoffset(f, jlread(io, RelOffset))
                continuation_length = jlread(io, Length)
            end
            seek(io, endpos)
        end

        continuation_offset == -1 && break
    end

    # Update the Checksum
    seek(io, chunk_start_offset)
    cio = begin_checksum_read(io)
    seek(cio, chunk_checksum_offset)
    seek(io, chunk_checksum_offset)
    jlwrite(io, end_checksum(cio))
    return
end
