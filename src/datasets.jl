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

    io = f.io
    seek(io, fileoffset(f, offset))
    cio = begin_checksum_read(io)
    sz = read_obj_start(cio)
    pmax = position(cio) + sz

    # Messages
    dataspace = ReadDataspace()
    attrs = EMPTY_READ_ATTRIBUTES
    datatype_class::UInt8 = 0
    datatype_offset::Int64 = 0
    data_offset::Int64 = 0
    data_length::Int = -1
    chunked_storage::Bool = false
    filter_id::UInt16 = 0
    while position(cio) <= pmax-4
        msg = read(cio, HeaderMessage)
        endpos = position(cio) + msg.size
        if msg.msg_type == HM_DATASPACE
            dataspace = read_dataspace_message(cio)
        elseif msg.msg_type == HM_DATATYPE
            datatype_class, datatype_offset = read_datatype_message(cio, f, (msg.flags & 2) == 2)
        elseif msg.msg_type == HM_FILL_VALUE
            (read(cio, UInt8) == 3 && read(cio, UInt8) == 0x09) || throw(UnsupportedFeatureException())
        elseif msg.msg_type == HM_DATA_LAYOUT
            read(cio, UInt8) == 4 || throw(UnsupportedVersionException())
            storage_type = read(cio, UInt8)
            if storage_type == LC_COMPACT_STORAGE
                data_length = read(cio, UInt16)
                data_offset = position(cio)
            elseif storage_type == LC_CONTIGUOUS_STORAGE
                data_offset = fileoffset(f, read(cio, RelOffset))
                data_length = read(cio, Length)
            elseif storage_type == LC_CHUNKED_STORAGE
                # TODO: validate this
                flags = read(cio, UInt8)
                dimensionality = read(cio, UInt8)
                dimensionality_size = read(cio, UInt8)
                skip(cio, Int(dimensionality)*Int(dimensionality_size))

                chunk_indexing_type = read(cio, UInt8)
                chunk_indexing_type == 1 || throw(UnsupportedFeatureException())
                data_length = read(cio, Length)
                read(cio, UInt32)
                data_offset = fileoffset(f, read(cio, RelOffset))
                chunked_storage = true
            else
                throw(UnsupportedFeatureException())
            end
        elseif msg.msg_type == HM_FILTER_PIPELINE
            version = read(cio, UInt8)
            version == 2 || throw(UnsupportedVersionException())
            nfilters = read(cio, UInt8)
            nfilters == 1 || throw(UnsupportedFeatureException())
            filter_id = read(cio, UInt16)
            filter_id == 1 || throw(UnsupportedFeatureException())
        elseif msg.msg_type == HM_ATTRIBUTE
            if attrs === EMPTY_READ_ATTRIBUTES
                attrs = ReadAttribute[read_attribute(cio, f)]
            else
                push!(attrs, read_attribute(cio, f))
            end
        elseif (msg.flags & 2^3) != 0
            throw(UnsupportedFeatureException())
        end
        seek(cio, endpos)
    end
    seek(cio, pmax)

    filter_id != 0 && !chunked_storage && throw(InvalidDataException())

    # Checksum
    end_checksum(cio) == read(io, UInt32) || throw(InvalidDataException())

    # TODO verify that data length matches
    val = read_data(f, dataspace, datatype_class, datatype_offset, data_offset, data_length,
                    filter_id, offset, attrs)
    val
end

"""
    read_attr_data(f::JLDFile, attr::ReadAttribute)

Read data from an attribute.
"""
read_attr_data(f::JLDFile, attr::ReadAttribute) =
    read_data(f, attr.dataspace, attr.datatype_class, attr.datatype_offset,
              attr.data_offset)

"""
    read_attr_data(f::JLDFile, attr::ReadAttribute, expected_datatype::H5Datatype,
                   rr::ReadRepresentation)

Read data from an attribute, assuming a specific HDF5 datatype and ReadRepresentation. If
the HDF5 datatype does not match, throws an `UnsupportedFeatureException`. This allows
better type stability while simultaneously validating the data.
"""
function read_attr_data(f::JLDFile, attr::ReadAttribute, expected_datatype::H5Datatype,
                        rr::ReadRepresentation)
    io = f.io
    if attr.datatype_class == class(expected_datatype)
        seek(io, attr.datatype_offset)
        dt = read(io, typeof(expected_datatype))
        if dt == expected_datatype
            seek(f.io, attr.data_offset)
            BOXED_READ_DATASPACE[] = (attr.dataspace, NULL_REFERENCE, -1, 0)
            return read_data(f, rr)
        end
    end
    throw(UnsupportedFeatureException())
end

"""
    read_data(f::JLDFile, dataspace::ReadDataspace, datatype_class::UInt8,
              datatype_offset::Int64, data_offset::Int64[, filter_id::UInt16,
              header_offset::RelOffset, attributes::Vector{ReadAttribute}])

Read data from a file. If `datatype_class` is typemax(UInt8), the datatype is assumed to be
committed, and `datatype_offset` points to the offset of the committed datatype's header.
Otherwise, datatype_offset points to the offset of the datatype attribute.
"""
function read_data(f::JLDFile, dataspace::ReadDataspace,
                   datatype_class::UInt8, datatype_offset::Int64,
                   data_offset::Int64, data_length::Int=-1, filter_id::UInt16=UInt16(0),
                   header_offset::RelOffset=NULL_REFERENCE,
                   attributes::Union{Vector{ReadAttribute},Void}=nothing)
    # See if there is a julia type attribute
    io = f.io
    if datatype_class == typemax(UInt8) # Committed datatype
        rr = jltype(f, f.datatype_locations[h5offset(f, datatype_offset)])
        seek(io, data_offset)
        BOXED_READ_DATASPACE[] = (dataspace, header_offset, data_length, filter_id)
        read_data(f, rr, attributes)
    else
        seek(io, datatype_offset)
        @read_datatype io datatype_class dt begin
            rr = jltype(f, dt)
            seek(io, data_offset)
            BOXED_READ_DATASPACE[] = (dataspace, header_offset, data_length, filter_id)
            read_data(f, rr, attributes)
        end
    end
end

# We can avoid a box by putting the dataspace here instead of passing
# it to read_data.  That reduces the allocation footprint, but doesn't
# really seem to help with performance.
const BOXED_READ_DATASPACE = Ref{Tuple{ReadDataspace,RelOffset,Int,UInt16}}()

# Most types can only be scalars or arrays
function read_data(f::JLDFile, rr, attributes::Union{Vector{ReadAttribute},Void}=nothing)
    dataspace, header_offset, data_length, filter_id = BOXED_READ_DATASPACE[]
    if dataspace.dataspace_type == DS_SCALAR
        filter_id != 0 && throw(UnsupportedFeatureException())
        read_scalar(f, rr, header_offset)
    elseif dataspace.dataspace_type == DS_SIMPLE
        read_array(f, dataspace, rr, data_length, filter_id, header_offset, attributes)
    else
        throw(UnsupportedFeatureException())
    end
end

# Reference arrays can only be arrays or null dataspace (for Union{} case)
function read_data(f::JLDFile, rr::ReadRepresentation{Any,RelOffset},
                   attributes::Vector{ReadAttribute})
    dataspace, header_offset, data_length, filter_id = BOXED_READ_DATASPACE[]
    filter_id != 0 && throw(UnsupportedFeatureException())
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
                    warn("type $(str) does not exist in workspace; interpreting Array{$str} as Array{Any}")
                    T = Any
                end
                seek(io, startpos)
                return read_array(f, dataspace, ReadRepresentation{T,RelOffset}(),
                                  -1, UInt16(0), header_offset, attributes)
            end
        end
    elseif dataspace.dataspace_type == DS_NULL
        return read_empty(ReadRepresentation{Union{},nothing}(), f,
                          attributes[find_dimensions_attr(attributes)],
                          header_offset)
    end
    throw(UnsupportedFeatureException())
end

# Types with no payload can only be null dataspace
function read_data(f::JLDFile,
                   rr::Union{ReadRepresentation{T,nothing} where T,
                             ReadRepresentation{T,CustomSerialization{S,nothing}} where {S,T}},
                   attributes::Vector{ReadAttribute})
    dataspace, header_offset, data_length, filter_id = BOXED_READ_DATASPACE[]
    filter_id != 0 && throw(UnsupportedFeatureException())
    dataspace.dataspace_type == DS_NULL || throw(UnsupportedFeatureException())

    dimensions_attr_index = find_dimensions_attr(attributes)
    if dimensions_attr_index == 0
        jlconvert(rr, f, Ptr{Void}(0), header_offset)
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
    ndims = Int(read(io, Length))

    seek(io, dimensions_attr.datatype_offset)
    read(io, FixedPointDatatype) == h5fieldtype(f, Int64, Int64, Val{true}) || throw(UnsupportedFeatureException())

    seek(io, dimensions_attr.data_offset)
    v = construct_array(io, T, ndims)
    if isleaftype(T)
        for i = 1:length(v)
            @inbounds v[i] = jlconvert(rr, f, Ptr{Void}(0), header_offset)
        end
    end
    header_offset !== NULL_REFERENCE && (f.jloffset[header_offset] = WeakRef(v))
    v
end

get_ndims_offset(f::JLDFile, dataspace::ReadDataspace, attributes::Void) =
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
                ndims = UInt8(read(f.io, Length))
                offset = x.data_offset
            end
        end
    end
    (ndims, offset)
end

"""
    construct_array{T}(io::IO, ::Type{T}, ndims::Int)

Construct array by reading `ndims` dimensions from `io`. Assumes `io` has already been
seeked to the correct position.
"""
function construct_array{T}(io::IO, ::Type{T}, ndims::Int)::Array{T}
    if ndims == 1
        n = read(io, Int64)
        Vector{T}(n)
    elseif ndims == 2
        d2 = read(io, Int64)
        d1 = read(io, Int64)
        Matrix{T}(d1, d2)
    elseif ndims == 3
        d3 = read(io, Int64)
        d2 = read(io, Int64)
        d1 = read(io, Int64)
        Array{T,3}(d1, d2, d3)
    else
        ds = reverse!(read(io, Int64, ndims))
        Array{T}(tuple(ds...))
    end
end


function read_array(f::JLDFile, dataspace::ReadDataspace,
                    rr::ReadRepresentation{T,RR}, data_length::Int,
                    filter_id::UInt16, header_offset::RelOffset, 
                    attributes::Union{Vector{ReadAttribute},Void}) where {T,RR}
    io = f.io
    data_offset = position(io)
    ndims, offset = get_ndims_offset(f, dataspace, attributes)
    seek(io, offset)
    v = construct_array(io, T, Int(ndims))
    header_offset !== NULL_REFERENCE && (f.jloffset[header_offset] = WeakRef(v))
    n = length(v)
    seek(io, data_offset)
    if filter_id == 1
        read_compressed_array!(v, f, rr, data_length)
    else
        read_array!(v, f, rr)
    end
    v
end

function payload_size_without_storage_message(dataspace::WriteDataspace, datatype::H5Datatype)
    sz = 6 + 4 + sizeof(dataspace) + 4 + sizeof(datatype) + length(dataspace.attributes)*sizeof(HeaderMessage)
    for attr in dataspace.attributes
        sz += sizeof(attr)
    end
    sz
end

# Like isdefined, but assumes arr is a pointer array, and can be inlined
unsafe_isdefined(arr::Array, i::Int) =
    unsafe_load(Ptr{Ptr{Void}}(pointer(arr)+(i-1)*sizeof(Ptr{Void}))) != Ptr{Void}(0)

function deflate_data(f::JLDFile, data::Array{T}, odr::S, wsession::JLDWriteSession) where {T,S}
    buf = Vector{UInt8}(odr_sizeof(odr) * length(data))
    cp = Ptr{Void}(pointer(buf))
    @simd for i = 1:length(data)
        @inbounds h5convert!(cp, odr, f, data[i], wsession)
        cp += odr_sizeof(odr)
    end
    read(ZlibDeflateInputStream(buf; gzip=false))
end

function write_dataset(f::JLDFile, dataspace::WriteDataspace, datatype::H5Datatype, odr::S, data::Array{T}, wsession::JLDWriteSession) where {T,S}
    io = f.io
    datasz = odr_sizeof(odr) * numel(dataspace)
    layout_class = datasz < 8192 ? LC_COMPACT_STORAGE :
                   f.compress ? LC_CHUNKED_STORAGE : LC_CONTIGUOUS_STORAGE
    psz = payload_size_without_storage_message(dataspace, datatype)
    if datasz < 8192
        layout_class = LC_COMPACT_STORAGE
        psz += sizeof(CompactStorageMessage) + datasz
    elseif f.compress && isleaftype(T) && isbits(T)
        layout_class = LC_CHUNKED_STORAGE
        psz += chunked_storage_message_size(ndims(data)) + length(DEFLATE_PIPELINE_MESSAGE)
    else
        layout_class = LC_CONTIGUOUS_STORAGE
        psz += sizeof(ContiguousStorageMessage)
    end
    fullsz = sizeof(ObjectStart) + size_size(psz) + psz + 4

    header_offset = f.end_of_data
    seek(io, header_offset)
    f.end_of_data = header_offset + fullsz

    if !isa(wsession, JLDWriteSession{Union{}})
        wsession.h5offset[object_id(data)] = h5offset(f, header_offset)
        push!(wsession.objects, data)
    end

    cio = begin_checksum_write(io, fullsz - 4)
    write_object_header_and_dataspace_message(cio, f, psz, dataspace)
    write_datatype_message(cio, datatype)

    # Data storage layout
    if layout_class == LC_COMPACT_STORAGE
        write(cio, CompactStorageMessage(datasz))
        if datasz != 0
            write_data(cio, f, data, odr, datamode(odr), wsession)
        end
        write(io, end_checksum(cio))
    elseif layout_class == LC_CHUNKED_STORAGE
        write(cio, DEFLATE_PIPELINE_MESSAGE)
        deflated = deflate_data(f, data, odr, wsession)
        write_chunked_storage_message(cio, odr_sizeof(odr), size(data), length(deflated), h5offset(f, f.end_of_data))
        write(io, end_checksum(cio))

        f.end_of_data += length(deflated)
        write(io, deflated)
    else
        write(cio, ContiguousStorageMessage(datasz, h5offset(f, f.end_of_data)))
        write(io, end_checksum(cio))

        f.end_of_data += datasz
        write_data(io, f, data, odr, datamode(odr), wsession)
    end

    h5offset(f, header_offset)
end

function write_dataset(f::JLDFile, dataspace::WriteDataspace, datatype::H5Datatype, odr::S, data, wsession::JLDWriteSession) where S
    io = f.io
    datasz = odr_sizeof(odr) * numel(dataspace)
    psz = payload_size_without_storage_message(dataspace, datatype) + sizeof(CompactStorageMessage) + datasz
    fullsz = sizeof(ObjectStart) + size_size(psz) + psz + 4

    header_offset = f.end_of_data
    seek(io, header_offset)
    f.end_of_data = header_offset + fullsz

    if ismutabletype(typeof(data)) && !isa(wsession, JLDWriteSession{Union{}})
        wsession.h5offset[object_id(data)] = h5offset(f, header_offset)
        push!(wsession.objects, data)
    end

    cio = begin_checksum_write(io, fullsz - 4)
    write_object_header_and_dataspace_message(cio, f, psz, dataspace)
    write_datatype_message(cio, datatype)
    write(cio, CompactStorageMessage(datasz))
    if datasz != 0
        write_data(cio, f, data, odr, datamode(odr), wsession)
    end
    write(io, end_checksum(cio))

    h5offset(f, header_offset)
end

function write_object_header_and_dataspace_message(cio::IO, f::JLDFile, psz::Int, dataspace::WriteDataspace)
    write(cio, ObjectStart(size_flag(psz)))
    write_size(cio, psz)

    # Fill value
    write(cio, HeaderMessage(HM_FILL_VALUE, 2, 0))
    write(cio, UInt8(3)) # Version
    write(cio, 0x09)     # Flags

    # Dataspace
    write(cio, HeaderMessage(HM_DATASPACE, sizeof(dataspace), 0))
    write(cio, dataspace)

    # Attributes
    for attr in dataspace.attributes
        write(cio, HeaderMessage(HM_ATTRIBUTE, sizeof(attr), 0))
        write_attribute(cio, f, attr, f.datatype_wsession)
    end
end

function write_datatype_message(cio::IO, datatype::H5Datatype)
    write(cio, HeaderMessage(HM_DATATYPE, sizeof(datatype), 1 | (2*isa(datatype, CommittedDatatype))))
    write(cio, datatype)
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
            HeaderMessage(HM_DATA_LAYOUT, sizeof(CompactStorageMessage) - sizeof(HeaderMessage) + datasz, 0),
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
        HeaderMessage(HM_DATA_LAYOUT, sizeof(ContiguousStorageMessage) - sizeof(HeaderMessage), 0),
        4, LC_CONTIGUOUS_STORAGE, offset, datasz
    )

@inline chunked_storage_message_size(ndims::Int) =
    sizeof(HeaderMessage) + 5 + (ndims+1)*sizeof(Length) + 1 + sizeof(Length) + 4 + sizeof(RelOffset)
function write_chunked_storage_message(io::IO, elsize::Int, dims::NTuple{N,Int}, filtered_size::Int, offset::RelOffset) where N
    write(io, HeaderMessage(HM_DATA_LAYOUT, chunked_storage_message_size(N) - sizeof(HeaderMessage), 0))
    write(io, UInt8(4))                     # Version
    write(io, UInt8(LC_CHUNKED_STORAGE))    # Layout Class
    write(io, UInt8(2))                     # Flags (= SINGLE_INDEX_WITH_FILTER)
    write(io, UInt8(N+1))                   # Dimensionality
    write(io, UInt8(sizeof(Length)))        # Dimensionality Size
    for i = N:-1:1
        write(io, Length(dims[i]))          # Dimensions 1...N
    end
    write(io, Length(elsize))               # Element size (last dimension)
    write(io, UInt8(1))                     # Chunk Indexing Type (= Single Chunk)
    write(io, Length(filtered_size))        # Size of filtered chunk
    write(io, UInt32(0))                    # Filters for chunk
    write(io, offset)                       # Address
end

const DEFLATE_PIPELINE_MESSAGE = let
    io = IOBuffer()
    write(io, HeaderMessage(HM_FILTER_PIPELINE, 12, 0))
    write(io, UInt8(2))                 # Version
    write(io, UInt8(1))                 # Number of Filters
    write(io, UInt16(1))                # Filter Identification Value (= deflate)
    write(io, UInt16(0))                # Flags
    write(io, UInt16(1))                # Number of Client Data Values
    write(io, UInt32(5))                # Client Data (Compression Level)
    take!(io)
end

@Base.pure ismutabletype(x::DataType) = x.mutable

@inline function write_dataset(f::JLDFile, x, wsession::JLDWriteSession)
    odr = objodr(x)
    write_dataset(f, WriteDataspace(f, x, odr), h5type(f, x), odr, x, wsession)
end

@inline function write_ref_mutable(f::JLDFile, x, wsession::JLDWriteSession)
    offset = get(wsession.h5offset, object_id(x), RelOffset(0))
    offset != RelOffset(0) ? offset : write_dataset(f, x, wsession)::RelOffset
end

@inline write_ref_mutable(f::JLDFile, x, wsession::JLDWriteSession{Union{}}) =
    write_dataset(f, x, wsession)

function write_ref(f::JLDFile, x, wsession::JLDWriteSession)
    if ismutabletype(typeof(x))
        write_ref_mutable(f, x, wsession)::RelOffset
    else
        write_dataset(f, x, wsession)::RelOffset
    end
end
write_ref(f::JLDFile, x::RelOffset, wsession::JLDWriteSession) = x
