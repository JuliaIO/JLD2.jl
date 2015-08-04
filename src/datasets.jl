#
# Datasets
#

# Use an Enum here when it doesn't make us allocate
const LC_COMPACT_STORAGE = 0x00
const LC_CONTIGUOUS_STORAGE = 0x01
const LC_CHUNKED_STORAGE = 0x02

function read_dataset(f::JLDFile, offset::RelOffset)
    if haskey(f.jloffset, offset)
        # Stored as WeakRefs and may no longer exist
        val = f.jloffset[offset].value
        val !== nothing && return val
    end

    io = f.io
    seek(io, fileoffset(f, offset))
    cio = begin_checksum(io)
    sz = read_obj_start(cio)
    pmax = position(cio) + sz

    # Messages
    dataspace = ReadDataspace()
    attrs = EMPTY_READ_ATTRIBUTES
    datatype_class::UInt8 = 0
    datatype_offset::FileOffset = 0
    data_offset::FileOffset = 0
    data_length::Int = 0
    while position(cio) < pmax
        msg = read(cio, HeaderMessage)
        endpos = position(cio) + msg.size
        if msg.msg_type == HM_DATASPACE
            dataspace = read_dataspace_message(io)
        elseif msg.msg_type == HM_DATATYPE
            datatype_class, datatype_offset = read_datatype_message(io, f, (msg.flags & 2) == 2)
        elseif msg.msg_type == HM_FILL_VALUE
            (read(cio, UInt8) == 3 && read(cio, UInt8) == 0x09) || throw(UnsupportedFeatureException())
        elseif msg.msg_type == HM_DATA_LAYOUT
            read(cio, UInt8) == 3 || throw(UnsupportedVersionException())
            storage_type = read(cio, UInt8)
            if storage_type == LC_COMPACT_STORAGE
                data_length = read(cio, UInt16)
                data_offset = position(cio)
            elseif storage_type == LC_CONTIGUOUS_STORAGE
                data_offset = fileoffset(f, read(cio, RelOffset))
                data_length = read(cio, Length)
            else
                throw(UnsupportedFeatureException())
            end
        elseif msg.msg_type == HM_ATTRIBUTE
            if attrs === EMPTY_READ_ATTRIBUTES
                attrs = ReadAttribute[read_attribute(cio, f)]
            else
                push!(attrs, read_attribute(cio, f))
            end
        elseif (msg.flags & 2^3) != 0
            throw(UnsupportedFeatureException())
        elseif msg.msg_type == HM_NIL
            break
        end
        seek(cio, endpos)
    end
    seek(cio, pmax)

    # Checksum
    end_checksum(cio) == read(io, UInt32) || throw(InvalidDataException())

    # TODO verify that data length matches
    val = read_data(f, dataspace, datatype_class, datatype_offset, data_offset, attrs)
    if !isbits(typeof(val))
        f.jloffset[offset] = WeakRef(val)
    end
    val
end

# Read data from an attribute
read_attr_data(f::JLDFile, attr::ReadAttribute) =
    read_data(f, attr.dataspace, attr.datatype_class, attr.datatype_offset, attr.data_offset)

# Read data from an attribute, assuming a specific HDF5 datatype and
# ReadRepresentation. If the HDF5 datatype does not match, throws an
# UnsupportedFeatureException. This allows better type stability while
# simultaneously validating the data.
function read_attr_data(f::JLDFile, attr::ReadAttribute, expected_datatype::H5Datatype, rr::ReadRepresentation)
    io = f.io
    if attr.datatype_class == class(expected_datatype)
        seek(io, attr.datatype_offset)
        dt = read(io, typeof(expected_datatype))
        if dt == expected_datatype
            seek(f.io, attr.data_offset)
            # BOXED_READ_DATASPACE[] = attr.dataspace
            return read_data(f, attr.dataspace, rr)
        end
    end
    throw(UnsupportedFeatureException())
end

# Read data from a file. If datatype_class is typemax(UInt8), the
# datatype is assumed to be committed, and datatype_offset points to
# the offset of the committed datatype's header. Otherwise,
# datatype_offset points to the offset of the datatype attribute.
function read_data(f::JLDFile, dataspace::ReadDataspace,
                   datatype_class::UInt8, datatype_offset::FileOffset, data_offset::FileOffset,
                   attributes::Union(Vector{ReadAttribute}, Void)=nothing)
    # See if there is a julia type attribute
    io = f.io
    if datatype_class == typemax(UInt8) # Committed datatype
        rr = jltype(f, f.datatype_locations[h5offset(f, datatype_offset)])
        seek(io, data_offset)
        # BOXED_READ_DATASPACE[] = dataspace
        read_data(f, dataspace, rr, attributes)
    else
        seek(io, datatype_offset)
        @read_datatype io datatype_class dt begin
            rr = jltype(f, dt)
            seek(io, data_offset)
            # BOXED_READ_DATASPACE[] = dataspace
            read_data(f, dataspace, rr, attributes)
        end
    end
end

rrodr{T,S}(::ReadRepresentation{T,S}) = S
empty_eltype(::ReadRepresentation{Any,RelOffset}) = Union{}
empty_eltype{T}(::ReadRepresentation{T}) = T

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

# We can avoid a box by putting the dataspace here instead of passing
# it to read_data.  That reduces the allocation footprint, but doesn't
# really seem to help with performance.
# const BOXED_READ_DATASPACE = Ref{ReadDataspace}()
function read_data(f::JLDFile{MmapIO}, dataspace::ReadDataspace, rr,
                   attributes::Union(Vector{ReadAttribute},Void)=nothing)
    # dataspace = BOXED_READ_DATASPACE[]
    io = f.io
    inptr = io.curptr
    if dataspace.dataspace_type == DS_SCALAR
        s = jlconvert(rr, f, inptr)
        io.curptr = inptr + sizeof(rr)
        s
    elseif dataspace.dataspace_type == DS_SIMPLE
        v = read_array(f, inptr, dataspace, rr, attributes)
        v
    elseif dataspace.dataspace_type == DS_NULL
        dimensions_attr_index = find_dimensions_attr(attributes)

        if dimensions_attr_index == 0
            isa(rrodr(rr), Void) || throw(UnsupportedFeatureException())
            jlconvert(rr, f, inptr)
        else
            # dimensions attribute => array of empty type
            dimensions_attr = attributes[dimensions_attr_index]
            seek(io, dimensions_attr.dataspace.dimensions_offset)
            ndims = read(io, Int)
            seek(io, dimensions_attr.data_offset)
            v = construct_array(io, empty_eltype(rr), ndims)
            io.curptr = inptr
            v
        end
    else
        throw(UnsupportedFeatureException())
    end
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

# Construct array by reading ndims dimensions from io
# Assumes io has already been seeked t oteh correct position
function construct_array{T}(io::IO, ::Type{T}, ndims::Integer)
    if ndims == 1
        n = read(io, Int)
        Array(T, n)
    elseif ndims == 2
        d2 = read(io, Int)
        d1 = read(io, Int)
        Array(T, d1, d2)
    elseif ndims == 3
        d3 = read(io, Int)
        d2 = read(io, Int)
        d1 = read(io, Int)
        Array(T, d1, d2, d3)
    else
        ds = reverse!(read(io, Int, ndims))
        Array(T, tuple(ds...))
    end
end

function read_array{T,RR}(f::JLDFile, inptr::Ptr{Void}, dataspace::ReadDataspace,
                          rr::ReadRepresentation{T,RR},
                          attributes::Union(Vector{ReadAttribute},Void))
    io = f.io
    ndims, offset = get_ndims_offset(f, dataspace, attributes)
    seek(io, offset)
    v = construct_array(io, T, ndims)
    n = length(v)

    if RR === T && isbits(T)
        nb = n*sizeof(RR)
        if nb > 1048576 # TODO tweak
            # It turns out that regular IO is faster here (at least on OS X)
            mmapio = f.io
            regulario = mmapio.f
            seek(regulario, inptr - pointer(f.io.arr))
            if ccall(:ios_readall, UInt,
                     (Ptr{Void}, Ptr{Void}, UInt), regulario.ios, v, nb) < nb
                throw(EOFError())
            end
        else
            unsafe_copy!(pointer(v), convert(Ptr{T}, inptr), Int(n))
        end
    # Would this actually help with performance?
    # elseif isbits(T)
    #     @simd for i = 1:n
    #         jlconvert!(outptr, rr, f, inptr)
    #         inptr += sizeof(RR)
    #         outptr += sizeof(T)
    #     end
    else
        @simd for i = 1:n
            if jlconvert_isinitialized(rr, inptr)
                @inbounds v[i] = jlconvert(rr, f, inptr)
            end
            inptr += sizeof(RR)
        end
    end

    io.curptr = inptr + sizeof(RR) * n
    v
end

function read_array(f::JLDFile, inptr::Ptr{Void}, dataspace::ReadDataspace,
                    rr::ReadRepresentation{Any,RelOffset},
                    attributes::Vector{ReadAttribute})
    # Since this is an array of references, there should be an attribute informing us of the type
    for x in attributes
        if x.name == :julia_type
            T = read_attr_data(f, x)
            if isa(T, UnknownType)
                str = typestring(T)
                warn("type $(str) does not exist in workspace; interpreting Array{$str} as Array{Any}")
                T = Any
            end
            return invoke(read_array, Tuple{JLDFile, Ptr{Void}, ReadDataspace, ReadRepresentation, Vector{ReadAttribute}},
                          f, inptr, dataspace, ReadRepresentation{T,RelOffset}(), attributes)
        end
    end

    # If not, something went wrong
    throw(UnsupportedFeatureException())
end

function payload_size(dataspace::WriteDataspace, datatype::H5Datatype, datasz::Int, layout_class::UInt8)
    sz = sizeof(dataspace) + sizeof(datatype) + 2 + (4 + length(dataspace.attributes))*sizeof(HeaderMessage) + 2
    for attr in dataspace.attributes
        sz += sizeof(attr)
    end
    if layout_class == LC_COMPACT_STORAGE
        sz + 2 + datasz
    else
        sz + sizeof(RelOffset) + sizeof(Length)
    end
end

# Might need to do something else someday for non-mmapped IO
function write_data(f::JLDFile, data, odr, wsession::JLDWriteSession)
    io = f.io
    ensureroom(io, sizeof(odr))
    arr = io.arr
    cp = io.curptr
    h5convert!(cp, odr, f, data, wsession)
    io.curptr = cp + sizeof(odr)
    arr # Keep old array rooted until the end
end

# Like isdefined, but assumes arr is a pointer array, and can be inlined
unsafe_isdefined(arr::Array, i::Int) =
    unsafe_load(Ptr{Ptr{Void}}(pointer(arr)+(i-1)*sizeof(Ptr{Void}))) != Ptr{Void}(0)

function write_data{T}(f::JLDFile, data::Array{T}, odr, wsession::JLDWriteSession)
    io = f.io
    ensureroom(io, sizeof(odr) * length(data))
    arr = io.arr
    cp = io.curptr
    ep = io.endptr
    @simd for i = 1:length(data)
        if (isleaftype(T) && isbits(T)) || unsafe_isdefined(data, i)
            # For now, just don't write anything unless the field is defined
            @inbounds h5convert!(cp, odr, f, data[i], wsession)
        else
            @inbounds h5convert_uninitialized!(cp, odr)
        end
        cp += sizeof(odr)
    end
    io.curptr = cp
    arr # Keep old array rooted until the end
end

# Force specialization on DataType
write_data(f::JLDFile, data::Array, odr::Type{Union{}}, wsession::JLDWriteSession) = error("ODR is invalid")

function write_dataset(f::JLDFile, dataspace::WriteDataspace, datatype::H5Datatype, odr, data, wsession::JLDWriteSession)
    io = f.io
    datasz = sizeof(odr) * numel(dataspace)
    layout_class = datasz < 8192 ? LC_COMPACT_STORAGE : LC_CONTIGUOUS_STORAGE
    psz = payload_size(dataspace, datatype, datasz, layout_class)
    fullsz = sizeof(ObjectStart) + size_size(psz) + psz + 4

    header_offset = f.end_of_data
    seek(io, header_offset)
    f.end_of_data = header_offset + fullsz + (layout_class == LC_CONTIGUOUS_STORAGE ? datasz : 0)

    cio = begin_checksum(io, fullsz - 4)

    write(cio, ObjectStart(size_flag(psz)))
    write_size(cio, psz)

    # Dataspace
    write(cio, HeaderMessage(HM_DATASPACE, sizeof(dataspace), 0))
    write(cio, dataspace)

    # Datatype
    write(cio, HeaderMessage(HM_DATATYPE, sizeof(datatype), 1+2*isa(datatype, CommittedDatatype)))
    write(cio, datatype)

    # Fill value
    write(cio, HeaderMessage(HM_FILL_VALUE, 2, 0))
    write(cio, UInt8(3)) # Version
    write(cio, 0x09)     # Flags

    # Attributes
    for attr in dataspace.attributes
        write(cio, HeaderMessage(HM_ATTRIBUTE, sizeof(attr), 0))
        write_attribute(cio, f, attr, f.datatype_wsession)
    end

    # Data storage layout
    if layout_class == LC_COMPACT_STORAGE
        write(cio, HeaderMessage(HM_DATA_LAYOUT, 4+datasz, 0))
        write(cio, UInt8(3))                  # Version
        write(cio, LC_COMPACT_STORAGE)        # Layout class
        write(cio, UInt16(datasz))            # Size
        if datasz != 0
            write_data(f, data, odr, wsession)
            seek(io, header_offset + fullsz - 4)
        end
        write(io, end_checksum(cio))
    else
        write(cio, HeaderMessage(HM_DATA_LAYOUT, 2+sizeof(RelOffset)+sizeof(Length), 0))
        write(cio, UInt8(3))                            # Version
        write(cio, LC_CONTIGUOUS_STORAGE)               # Layout class
        write(cio, h5offset(f, header_offset + fullsz)) # RelOffset
        write(cio, Length(sizeof(data)))                # Length
        write(io, end_checksum(cio))
        if datasz != 0
            write_data(f, data, odr, wsession)
        end
    end

    if typeof(data).mutable && !isa(wsession, JLDWriteSession{None})
        wsession.h5offset[object_id(data)] = h5offset(f, header_offset)
        push!(wsession.objects, data)
    end

    h5offset(f, header_offset)
end

# Force specialization on DataType
write_dataset(f::JLDFile, dataspace::WriteDataspace, datatype::H5Datatype, odr::Type{Union{}}, data, wsession::JLDWriteSession) =
    error("ODR is invalid")

function write_dataset(f::JLDFile, x, wsession::JLDWriteSession)
    odr = objodr(x)
    write_dataset(f, WriteDataspace(f, x, odr), h5type(f, x), odr, x, wsession)
end

@noinline function write_ref_mutable(f::JLDFile, x, wsession::JLDWriteSession)
    offset = get(wsession.h5offset, object_id(x), RelOffset(0))
    offset != RelOffset(0) ? offset : write_dataset(f, x, wsession)::RelOffset
end

@inline function write_ref(f::JLDFile, x, wsession::JLDWriteSession)
    if !typeof(x).mutable || isa(wsession, JLDWriteSession{None})
        write_dataset(f, x, wsession)::RelOffset
    else
        write_ref_mutable(f, x, wsession)::RelOffset
    end
end
