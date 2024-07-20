#
# Object headers
#
struct ObjectStart
    signature::UInt32
    version::UInt8
    flags::UInt8
end
ObjectStart(flags::UInt8) = ObjectStart(OBJECT_HEADER_SIGNATURE, 2, flags)
define_packed(ObjectStart)

# Reads the start of an object including the signature, version, flags,
# and (payload) size. Returns the size.
function read_obj_start(io::IO)
    curpos = position(io)
    os = jlread(io, ObjectStart)
    if os.version == 2 && os.signature == OBJECT_HEADER_SIGNATURE
        if (os.flags & OH_TIMES_STORED) != 0
            # Skip access, modification, change and birth times
            skip(io, 16)
        end
        if (os.flags & OH_ATTRIBUTE_PHASE_CHANGE_VALUES_STORED) != 0
            # Skip maximum # of attributes fields
            skip(io, 4)
        end

        return read_size(io, os.flags), 2, os.flags
    else
        seek(io, curpos)
        version = jlread(io, UInt8)
        version == 1 || throw(error("This should not have happened"))
        
        skip(io, 1)
        num_messages = jlread(io, UInt16)
        obj_ref_count = jlread(io, UInt32)
        obj_header_size = Int(jlread(io, UInt32))
        return obj_header_size, 1, os.flags
    end
end

"""
    start_obj_read(f::JLDFile, offset::RelOffset)

Internal function that does some prep-work for reading a header.
"""
function start_obj_read(f, offset)
    io = f.io
    chunk_start::Int64 = fileoffset(f, offset)
    seek(io, chunk_start)
    # Version 1 object header have no signature and start with version
    header_version = jlread(io, UInt8)#::UInt8
    if header_version == 1
        seek(io, chunk_start)
        cio = io
        sz,_,groupflags = read_obj_start(cio)
        # Skip to nearest 8byte aligned position
        #skip_to_aligned!(cio, fileoffset(f, offset))
        skip(cio, 4)
        chunk_end = position(cio) + sz
    else
        header_version = 0x2
        seek(io, chunk_start)
        cio = begin_checksum_read(io)
        sz,_,groupflags = read_obj_start(cio)
        chunk_end = position(cio) + sz
    end
    chunk = (; chunk_start, chunk_end)
    return cio, header_version, chunk, groupflags
end

"""
    start_chunk_read(io, chunk, header_version)

Internal function that does some prep-work for reading a chunk
(which is a continuation of a regular object header).
"""
function start_chunk_read(io, chunk, header_version)
    seek(io, chunk.chunk_start)
    if header_version == 2
        cio = begin_checksum_read(io)
        jlread(cio, UInt32) == OBJECT_HEADER_CONTINUATION_SIGNATURE || throw(InvalidDataException())
        cio
    else
        cio = io
    end
    return cio
end

"""

"""
struct Message{IO}
    type::HeaderMessageTypes
    address::UInt64
    offset::RelOffset
    io::IO
    Message(type::HeaderMessageTypes, address::Integer, o::RelOffset, io::IO) =
        new{typeof(io)}(type, UInt64(address), o, io)
end
Message(type::UInt8, args...) = Message(HeaderMessageTypes(type), args...)
Message(type::HeaderMessageTypes, f::JLDFile, offset::RelOffset) = 
    Message(type, fileoffset(f, offset), offset, f.io)
Message(type::HeaderMessageTypes, io::IO) = Message(type, position(io), UNDEFINED_ADDRESS, io)
Message(type::HeaderMessageTypes, data::Vector{UInt8}) = Message(type, 0, UNDEFINED_ADDRESS, IOBuffer(data))


@generated function Base.getproperty(m::Message, s::Symbol)
    ex = Expr(:block)
    for v in instances(HeaderMessageTypes)
        push!(ex.args, :(getfield(m, :type) == $(v) && return iogetprop($(Val(v)), m, s)))
    end
    return quote
        s in (:type, :address, :io) && return getfield(m, s)
        $(ex)
    end
end

"""
    Hmessage{IO}

Representation of a Header Message in memory. Provides `getproperty` access
to the fields of the message.
Can also be used to construct and write custom messages.
"""
struct Hmessage{IO}
    type::HeaderMessageTypes
    size::UInt16
    hflags::UInt8
    offset::RelOffset
    payload_offset::RelOffset
    m::Message{IO}
end

@generated function Base.getproperty(hm::Hmessage, s::Symbol)
    ex = Expr(:block)
    for v in instances(HeaderMessageTypes)
        push!(ex.args, :(getfield(hm,:type) == $(v) && return iogetprop($(Val(v)), getfield(hm,:m), s,
            getfield(hm, :hflags), getfield(hm, :size))))
    end
    return quote
        s in (:type, :size, :hflags, :offset, :payload_offset, :m) && return getfield(hm, s)
        $(ex)
    end
end

function Hmessage(type::HeaderMessageTypes, hflags=0x00, size=0; kwargs...)
    kw = (; kwargs...)
    size = sizefun(Val(type), hflags, size, kw)
    payload = construct_hm_payload(Val(type), hflags, size, kw)
    Hmessage(type, UInt16(size), UInt8(hflags), UNDEFINED_ADDRESS,UNDEFINED_ADDRESS,
        Message(type, 0, UNDEFINED_ADDRESS, payload))
end

write_message(io, f::JLDFile, msg::Hmessage) = jlwrite(io, msg)

function jlwrite(io, msg::Hmessage)
    write(io, msg.type)
    write(io, msg.size)
    write(io, msg.hflags)
    m = msg.m
    mio = m.io
    seek(mio, m.address)
    write(io, take!(mio))
end

jlsizeof(msg::Hmessage) = jlsizeof(HeaderMessage) + msg.size

function Base.show(io::IO, hm::Hmessage)
    println(io, 
     """┌─ Header Message: $(hm.type)
        │ ┌─ offset:\t$(hm.offset)
        │ │  size:\t$(hm.size)
        │ └─ flags:\t$(hm.hflags)""")
    keyvalue = messageshow(Val(hm.type), hm.m, hm.hflags, hm.size)

    N = length(keyvalue)
    for n = 1:N
        k, v = keyvalue[n]
        print(io, n < N ? "│    " : "└─   ") 
        println(io, "$k:\t$v")
    end
    if N == 0
        println(io, "└─")
    end
end

"""
    print_header_messages(f::JLDFile, name::AbstractString)
    print_header_messages(g::Group, name::AbstractString)
    print_header_messages(f::JLDFile, offset::RelOffset)

Prints the header messages of a group or dataset in a file.
"""
function print_header_messages(f::JLDFile, name::AbstractString)
    if isempty(name) || name == "/"
        print_header_messages(f, f.root_group_offset)
    else 
        print_header_messages(f.root_group,name)
    end
end

function print_header_messages(g::Group, name::AbstractString)
    f = g.f
    f.n_times_opened == 0 && throw(ArgumentError("file is closed"))
    (g, name) = pathize(g, name, false)
    roffset = lookup_offset(g, name)
    roffset != UNDEFINED_ADDRESS || throw(ArgumentError("did not find a group or dataset named \"$name\""))
    print_header_messages(f, roffset)
end

function print_header_messages(f::JLDFile, offset::RelOffset)
    hmitr = HeaderMessageIterator(f, offset)
    for msg in hmitr
        print(msg)
        if msg.type == HM_FILTER_PIPELINE
            filter_pipeline = FilterPipeline(msg)
            @info filter_pipeline
        elseif msg.type == HM_ATTRIBUTE
            attr = read_attribute(f, msg)
            data = read_attr_data(f, attr)
            println("""    data: "$data" """)
        elseif (msg.hflags & 2^3) != 0
            throw(UnsupportedFeatureException())
        end
    end
    nothing
end


function read_header_message(f, io, header_version, chunk_start, groupflags, lazy=true)
    msgpos = h5offset(f, position(io))
    if header_version == 1
        # Message start 8byte aligned relative to object start
        skip_to_aligned!(io, chunk_start)
        msgpos = h5offset(f, position(io))
        # Version 1 header message is padded
        msg = HeaderMessage(UInt8(jlread(io, UInt16)), jlread(io, UInt16), jlread(io, UInt8))
        skip(io, 3)
    else # header_version == 2
        msg = jlread(io, HeaderMessage)
        (groupflags & 4) == 4 && skip(io, 2) 
    end
    payload_address = position(io)
    payload_offset = h5offset(f, position(io))
    if lazy == true
        #skip(io, msg.size)
        Hmessage(
            HeaderMessageTypes(msg.msg_type),
            msg.size, msg.flags, msgpos, payload_offset,
            Message(HeaderMessageTypes(msg.msg_type), payload_address, payload_offset, io))
    else
        payload = IOBuffer(jlread(io, UInt8, msg.size))
        Hmessage(
            HeaderMessageTypes(msg.msg_type),
            msg.size, msg.flags, msgpos, payload_offset,
            Message(HeaderMessageTypes(msg.msg_type), 0, payload_offset, payload))
    end
end



"""
    mutable struct HeaderMessageIterator{IO}
        HeaderMessageIterator(f::JLDFile, offset::RelOffset, lazy=true)

Implements an iterator over header messages.
"""
mutable struct HeaderMessageIterator{IO}
    f::JLDFile{IO}
    curpos::Int64
    header_version::UInt8
    objflags::UInt8
    lazy::Bool
    chunk::@NamedTuple{chunk_start::Int64, chunk_end::Int64}
    next_chunk::@NamedTuple{chunk_start::Int64, chunk_end::Int64}
    second_to_next_chunk::@NamedTuple{chunk_start::Int64, chunk_end::Int64}
end

function HeaderMessageIterator(f::JLDFile, offset::RelOffset, lazy=true)
    cio, header_version, chunk, objflags = start_obj_read(f, offset)
    io = f.io
    pos = position(cio)
    if header_version == 2
        # Verify correctness now
        seek(cio, chunk.chunk_end)
        end_checksum(cio) == jlread(io, UInt32)
        seek(io, pos)
    end
    HeaderMessageIterator{typeof(io)}(
        f, pos, header_version, objflags, lazy,
        chunk, (; chunk_start = -1, chunk_end = -1), (; chunk_start = -1, chunk_end = -1))
end
    
Base.IteratorSize(::HeaderMessageIterator) = Base.SizeUnknown()

function Base.iterate(itr::HeaderMessageIterator, state=nothing)
    io = itr.f.io
    chunk = itr.chunk
    if itr.curpos > chunk.chunk_end
        throw(InternalError("This should not happen"))
    elseif itr.curpos > (chunk.chunk_end - 4 - 2*((itr.objflags & 4) == 4))
        if itr.next_chunk.chunk_start == -1
            return nothing
        end
        itr.chunk = chunk = itr.next_chunk
        itr.next_chunk = itr.second_to_next_chunk
        itr.second_to_next_chunk = (; chunk_start=-1, chunk_end=-1)

        seek(io, chunk.chunk_start)
        if itr.header_version == 2
            # Verify integrity of new chunk
            cio = begin_checksum_read(io)
            jlread(cio, UInt32) == OBJECT_HEADER_CONTINUATION_SIGNATURE || throw(InvalidDataException())
            seek(cio, chunk.chunk_end-4)
            end_checksum(cio) == jlread(io, UInt32)
            seek(io, chunk.chunk_start + 4)
            chunk = itr.chunk = (; chunk.chunk_start, chunk_end=chunk.chunk_end-4)
        end        
        itr.curpos = position(io)
    end
    seek(io, itr.curpos)
    msg = read_header_message(itr.f, io, itr.header_version, chunk.chunk_start, itr.objflags, itr.lazy)
    #itr.curpos = position(io)
    itr.curpos = fileoffset(itr.f, msg.payload_offset)+msg.size
    if msg.type == HM_OBJECT_HEADER_CONTINUATION
        new_chunk =  (;
            chunk_start = fileoffset(itr.f, msg.continuation_offset),
            chunk_end = fileoffset(itr.f, msg.continuation_offset + msg.continuation_length))
        if itr.next_chunk.chunk_start == -1
            itr.next_chunk = new_chunk
        elseif itr.second_to_next_chunk.chunk_start == -1
            itr.second_to_next_chunk = new_chunk
        else
            throw(InternalError("This should not happen"))
        end
    end
    return msg, nothing
end


## Left over header message parsing that does not have a good place.

struct DataLayout
    version::UInt8
    storage_type::LayoutClass
    data_length::Int64
    data_offset::Int64
    dimensionality::UInt8
    chunk_indexing_type::UInt8 # only in version 4
    chunk_dimensions::Vector{UInt64} # only defined if dimensionality > 0
    DataLayout(version, storage_type, data_length, data_offset) = 
        new(version, storage_type, data_length, data_offset, 0, 0)
    DataLayout(version, storage_type, data_length, data_offset, dimensionality, chunk_indexing_type, chunk_dimensions) = 
        new(version, storage_type, data_length, data_offset, dimensionality, chunk_indexing_type, chunk_dimensions)
end

ischunked(dl::DataLayout) = dl.storage_type == LC_CHUNKED_STORAGE

function DataLayout(f::JLD2.JLDFile, msg::Hmessage)
    version = msg.version::UInt8
    storage_type = msg.layout_class::LayoutClass
    rf = msg.data_address::RelOffset
    data_offset::Int64 = rf != UNDEFINED_ADDRESS ? fileoffset(f, rf) : typemax(Int64)
    if version == 4 || version == 3
        if storage_type in (LC_COMPACT_STORAGE, LC_CONTIGUOUS_STORAGE)
            data_length = Int64(msg.data_size)

            return DataLayout(version, storage_type, data_length, data_offset) 
        elseif version == 4 && storage_type == LC_CHUNKED_STORAGE
            chunk_dimensions = Int[msg.dimensions...]
            chunk_indexing_type = msg.chunk_indexing_type
            chunk_indexing_type == 1 || throw(UnsupportedFeatureException("Unknown chunk indexing type"))
            data_length = Int64(msg.data_size)

            #filters = msg.filters#jlread(cio, UInt32)
            chunked_storage = true
            DataLayout(version, storage_type, data_length, data_offset, msg.dimensionality, msg.chunk_indexing_type, chunk_dimensions) 
        elseif version == 3 && storage_type == LC_CHUNKED_STORAGE
            data_length = Int64(msg.data_size)

            chunk_dimensions = Int[msg.dimensions[1:end-1]...] # drop element size as last dimension
            chunked_storage = true
            DataLayout(version, storage_type, data_length, data_offset, msg.dimensionality, 0, chunk_dimensions) 
        else
            throw(UnsupportedFeatureException("Unknown data layout"))
        end
    else
        throw(UnsupportedVersionException("Data layout message version $version is not supported"))
    end
end

function FilterPipeline(msg::Hmessage)
    version = msg.version
    nfilters = msg.nfilters
    io = msg.m.io
    seek(io, msg.m.address+2)
    if version == 1
        skip(io, 6)
        filters = map(1:nfilters) do _
            id = jlread(io, UInt16)
            name_length = jlread(io, UInt16)
            flags = jlread(io, UInt16)
            nclient_vals = jlread(io, UInt16)
            if iszero(name_length) 
                name = ""
            else
                name = read_bytestring(io)
                skip(io, 8-mod1(sizeof(name), 8)-1)
            end
            client_data = jlread(io, UInt32, nclient_vals)
            isodd(nclient_vals) && skip(io, 4)
            Filter(id, flags, name, client_data)
        end
        return FilterPipeline(filters)
    elseif version == 2
        filters = map(1:nfilters) do _
            id = jlread(io, UInt16)
            if id > 255
                name_length = jlread(io, UInt16)
                flags = jlread(io, UInt16)
                nclient_vals = jlread(io, UInt16)
                if iszero(name_length) 
                    name = ""
                else
                    name = read_bytestring(io)
                    skip(io, 8-mod1(sizeof(name), 8)-1)
                end
            else
                name = ""
                flags = jlread(io, UInt16)
                nclient_vals = jlread(io, UInt16)
            end
            client_data = jlread(io, UInt32, nclient_vals)
            Filter(id, flags, name, client_data)
        end
        return FilterPipeline(filters)
    else
        throw(UnsupportedVersionException("Filter Pipeline Message version $version is not implemented"))
    end
end