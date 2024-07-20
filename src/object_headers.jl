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

const CONTINUATION_PLACEHOLDER = Hmessage(HM_NIL, 0, 16)

write_message(io, f::JLDFile, msg::Hmessage) = jlwrite(io, msg)

function jlwrite(io, msg::Hmessage)
    write(io, msg.type)
    write(io, msg.size)
    write(io, msg.hflags)
    m = msg.m
    mio = m.io
    seek(mio, m.address)
    for _ in 1:msg.size
        write(io, jlread(mio, UInt8))
    end
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


function read_header_message(f, io, header_version, chunk_start, groupflags)#, lazy=true)
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
    # if lazy == true
    #     #skip(io, msg.size)
        Hmessage(
            HeaderMessageTypes(msg.msg_type),
            msg.size, msg.flags, msgpos, payload_offset,
            Message(HeaderMessageTypes(msg.msg_type), payload_address, payload_offset, io))
    # else
    #     payload = IOBuffer(jlread(io, UInt8, msg.size))
    #     Hmessage(
    #         HeaderMessageTypes(msg.msg_type),
    #         msg.size, msg.flags, msgpos, payload_offset,
    #         Message(HeaderMessageTypes(msg.msg_type), 0, payload_offset, payload))
    # end
end



"""
    mutable struct HeaderMessageIterator{IO}
        HeaderMessageIterator(f::JLDFile, offset::RelOffset)

Implements an iterator over header messages.
"""
mutable struct HeaderMessageIterator{IOT}
    f::JLDFile{IOT}
    curpos::Int64
    header_version::UInt8
    objflags::UInt8
    chunk::@NamedTuple{chunk_start::Int64, chunk_end::Int64}
    next_chunk::@NamedTuple{chunk_start::Int64, chunk_end::Int64}
    second_to_next_chunk::@NamedTuple{chunk_start::Int64, chunk_end::Int64}
end

function HeaderMessageIterator(f::JLDFile{IOT}, offset::RelOffset) where {IOT}
    cio, header_version, chunk, objflags = start_obj_read(f, offset)
    io = f.io
    pos = position(cio)
    if header_version == 2
        # Verify correctness now
        seek(cio, chunk.chunk_end)
        end_checksum(cio) == jlread(io, UInt32)
        seek(io, pos)
    end
    HeaderMessageIterator{IOT}(
        f, pos, header_version, objflags,
        chunk, (; chunk_start = -1, chunk_end = -1), (; chunk_start = -1, chunk_end = -1))
end
    
Base.IteratorSize(::HeaderMessageIterator) = Base.SizeUnknown()

function Base.iterate(itr::HeaderMessageIterator{IO}, state=nothing) where IO
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
    msg = read_header_message(itr.f, io, itr.header_version, chunk.chunk_start, itr.objflags)::Hmessage{IO}
    itr.curpos = fileoffset(itr.f, msg.payload_offset)+msg.size
    if msg.type == HM_OBJECT_HEADER_CONTINUATION
        chunk_start = fileoffset(itr.f, msg.continuation_offset::RelOffset)
        chunk_end = chunk_start + msg.continuation_length::Int64
        new_chunk =  (; chunk_start, chunk_end)
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