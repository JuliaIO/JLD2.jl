#
# Object headers
#
@enum HeaderMessageTypes::UInt8 begin
    HM_NIL = 0x00
    HM_DATASPACE = 0x01
    HM_LINK_INFO = 0x02
    HM_DATATYPE = 0x03
    HM_FILL_VALUE_OLD = 0x04
    HM_FILL_VALUE = 0x05
    HM_LINK_MESSAGE = 0x06
    HM_EXTERNAL_FILE_LIST = 0x07
    HM_DATA_LAYOUT = 0x08
    HM_BOGUS = 0x09
    HM_GROUP_INFO = 0x0a
    HM_FILTER_PIPELINE = 0x0b
    HM_ATTRIBUTE = 0x0c
    HM_OBJECT_COMMENT = 0x0d
    HM_SHARED_MESSAGE_TABLE = 0x0f
    HM_OBJECT_HEADER_CONTINUATION = 0x10
    HM_SYMBOL_TABLE = 0x11
    HM_MODIFICATION_TIME = 0x12
    HM_BTREE_K_VALUES = 0x13
    HM_DRIVER_INFO = 0x14
    HM_ATTRIBUTE_INFO = 0x15
    HM_REFERENCE_COUNT = 0x16
end
Base.convert(::Type{UInt8}, h::HeaderMessageTypes) = UInt8(h)

const OH_ATTRIBUTE_CREATION_ORDER_TRACKED = 2^2
const OH_ATTRIBUTE_CREATION_ORDER_INDEXED = 2^3
const OH_ATTRIBUTE_PHASE_CHANGE_VALUES_STORED = 2^4
const OH_TIMES_STORED = 2^5

const OBJECT_HEADER_CONTINUATION_SIGNATURE = htol(0x4b48434f) # "OCHK"

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
        obj_header_size = jlread(io, UInt32)
        return obj_header_size, 1, os.flags
    end
end

struct HeaderMessage
    msg_type::UInt8
    size::UInt16
    flags::UInt8
end
define_packed(HeaderMessage)


function isgroup(f::JLDFile, roffset::RelOffset)
    io = f.io
    chunk_start = fileoffset(f, roffset)
    seek(io, chunk_start)

    sz, version, = read_obj_start(io)
    chunk_end::Int64 = position(io) + sz
    if version == 2
        while position(io) <= chunk_end-4
            msg = jlread(io, HeaderMessage)
            endpos = position(io) + msg.size
            if HeaderMessageTypes(msg.msg_type) == HM_LINK_INFO || HeaderMessageTypes(msg.msg_type) == HM_GROUP_INFO || HeaderMessageTypes(msg.msg_type) == HM_LINK_MESSAGE || HeaderMessageTypes(msg.msg_type) == HM_SYMBOL_TABLE
                return true
            elseif HeaderMessageTypes(msg.msg_type) == HM_DATASPACE || HeaderMessageTypes(msg.msg_type) == HM_DATATYPE || HeaderMessageTypes(msg.msg_type) == HM_FILL_VALUE || HeaderMessageTypes(msg.msg_type) == HM_DATA_LAYOUT
                return false
            end
            seek(io, endpos)
        end
    elseif version == 1
        chunks = [(; chunk_start, chunk_end)]
        chunk_number = 0
        skip_to_aligned!(io, chunk_start)

        while !isempty(chunks)
            chunk = popfirst!(chunks)
            chunk_start = chunk.chunk_start
            chunk_end = chunk.chunk_end
        
            if chunk_number > 0
                seek(io, chunk_start)
            end
            chunk_number += 1

            while position(io) < chunk_end - 4
                # Message start 8byte aligned relative to object start
                skip_to_aligned!(io, chunk_start)
                # Version 1 header message is padded
                msg = HeaderMessage(UInt8(jlread(io, UInt16)), jlread(io, UInt16), jlread(io, UInt8))
                skip(io, 3)
                endpos = position(io) + msg.size

                if HeaderMessageTypes(msg.msg_type) in (HM_LINK_INFO, HM_GROUP_INFO, HM_LINK_MESSAGE, HM_SYMBOL_TABLE)
                    return true
                elseif HeaderMessageTypes(msg.msg_type) in (HM_DATASPACE, HM_DATATYPE, HM_FILL_VALUE, HM_DATA_LAYOUT)
                    return false
                elseif HeaderMessageTypes(msg.msg_type) == HM_OBJECT_HEADER_CONTINUATION
                    cont_chunk_start = fileoffset(f, jlread(io, RelOffset))
                    chunk_length = jlread(io, Length)
                    push!(chunks, (; chunk_start=cont_chunk_start, chunk_end=cont_chunk_start+chunk_length))
                end
                seek(io, endpos)
            end
        end
    end
    return false
end

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


function print_header_messages(f::JLDFile, roffset::RelOffset)
    io = f.io
    cio, header_version, chunk, groupflags = start_obj_read(f, offset)
    chunk_start, chunk_end = chunk

    # Messages
    continuation_message_goes_here::Int64 = -1
    chunks = [(; chunk_start, chunk_end)]
    chunk_number = 0
    next_link_offset::Int64 = -1

    chunk_end::Int64
    attrs = ReadAttribute[]
    while !isempty(chunks)
        chunk = popfirst!(chunks)
        chunk_start, chunk_end = chunk.chunk_start, chunk.chunk_end
        @info "Starting to read chunk no $chunk_number of length $(chunk_end-chunk_start)"

        if chunk_number > 0
            cio = start_chunk_read(io, chunk, header_version)
            header_version == 2 && (chunk_end -= 4)
        end
        chunk_number += 1
        @info "positions" position(cio) chunk_start chunk_end
        while (curpos = position(cio)) < chunk_end-4
            msg = read_header_message(f, cio, header_version, chunk_start, groupflags)
            println("""
            Message:  $(msg.type) ($(Int(msg.type)))
                size: $(msg.size)
                flags: $(msg.hflags)
                offset: $(msg.offset)""")
            if msg.type == HM_NIL
                if continuation_message_goes_here == -1 && 
                    chunk_end - curpos == CONTINUATION_MSG_SIZE
                    continuation_message_goes_here = curpos
                elseif endpos + CONTINUATION_MSG_SIZE == chunk_end
                    # This is the remaining space at the end of a chunk
                    # Use only if a message can potentially fit inside
                    # Single Character Name Link Message has 13 bytes payload
                    if msg.size >= 13 
                        next_link_offset = curpos
                    end
                end
            else
                continuation_message_goes_here = -1
                if msg.type == HM_LINK_INFO
                    if isset(msg.flags,0 )
                        println("    max_creation_index: $(msg.max_creation_index)")
                    end
                    println("    fractal_heap_address: $(msg.fractal_heap_address)")
                    println("    v2_btree_name_index: $(msg.v2_btree_name_index)")
                    if isset(msg.flags, 1)
                        println("    v2_btree_creation_index: $(msg.v2_btree_creation_index)")
                    end
                elseif msg.type == HM_GROUP_INFO
                    if msg.size > 2
                        flag = msg.flags
                        if isset(flags, 0)
                            println("    link_phase_change_max_compact = $(msg.link_phase_change_max_compact)")
                            println("    link_phase_change_min_dense = $(msg.link_phase_change_min_dense)")
                        end
                        if isset(flags, 1)
                            # Verify that non-default group size is given
                            println("    est_num_entries = $(msg.est_num_entries)")
                            println("    est_link_name_len = $(msg.est_link_name_len)")
                        end
                    end
                elseif msg.type == HM_LINK_MESSAGE
                    name, loffset = msg.link_name, msg.target
                    println("   name = \"$name\"")
                    println("   offset = $(Int(loffset.offset))")
                elseif msg.type == HM_OBJECT_HEADER_CONTINUATION
                    push!(chunks, (; chunk_start = msg.continuation_offset,
                                     chunk_end = msg.continuation_offset + msg.continuation_length))
                    println("""    offset = $(continuation_offset)\n    length = $(continuation_length)""")
                    println("pos=$(position(cio)) $chunk_end")
                elseif msg.type == HM_DATASPACE
                    println("    $(ReadDataspace(f, msg))")
                elseif msg.type == HM_DATATYPE
                    datatype_class, datatype_offset = datatype_from_message(f, msg)
                    println("""    class: $datatype_class\n    offset: $datatype_offset""")
                elseif msg.type == HM_FILL_VALUE_OLD
                elseif msg.type == HM_FILL_VALUE
                elseif msg.type == HM_DATA_LAYOUT
                    layout = DataLayout(f, msg)
                    @info layout
                elseif msg.type == HM_FILTER_PIPELINE
                    filter_pipeline = FilterPipeline(msg)
                    @info filter_pipeline
                elseif msg.type == HM_SYMBOL_TABLE
                    println("""    required for \"old style" groups\n    v1 B-Tree Address: $(msg.v1_btree_address)\n    Local Heap Address: $(msg.local_heap_address)""")
                elseif msg.type == HM_ATTRIBUTE
                    push!(attrs, read_attribute(f, msg))
                    attr = attrs[end]
                    println("""    name: \"$(attr.name)\" """)
                    if attr.datatype_class != 0xff
                        println("""    datatype: $(DATATYPES[attr.datatype_class%16])""")
                    else
                        println("""    datatype: committed at $(attr.datatype_offset)""")
                    end
                    #try
                        data = read_attr_data(f, attr)
                        println("""    data: "$data" """)
                    #= catch e
                        println("""    loading data failed""")
                    end =#
                elseif (msg.flags & 2^3) != 0
                    throw(UnsupportedFeatureException())
                end
            end
        end

        # Checksum
        seek(cio, chunk_end)
        if header_version == 2
            end_checksum(cio) == jlread(io, UInt32) || throw(InvalidDataException())
        end
    end
    nothing
end


struct DataLayout
    version::UInt8
    storage_type::UInt8
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

ischunked(dl::DataLayout) = dl.storage_type == 2

function read_nb_uint(io::IO, nb)
    val = zero(UInt)
    for n = 1:nb
        #val = val << 8
        val += jlread(io, UInt8)*(2^(8n))
    end
    val
end