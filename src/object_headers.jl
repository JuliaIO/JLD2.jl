#
# Object headers
#

const HM_NIL = 0x00
const HM_DATASPACE = 0x01
const HM_LINK_INFO = 0x02
const HM_DATATYPE = 0x03
const HM_FILL_VALUE_OLD = 0x04
const HM_FILL_VALUE = 0x05
const HM_LINK_MESSAGE = 0x06
const HM_EXTERNAL_FILE_LIST = 0x07
const HM_DATA_LAYOUT = 0x08
const HM_BOGUS = 0x09
const HM_GROUP_INFO = 0x0a
const HM_FILTER_PIPELINE = 0x0b
const HM_ATTRIBUTE = 0x0c
const HM_OBJECT_COMMENT = 0x0d
const HM_SHARED_MESSAGE_TABLE = 0x0f
const HM_OBJECT_HEADER_CONTINUATION = 0x10
const HM_SYMBOL_TABLE = 0x11
const HM_MODIFICATION_TIME = 0x12
const HM_BTREE_K_VALUES = 0x13
const HM_DRIVER_INFO = 0x14
const HM_ATTRIBUTE_INFO = 0x15
const HM_REFERENCE_COUNT = 0x16

MESSAGE_TYPES = Dict(
    0x00 => "HM_NIL",
    0x01 => "HM_DATASPACE",
    0x02 => "HM_LINK_INFO",
    0x03 => "HM_DATATYPE",
    0x04 => "HM_FILL_VALUE_OLD",
    0x05 => "HM_FILL_VALUE",
    0x06 => "HM_LINK_MESSAGE",
    0x07 => "HM_EXTERNAL_FILE_LIST",
    0x08 => "HM_DATA_LAYOUT",
    0x09 => "HM_BOGUS",
    0x0a => "HM_GROUP_INFO",
    0x0b => "HM_FILTER_PIPELINE",
    0x0c => "HM_ATTRIBUTE",
    0x0d => "HM_OBJECT_COMMENT",
    0x0f => "HM_SHARED_MESSAGE_TABLE",
    0x10 => "HM_OBJECT_HEADER_CONTINUATION",
    0x11 => "HM_SYMBOL_TABLE",
    0x12 => "HM_MODIFICATION_TIME",
    0x13 => "HM_BTREE_K_VALUES",
    0x14 => "HM_DRIVER_INFO",
    0x15 => "HM_ATTRIBUTE_INFO",
    0x16 => "HM_REFERENCE_COUNT",
    )

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
# function read_obj_start(io::IO)
#     os = jlread(io, ObjectStart)
#     os.signature == OBJECT_HEADER_SIGNATURE || throw(InvalidDataException())
#     os.version == 2 || throw(UnsupportedVersionException())

#     if (os.flags & OH_TIMES_STORED) != 0
#         # Skip access, modification, change and birth times
#         skip(io, 128)
#     end
#     if (os.flags & OH_ATTRIBUTE_PHASE_CHANGE_VALUES_STORED) != 0
#         # Skip maximum # of attributes fields
#         skip(io, 32)
#     end

#     read_size(io, os.flags)
# end

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
            if msg.msg_type == HM_LINK_INFO || msg.msg_type == HM_GROUP_INFO || msg.msg_type == HM_LINK_MESSAGE || msg.msg_type == HM_SYMBOL_TABLE
                return true
            elseif msg.msg_type == HM_DATASPACE || msg.msg_type == HM_DATATYPE || msg.msg_type == HM_FILL_VALUE || msg.msg_type == HM_DATA_LAYOUT
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
                msg = HeaderMessage(jlread(io, UInt16), jlread(io, UInt16), jlread(io, UInt8))
                skip(io, 3)
                endpos = position(io) + msg.size

                if msg.msg_type in (HM_LINK_INFO, HM_GROUP_INFO, HM_LINK_MESSAGE, HM_SYMBOL_TABLE)
                    return true
                elseif msg.msg_type in (HM_DATASPACE, HM_DATATYPE, HM_FILL_VALUE, HM_DATA_LAYOUT)
                    return false
                elseif msg.msg_type == HM_OBJECT_HEADER_CONTINUATION
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

# code below is work in progress and for debugging
struct Message
    type::UInt8
    header
    fields
end


function read_link_info(io, msg_header, OffsetType)
    @assert msg_header.msg_type == HM_LINK_INFO
    version = jlread(io, UInt8)
    flags = jlread(io, UInt8)
    # Maximum Creation index
    # exists if bit 0 of flag is set
    if (flags & 0x1) == 0x1
        max_creation_index = jlread(io, UInt64)
    else
        max_creation_index = typemax(UInt64)
    end
    fractal_heap_address = jlread(io, OffsetType)
    v2btree_name_index = jlread(io, OffsetType)
    if (flags & 0x1) == 0x1
        v2btree_creation_index = jlread(io, OffsetType)
    else
        v2btree_creation_index = UNDEFINED_ADDRESS
    end
    return Message(HM_LINK_INFO,msg_header, (; 
                    version,
                    flags,
                    fractal_heap_address,
                    v2btree_creation_index, 
                    v2btree_name_index))
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
    chunk_start::Int64 = fileoffset(f, roffset)
    seek(io, chunk_start)

    # Test for V1 Obj header        

    header_version = Int(jlread(io, UInt8))
    if header_version == 1
        seek(io, chunk_start)
        cio = io
        sz,_,groupflags = read_obj_start(cio)
        chunk_end = position(cio) + sz
        # Skip to nearest 8byte aligned position
        skip_to_aligned!(cio, chunk_start)
    else
        println("Object Header Message Version 2")
        header_version = 2
        seek(io, chunk_start)
        cio = begin_checksum_read(io)
        sz,_,groupflags = read_obj_start(cio)
        chunk_end = position(cio) + sz
        @info "chunk 0" position(cio) sz chunk_end
    end
    # Messages
    continuation_message_goes_here::Int64 = -1
    links = OrderedDict{String,RelOffset}()
    chunks = [(; chunk_start, chunk_end)]
    chunk_number = 0
    next_link_offset::Int64 = -1
    link_phase_change_max_compact::Int64 = -1 
    link_phase_change_min_dense::Int64 = -1
    est_num_entries::Int64 = 4
    est_link_name_len::Int64 = 8
    chunk_end::Int64
    attrs = EMPTY_READ_ATTRIBUTES
    while !isempty(chunks)
        chunk = popfirst!(chunks)
        chunk_start = chunk.chunk_start
        chunk_end = chunk.chunk_end

        @info "Starting to read chunk no $chunk_number of length $(chunk_end-chunk_start)"
        if chunk_number > 0 # Don't do this the first time around
            seek(io, chunk_start)
            if header_version == 2
                chunk_end -= 4
                cio = begin_checksum_read(io)
                jlread(cio, UInt32) == OBJECT_HEADER_CONTINUATION_SIGNATURE || throw(InvalidDataException())
            end
        end
        chunk_number += 1
        @info "positions" position(cio) chunk_start chunk_end
        while (curpos = position(cio)) < chunk_end-4
            if header_version == 1
                skip_to_aligned!(cio, chunk_start)
                # Version 1 header message is padded
                msg = HeaderMessage(jlread(cio, UInt16), jlread(cio, UInt16), jlread(cio, UInt8))
                skip(cio, 3)
            else # version == 2
                msg = jlread(cio, HeaderMessage)
                (groupflags & 4) == 4 && skip(cio, 2) 
            end
            endpos = position(cio) + msg.size
            println("""
            Message:  $(MESSAGE_TYPES[msg.msg_type]) ($(msg.msg_type)))
                size: $(msg.size)
                flags: $(msg.flags)
                at pos $(position(cio)-chunk_start)""")
            if msg.msg_type == HM_NIL
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
                if msg.msg_type == HM_LINK_INFO
                    fullmsg = read_link_info(cio, msg, RelOffset)
                    for (k,v) in pairs(fullmsg.fields)
                        println("    $k: $v")
                    end
                elseif msg.msg_type == HM_GROUP_INFO
                    if msg.size > 2
                        # Version Flag
                        jlread(io, UInt8) == 0 || throw(UnsupportedFeatureException()) 
                        flag = jlread(io, UInt8)
                        if flag%2 == 1 # first bit set
                            link_phase_change_max_compact = jlread(io, UInt16)
                            link_phase_change_min_dense = jlread(io, UInt16)
                            println("    link_phase_change_max_compact = $link_phase_change_max_compact")
                            println("    link_phase_change_min_dense = $link_phase_change_min_dense")
                        end
                        if (flag >> 1)%2 == 1 # second bit set
                            # Verify that non-default group size is given
                            est_num_entries = jlread(io, UInt16)
                            est_link_name_len = jlread(io, UInt16)
                            println("    est_num_entries = $est_num_entries")
                            println("    est_link_name_len = $est_link_name_len")
                        end
                    end
                elseif msg.msg_type == HM_LINK_MESSAGE
                    name, loffset = read_link(cio)
                    links[name] = loffset
                    println("   name = \"$name\"")
                    println("   offset = $(Int(loffset.offset))")
                elseif msg.msg_type == HM_OBJECT_HEADER_CONTINUATION
                    continuation_offset = fileoffset(f, jlread(cio, RelOffset))
                    continuation_length = jlread(cio, Length)
                    push!(chunks, (; chunk_start = continuation_offset,
                                     chunk_end = continuation_offset + continuation_length))
                    println("""    offset = $(continuation_offset)\n    length = $(continuation_length)""")
                    println("pos=$(position(cio)) $chunk_end")
                elseif msg.msg_type == HM_DATASPACE
                    dataspace = read_dataspace_message(cio)
                    println("    $dataspace")
                elseif msg.msg_type == HM_DATATYPE
                    datatype_class, datatype_offset = read_datatype_message(cio, f, (msg.flags & 2) == 2)
                    println("""    class: $datatype_class\n    offset: $datatype_offset""")
                elseif msg.msg_type == HM_FILL_VALUE_OLD
                    #(jlread(cio, UInt8) == 3 && jlread(cio, UInt8) == 0x09) || throw(UnsupportedFeatureException())
                elseif msg.msg_type == HM_FILL_VALUE
                    #(jlread(cio, UInt8) == 3 && jlread(cio, UInt8) == 0x09) || throw(UnsupportedFeatureException())
                elseif msg.msg_type == HM_DATA_LAYOUT
                    version = jlread(cio, UInt8)
                    println("""    version: $version""")
                    if version == 4 || version == 3
                        storage_type = jlread(cio, UInt8)
                        if storage_type == LC_COMPACT_STORAGE
                            data_length = jlread(cio, UInt16)
                            data_offset = position(cio)
                            println("""    type: compact storage\n    length: $length\n    offset: $(data_offset)""")
                        elseif storage_type == LC_CONTIGUOUS_STORAGE
                            rf = jlread(cio, RelOffset)
                            data_offset = rf != UNDEFINED_ADDRESS ? fileoffset(f, rf) : typemax(Int64)
                            data_length = jlread(cio, Length)
                            println("""    type: contiguous storage\n    length: $(data_length)\n    offset: $(data_offset)""")

                        elseif storage_type == LC_CHUNKED_STORAGE
                            # TODO: validate this
                            flags = jlread(cio, UInt8)
                            dimensionality = jlread(cio, UInt8)
                            dimensionality_size = jlread(cio, UInt8)
                            skip(cio, Int(dimensionality)*Int(dimensionality_size))
        
                            chunk_indexing_type = jlread(cio, UInt8)
                            chunk_indexing_type == 1 || throw(UnsupportedFeatureException("Unknown chunk indexing type"))
                            data_length = jlread(cio, Length)
                            jlread(cio, UInt32)
                            data_offset = fileoffset(f, jlread(cio, RelOffset))
                            chunked_storage = true
                            println("""    type: chunked storage
                                    length: $length
                                    offset: $(data_offset)
                                    dimensionality: $dimensionality
                                    dimensionality_size: $dimensionality_size
                                    chunk indexing type: $chunk_indexing_type""")

                        else
                            throw(UnsupportedFeatureException("Unknown data layout"))
                        end
                    end
                elseif msg.msg_type == HM_FILTER_PIPELINE
                    version = jlread(cio, UInt8)
                    version == 2 || throw(UnsupportedVersionException("Filter Pipeline Message version $version is not implemented"))
                    nfilters = jlread(cio, UInt8)
                    nfilters == 1 || throw(UnsupportedFeatureException())
                    filter_id = jlread(cio, UInt16)
                    issupported_filter(filter_id) || throw(UnsupportedFeatureException("Unknown Compression Filter $filter_id"))
                elseif msg.msg_type == HM_SYMBOL_TABLE
                    v1_btree_address = jlread(cio, RelOffset)
                    local_heap_address = jlread(cio, RelOffset)
                    println("""    required for \"old style" groups\n    v1 B-Tree Adress: $(v1_btree_address)\n    Local Heap Adress: $(local_heap_address)""")
                elseif msg.msg_type == HM_ATTRIBUTE
                    if attrs === EMPTY_READ_ATTRIBUTES
                        attrs = ReadAttribute[read_attribute(cio, f)]
                    else
                        push!(attrs, read_attribute(cio, f))
                    end
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
            seek(cio, endpos)
        end

        # Checksum
        seek(cio, chunk_end)
        if header_version == 2
            end_checksum(cio) == jlread(io, UInt32) || throw(InvalidDataException())
        end
    end
    nothing
end