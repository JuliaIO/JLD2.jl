export extract_metadata

using AbstractTrees
# AbstractTrees.children() 
# AbstractTrees.nodevalue(t::FloatTree) = t.x;
mutable struct JLD2Tree7
    f::JLDFile
    offset::RelOffset
    name::String
    headermessages
    children
    JLD2Tree7(f::JLDFile, offset::RelOffset, name::String="") = new(f, offset, name, nothing, nothing)
end

function AbstractTrees.children(t::JLD2Tree7)
    if isnothing(t.children)
        f = t.f
        g = get_group(f, t.offset)
        t.children = map(keys(g)) do k
            offset = get_offset(g,k)
            if isgroup(f,offset)
                JLD2Tree(f, offset, k)
            else
                JLD2Object(f, offset, k)
            end
        end
    end
    t.children
end

AbstractTrees.nodevalue(t::JLD2Tree7) = t.name

mutable struct JLD2Objectv6
    f::JLDFile
    offset::RelOffset
    name::String
    datatype
    header_messages
    JLD2Objectv6(f::JLDFile, offset::RelOffset,name::String="") = new(f, offset,name, nothing, nothing) 
end
function AbstractTrees.nodevalue(o::JLD2Objectv6)
    if isnothing(o.datatype)
        load_metadata!(o)
    end
    (o.name, o.datatype)
end
JLD2Object = JLD2Objectv6
JLD2Tree = JLD2Tree7

function get_group(f::JLDFile, roffset::RelOffset)
    get!(f.loaded_groups, roffset) do 
        load_group(f, roffset)
    end
end


function extract_metadata(fname::AbstractString)
    f = jldopen(fname)
    try
        extract_metadata(f)
    finally
        close(f)
    end
end


function extract_metadata(f::JLDFile)
    tree = JLD2Tree(f.root_group_offset)
    extract_metadata!(tree, f.root_group)
    return tree
end

function extract_metadata!(data, g::Group)
    f = g.f
    for k in keys(g)
        offset = get_offset(g,k)
        if isgroup(f,offset)
            data[k] = extract_metadata!(Dict{String,Any}(), g[k])
        else
            data[k] = JLD2Object(offset)
        end
    end
    return data
end


function get_offset(g::Group, name::AbstractString)
    f = g.f
    f.n_times_opened == 0 && throw(ArgumentError("file is closed"))

    (g, name) = pathize(g, name, false)

    roffset = lookup_offset(g, name)
end


function load_metadata!(o::JLD2Objectv6)
    f = o.f
    name = o.name
    offset = o.offset
    io = f.io
    seek(io, fileoffset(f, offset))
    cio = begin_checksum_read(io)
    sz, = read_obj_start(cio)
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

    storage_type = 0xFF
    while position(cio) <= pmax-4
        msg = jlread(cio, HeaderMessage)
        endpos = position(cio) + msg.size
        if msg.msg_type == HM_DATASPACE
            dataspace = read_dataspace_message(cio)
        elseif msg.msg_type == HM_DATATYPE
            datatype_class, datatype_offset = read_datatype_message(cio, f, (msg.flags & 2) == 2)
        elseif msg.msg_type == HM_FILL_VALUE
            (jlread(cio, UInt8) == 3 && jlread(cio, UInt8) == 0x09) || throw(UnsupportedFeatureException())
        elseif msg.msg_type == HM_DATA_LAYOUT
            jlread(cio, UInt8) == 4 || throw(UnsupportedVersionException())
            storage_type = jlread(cio, UInt8)
            if storage_type == LC_COMPACT_STORAGE
                data_length = jlread(cio, UInt16)
                data_offset = position(cio)
            elseif storage_type == LC_CONTIGUOUS_STORAGE
                data_offset = fileoffset(f, jlread(cio, RelOffset))
                data_length = jlread(cio, Length)
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
            else
                throw(UnsupportedFeatureException("Unknown data layout"))
            end
        elseif msg.msg_type == HM_FILTER_PIPELINE
            version = jlread(cio, UInt8)
            version == 2 || throw(UnsupportedVersionException("Filter Pipeline Message version $version is not implemented"))
            nfilters = jlread(cio, UInt8)
            nfilters == 1 || throw(UnsupportedFeatureException())
            filter_id = jlread(cio, UInt16)
            issupported_filter(filter_id) || throw(UnsupportedFeatureException("Unknown Compression Filter $filter_id"))
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

    filter_id != 0 && !chunked_storage && throw(InvalidDataException("Compressed data must be chunked"))

    # Checksum
    end_checksum(cio) == jlread(io, UInt32) || throw(InvalidDataException("Invalid Checksum"))

    # TODO verify that data length matches
    #val = read_data(f, dataspace, datatype_class, datatype_offset, data_offset, data_length,
     #               filter_id, offset, attrs)
    #val

    #println(dataspace)
    #println(datatype_class)
    #println(datatype_offset)
    #println(data_length)
    #println(attrs)

    if datatype_class == typemax(UInt8) # Committed datatype
        typestr = stringify_committed_datatype(f, f.datatype_locations[h5offset(f, datatype_offset)])
    else
        seek(io, datatype_offset)
        @read_datatype io datatype_class dt begin
            rr = jltype(f, dt)
            typestr = string(typeof(rr).parameters[1])
            #seek(io, data_offset)
            #read_dataspace = (dataspace, NULL_REFERENCE, data_length, filter_id)
            #data = read_data(f, rr, read_dataspace, attrs)
        end
    end
    #println("Dataset \"$name\" at position: $offset")
    storage_type_str = Dict(0x00 => "Compact Storage",
                            0x01 => "Contiguous Storage",
                            0x02 => "Chunked Storage")[storage_type]
    dspace = Dict(0x00 => "Scalar", 0x01 => "Simple", 0x02 => "Null")[dataspace.dataspace_type]


    #println("Storage type: $storage_type_str")
    #println("Dataspace: $dspace")

    ndims, ndim_offset = get_ndims_offset(f, dataspace, attrs)
    seek(io, ndim_offset)
    dims = array_dims(io, ndims)
    #println("Dimensions: $dims")

    if ndims > 0
       typestr =  "Array{$typestr, $(Int(ndims))}"
    end
    #if filter_id != 0
    #    println("Compressed with $(ID_TO_DECOMPRESSOR[filter_id][2])")
    #end
    #println("Bytes on disk: $data_length")
    #= if datatype_class != typemax(UInt8) # basic datatype
        print("Value")
        print(stdout, data)
    end =#

    o.datatype = typestr
    return nothing
end
#= 

function parse_headermessages(f::JLDFile, roffset::RelOffset)
    io = f.io
    chunk_start::Int64 = fileoffset(f, roffset)
    seek(io, chunk_start)

    header_version = jlread(io, UInt8)
    if header_version == 1
        seek(io, chunk_start)
        cio = io
        sz,_,groupflags = read_obj_start(cio)
        chunk_end = position(cio) + sz
        # Skip to nearest 8byte aligned position
        skip_to_aligned!(cio, chunk_start)
    else
        header_version = 2
        seek(io, chunk_start)
        cio = begin_checksum_read(io)
        sz,_,groupflags = read_obj_start(cio)
        chunk_end = position(cio) + sz
    end
    # Messages
    chunk_end::Int64
    continuation_message_goes_here::Int64 = -1
    links = OrderedDict{String,RelOffset}()
    chunks = [(; chunk_start, chunk_end)]
    chunk_number = 0

    next_link_offset::Int64 = -1
    link_phase_change_max_compact::Int64 = -1 
    link_phase_change_min_dense::Int64 = -1
    est_num_entries::Int64 = 4
    est_link_name_len::Int64 = 8
    fractal_heap_address = UNDEFINED_ADDRESS
    name_index_btree = UNDEFINED_ADDRESS

    v1btree_address = UNDEFINED_ADDRESS
    name_index_heap = UNDEFINED_ADDRESS

    while !isempty(chunks)
        chunk = popfirst!(chunks)
        chunk_start = chunk.chunk_start
        chunk_end = chunk.chunk_end

        if chunk_number > 0
            seek(io, chunk_start)
            chunk_end -= 4
            if header_version == 2
                cio = begin_checksum_read(io)
                jlread(cio, UInt32) == OBJECT_HEADER_CONTINUATION_SIGNATURE || throw(InvalidDataException())
            end
        end
        chunk_number += 1
        while (curpos = position(cio)) < chunk_end-4
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
                    link_info = jlread(cio, LinkInfo)
                    fractal_heap_address = link_info.fractal_heap_address
                    name_index_btree = link_info.name_index_btree
                elseif msg.msg_type == HM_GROUP_INFO
                    if msg.size > 2
                        # Version Flag
                        jlread(io, UInt8) == 0 || throw(UnsupportedFeatureException()) 
                        flag = jlread(io, UInt8)
                        if flag%2 == 1 # first bit set
                            link_phase_change_max_compact = jlread(io, UInt16)
                            link_phase_change_min_dense = jlread(io, UInt16)
                        end
                        if (flag >> 1)%2 == 1 # second bit set
                            # Verify that non-default group size is given
                            est_num_entries = jlread(io, UInt16)
                            est_link_name_len = jlread(io, UInt16)
                        end
                    end
                elseif msg.msg_type == HM_LINK_MESSAGE
                    name, loffset = read_link(cio)
                    links[name] = loffset
                elseif msg.msg_type == HM_OBJECT_HEADER_CONTINUATION
                    cont_chunk_start = fileoffset(f, jlread(cio, RelOffset))
                    chunk_length = jlread(cio, Length)
                    push!(chunks, (;chunk_start=cont_chunk_start,
                                    chunk_end  =cont_chunk_start+chunk_length))
                    # For correct behaviour, empty space can only be filled in the 
                    # very last chunk. Forget about previously found empty space
                    next_link_offset = -1
                elseif msg.msg_type == HM_SYMBOL_TABLE
                    v1btree_address = jlread(cio, RelOffset)
                    name_index_heap = jlread(cio, RelOffset)                
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

    if fractal_heap_address != UNDEFINED_ADDRESS
        records = read_btree(f, fractal_heap_address, name_index_btree)
        for r in records
            links[r[1]] = r[2]
        end
    end

    if v1btree_address != UNDEFINED_ADDRESS
        records = read_oldstyle_group(f, v1btree_address, name_index_heap)
        for r in records
            links[r[1]] = r[2]
        end
    end

    Group{typeof(f)}(f, chunk_start, continuation_message_goes_here,        
                     chunk_end, next_link_offset, est_num_entries,
                     est_link_name_len,
                     OrderedDict{String,RelOffset}(), OrderedDict{String,Group}(), links)
end =#



function load_headermessages!(o::JLD2Objectv4)
    f = o.f
    roffset = o.offset
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
            Message:  $(MESSAGE_TYPES[msg.msg_type]) ($(msg.msg_type))
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
                    layout = jlread(cio, DataLayout, f)
                    @info layout
                elseif msg.msg_type == HM_FILTER_PIPELINE
                    filter_pipeline = jlread(cio, FilterPipeline)
                    @info filter_pipeline
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





############################################################################################
##                                      describe                                          ##
############################################################################################

describe(f::JLDFile, name::AbstractString) =
    describe(f.root_group, name)

function describe(g::Group, name::AbstractString)
    f = g.f
    f.n_times_opened == 0 && throw(ArgumentError("file is closed"))

    (g, name) = pathize(g, name, false)

    roffset = lookup_offset(g, name)
    if roffset == UNDEFINED_ADDRESS
        haskey(g.unwritten_child_groups, name) && return g.unwritten_child_groups[name]
        throw(KeyError(name))
    end

    if isgroup(f, roffset)
        let loaded_groups = f.loaded_groups
            get!(()->load_group(f, roffset), loaded_groups, roffset)
        end
    else
        describe_dataset(f, roffset, name)
    end
end

function describe_dataset(f::JLDFile, offset::RelOffset, name)
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

    storage_type = 0xFF
    while position(cio) <= pmax-4
        msg = jlread(cio, HeaderMessage)
        endpos = position(cio) + msg.size
        if msg.msg_type == HM_DATASPACE
            dataspace = read_dataspace_message(cio)
        elseif msg.msg_type == HM_DATATYPE
            datatype_class, datatype_offset = read_datatype_message(cio, f, (msg.flags & 2) == 2)
        elseif msg.msg_type == HM_FILL_VALUE
            (jlread(cio, UInt8) == 3 && jlread(cio, UInt8) == 0x09) || throw(UnsupportedFeatureException())
        elseif msg.msg_type == HM_DATA_LAYOUT
            jlread(cio, UInt8) == 4 || throw(UnsupportedVersionException())
            storage_type = jlread(cio, UInt8)
            if storage_type == LC_COMPACT_STORAGE
                data_length = jlread(cio, UInt16)
                data_offset = position(cio)
            elseif storage_type == LC_CONTIGUOUS_STORAGE
                data_offset = fileoffset(f, jlread(cio, RelOffset))
                data_length = jlread(cio, Length)
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
            else
                throw(UnsupportedFeatureException("Unknown data layout"))
            end
        elseif msg.msg_type == HM_FILTER_PIPELINE
            version = jlread(cio, UInt8)
            version == 2 || throw(UnsupportedVersionException("Filter Pipeline Message version $version is not implemented"))
            nfilters = jlread(cio, UInt8)
            nfilters == 1 || throw(UnsupportedFeatureException())
            filter_id = jlread(cio, UInt16)
            issupported_filter(filter_id) || throw(UnsupportedFeatureException("Unknown Compression Filter $filter_id"))
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

    filter_id != 0 && !chunked_storage && throw(InvalidDataException("Compressed data must be chunked"))

    # Checksum
    end_checksum(cio) == jlread(io, UInt32) || throw(InvalidDataException("Invalid Checksum"))

    # TODO verify that data length matches
    #val = read_data(f, dataspace, datatype_class, datatype_offset, data_offset, data_length,
     #               filter_id, offset, attrs)
    #val

    #println(dataspace)
    #println(datatype_class)
    #println(datatype_offset)
    #println(data_length)
    #println(attrs)

    if datatype_class == typemax(UInt8) # Committed datatype
        typestr = stringify_committed_datatype(f, f.datatype_locations[h5offset(f, datatype_offset)])
    else
        seek(io, datatype_offset)
        @read_datatype io datatype_class dt begin
            rr = jltype(f, dt)
            typestr = string(typeof(rr).parameters[1])
            #seek(io, data_offset)
            #read_dataspace = (dataspace, NULL_REFERENCE, data_length, filter_id)
            #data = read_data(f, rr, read_dataspace, attrs)
        end
    end
    println("Dataset \"$name\" at position: $offset")
    storage_type_str = Dict(0x00 => "Compact Storage",
                            0x01 => "Contiguous Storage",
                            0x02 => "Chunked Storage")[storage_type]
    dspace = Dict(0x00 => "Scalar", 0x01 => "Simple", 0x02 => "Null")[dataspace.dataspace_type]


    println("Storage type: $storage_type_str")
    println("Dataspace: $dspace")

    ndims, ndim_offset = get_ndims_offset(f, dataspace, attrs)
    seek(io, ndim_offset)
    dims = array_dims(io, ndims)
    println("Dimensions: $dims")

    if ndims > 0
        println("Type signature: Array{$typestr, $(Int(ndims))}")
    else
        println("Type signature: $typestr")
    end
    if filter_id != 0
        println("Compressed with $(ID_TO_DECOMPRESSOR[filter_id][2])")
    end
    println("Bytes on disk: $data_length")
    #= if datatype_class != typemax(UInt8) # basic datatype
        print("Value")
        print(stdout, data)
    end =#
    return nothing
end

function array_dims(io::IO, ndims)
    ds = tuple(reverse!(read!(io, Vector{Int64}(undef, ndims)))...)
end


export committed_datatypes
function committed_datatypes(f::JLDFile)
    if !haskey(f, "_types")
        println("No committed types")
    end
    for (n, (offset,cdt)) in enumerate(f.datatype_locations)
        # skip Datatype
        #= if cdt.index == 1
            continue
        end =#
        print("$n) ")
        typestr = stringify_committed_datatype(f, cdt; showfields=true)
        #println("$n)\n$typestr")
        println(typestr)
    end
end

function stringify_committed_datatype(f, cdt; showfields=false)
    io = f.io
    dt, attrs = read_shared_datatype(f, cdt)
    written_type_str = ""
    julia_type_str = ""
    for attr in attrs
        if !(attr.name == :julia_type || attr.name == :written_type)
            continue
        end
        datatype_offset = attr.datatype_offset
        data_offset = attr.data_offset    
        rr = jltype(f, f.datatype_locations[h5offset(f, datatype_offset)])
        @assert attr.dataspace.dataspace_type == DS_SCALAR
        seek(io, data_offset)
        str = jlconvert_string(rr, f, io.curptr)
        if attr.name == :written_type
            written_type_str = str
        elseif attr.name ==:julia_type
            julia_type_str = str
        end
    end

            
#=     end
    attr = first(attrs)
    datatype_offset = attr.datatype_offset
    data_offset = attr.data_offset
    
    rr = jltype(f, f.datatype_locations[h5offset(f, datatype_offset)])
    @assert attr.dataspace.dataspace_type == DS_SCALAR
    seek(io, data_offset)
    typestr = jlconvert_string(rr, f, io.curptr)
 =#
    #@show typestr
    if !showfields ||
        dt isa BasicDatatype ||
        dt isa VariableLengthDatatype ||
        startswith(julia_type_str, r"Tuple|Union") ||
        julia_type_str == "DataType"
        return julia_type_str
    end
    typestr = "struct "*julia_type_str
    if !isempty(written_type_str)
        typestr = "julia type: $typestr\n"*
        "written type:\n"*"struct $written_type_str"
    end
    field_datatypes = read_field_datatypes(f, attrs)
    #rodr = reconstruct_odr(f, dt, field_datatypes)
    #types = typeof(rodr).parameters[2].parameters
    # Get the type and ODR information for each field
    #types = Vector{Any}(undef, length(dt.names))
    #h5types = Vector{Any}(undef, length(dt.names))
    for i = 1:length(dt.names)
        if !isempty(field_datatypes) && (ref = field_datatypes[i]) != NULL_REFERENCE
            #dtrr = jltype(f, f.datatype_locations[ref])
            fieldtype = stringify_committed_datatype(f, f.datatype_locations[ref])
        else
            # These are normal julia types
            #@show dt.members[i]
            dtrr = jltype(f, dt.members[i])
            fieldtype = string(typeof(dtrr).parameters[1])
            #field = stringify_committed_datatype(f, dt.members[i])

        end
        #types[i], h5types[i] = typeof(dtrr).parameters
        #println(dt.names[i],"::",field)
        typestr *= "\n\t"*"$(dt.names[i])::$(fieldtype)"
    end
    #if length(dt.names) > 0

    typestr = typestr*"\n"*"end"
    #end
    #@show field_datatypes
    #@show rodr
    #@show types
    
#=     structexpr =Expr(:struct, false, reconname,
                   Expr(:block, Any[ Expr(Symbol("::"), dt.names[i], types[i]) for i = 1:length(types) ]...,
                        # suppress default constructors, plus a bogus `new()` call to make sure
                        # ninitialized is zero.
                        Expr(:if, false, Expr(:call, :new))))
    @info structexpr
 =#    
    return typestr  
end

function stringify_object(f, offset)
    io = f.io
    seek(io, fileoffset(f, offset))
    cio = begin_checksum_read(io)
    sz, = read_obj_start(cio)
    pmax = position(cio) + sz

    # Messages
    dataspace = ReadDataspace()
    attrs = EMPTY_READ_ATTRIBUTES
    datatype_class::UInt8 = 0
    datatype_offset::Int64 = 0
    data_offset::Int64 = 0
    data_length::Int = -1
    layout::DataLayout = DataLayout(0,0,0,-1)
    chunked_storage::Bool = false
    filter_pipeline::FilterPipeline = FilterPipeline(Filter[])
    while position(cio) <= pmax-4
        msg = jlread(cio, HeaderMessage)
        endpos = position(cio) + msg.size
        if msg.msg_type == HM_DATASPACE
            dataspace = read_dataspace_message(cio)
        elseif msg.msg_type == HM_DATATYPE
            datatype_class, datatype_offset = read_datatype_message(cio, f, (msg.flags & 2) == 2)
        elseif msg.msg_type == HM_FILL_VALUE
            (jlread(cio, UInt8) == 3 && jlread(cio, UInt8) == 0x09) || throw(UnsupportedFeatureException())
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
        elseif (msg.flags & 2^3) != 0
            throw(UnsupportedFeatureException())
        end
        seek(cio, endpos)
    end
    seek(cio, pmax)

    iscompressed(filter_pipeline) && !ischunked(layout) && throw(InvalidDataException("Compressed data must be chunked"))

    # Checksum
    end_checksum(cio) == jlread(io, UInt32) || throw(InvalidDataException("Invalid Checksum"))


    # TODO verify that data length matches
    #val = read_data(f, dataspace, datatype_class, datatype_offset, data_offset, data_length,
    #                filter_id, offset, attrs)
    # An excerpt from the commented function 

    if datatype_class == typemax(UInt8) # Committed datatype
        rr = jltype(f, f.datatype_locations[h5offset(f, datatype_offset)])
        seek(io, data_offset)
        typestr = jlconvert_string(rr, f, io.curptr)
    else
        seek(io, datatype_offset)
        @read_datatype io datatype_class dt begin
            rr = jltype(f, dt)
            seek(io, data_offset)
            read_dataspace = (dataspace, NULL_REFERENCE, layout, filter_pipeline)
            res = read_data(f, rr, read_dataspace, nothing)
            string(res)
        end
    end    
end

function typestring_from_refs(f::JLDFile, ptr::Ptr)
    # Test for a potential null pointer indicating an empty array
    isinit = jlunsafe_load(convert(Ptr{UInt32}, ptr)) != 0
    if isinit
        refs = jlconvert(ReadRepresentation{RelOffset, Vlen{RelOffset}}(), f, ptr, NULL_REFERENCE)
        #println("datatypes at refs $(Int.(getproperty.(refs,:offset)))")
        params =  Any[let
            # If the reference is to a committed datatype, read the datatype
            nulldt = CommittedDatatype(UNDEFINED_ADDRESS, 0)
            cdt = get(f.datatype_locations, ref, nulldt)
            res = if cdt !== nulldt 
                stringify_committed_datatype(f, cdt)
            else
                stringify_object(f, ref)
            end
            if startswith(res, r"Core|Main")
                res = last(split(res, "."; limit=2))
            end
            res
        end for ref in refs]
        return params
    end
    return []
end

function jlconvert_string(rr::ReadRepresentation{T,DataTypeODR()},
                        f::JLDFile,
                        ptr::Ptr) where T
    mypath = String(jlconvert(ReadRepresentation{UInt8,Vlen{UInt8}}(), f, ptr, NULL_REFERENCE))
    println(mypath)
    params = typestring_from_refs(f, ptr+odr_sizeof(Vlen{UInt8}))
    if startswith(mypath, r"Core|Main")
        mypath = last(split(mypath, "."; limit=2))
    end

    if !isempty(params)
        return mypath*"{"*interleave(params, ",")*"}"
    else
        return mypath
    end
end

function jlconvert_string(::ReadRepresentation{Union, UnionTypeODR()}, f::JLDFile,
    ptr::Ptr)#, header_offset::RelOffset)
    # Skip union type description in the beginning
    ptr += odr_sizeof(Vlen{String})
    # Reconstruct a Union by reading a list of DataTypes and UnionAlls
    # Lookup of RelOffsets is taken from jlconvert of DataTypes
    datatypes = typestring_from_refs(f, ptr)
    unionalls = typestring_from_refs(f, ptr+odr_sizeof(Vlen{RelOffset}))
    "Union{"*interleave(vcat(datatypes, unionalls), ",")*"}"
    #= v = Union{datatypes..., unionalls...}
    track_weakref!(f, header_offset, v)
    v =#
end


function interleave(strs, int)
    if isempty(strs)
        return ""
    elseif length(strs) == 1
        return only(strs)
    end
    prod(str*int*" " for str in strs[1:end-1]) * last(strs)
end