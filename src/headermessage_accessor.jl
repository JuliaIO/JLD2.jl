function getprop end
function construct_hm_payload end
function sizefun end
function messageshow end

function linefun(ex)
    v = nothing
    kw = esc(:kw)
    ptr = esc(:ptr)
    offset = esc(:offset)
    if ex isa LineNumberNode
        return [ex, ex, ex, ex]
    elseif @capture(ex, cond_ && body_)
        rets = linefun(ex.args[2])
        return [ Expr(:&&, esc(cond), ret ) for ret in rets ]
    elseif @capture(ex, if cond_; body_; end )
        accs = build_fun_body((Any[], Any[], Any[], Any[]), body)
        return [ Expr(:if, esc(cond), Expr(:block, accs[i]...)) for i in 1:4 ]
    elseif @capture(ex, s_Symbol::T_) || @capture(ex, s_Symbol::T_ = v_)
        getprop_ = :($(esc(s)) = $kw.$(s))
        default = Symbol(s,"_default")
        default_ = :($default = $(esc(v)))
        get_ = :($(esc(s)) = get($kw, $(QuoteNode(s)), $default))
        haskey_ = :(haskey($kw, $(QuoteNode(s))) || throw(ArgumentError($("Argument $(QuoteNode(s)) is required"))))

        if @capture(T, @FixedLengthString(len_))
            len = esc(len)
            read_statement = :(unsafe_string($ptr+$offset, $len))
            increment = :($len)
            write_statement = :(jlwrite(io, $(esc(s))))
        elseif @capture(T, @Blob(len_))
            len = esc(len)
            read_statement = :(getfield(hm,:body)[($offset+1):($offset+$len)])
            increment = :($len)
            write_statement = :(jlwrite(io, $(esc(s))))
        elseif @capture(T, @Offset)
            read_statement = :(getfield(hm,:payload_offset) + $offset)
            increment = 0
            getprop_ = nothing
            haskey_=nothing
            write_statement = nothing
        elseif @capture(T, @computed(expr_))
            @assert isnothing(v) "Defaults for @computed fields are not supported"
            read_statement = esc(expr)
            getprop_ = :($(esc(s)) = $(esc(expr)))
            increment = 0
            write_statement = nothing
            haskey_ = nothing
        elseif @capture(T, @read(type_, rsize_)) || @capture(T, @read(type_))
            read_statement = quote
                _io = IOBuffer(getfield(hm,:body))
                seek(_io, $offset)
                jlread(_io, $(esc(type)))
            end
            write_statement = :(jlwrite(_io, $(esc(s))))
            if !isnothing(rsize)
                increment = esc(rsize)
            else
                increment = :(sizeof(typeof($(esc(s)))))
            end
        else
            T = esc(T)
            read_statement = :(unsafe_load(Ptr{$T}($ptr+$offset)))
            increment = :(sizeof($(T)))
            write_statement = :(jlwrite(io, $T($(esc(s)))))
        end

        assign_statement = :($(esc(s)) = $read_statement)
        offset_incr = :($offset += $increment)
        
        return [
            # getproperty function
            Expr(:block,
                assign_statement,
                :(s == $(QuoteNode(s)) && return $(esc(s))),
                offset_incr
                ),
            # writing function
            if !isnothing(v)
                Expr(:block, default_, get_, write_statement)
            else
                Expr(:block, haskey_, getprop_, write_statement)
            end,
            # size function
            if !isnothing(v)
                Expr(:block, default_, get_, offset_incr)
            else
                Expr(:block, haskey_, getprop_, offset_incr)
            end,
            # pretty printer
            Expr(:block, assign_statement,
                :(push!(keyvalue, $(QuoteNode(s)) => $(esc(s)))),
                offset_incr)
        ]
    elseif @capture(ex, @skip(n_))
        increment = esc(n)
        off_inc = :($offset += $increment)
        write_inc = :(write_zerobytes(io, $increment))
        return [off_inc, write_inc, off_inc, off_inc]
    elseif @capture(ex, @skiptoaligned(n_))
        increment = :(8 - mod1($offset-$(esc(n)), 8))
        off_inc = :($offset += $increment)
        write_inc = :(write_zerobytes(io, $increment))
        return [off_inc, write_inc, off_inc, off_inc]
    end
    throw(ArgumentError("Invalid field syntax: $ex"))
end

function build_fun_body(accs, blk)
    for ex in blk.args
        rets = linefun(ex)
        for i in 1:length(accs)
            push!(accs[i], rets[i])
        end
    end
    return accs
end

macro pseudostruct(name, blck)
    getprop_body, funbody, sizefun, messageshowfun = 
        build_fun_body((Any[], Any[], Any[], Any[]), blck)

    getpropfun = (quote
        function $(esc(:getprop))(::Val{$name}, $(esc(:hm))::Hmessage, s::Symbol)
            $(__source__)
            $(esc(:offset)) = 0
            $(:($(esc(:ptr)) = pointer(getfield(hm,:body))))
            $(:($(esc(:hflags)) = getfield(hm, :hflags)))
            $(:($(esc(:hsize)) = getfield(hm, :size)))
            $(getprop_body...)
            throw(ArgumentError("Field $s not found"))
        end
    end).args[2]
    
    constructfun = (quote
        function $(esc(:construct_hm_payload))(::Val{$name}, $(esc(:hflags)), $(esc(:hsize)), $(esc(:kw)))
            $(__source__)
            io = IOBuffer()
            $(funbody...)
            take!(io)
        end
    end).args[2]

    size_fun = (quote
        function $(esc(:sizefun))(::Val{$name}, $(esc(:hflags)), $(esc(:hsize)), $(esc(:kw)))
            $(__source__)
            $(esc(:offset)) = 0
            $(sizefun...)
            return $(esc(:offset))
        end
    end).args[2]

    message_show = (quote
        function $(esc(:messageshow))(::Val{$name}, $(esc(:hm))::Hmessage)
            $(__source__)
            $(esc(:offset)) = 0
            $(esc(:ptr)) = pointer(getfield(hm,:body))
            $(:($(esc(:hflags)) = getfield(hm, :hflags)))
            $(:($(esc(:hsize)) = getfield(hm, :size)))
            keyvalue = Pair{Symbol,Any}[]
            $(messageshowfun...)
            return keyvalue
        end
    end).args[2]

    return Expr(:block, getpropfun, constructfun, size_fun, message_show, nothing)
end

@pseudostruct HM_NIL begin @skip(hsize) end

@pseudostruct HM_DATASPACE begin
    version::UInt8 = 2
    dimensionality::UInt8 = length(kw.dimensions)
    flags::UInt8
    (version == 2) && dataspace_type::UInt8
    (version == 1) && dataspace_type::@computed(DS_V1)
    version == 1 && @skip(5)
    dim_offset::@Offset
    dimensions::NTuple{Int(dimensionality), Int64}
    isset(flags,0) && max_dimension_size::NTuple{Int(dimensionality), Int64}
end

@pseudostruct HM_LINK_INFO begin
    version::UInt8 = 0x00
    flags::UInt8 = 0x00 
    isset(flags,0) && max_creation_index::Int64
    fractal_heap_address::RelOffset = UNDEFINED_ADDRESS
    v2_btree_name_index::RelOffset = UNDEFINED_ADDRESS
    isset(flags, 1) && (v2_btree_creation_order_index::RelOffset = UNDEFINED_ADDRESS)
end
 
@pseudostruct HM_DATATYPE begin
    if isset(hflags,1)
        version::UInt8
        msgtype::UInt8
        dt::SharedDatatype
        datatype_offset::@computed(dt.header_offset)
    end
    if !isset(hflags,1)
        datatype_offset::@Offset
        dt::@read(H5Datatype)
    end
end

@pseudostruct HM_FILL_VALUE_OLD begin
    size::UInt32
    fill_value::NTuple{Int(size), UInt8}
end
@pseudostruct HM_FILL_VALUE begin
    @skip(hsize)
end

@pseudostruct HM_LINK_MESSAGE begin 
    version::UInt8 = 1
    flags::UInt8 = 0b10011
    isset(flags, 3) && link_type::UInt8
    isset(flags, 2) && creation_order::Int64
    isset(flags, 4) && (link_name_charset::UInt8 = CSET_UTF8)
    link_name_len::flag2uint(flags) = sizeof(kw.link_name)
    link_name::@FixedLengthString(link_name_len) # non-null-terminated
    (!isset(flags, 3) || link_type==0) && target::RelOffset
    if isset(flags, 3) && link_type == 1
        link_info_size::UInt16
        soft_link::NTuple{link_info_size, UInt8} # non-null terminated string
    end
    if isset(flags, 3) && link_type == 64
        link_info_size::UInt16
        external_link::NTuple{link_info_size, UInt8} # two null-terminated strings
    end
end

@pseudostruct HM_EXTERNAL_FILE_LIST begin 
    version::UInt8
    @skip(3)
    allocated_slots::UInt16
    used_slots::UInt16
    heap_address::RelOffset
    external_file_list::@Blob(allocated_slots*3*8)
end

@pseudostruct HM_DATA_LAYOUT begin
    version::UInt8
    if version in (1,2)
        dimensionality::UInt8
        layout_class::LayoutClass
        @skip(5)
        (layout_class != LC_COMPACT_STORAGE) && data_address::RelOffset
        dimensions::NTuple{Int(dimensionality), Int32}
        (layout_class == 2) && element_size::UInt32
        if (layout_class == LC_COMPACT_STORAGE) 
            data_size::UInt32
            data_address::@Offset
            data::@Blob(data_size)
        end
    end
    if version == 3 || version == 4
        layout_class::LayoutClass
        if layout_class == LC_COMPACT_STORAGE
            data_size::UInt16
            data_address::@Offset
            data::@Blob(data_size)
        end
        if layout_class == LC_CONTIGUOUS_STORAGE
            data_address::RelOffset
            data_size::Int64# Lengths
        end
        if version == 3 && layout_class == LC_CHUNKED_STORAGE
            dimensionality::UInt8
            data_address::RelOffset
            dimensions::NTuple{Int(dimensionality), Int32}
            data_size::UInt32#element_size::UInt32
        end
        if version == 4 && layout_class == LC_CHUNKED_STORAGE
            flags::UInt8
            dimensionality::UInt8
            dim_size::UInt8
            dimensions::NTuple{Int(dimensionality), uintofsize(dim_size)}
            chunk_indexing_type::UInt8
            if chunk_indexing_type == 1 # Single Chunk
                data_size::Int64 # Lengths
                filters::UInt32
            end
            if chunk_indexing_type == 3
                page_bits::UInt8
            end
            if chunk_indexing_type == 4
                maxbits::UInt8
                index_elements::UInt8
                minpointers::UInt8
                minelements::UInt8
                page_bits:::UInt16
            end
            if chunk_indexing_type == 5
                node_size::UInt32
                splitpercent::UInt8
                mergepercent::UInt8
            end
            data_address::RelOffset
        end
        if layout_class == LC_VIRTUAL_STORAGE # Virtual Storage
            data_address::RelOffset
            index::UInt32
        end
    end
end

function DataLayout(f::JLD2.JLDFile, msg::Hmessage)
    version = msg.version
    storage_type = msg.layout_class
    rf = msg.data_address
    data_offset = rf != UNDEFINED_ADDRESS ? fileoffset(f, rf) : typemax(Int64)
    data_length = msg.data_size
    if version == 4 || version == 3
        if storage_type == LC_COMPACT_STORAGE || storage_type == LC_CONTIGUOUS_STORAGE
            return DataLayout(version, storage_type, data_length, data_offset) 
        elseif version == 4 && storage_type == LC_CHUNKED_STORAGE
            chunk_dimensions = Int[msg.dimensions...]
            chunk_indexing_type = msg.chunk_indexing_type
            chunk_indexing_type == 1 || throw(UnsupportedFeatureException("Unknown chunk indexing type"))
            #filters = msg.filters#jlread(cio, UInt32)
            chunked_storage = true
            DataLayout(version, storage_type, data_length, data_offset, msg.dimensionality, msg.chunk_indexing_type, chunk_dimensions) 
        elseif version == 3 && storage_type == LC_CHUNKED_STORAGE
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

@pseudostruct HM_GROUP_INFO begin
    version::UInt8 = 0
    flags::UInt8 = 0
    if isset(flags, 0)
        link_phase_change_max_compact::UInt16
        link_phase_change_min_dense::UInt16
    end
    if isset(flags, 1)
        est_num_entries::UInt16
        est_link_name_len::UInt16
    end
end

@pseudostruct HM_FILTER_PIPELINE begin
    version::UInt8
    nfilters::UInt8
    @skip(hsize-1)
end

function FilterPipeline(msg::Hmessage)
    version = msg.version
    nfilters = msg.nfilters
    io = IOBuffer(msg.body)
    skip(io, 2)
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

@pseudostruct HM_ATTRIBUTE begin
    version::UInt8
    flags::UInt8
    name_size::UInt16
    datatype_size::UInt16
    dataspace_size::UInt16
    version == 3 && name_charset_encoding::UInt8
    name::@FixedLengthString(name_size-1)
    @skip(1)
    (version == 1) && @skip(8-mod1(name_size,8))
    # committed if isset(hm.flags, 0)
    if isset(flags, 0) #this is slightly more complicated in general
        vshared::UInt8
        sharedtype::UInt8
        datatype::SharedDatatype
    end
    if !isset(flags, 0)
        datatype_offset::@Offset
        datatype::@read(H5Datatype, datatype_size)
    end
    (version == 1) && @skip(8-mod1(datatype_size,8))
    dataspace_offset::@Offset
    dataspace_message::NTuple{Int(dataspace_size), UInt8}
    (version == 1) && @skip(8-mod1(dataspace_size,8))
    data_offset::@Offset
    data::@Blob(hsize-offset)
end

function read_attribute(f::JLDFile, hm::Hmessage)
    committed = hm.flags == 1
    !committed && hm.flags != 0 && throw(UnsupportedFeatureException())

    hm.hflags & 0x10 == 0x10 && @warn "We've got a shared dataspace"
    dshm = Hmessage(HM_DATASPACE, hm.dataspace_size, hm.hflags,  [hm.dataspace_message...], UNDEFINED_ADDRESS, hm.dataspace_offset)
    dataspace = ReadDataspace(f, dshm)
    data_offset = fileoffset(f, hm.data_offset)
    ReadAttribute(Symbol(hm.name), dataspace, hm.datatype, data_offset)
end

@pseudostruct HM_OBJECT_HEADER_CONTINUATION begin
    continuation_offset::RelOffset
    continuation_length::Int64# Length
end

@pseudostruct HM_SYMBOL_TABLE begin
    v1btree_address::RelOffset
    name_index_heap::RelOffset
end

function write_message(io, f::JLDFile, msg::Hmessage)
    write(io, msg.type)
    write(io, msg.size)
    write(io, msg.hflags)
    write(io, msg.body)
end

function jlwrite(io, msg::Hmessage)
    write(io, msg.type)
    write(io, msg.size)
    write(io, msg.hflags)
    write(io, msg.body)
end

function read_header_message(f, io, header_version, chunk_start, groupflags)
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
    payload_offset = h5offset(f, position(io))
    payload = jlread(io, UInt8, msg.size)
    Hmessage(
        HeaderMessageTypes(msg.msg_type),
        msg.size, msg.flags, payload,
        msgpos, payload_offset)
end


function start_obj_read(f, offset)
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
        chunk_end = position(cio) + sz
    end
    chunk = (; chunk_start, chunk_end)
    return cio, header_version, chunk, groupflags
end

function start_chunk_read(io, chunk, header_version)
    seek(io, chunk.chunk_start)
    if header_version == 2
        cio = begin_checksum_read(io)
        jlread(cio, UInt32) == OBJECT_HEADER_CONTINUATION_SIGNATURE || throw(InvalidDataException())
    else
        cio = io
    end
    return cio
end

function read_header(f, offset::RelOffset)
    io = f.io
    cio, header_version, chunk, objflags = start_obj_read(f, offset)
    msgs = Hmessage[]
    chunks = [chunk]
    chunk_number = 1

    while chunk_number <= length(chunks)
        chunk = chunks[chunk_number]
        chunk_start, chunk_end = chunk.chunk_start, chunk.chunk_end 

        if chunk_number > 1 # Don't do this the first time around
            cio = start_chunk_read(io, chunk, header_version)
            header_version == 2 && (chunk_end -= 4)
        end
        chunk_number += 1

        while position(cio) <= chunk_end-4
            msg = read_header_message(f, cio, header_version, chunk_start, objflags)
            push!(msgs, msg)
            if msg.type == HM_OBJECT_HEADER_CONTINUATION
                push!(chunks, (; chunk_start = fileoffset(f, msg.continuation_offset),
                                 chunk_end = fileoffset(f, msg.continuation_offset + msg.continuation_length)))
            end
        end
        seek(cio, chunk_end)

        if header_version == 2
            end_checksum(cio) == jlread(io, UInt32) || throw(InvalidDataException("Invalid Checksum"))
        end
    end
    return msgs, chunks
end


@pseudostruct HM_ATTRIBUTE_INFO begin
    @skip(hsize)
end