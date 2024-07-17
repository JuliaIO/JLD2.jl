using MacroTools

function getprop end
function construct_hm_payload end

function isset(flag, bit)
    #return !iszero(flag & UInt8(2^(bit-1)))
    return Bool(flag >> (bit) & 1)
end

function flag2uint(flag)
    # use lowest two bits
    value = flag << 6 >> 6
    if value == 0
        UInt8
    elseif value == 1
        UInt16
    elseif value == 2
        UInt32
    else
        UInt64
    end
end

function build_lines(ex)
    ptr = esc(:ptr)
    if ex isa LineNumberNode
        return ex
    elseif @capture(ex, cond_ && body_)# || @capture(ex, cond_ && body_::T_)
        cond = esc(cond)
        return Expr(:&&, cond, build_lines(ex.args[2]))
    elseif @capture(ex, s_Symbol::T_) || @capture(ex, s_Symbol::T_ = v_)
        if @capture(T, @FixedLengthString(len_))
            len = esc(len)
            read_statement = :(unsafe_string($ptr+offset, $len))
            increment = :($len)
        elseif @capture(T, @Blob(len_))
            len = esc(len)
            read_statement = :(getfield(hm,:body)[(offset+1):(offset+1+$len)])
            increment = :($len)
        elseif @capture(T, @Offset)
            read_statement = :(getfield(hm,:payload_offset) + offset)
            increment = 0
        else
            T = esc(T)
            read_statement = :(unsafe_load(Ptr{$T}($ptr+offset)))
            increment = :(sizeof($(T)))
        end

        assign_statement = :($(esc(s)) = $read_statement)
        bl = Expr(:block,
            assign_statement,
            :(s == $(QuoteNode(s)) && return $(esc(s))),
            :(offset += $increment)
            )
        return bl
    elseif @capture(ex, @skip(n_))
        return :(offset += $(esc(n)))
    elseif @capture(ex, @skiptoaligned(n_))
        return :(offset += 8 - mod1(offset-$(esc(n)), 8))
    elseif @capture(ex, if cond_; body_; end )
        cond = esc(cond)
        return Expr(:if, cond, Expr(:block, build_accessor!([], body)...))
    else
        throw(ArgumentError("Invalid field syntax: $ex"))
    end
end

function build_accessor!(getprop_body, blk)
    for ex in blk.args
        push!(getprop_body, build_lines(ex))
    end
    return getprop_body
end

macro pseudostruct(name, blck)
    getprop_body = Any[:(offset=0)]

    build_accessor!(getprop_body, blck)

    getpropfun = (quote
        function $(esc(:getprop))(::Val{$name}, $(esc(:hm))::Hmessage, s::Symbol)
            $(__source__)
            $(:($(esc(:ptr)) = pointer(getfield(hm,:body))))
            $(:($(esc(:hflags)) = getfield(hm, :hflags)))
            $(getprop_body...)
            throw(ArgumentError("Field $s not found"))
        end
    end).args[2]
    
    return Expr(:block, getpropfun, nothing)
end


@pseudostruct HM_NIL begin end
construct_hm_payload(::Val{HM_NIL}, hflags, size) = zeros(UInt8, size)

@pseudostruct HM_DATASPACE begin
    version::UInt8 = 1
    dimensionality::UInt8
    flags::UInt8
    (version == 2) && dataspace_type::UInt8
    version == 1 && @skip(5)
    dim_offset::@Offset
    dimension_size::NTuple{Int(dimensionality), Int64}
    isset(flags,0) && max_dimension_size::NTuple{Int(dimensionality), Int64}
end

function ReadDataspace(f, msg::Hmessage)
    v = msg.version
    ReadDataspace(v == 2 ? msg.dataspace_type : DS_V1, 
        msg.dimensionality, 
        fileoffset(f, msg.dim_offset))
end

@pseudostruct HM_LINK_INFO begin
    version::UInt8 = 0x00
    # zero-bit: creation order tracked, 1-bit: creation order is indexed
    flags::UInt8 = 0x00 
    isset(flags,0) && max_creation_index::Int64
    fractal_heap_address::RelOffset = UNDEFINED_ADDRESS
    v2_btree_name_index::RelOffset = UNDEFINED_ADDRESS
    isset(flags, 1) && (v2_btree_creation_order_index::RelOffset = UNDEFINED_ADDRESS)
end

#@MacroTools.expand 
@pseudostruct HM_DATATYPE begin
    if isset(hflags, 1) # shared message
        version::UInt8
        msgtype::UInt8
        datatype_offset::RelOffset
    end
    if !isset(hflags, 1)
        datatype_offset::@Offset
        tc::UInt8 # typeclass (low four bits) and version (upper four bits)
    end
    # class1::UInt8
    # class2::UInt8
    # class3::UInt8
    # size::UInt32
    # # Properties
    # # Class 0: Fixed-point
    # # Class 1: Floating-point
    # # Class 4: Bitfield Types
    # (hm.tc in (0, 1, 4,)) && bitoffset::UInt16
    # (hm.tc in (0, 1, 4,)) && bitprecision::UInt16
    # # further fields
    # # Class 3: String (but no properties)
    # # Class 5: Opaque
    # # Class 6: Compound
    # (hm.tc == 6) && name::String # null-terminated
    #skip to aligned
    # Class 8: Enumeration
    # Class 9: Variable Length Types
    #@skip(hsize-1)
end

# Reads a datatype message and returns a (offset::RelOffset, class::UInt8)
# tuple. If the datatype is committed, the offset is the offset of the
# committed datatype and the class is typemax(UInt8). Otherwise, the
# offset is the offset of the datatype in the file, and the class is
# the corresponding datatype class.
function datatype_from_message(f::JLDFile, msg)
    committed = msg.hflags & 2 == 2
    if committed
        version = msg.version
        msgtype = msg.msgtype
        # supported combinations are 
        (version == 3 && msgtype == 2) || (version == 2) || throw(UnsupportedVersionException("Unsupported shared message"))
        tc = typemax(UInt8)
    else
        tc = msg.tc
    end
    (tc, Int(fileoffset(f, msg.datatype_offset)))
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
    flags::UInt8
    isset(flags, 3) && link_type::UInt8
    isset(flags, 2) && creation_order::Int64
    isset(flags, 4) && link_name_charset::UInt8
    link_name_len::flag2uint(flags)
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
        layout_class::UInt8
        @skip(5)
        (layout_class != 0) && data_address::RelOffset
        dimensions::NTuple{Int(dimensionality), Int32}
        (layout_class == 2) && element_size::UInt32
        if (layout_class == 0) 
            data_size::UInt32
            data_address::@Offset
            data::@Blob(data_size)
        end
    end
    if version == 3 || version == 4
        layout_class::UInt8
        if layout_class == 0 # Compact Storage
            data_size::UInt16
            data_address::@Offset
            data::@Blob(data_size)
        end
        if layout_class == 1
            data_address::RelOffset
            data_size::Int64# Lengths
        end
        if version == 3 && layout_class == 2
            dimensionality::UInt8
            data_address::RelOffset
            dimensions::NTuple{Int(dimensionality), Int32}
            data_size::UInt32#element_size::UInt32
        end
        if version == 4 && layout_class == 2
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
        if layout_class == 3 # Virtual Storage
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
    (version == 1) && @skiptoaligned(0)

    # committed if isset(hm.flags, 0)
    if isset(flags, 0) #this is slightly more complicated in general
        vshared::UInt8
        sharedtype::UInt8
        datatype_offset::RelOffset
    end
    if !isset(flags, 0)
        datatype_offset::@Offset
        datatype_message::NTuple{Int(datatype_size), UInt8}
    end
    (version == 1) && @skiptoaligned(0)
    dataspace_offset::@Offset
    dataspace_message::NTuple{Int(dataspace_size), UInt8}
    (version == 1) && @skiptoaligned(0)
    data_offset::@Offset
    data::@Blob(hsize-offset)
end

function read_attribute(f::JLDFile, hm::Hmessage)
    committed = hm.flags == 1
    !committed && hm.flags != 0 && throw(UnsupportedFeatureException())

    name = hm.name
    datatype_class = committed ? typemax(UInt8) : hm.datatype_message[1]
    datatype_offset = fileoffset(f, hm.datatype_offset)

    hm.hflags & 0x10 == 0x10 && @warn "We've got a shared dataspace"
    dshm = Hmessage(HM_DATASPACE, hm.dataspace_size, hm.hflags,  [hm.dataspace_message...], UNDEFINED_ADDRESS, hm.dataspace_offset)
    dataspace = ReadDataspace(f, dshm)
    data_offset = fileoffset(f, hm.data_offset)
    ReadAttribute(Symbol(name), dataspace, datatype_class, datatype_offset, data_offset)
end

@pseudostruct HM_OBJECT_HEADER_CONTINUATION begin
    continuation_offset::RelOffset
    continuation_length::Int64# Length
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

function write_message(io, f::JLDFile, msg::Hmessage)
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


@pseudostruct HM_SYMBOL_TABLE begin
    v1btree_address::RelOffset
    name_index_heap::RelOffset
end