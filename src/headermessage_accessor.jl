using MacroTools

function getprop end
function construct_hm_payload end


macro pseudostruct(name, blck)
    fields = Symbol[]
    types = []
    present = []
    kwdefs = []
    increments = Any[0]
    for ex in blck.args
        ex isa LineNumberNode && continue
        if @capture(ex, s_Symbol::T_)
            push!(fields, s)
            push!(types, T)
            push!(present, :(true))
            push!(increments, :(sizeof($T)))
        elseif @capture(ex, s_Symbol::T_ = v_)
            push!(fields, s)
            push!(types, T)
            push!(present, :(true))
            push!(kwdefs, Expr(:kw, s, v))
            push!(increments, :(sizeof($T)))
        elseif @capture(ex, skip(n_))
            increments[end] = :($(increments[end])+$n)

        elseif @capture(ex, cond_ && s_Symbol::T_)
            push!(fields, s)
            push!(types, T)
            push!(present, cond)
            push!(increments, :(sizeof($T)))
        elseif @capture(ex, cond_ && s_Symbol::T_ = v_)
            push!(fields, s)
            push!(types, T)
            push!(present, cond)
            push!(kwdefs, Expr(:kw, s, v))
            push!(increments, :(sizeof($T)))
        else
            throw(ArgumentError("Invalid field syntax: $ex"))
        end
    end
    # assemble accessor function 
    getprop_body = [:(offset=0)]
    for i in eachindex(fields)
        push!(getprop_body, :(offset += $(increments[i])),
        Expr(:if, :(s == $(QuoteNode(fields[i]))), Expr(:block,
            !(present[i]==true) ? :( $(present[i]) || throw(ArgumentError("Field not present"))) : nothing,
            :(return unsafe_load(Ptr{$(types[i])}(pointer(hm.body)+offset)))))
        )
    end

    getpropfun = (quote
        function $(esc(:getprop))(::Val{$name}, hm::Hmessage, s::Symbol)
            $(__source__)
            $(getprop_body...)
            throw(ArgumentError("Field $s not found"))
        end
    end).args[2]
    # assemble constructor function
    funbody = [ :(hm = (; $(kwdefs...), kwargs...)), :(body = IOBuffer()), :(offset=0)]

    for i in eachindex(fields)
        push!(funbody, 
            :(offset += $(increments[i])),
            :(offset > position(body) && jlwrite(body, zeros(UInt8, offset-position(body)))))
        if present[i] == true
            # is a required field
            push!(funbody, 
                :(jlwrite(body, $(types[i])(hm.$(fields[i]))))
                )
        else
            # is an optional field
            push!(funbody, 
                :($(present[i]) && jlwrite(body, $(types[i])(hm.$(fields[i]))))
                )
        end
    end
    push!(funbody, 
        :(hsize > position(body) && jlwrite(body, zeros(UInt8, hsize-position(body)))))

    constructorfun = Expr(:function, :($(esc(:construct_hm_payload))(::Val{$name}, hflags, hsize; kwargs...)), 
        Expr(:block, __source__, funbody..., :(return take!(body))))
    
    return Expr(:block, getpropfun, constructorfun, nothing)
end


@pseudostruct HM_NIL begin end

@pseudostruct HM_DATASPACE begin
    version::UInt8 = 1
    dimensionality::UInt8
    flags::UInt8
    (hm.version == 2) && dataspace_type::UInt8
    skip(4*(hm.version == 1))
    dimension_size::NTuple{Int(hm.dimensionality), Int64}
    (hm.flags & 0b01 == 0b01) && max_dimension_size::NTuple{Int(hm.dimensionality), Int64}
end
function ReadDataspace(f, msg::Hmessage)
    v = msg.version
    ReadDataspace(v == 2 ? msg.dataspace_type : DS_V1, 
        msg.dimensionality, 
        fileoffset(f, msg.offset) + 3 + (v==2) + 4*(v==1))
end

@pseudostruct HM_LINK_INFO begin
    version::UInt8 = 0x00
    # zero-bit: creation order tracked, 1-bit: creation order is indexed
    flags::UInt8 = 0x00 
    (hm.flags & 0b01 == 0b01) && max_creation_index::Int64
    fractal_heap_address::RelOffset = UNDEFINED_ADDRESS
    v2_btree_name_index::RelOffset = UNDEFINED_ADDRESS
    (hm.flags & 0b10 == 0b10) && v2_btree_creation_order_index::RelOffset = UNDEFINED_ADDRESS
end

@pseudostruct HM_DATATYPE begin
    tc::UInt8 # typeclass (low four bits) and version (upper four bits)
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
    skip(hsize-1)
end

# Reads a datatype message and returns a (offset::RelOffset, class::UInt8)
# tuple. If the datatype is committed, the offset is the offset of the
# committed datatype and the class is typemax(UInt8). Otherwise, the
# offset is the offset of the datatype in the file, and the class is
# the corresponding datatype class.
function datatype_from_message(f::JLDFile, msg)
    committed = msg.hflags & 2 == 2
    if committed
        version = msg.body[1]
        msgtype = msg.body[2]
        # supported combinations are 
        (version == 3 && msgtype == 2) || (version == 2) || throw(UnsupportedVersionException("Unsupported shared message"))
        (typemax(UInt8), Int(fileoffset(f, jlunsafe_load(Ptr{RelOffset}(pointer(msg.body, 2))))))
    else
        (msg.tc, Int(fileoffset(f, msg.offset + 4)))
    end
end

@pseudostruct HM_FILL_VALUE_OLD begin
    size::UInt32
    fill_value::NTuple{Int(hm.size), UInt8}
end
@pseudostruct HM_FILL_VALUE begin
    skip(hsize)
end
# can't access anything here
@pseudostruct HM_LINK_MESSAGE begin 
    version::UInt8 = 1
    flags::UInt8
    # link_type::UInt8
    # creation_order::Int64
    # link_name_charset::UInt8
    # link_name_len::UInt8
    # link_name::String
    # link_info::
    skip(hsize-2)
end
function read_link(msg::Hmessage)
    io = IOBuffer(msg.body)
    skip(io, 2)
    # Version
    version = msg.version
    version == 1 || throw(UnsupportedVersionException())

    # Flags
    flags = msg.flags

    if (flags & LM_LINK_TYPE_FIELD_PRESENT) != 0
        jlread(io, UInt8) == 0 || throw(UnsupportedFeatureException())
    end

    if (flags & LM_CREATION_ORDER_PRESENT) != 0
        skip(io, 8)
    end

    # Link name character set
    cset = CSET_ASCII
    if (flags & LM_LINK_NAME_CHARACTER_SET_FIELD_PRESENT) != 0
        cset_byte = jlread(io, UInt8)
        cset = CharacterSet(cset_byte)
    end

    sz = read_size(io, flags)  # Size
    name = jlread(io, UInt8, sz) # Link name
    target = jlread(io, RelOffset)  # Link information

    (String(name), target)
end

# not implemented yet
@pseudostruct HM_EXTERNAL_FILE_LIST begin end

@pseudostruct HM_DATA_LAYOUT begin
    version::UInt8
    # this is incomplete
    skip(hsize-1)
    # Version 1 and 2
    # dimensionality::UInt8
    # layout_class::UInt8
    # skip(5)
    # address::RelOffset
    # dimensions::NTuple{Int(hm.dimensionality), Int32}

    # # Version 3 & 4
    # version::UInt8
    # layout_class::UInt8
    # # Compact 
end

function DataLayout(f::JLD2.JLDFile, msg::Hmessage)
    cio = IOBuffer(msg.body)
    version = jlread(cio, UInt8)#msg.version
    if version == 4 || version == 3
        storage_type = jlread(cio, UInt8)
        if storage_type == LC_COMPACT_STORAGE
            data_length = jlread(cio, UInt16)
            data_offset =  msg.offset+4 + position(cio)
            data_offset_int = fileoffset(f, data_offset)
            return DataLayout(version, storage_type, data_length, data_offset_int) 
        elseif storage_type == LC_CONTIGUOUS_STORAGE
            rf = jlread(cio, RelOffset)
            data_offset = rf != UNDEFINED_ADDRESS ? fileoffset(f, rf) : typemax(Int64)
            data_length = jlread(cio, Length)
            DataLayout(version, storage_type, data_length, data_offset) 
        elseif version == 4 && storage_type == LC_CHUNKED_STORAGE
            # TODO: validate this
            flags = jlread(cio, UInt8)
            dimensionality = jlread(cio, UInt8)
            dimensionality_size = jlread(cio, UInt8)
            #skip(cio, Int(dimensionality)*Int(dimensionality_size))
            chunk_dimensions = [read_nb_uint(cio, dimensionality_size) for _=1:dimensionality]
            chunk_indexing_type = jlread(cio, UInt8)
            chunk_indexing_type == 1 || throw(UnsupportedFeatureException("Unknown chunk indexing type"))
            data_length = jlread(cio, Length)
            jlread(cio, UInt32)
            data_offset = fileoffset(f, jlread(cio, RelOffset))
            chunked_storage = true
            DataLayout(version, storage_type, data_length, data_offset, dimensionality, chunk_indexing_type, chunk_dimensions) 

        elseif version == 3 && storage_type == LC_CHUNKED_STORAGE
            dimensionality = jlread(cio, UInt8)
            rf = jlread(cio, RelOffset)
            data_offset = rf != UNDEFINED_ADDRESS ? fileoffset(f, rf) : typemax(Int64)
            chunk_dimensions = jlread(cio, UInt32, dimensionality-1)
            data_length = jlread(cio, UInt32)
            chunked_storage = true
            DataLayout(version, storage_type, data_length, data_offset, dimensionality, 0, chunk_dimensions) 
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
    skip(hsize-1)
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
    # version::UInt8
    # # zero bit: shared datatype if set
    # # first bit: shared dataspace if set
    # flags::UInt8
    # name_size::UInt16 # includes null terminator
    # datatype_size::UInt16
    # dataspace_size::UInt16
    # # Character encoding: 0x00: ASCII, 0x01: UTF-8
    # (hm.version & 0x03==0x03) && name_encoding::UInt8
    # name::String # null-terminated, not padded
    # #(flags & 0x01 == 0x01) && datatype::SharedDatatypeMessage
    # #(flags ⊻ 0x00 == 0x00) && datatype::SharedDatatypeMessage
    # datatype::DataType # datatype size 
    # dataspace::DataSpace
    skip(hsize)    
end

function read_attribute(f::JLDFile, hm::Hmessage)
    io = IOBuffer(hm.body)
    fio = f.io
    ah = jlread(io, AttributeHeader)
    if ah.version == 1
        committed = false
        name = Symbol(jlread(io, UInt8, ah.name_size-1))
        jlread(io, UInt8) == 0 || throw(InvalidDataException())
        skip_to_aligned!(io, pos)

        datatype_end = hm.offset + position(io) + ah.datatype_size
        datatype_class, datatype_offset = read_datatype_message(io, f, committed)
        seek(io, datatype_end)
        skip_to_aligned!(io, pos)


        dataspace_end = hm.offset + position(io) + ah.dataspace_size
        dataspace = read_dataspace_message(io)
        seek(io, dataspace_end)
        skip_to_aligned!(io, pos)

        ReadAttribute(name, dataspace, datatype_class, datatype_offset, fileoffset(f, hm.offset) + position(io))
    elseif ah.version == 2 || ah.version == 3
        committed = ah.flags == 1
        !committed && ah.flags != 0 && throw(UnsupportedFeatureException())

        if ah.version == 3
            name_charset_encoding = jlread(io, UInt8)
        end

        name = Symbol(jlread(io, UInt8, ah.name_size-1))
        jlread(io, UInt8) == 0 || throw(InvalidDataException())

        pos=position(io)
        dthm = Hmessage(HM_DATATYPE, ah.datatype_size, hm.hflags,
            hm.body[pos+1:pos+ah.datatype_size], hm.offset+(pos))
        datatype_class, datatype_offset = datatype_from_message(f, dthm)
        pos += ah.datatype_size

        dshm = Hmessage(HM_DATASPACE, ah.dataspace_size, hm.hflags,  hm.body[pos+1:pos+ah.dataspace_size], hm.offset+4+pos)
        dataspace = ReadDataspace(f, dshm)
        pos += ah.dataspace_size
        data_offset = fileoffset(f, hm.offset) + jlsizeof(HeaderMessage) + pos
        ReadAttribute(name, dataspace, datatype_class, datatype_offset, data_offset)
    else
        throw(UnsupportedVersionException("Unknown Attribute Header Version $(ah.version)"))
    end
end

@pseudostruct HM_OBJECT_HEADER_CONTINUATION begin
    continuation_offset::RelOffset
    continuation_length::Int64# Length
end

@pseudostruct HM_GROUP_INFO begin
    version::UInt8 = 0
    flags::UInt8 = 0
    (hm.flags & 0b01==0b01) && link_phase_change_max_compact::UInt16
    (hm.flags & 0b01==0b01) && link_phase_change_min_dense::UInt16
    (hm.flags & 0b10==0b10) && est_num_entries::UInt16
    (hm.flags & 0b10==0b10) && est_link_name_len::UInt16

end

function read_header_message(f, io, header_version, chunk_start, groupflags)
    msgpos = h5offset(f, position(io))
    if header_version == 1
        # Message start 8byte aligned relative to object start
        skip_to_aligned!(io, chunk_start)
        # Version 1 header message is padded
        msg = jlread(io, HeaderMessage)
        skip(io, 3)
    else # header_version == 2
        msg = jlread(io, HeaderMessage)
        (groupflags & 4) == 4 && skip(io, 2) 
    end
    payload = read(io, UInt8, msg.size)
    Hmessage(
        HeaderMessageTypes(msg.msg_type),
        msg.size, msg.flags, payload,
        msgpos)
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
        (; chunk_start, chunk_end) = chunk = chunks[chunk_number]

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
            # Checksum
            end_checksum(cio) == jlread(io, UInt32) || throw(InvalidDataException("Invalid Checksum"))
        end
    end
    return msgs, chunks
end
