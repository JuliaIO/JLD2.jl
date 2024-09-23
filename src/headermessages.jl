# This file declares the format specification for header messages.


@pseudostruct HmNil begin @skip(hsize) end

@pseudostruct HmDataspace begin
    version::UInt8 = 2
    dimensionality::UInt8 = length(kw.dimensions)
    flags::UInt8 = 0
    (version == 2) && dataspace_type::UInt8
    (version == 1) && dataspace_type::@computed(DS_V1)
    version == 1 && @skip(5)
    dim_offset::@Offset
    dimensions::NTuple{Int(dimensionality), Int64}
    isset(flags,0) && max_dimension_size::NTuple{Int(dimensionality), Int64}
end

@pseudostruct HmLinkInfo begin
    version::UInt8 = 0x00
    flags::UInt8 = 0x00 
    isset(flags,0) && max_creation_index::Int64
    fractal_heap_address::RelOffset = UNDEFINED_ADDRESS
    v2_btree_name_index::RelOffset = UNDEFINED_ADDRESS
    isset(flags, 1) && (v2_btree_creation_order_index::RelOffset = UNDEFINED_ADDRESS)
end
 
@pseudostruct HmDatatype begin
    if isset(hflags,1)
        version::UInt8 = 3
        msgtype::UInt8 = 2
        dt::SharedDatatype
        datatype_offset::@computed(dt.header_offset)
    end
    if !isset(hflags,1)
        datatype_offset::@Offset
        dt::@read(H5Datatype)
    end
end

@pseudostruct HmFillValueOld begin
    size::UInt32
    fill_value::@Blob(size)
end

@pseudostruct HmFillValue begin
    version::UInt8 = 3
    if version == 1 || version == 2
        space_allocation_time::UInt8
        fill_value_write_time::UInt8
        fill_value_defined::UInt8
        if !(version > 1 && fill_value_defined==0) 
            size::UInt32
            fill_value::@Blob(size)
        end
    end
    if version == 3
        flags::UInt8
        isset(flags, 5) && size::UInt32
        isset(flags, 5) && fill_value::@Blob(size)
    end
end

@pseudostruct HmLinkMessage begin 
    version::UInt8 = 1
    flags::UInt8 = (0x10 | size_flag(sizeof(kw.link_name)))
    isset(flags, 3) && link_type::UInt8
    isset(flags, 2) && creation_order::Int64
    isset(flags, 4) && (link_name_charset::UInt8 = CSET_UTF8)
    link_name_len::@Int(2^(flags%4)) = sizeof(kw.link_name)
    link_name::@FixedLengthString(link_name_len) # non-null-terminated
    (!isset(flags, 3) || link_type==0) && (target::RelOffset = UNDEFINED_ADDRESS)
    if isset(flags, 3) && link_type == 1
        link_info_size::UInt16
        soft_link::@Blob(link_info_size) # non-null terminated string
    end
    if isset(flags, 3) && link_type == 64
        link_info_size::UInt16
        external_link::@Blob(link_info_size) # two null-terminated strings
    end
end

@pseudostruct HmExternalFileList begin 
    version::UInt8
    @skip(3)
    allocated_slots::UInt16
    used_slots::UInt16
    heap_address::RelOffset
    external_file_list::@Blob(allocated_slots*3*8)
end

@pseudostruct HmDataLayout begin
    version::UInt8 = 4
    if version == 1 || version == 2
        dimensionality::UInt8
        layout_class::LayoutClass
        @skip(5)
        (layout_class != LcCompact) && data_address::RelOffset
        dimensions::NTuple{Int(dimensionality), Int32}
        (layout_class == LcChunked) && element_size::UInt32
        if (layout_class == LcCompact) 
            data_size::@Int(4)
            data_address::@Offset
            data::@Blob(data_size)
        end
    end
    if version == 3 || version == 4
        layout_class::LayoutClass
        if layout_class == LcCompact
            data_size::@Int(2)
            data_address::@Offset
            data::@Blob(data_size) = UInt8[] # don't write anything if nothing is passed
        end
        if layout_class == LcContiguous
            data_address::RelOffset = UNDEFINED_ADDRESS
            data_size::@Int(8) = 0# Lengths
        end
        if version == 3 && layout_class == LcChunked
            dimensionality::UInt8
            data_address::RelOffset
            dimensions::NTuple{Int(dimensionality), Int32}
            data_size::@Int(4)#UInt32#element_size::UInt32
        end
        if version == 4 && layout_class == LcChunked
            flags::UInt8
            dimensionality::UInt8 = length(kw.dimensions) 
            dim_size::UInt8 = 8 # 8 bytes per dimension
            dimensions::NTuple{Int(dimensionality), uintofsize(dim_size)}
            chunk_indexing_type::UInt8
            if chunk_indexing_type == 1 # Single Chunk
                data_size::@Int(8)#Int64 # Lengths
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
        if layout_class == LcVirtual # Virtual Storage
            data_address::RelOffset
            index::UInt32
        end
    end
end

@pseudostruct HmGroupInfo begin
    version::UInt8 = 0
    flags::UInt8 = 0b10*(get(kw, :est_num_entries, 4) != 4 || get(kw, :est_link_name_len, 8) != 8)
    if isset(flags, 0)
        link_phase_change_max_compact::UInt16
        link_phase_change_min_dense::UInt16
    end
    if isset(flags, 1)
        est_num_entries::UInt16
        est_link_name_len::UInt16
    end
end

@pseudostruct HmFilterPipeline begin
    version::UInt8
    nfilters::UInt8
    @skip(hsize-2)
end

@pseudostruct HmAttribute begin
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
    dataspace_message::@Blob(Int(dataspace_size))
    (version == 1) && @skip(8-mod1(dataspace_size,8))
    data_offset::@Offset
    #data::@Blob(hsize- (data_offset.offset - getfield(hm,:payload_offset).offset))#+getfield(hm,:base_address)+hsize-position(io))
end

@pseudostruct HmObjectHeaderContinuation begin
    continuation_offset::RelOffset
    continuation_length::Int64# Length
end

@pseudostruct HmSymbolTable begin
    v1btree_address::RelOffset
    name_index_heap::RelOffset
end

@pseudostruct HmAttributeInfo begin
    @skip(hsize)
end

@pseudostruct HmBogus begin @skip(hsize) end
@pseudostruct HmObjectComment begin @skip(hsize) end
@pseudostruct HmSharedMessageTable begin @skip(hsize) end
@pseudostruct HmModificationTime begin @skip(hsize) end
@pseudostruct HmBtreeKValues begin @skip(hsize) end
@pseudostruct HmDriverInfo begin @skip(hsize) end
@pseudostruct HmReferenceCount begin @skip(hsize) end
