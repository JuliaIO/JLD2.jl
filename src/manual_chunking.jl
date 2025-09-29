function write_chunked_array(f::JLDFile, name::String, data::AbstractArray, chunk_dims::Vector{Int})
    if !f.writable
        throw(ArgumentError("Cannot write to file opened in read-only mode"))
    end

    if length(chunk_dims) != ndims(data)
        throw(ArgumentError("chunk_dims length ($(length(chunk_dims))) must match array dimensions ($(ndims(data)))"))
    end

    if any(chunk_dims .<= 0)
        throw(ArgumentError("All chunk dimensions must be positive"))
    end

    # Validate chunk dimensions don't exceed array dimensions
    data_dims = size(data)
    if any(chunk_dims .> data_dims)
        @warn "Some chunk dimensions exceed array dimensions - chunks will be automatically clipped"
    end

    println("📦 Writing chunked array: $(size(data)) with chunks $(chunk_dims)")

    # Create write session and get data type representation
    wsession = JLDWriteSession()
    g = f.root_group
    prewrite(f)
    (g, name) = pathize(g, name, true)

    y = data
    odr = objodr(y)
    dataspace = WriteDataspace(f, y, odr)
    datatype = h5type(f, y)
    io = f.io

    datasz = (odr_sizeof(odr)::Int * numel(dataspace))::Int
    psz = payload_size_without_storage_message(dataspace, datatype)
    psz += CONTINUATION_MSG_SIZE

    filters = f.compress
    local_filters = FilterPipeline(map(filters) do filter
        Filters.set_local(filter, odr, dataspace, ())
    end)

    @assert data isa ArrayMemory
    layout_class = LcChunked
    psz += jlsizeof(Val(HmDataLayout);
        version = 3,
        layout_class,
        dimensions = fill(1, ndims(data)+1),
        # Dummy values for message size computation
        data_address = 0,
    )
    if !isempty(local_filters)
        psz += Filters.pipeline_message_size(local_filters)
    end

    fullsz = jlsizeof(ObjectStart) + size_size(psz) + psz + 4

    header_offset = f.end_of_data
    seek(io, header_offset)
    f.end_of_data = header_offset + fullsz

    cio = begin_checksum_write(io, fullsz - 4)
    jlwrite(cio, ObjectStart(size_flag(psz)))
    write_size(cio, psz)
    write_header_message(cio, Val(HmFillValue); flags=0x09)
    write_header_message(cio, Val(HmDataspace); dataspace.dataspace_type, dimensions=dataspace.size)
    for attr in dataspace.attributes
        write_header_message(cio, f, attr)
    end
    write_header_message(cio, Val(HmDatatype), 1 | (2*isa(datatype, CommittedDatatype)); dt=datatype)

    if !isempty(local_filters)
        Filters.write_filter_pipeline_message(cio, local_filters)
    end

    pos = position(cio)

    btree, total_chunk_size, num_chunks = write_chunked_dataset_with_v1btree(f, data, odr, local_filters, wsession, chunk_dims)

    seek(cio, pos)
    # Prepare DataLayout parameters
    dl_dims = UInt32.([reverse(chunk_dims)..., odr_sizeof(odr)])

    # Write DataLayout message version 3 with V1 B-tree
    write_header_message(cio, Val(HmDataLayout);
        version=3,
        layout_class,
        dimensionality=UInt8(length(chunk_dims)+1 ),
        data_address=btree.root,
        # Reversed dimensions with element size as last dim for V3 format (Int32)
        dimensions=dl_dims
    )

    write_continuation_placeholder(cio)
    jlwrite(f.io, end_checksum(cio))

    g[name] =  h5offset(f, header_offset)

    return btree
end
