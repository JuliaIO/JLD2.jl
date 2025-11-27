"""
    ChunkIndexMetadata

Metadata returned by chunk index writers for use by the unified header writer.
"""
struct ChunkIndexMetadata
    data_address::RelOffset
    layout_version::UInt8
    chunk_indexing_type::Union{UInt8,Nothing}
    layout_params::NamedTuple
    requires_maxshape::Bool
    requires_fill_value::Bool
    fill_value_params::Union{NamedTuple,Nothing}
end
"""
    write_single_chunk_index(f::JLDFile, data::AbstractArray{T,N}, chunks, odr, filter_pipeline, wsession) where {T,N}

Write chunks using Single Chunk indexing and return metadata.

Returns `ChunkIndexMetadata` for use by `write_chunked_dataset_object`.
"""
function write_single_chunk_index(f::JLDFile, data::AbstractArray{T,N}, chunks, odr, filter_pipeline, wsession) where {T,N}
    @assert chunks == size(data) "Single chunk requires chunks == size(data)"

    chunk_data, chunk_size = if !iscompressed(filter_pipeline)
        io_buf = IOBuffer()
        JLD2.write_data(io_buf, f, data, odr, JLD2.datamode(odr), wsession)
        raw_data = take!(io_buf)
        (raw_data, sizeof(raw_data))
    else
        compressed, retcodes = Filters.compress(filter_pipeline, data, odr, f, wsession)
        (compressed, sizeof(compressed))
    end

    chunk_offset = f.end_of_data
    seek(f.io, chunk_offset)
    write(f.io, chunk_data)
    f.end_of_data = chunk_offset + chunk_size

    layout_params = (
        flags = iscompressed(filter_pipeline) ? UInt8(0x02) : UInt8(0x00),
        data_size = chunk_size,
        filters = UInt32(0)
    )

    return ChunkIndexMetadata(
        h5offset(f, chunk_offset),
        UInt8(4),
        UInt8(1),
        layout_params,
        false,
        false,
        nothing
    )
end
"""
    write_implicit_index(f::JLDFile, data::AbstractArray{T,N}, chunks, fill_value, odr, filter_pipeline, wsession) where {T,N}

Write chunks using Implicit Index and return metadata.

Implicit Index stores chunks contiguously in the file starting at a base address.
No explicit index structure is needed - chunk addresses are calculated as:
    chunk_address = base_address + (chunk_index × chunk_size_bytes)

This is the simplest chunk indexing type and is most memory-efficient for sparse datasets
where a fill value is used for unallocated chunks.

# Validation
- Requires fill_value (HDF5 format requirement)
- Does not support filters/compression (HDF5 format requirement)

Returns `ChunkIndexMetadata` for use by `write_chunked_dataset_object`.
"""
function write_implicit_index(f::JLDFile, data::AbstractArray{T,N}, chunks, fill_value, odr, filter_pipeline, wsession) where {T,N}
    if isnothing(fill_value)
        throw(ArgumentError(
            "Implicit index (Type 2) requires a fill_value. " *
            "Please specify fill_value in WriteChunkedArray constructor."
        ))
    end

    if iscompressed(filter_pipeline)
        throw(UnsupportedFeatureException(
            "Compression/filters are not supported for Implicit Index (Type 2) chunk indexing. " *
            "This is an HDF5 format requirement. Use Fixed Array (Type 3) for filtered chunks."
        ))
    end

    chunk_grid = ChunkIndexIterator(size(data), chunks)
    n_chunks = length(chunk_grid)
    chunk_size_bytes = prod(chunks) * odr_sizeof(odr)

    chunks_start_offset = f.end_of_data
    seek(f.io, chunks_start_offset)

    # Write chunks sequentially in linear index order
    for (julia_chunk_idx, linear_idx) in chunk_grid
        chunk_data_partial, _, _ = extract_chunk_region(data, julia_chunk_idx, chunks)
        chunk_data = pad_chunk_data(chunk_data_partial, chunks, fill_value)

        io_buf = IOBuffer()
        JLD2.write_data(io_buf, f, chunk_data, odr, JLD2.datamode(odr), wsession)
        write(f.io, take!(io_buf))
    end

    f.end_of_data = chunks_start_offset + n_chunks * chunk_size_bytes

    fill_value_size = odr_sizeof(odr)
    fill_value_bytes = collect(reinterpret(UInt8, [T(fill_value)]))
    fill_params = (
        version = 3,
        flags = UInt8(0x20),
        size = UInt32(fill_value_size),
        fill_value = fill_value_bytes
    )

    return ChunkIndexMetadata(
        h5offset(f, chunks_start_offset),
        UInt8(4),
        UInt8(2),
        (flags = UInt8(0x00),),
        false,
        true,
        fill_params
    )
end
"""
    write_fixed_array_index(f::JLDFile, data::AbstractArray{T,N}, chunks, odr, filter_pipeline, wsession) where {T,N}

Write chunks using Fixed Array indexing and return metadata.

Returns `ChunkIndexMetadata` for use by `write_chunked_dataset_object`.
"""
function write_fixed_array_index(f::JLDFile, data::AbstractArray{T,N}, chunks, odr, filter_pipeline, wsession) where {T,N}
    chunk_metadata = write_all_chunks(f, data, chunks, odr, filter_pipeline, wsession)
    n_chunks = length(chunk_metadata)

    entry_size = iscompressed(filter_pipeline) ? UInt8(20) : UInt8(8)
    db_size = 4 + 1 + 1 + 8 + n_chunks * entry_size + 4

    data_block_offset = f.end_of_data
    seek(f.io, data_block_offset)
    f.end_of_data = data_block_offset + db_size

    db_cio = begin_checksum_write(f.io, db_size - 4)
    jlwrite(db_cio, FIXED_ARRAY_DATABLOCK_SIGNATURE)
    jlwrite(db_cio, UInt8(0))

    client_id = iscompressed(filter_pipeline) ? UInt8(1) : UInt8(0)
    jlwrite(db_cio, client_id)

    header_addr_pos = position(f.io)
    jlwrite(db_cio, RelOffset(0))

    for chunk_meta in chunk_metadata
        jlwrite(db_cio, chunk_meta.offset)
        if iscompressed(filter_pipeline)
            jlwrite(db_cio, chunk_meta.chunk_size)
            jlwrite(db_cio, chunk_meta.filter_mask)
        end
    end

    jlwrite(f.io, end_checksum(db_cio))

    header_pos = f.end_of_data
    header_offset = h5offset(f, header_pos)
    seek(f.io, header_pos)

    # Page bits: log2(page_size) - HDF5 uses 1024 by default, so log2(1024) = 10
    page_bits = UInt8(10)

    hdr = FixedArrayHeader(
        UInt8(0), client_id, entry_size, page_bits,
        Int64(n_chunks), h5offset(f, data_block_offset)
    )
    header_size = write_fixed_array_header(f.io, hdr)
    f.end_of_data = header_pos + header_size

    current_pos = position(f.io)
    seek(f.io, header_addr_pos)
    jlwrite(f.io, header_offset)
    seek(f.io, current_pos)

    return ChunkIndexMetadata(
        header_offset,
        UInt8(4),
        UInt8(3),
        (
        flags = UInt8(0x00),
        page_bits = page_bits
        ),
        false,
        false,
        nothing
    )
end
"""
    write_extensible_array_index(f::JLDFile, data::AbstractArray{T,N}, chunks, maxshape, odr, filter_pipeline, wsession) where {T,N}

Write chunks using Extensible Array indexing and return metadata.

Extensible Array is used for datasets with exactly one unlimited dimension.
It uses a multi-level index structure (Header → Index Block → Data Blocks) that can
grow dynamically as the dataset is extended.

For initial implementation, we support cases where all chunks fit in the index block directly
(no data blocks needed).

# Validation
- Requires maxshape with at least one unlimited dimension

Returns `ChunkIndexMetadata` for use by `write_chunked_dataset_object`.
"""
function write_extensible_array_index(f::JLDFile, data::AbstractArray{T,N}, chunks, maxshape, odr, filter_pipeline, wsession) where {T,N}
    if isnothing(maxshape)
        throw(ArgumentError(
            "Extensible array (Type 4) requires at least one unlimited dimension. " *
            "Provide maxshape with at least one nothing element (e.g., maxshape=(nothing, 100))."
        ))
    end

    n_unlimited = count(isnothing, maxshape)
    if n_unlimited == 0
        throw(ArgumentError(
            "Extensible array (Type 4) requires at least one unlimited dimension. " *
            "Use Fixed Array (Type 3) for fixed-size datasets."
        ))
    end

    grid_dims = cld.(size(data), chunks)
    n_chunks = prod(grid_dims)

    index_blk_elmts = UInt8(min(n_chunks, 64))
    if n_chunks > Int(index_blk_elmts)
        index_blk_elmts = UInt8(min(n_chunks, 255))
        if n_chunks > 255
            throw(UnsupportedFeatureException(
                "Extensible arrays with >255 chunks require data block support"
            ))
        end
    end

    element_size = iscompressed(filter_pipeline) ? UInt8(20) : UInt8(8)
    client_id = iscompressed(filter_pipeline) ? UInt8(1) : UInt8(0)

    # EA header parameters (matching h5py defaults)
    max_nelmts_bits = UInt8(32)
    data_blk_min_elmts = UInt8(16)
    secondary_blk_min_data_ptrs = UInt8(4)
    max_dblk_page_nelmts_bits = UInt8(10)

    chunk_metadata = write_all_chunks(f, data, chunks, odr, filter_pipeline, wsession)

    # Write EA header FIRST (before index block)
    header_size = 4 + jlsizeof(ExtensibleArrayHeader) + 4
    header_pos = f.end_of_data
    header_offset = h5offset(f, header_pos)

    # Calculate allocated address slots (not just used ones!)
    # This must match the formula in read_index_types.jl
    first_sup_blk_log2 = Int(max_dblk_page_nelmts_bits)
    data_blk_min_log2 = trailing_zeros(Int(data_blk_min_elmts))
    ndblk_addrs = first_sup_blk_log2 - data_blk_min_log2

    total_addr_bits = Int(max_nelmts_bits) - data_blk_min_log2
    total_addr_slots = total_addr_bits + (index_blk_elmts > 0 ? 3 : 0)  # Empirical adjustment
    nsup_addrs = total_addr_slots - ndblk_addrs

    # Calculate index block size
    # Structure: sig(4) + ver(1) + client(1) + header_addr(8) +
    #           elements[index_blk_elmts] + data_block_addrs[ndblk_addrs] +
    #           super_block_addrs[nsup_addrs] + checksum(4)
    index_block_size = 4 + 1 + 1 + 8 + Int(index_blk_elmts) * Int(element_size) +
                       ndblk_addrs * 8 + nsup_addrs * 8 + 4
    index_block_pos = header_pos + header_size

    # Write EA header
    seek(f.io, header_pos)
    hdr = ExtensibleArrayHeader(
        UInt8(0), client_id, element_size, max_nelmts_bits, index_blk_elmts,
        data_blk_min_elmts, secondary_blk_min_data_ptrs, max_dblk_page_nelmts_bits,
        UInt64(0), UInt64(0), UInt64(0), UInt64(0),
        UInt64(n_chunks), UInt64(n_chunks),
        h5offset(f, index_block_pos)
    )

    cio = begin_checksum_write(f.io, header_size - 4)
    jlwrite(cio, EXTENSIBLE_ARRAY_HEADER_SIGNATURE)
    jlwrite(cio, hdr)
    jlwrite(f.io, end_checksum(cio))
    f.end_of_data = header_pos + header_size

    # Write index block AFTER header
    seek(f.io, index_block_pos)
    ib_cio = begin_checksum_write(f.io, index_block_size - 4)
    jlwrite(ib_cio, EXTENSIBLE_ARRAY_INDEX_BLOCK_SIGNATURE)
    jlwrite(ib_cio, UInt8(0))
    jlwrite(ib_cio, UInt8(client_id))
    # Index block stores header address as relative offset (RelOffset)
    jlwrite(ib_cio, header_offset)

    for i in 1:Int(index_blk_elmts)
        if i <= n_chunks
            jlwrite(ib_cio, chunk_metadata[i].offset)
            if iscompressed(filter_pipeline)
                jlwrite(ib_cio, chunk_metadata[i].chunk_size)
                jlwrite(ib_cio, chunk_metadata[i].filter_mask)
            end
        else
            jlwrite(ib_cio, RelOffset(typemax(UInt64)))
            if iscompressed(filter_pipeline)
                jlwrite(ib_cio, UInt64(0))
                jlwrite(ib_cio, UInt32(0))
            end
        end
    end

    # Write data block address SLOTS (allocate space for future expansion)
    for i in 1:ndblk_addrs
        # Only the first one might be used if we have data blocks
        # For now, all are undefined since all chunks fit in index block
        jlwrite(ib_cio, UNDEFINED_ADDRESS)
    end

    # Write super block address SLOTS (all undefined for now)
    for i in 1:nsup_addrs
        jlwrite(ib_cio, UNDEFINED_ADDRESS)
    end

    jlwrite(f.io, end_checksum(ib_cio))
    f.end_of_data = index_block_pos + index_block_size

    layout_params = (
        flags = iscompressed(filter_pipeline) ? UInt8(0x02) : UInt8(0x00),
        maxbits = UInt8(32),
        index_elements = index_blk_elmts,
        minpointers = UInt8(4),
        minelements = UInt8(16),
        page_bits = UInt8(10)
    )

    return ChunkIndexMetadata(
        header_offset,
        UInt8(4),
        UInt8(4),
        layout_params,
        true,
        false,
        nothing
    )
end

"""
    write_v2btree_index(f::JLDFile, data::AbstractArray{T,N}, chunks, maxshape, odr, filter_pipeline, wsession) where {T,N}

Write chunks using V2 B-tree indexing and return metadata.

V2 B-tree indexing is used for:
- Datasets with 2 or more unlimited dimensions
- Very large datasets with >64K chunks

# Implementation Notes

Simplified implementation with depth=0 (single leaf node):
- All chunk records stored in root leaf node
- Node size: 2048 bytes (matching h5py)
- Record size: 8 + 8*ndims (address + chunk indices)
- Split/merge percentages: 100/40 (matching h5py)

Structure written:
1. Chunks (unpadded, like Extensible Array)
2. Leaf node with chunk records
3. B-tree header
4. Object header with DataLayout message (version 4, type 5)

# Validation
- Requires maxshape with at least one unlimited dimension (typically 2+)

Returns `ChunkIndexMetadata` for use by `write_chunked_dataset_object`.
"""
function write_v2btree_index(f::JLDFile, data::AbstractArray{T,N}, chunks, maxshape, odr, filter_pipeline, wsession) where {T,N}
    if isnothing(maxshape)
        throw(ArgumentError(
            "V2 B-tree (Type 5) is typically used for datasets with multiple unlimited dimensions. " *
            "Provide maxshape with at least two nothing elements (e.g., maxshape=(nothing, nothing))."
        ))
    end

    n_unlimited = count(isnothing, maxshape)
    if n_unlimited == 0
        throw(ArgumentError(
            "V2 B-tree (Type 5) requires at least one unlimited dimension. " *
            "Use Fixed Array (Type 3) for fixed-size datasets."
        ))
    end

    chunk_records = write_all_chunks(f, data, chunks, odr, filter_pipeline, wsession)

    header_offset = BTrees.write_v2btree_chunked_dataset(f, chunk_records, iscompressed(filter_pipeline))

    layout_params = (
        flags = iscompressed(filter_pipeline) ? UInt8(0x02) : UInt8(0x00),
        node_size = V2_BTREE_NODE_SIZE,
        splitpercent = V2_BTREE_SPLIT_PERCENT,
        mergepercent = V2_BTREE_MERGE_PERCENT
    )

    return ChunkIndexMetadata(
        header_offset,
        UInt8(4),
        UInt8(5),
        layout_params,
        true,
        false,
        nothing
    )
end
"""
    write_v1btree_index(f::JLDFile, data::AbstractArray{T,N}, chunks, odr, filter_pipeline, wsession) where {T,N}

Write chunks using V1 B-tree indexing and return metadata.

Note: V1 B-tree uses version 3 DataLayout, so this includes writing the object header.
This is kept for compatibility but doesn't follow the new refactored pattern completely.

Returns `ChunkIndexMetadata` for use by `write_chunked_dataset_object`.
"""
function write_v1btree_index(f::JLDFile, data::AbstractArray{T,N}, chunks, odr, filter_pipeline, wsession) where {T,N}

    btree = BTrees.V1BTree(JLD2.UNDEFINED_ADDRESS, UInt8(N), BTrees.calculate_max_entries(f, N), f)

    chunk_metadata = write_all_chunks(f, data, chunks, odr, filter_pipeline, wsession)
    for chunk_meta in chunk_metadata
        BTrees.insert_chunk!(btree, chunk_meta..., chunks)
    end

    boundary_index = size(data) .+ chunks .- mod1.(size(data), chunks)
    BTrees.finalize_btree!(btree, UInt64[reverse(boundary_index)..., odr_sizeof(odr)])

    return ChunkIndexMetadata(
        btree.root,
        UInt8(3),
        nothing,
        (;),
        false,
        true,
        (flags = UInt8(0x09),)
    )
end
