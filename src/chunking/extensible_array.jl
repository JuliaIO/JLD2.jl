# Extensible Array Index (v4 chunk indexing type 4)
# Used for datasets with ONE unlimited dimension

# Signature constants
const EXTENSIBLE_ARRAY_HEADER_SIGNATURE = htol(0x44484145)  # "EAHD"
const EXTENSIBLE_ARRAY_INDEX_BLOCK_SIGNATURE = htol(0x42494145)  # "EAIB"
const EXTENSIBLE_ARRAY_DATA_BLOCK_SIGNATURE = htol(0x42444145)  # "EADB"

"""
    ExtensibleArrayHeader

Header structure for HDF5 Extensible Array index (for writing).
Fields match HDF5 specification Section VII.D.
"""
struct ExtensibleArrayHeader
    version::UInt8
    client_id::UInt8
    element_size::UInt8
    max_nelmts_bits::UInt8
    index_blk_elmts::UInt8
    data_blk_min_elmts::UInt8
    secondary_blk_min_data_ptrs::UInt8
    max_dblk_page_nelmts_bits::UInt8
    num_secondary_blks::UInt64
    secondary_blk_size::UInt64
    num_data_blks::UInt64
    data_blk_size::UInt64
    max_index_set::UInt64
    nelmts::UInt64
    index_blk_addr::RelOffset
end
define_packed(ExtensibleArrayHeader)

"""
    read_extensible_array_chunks(f, v, dataspace, rr, layout, filters, header_offset, ndims)

Read chunks indexed by HDF5 v4 Extensible Array (type 4).
Used when dataset has one unlimited dimension.

Navigation: Header → Index Block → Secondary Blocks → Data Blocks → Chunk Records
"""
function read_extensible_array_chunks(f::JLDFile, v::Array, dataspace, rr,
                                       layout::DataLayout, filters,
                                       header_offset, ndims::Int)

    # Get header address from layout (already an absolute file offset)
    header_addr = layout.data_offset

    # Parse header to get index block address
    seek(f.io, header_addr)

    # Read header
    sig = jlread(f.io, UInt32)
    sig == htol(0x44484145) || throw(InvalidDataException("Invalid Extensible Array header signature"))  # "EAHD"

    hdr = jlread(f.io, ExtensibleArrayHeader)
    hdr.version == 0 || throw(UnsupportedVersionException("Unsupported Extensible Array version $(hdr.version)"))


    hdr.index_blk_addr.offset == typemax(UInt64) && return v
    read_extensible_array_index_block!(f, v, dataspace, rr, layout, filters,
                                        hdr.index_blk_addr, hdr.element_size,
                                        hdr.index_blk_elmts, hdr.num_data_blks, ndims)
    return v
end

"""
    read_extensible_array_index_block!(f, v, dataspace, rr, layout, filters,
                                        index_blk_addr, element_size,
                                        index_blk_elmts, num_data_blks, ndims)

Read the Extensible Array Index Block and navigate to chunk records.
"""
function read_extensible_array_index_block!(f::JLDFile, v::Array, dataspace, rr,
                                             layout::DataLayout, filters,
                                             index_blk_addr::RelOffset, element_size::UInt8,
                                             index_blk_elmts::UInt8, num_data_blks::UInt64,
                                             ndims::Int)

    offset = fileoffset(f, index_blk_addr)
    seek(f.io, offset)

    # Read index block header
    sig = jlread(f.io, UInt32)
    sig == htol(0x42494145) || throw(InvalidDataException("Invalid Extensible Array Index Block signature"))  # "EAIB"

    version = jlread(f.io, UInt8)
    version == 0 || throw(UnsupportedVersionException("Unsupported Index Block version $version"))

    client_id = jlread(f.io, UInt8)
    header_addr = jlread(f.io, RelOffset)

    # Read elements stored directly in index block
    num_direct_elements = Int(index_blk_elmts)

    # Get array dimensions for chunk navigation
    array_dims_julia = size(v)
    array_dims_hdf5 = UInt64.(collect(reverse(array_dims_julia)))  # Convert to Vector{UInt64}

    chunk_dims_hdf5 = layout.chunk_dimensions[1:ndims]
    chunk_dims_julia = UInt64.(collect(reverse(chunk_dims_hdf5)))  # Convert to Vector{UInt64}
    # First, read all chunk addresses/records from direct elements
    chunk_records = Vector{Tuple{RelOffset, UInt64, UInt32}}(undef, num_direct_elements)
    for i in 1:num_direct_elements
        if isempty(filters.filters)
            chunk_addr = jlread(f.io, RelOffset)
            chunk_size = prod(chunk_dims_julia) * sizeof(eltype(v))
            filter_mask = 0x00000000
        else
            chunk_addr = jlread(f.io, RelOffset)
            chunk_size = jlread(f.io, UInt64)
            filter_mask = jlread(f.io, UInt32)
        end

        chunk_records[i] = (chunk_addr, chunk_size, filter_mask)
    end

    # Read data block addresses from index block (immediately after direct elements)
    data_block_addrs = Vector{RelOffset}(undef, num_data_blks)
    for i in 1:num_data_blks

        data_block_addrs[i] = jlread(f.io, RelOffset)
    end

    # Now read actual chunk data from direct elements
    for i in 1:num_direct_elements
        chunk_addr, chunk_size, filter_mask = chunk_records[i]

        # Skip undefined chunks
        if isnothing(chunk_addr) || chunk_addr.offset == typemax(UInt64)
            continue
        end

        # Calculate which chunk this is (linear index to multi-dimensional coordinates)
        chunk_idx = i - 1  # 0-based

        # Convert linear chunk index to chunk coordinates
        chunk_coords = linear_to_chunk_coords(chunk_idx, array_dims_hdf5, chunk_dims_hdf5)

        # Read chunk into array
        read_chunk_into_array!(f, v, chunk_addr, chunk_size, chunk_coords,
                               chunk_dims_julia, filters, filter_mask, rr)
    end

    # Read chunks from each data block
    chunk_idx = num_direct_elements  # Continue from where direct elements left off
    for data_block_addr in data_block_addrs
        if !isnothing(data_block_addr) && data_block_addr.offset != typemax(UInt64)
            chunk_idx = read_extensible_array_data_block!(f, v, dataspace, rr, layout,
                                                          filters, data_block_addr,
                                                          chunk_idx, array_dims_hdf5,
                                                          chunk_dims_hdf5, chunk_dims_julia)
        end
    end

    return nothing
end

"""
    read_extensible_array_data_block!(f, v, dataspace, rr, layout, filters,
                                       data_block_addr, chunk_idx_start,
                                       array_dims_hdf5, chunk_dims_hdf5, chunk_dims_julia)

Read an Extensible Array Data Block and its chunk records.
Returns the next chunk_idx after reading this block.
"""
function read_extensible_array_data_block!(f::JLDFile, v::Array, dataspace, rr,
                                           layout::DataLayout, filters,
                                           data_block_addr::RelOffset, chunk_idx_start::Int,
                                           array_dims_hdf5::AbstractVector,
                                           chunk_dims_hdf5::AbstractVector,
                                           chunk_dims_julia::AbstractVector)
    seek(f.io, fileoffset(f, data_block_addr))

    # Read data block header
    sig = jlread(f.io, UInt32)
    sig == htol(0x42444145) || throw(InvalidDataException("Invalid Extensible Array Data Block signature"))  # "EADB"

    version = jlread(f.io, UInt8)
    version == 0 || throw(UnsupportedVersionException("Unsupported Data Block version $version"))

    client_id = jlread(f.io, UInt8)
    header_addr = jlread(f.io, RelOffset)

    # Block offset (variable size based on max_nelmts_bits)
    # Size in bytes = ceil(max_nelmts_bits / 8)
    ea_info = layout.chunk_indexing_info::ExtensibleArrayInfo
    block_offset_size = cld(Int(ea_info.max_bits), 8)
    block_offset = if block_offset_size == 1
        UInt64(jlread(f.io, UInt8))
    elseif block_offset_size == 2
        UInt64(jlread(f.io, UInt16))
    elseif block_offset_size == 4
        UInt64(jlread(f.io, UInt32))
    else
        jlread(f.io, UInt64)
    end
    # Calculate total number of chunks needed
    total_chunks = prod(cld.(array_dims_hdf5, chunk_dims_hdf5))
    remaining_chunks = Int(total_chunks) - chunk_idx_start

    # Read all remaining chunks (not limited to min_elements)
    max_elements = remaining_chunks
    # Read chunk records until we hit EOF or max_elements
    elements_read = 0
    for i in 1:max_elements
        chunk_idx = chunk_idx_start + i - 1
        # Try to read chunk record - break on EOF
        try
            if isempty(filters.filters)
                chunk_addr = jlread(f.io, RelOffset)
                chunk_size = prod(chunk_dims_julia) * sizeof(eltype(v))
                filter_mask = 0x00000000
            else
                chunk_addr = jlread(f.io, RelOffset)
                chunk_size = jlread(f.io, UInt64)
                filter_mask = jlread(f.io, UInt32)
            end
            # Skip undefined chunks
            if isnothing(chunk_addr) || chunk_addr.offset == typemax(UInt64)
                elements_read += 1
                continue
            end

            # Convert linear chunk index to chunk coordinates
            chunk_coords = linear_to_chunk_coords(chunk_idx, array_dims_hdf5, chunk_dims_hdf5)

            # Save position before reading chunk data
            saved_pos = position(f.io)

            # Read chunk into array
            read_chunk_into_array!(f, v, chunk_addr, chunk_size, chunk_coords,
                                   chunk_dims_julia, filters, filter_mask, rr)

            # Restore position to continue reading data block addresses
            seek(f.io, saved_pos)
            elements_read += 1
        catch e
            if e isa EOFError
                break
            else
                rethrow()
            end
        end
    end

    return chunk_idx_start + elements_read
end

"""
    linear_to_chunk_coords(linear_idx, array_dims_hdf5, chunk_dims_hdf5)

Convert a linear chunk index to multi-dimensional chunk coordinates.
Returns chunk coordinates in Julia order (reversed from HDF5).
"""
function linear_to_chunk_coords(linear_idx::Int, array_dims_hdf5::Vector{UInt64},
                                chunk_dims_hdf5::Vector{UInt64})
    # Calculate number of chunks in each dimension (HDF5 order)
    nchunks_hdf5 = [cld(Int(array_dims_hdf5[i]), Int(chunk_dims_hdf5[i]))
                    for i in 1:length(array_dims_hdf5)]

    # Convert linear index to multi-dimensional coordinates (HDF5 order, 0-based)
    coords_hdf5 = zeros(Int, length(nchunks_hdf5))
    idx = linear_idx
    for i in length(nchunks_hdf5):-1:1
        coords_hdf5[i] = idx % nchunks_hdf5[i]
        idx ÷= nchunks_hdf5[i]
    end

    # Reverse to Julia order and convert to 1-based
    coords_julia = reverse(coords_hdf5) .+ 1
    return coords_julia
end

"""
    read_chunk_into_array!(f, v, chunk_addr, chunk_size, chunk_coords,
                           chunk_dims_julia, filters, filter_mask, rr)

Read a single chunk and write it into the appropriate location in the array.
"""
function read_chunk_into_array!(f::JLDFile, v::Array, chunk_addr::RelOffset,
                                chunk_size::UInt64, chunk_coords::Vector{Int},
                                chunk_dims_julia::Vector{UInt64}, filters,
                                filter_mask::UInt32, rr)

    # Calculate array slice for this chunk
    array_dims = size(v)
    start_idx = tuple([(chunk_coords[i] - 1) * Int(chunk_dims_julia[i]) + 1
                       for i in 1:length(chunk_coords)]...)
    end_idx = tuple([min(chunk_coords[i] * Int(chunk_dims_julia[i]), array_dims[i])
                     for i in 1:length(chunk_coords)]...)

    # Calculate actual chunk size in elements
    chunk_elem_dims = tuple([end_idx[i] - start_idx[i] + 1 for i in 1:length(start_idx)]...)

    # Create temporary buffer for chunk data
    chunk_buffer = Array{eltype(v)}(undef, chunk_elem_dims)

    # Read chunk data into buffer
    seek(f.io, fileoffset(f, chunk_addr))

    if isempty(filters.filters)
        # Unfiltered: read directly into buffer
        read_array!(chunk_buffer, f, rr)
    else
        # Filtered: use filter pipeline
        read_chunk_with_filters!(chunk_buffer, f, rr, Int(chunk_size), filters, Int64(filter_mask))
    end

    # Copy buffer to array slice
    ranges = tuple([start_idx[i]:end_idx[i] for i in 1:length(start_idx)]...)
    v[ranges...] .= chunk_buffer

    return nothing
end
