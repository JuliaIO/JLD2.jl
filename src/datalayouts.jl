## Left over header message parsing that does not have a good place.

# Chunk indexing information types for DataLayout v4
abstract type ChunkIndexingInfo end

struct SingleChunkInfo <: ChunkIndexingInfo
    data_size::UInt64
    filters::UInt32
end

struct ImplicitIndexInfo <: ChunkIndexingInfo
    # No additional fields - chunks stored contiguously
end

struct FixedArrayInfo <: ChunkIndexingInfo
    page_bits::UInt8
end

struct ExtensibleArrayInfo <: ChunkIndexingInfo
    max_bits::UInt8
    index_elements::UInt8
    min_pointers::UInt8
    min_elements::UInt8
    page_bits::UInt8
end

struct V2BTreeInfo <: ChunkIndexingInfo
    node_size::UInt32
    split_percent::UInt8
    merge_percent::UInt8
end

struct DataLayout
    version::UInt8
    storage_type::LayoutClass
    data_length::Int64
    data_offset::RelOffset
    dimensionality::UInt8
    chunk_indexing_type::UInt8 # only in version 4
    chunk_indexing_info::Union{Nothing, ChunkIndexingInfo} # v4 indexing info
    chunk_dimensions::Vector{UInt64} # only defined if dimensionality > 0

    # Constructor for non-chunked layouts (v3 and v4)
    DataLayout(version, storage_type, data_length, data_offset) =
        new(version, storage_type, data_length, data_offset, 0, 0, nothing, UInt64[])

    # Constructor for v3 chunked layouts
    DataLayout(version, storage_type, data_length, data_offset, dimensionality, chunk_indexing_type, chunk_dimensions) =
        new(version, storage_type, data_length, data_offset, dimensionality, chunk_indexing_type, nothing, chunk_dimensions)

    # Constructor for v4 chunked layouts with indexing info
    DataLayout(version, storage_type, data_length, data_offset, dimensionality, chunk_indexing_type, chunk_indexing_info, chunk_dimensions) =
        new(version, storage_type, data_length, data_offset, dimensionality, chunk_indexing_type, chunk_indexing_info, chunk_dimensions)
end

ischunked(dl::DataLayout) = dl.storage_type == LcChunked
DataLayout(f::JLD2.JLDFile, msg_::Hmessage) =
    DataLayout(f, HmWrap(HmDataLayout, msg_))

function DataLayout(f::JLD2.JLDFile, msg::HmWrap{HmDataLayout})
    version = msg.version::UInt8
    storage_type = msg.layout_class::LayoutClass
    data_offset = msg.data_address::RelOffset

    if version == 4 || version == 3
        if storage_type == LcCompact
            data_length = Int64(msg.data_size)
            return DataLayout(version, storage_type, data_length, data_offset)
        elseif storage_type == LcContiguous
            data_length = Int64(msg.data_size)
            return DataLayout(version, storage_type, data_length, data_offset)
        elseif version == 4 && storage_type == LcChunked
            chunk_dimensions = Int[msg.dimensions...]
            chunk_indexing_type = msg.chunk_indexing_type

            # Extract indexing type information
            indexing_info = if chunk_indexing_type == 1
                # Single Chunk - data_size and filters only exist if filtered (flags & 0x02)
                is_filtered = (msg.flags & 0x02) != 0
                data_size = is_filtered ? UInt64(msg.data_size) : UInt64(0)
                filters = is_filtered ? msg.filters : UInt32(0)
                SingleChunkInfo(data_size, filters)
            elseif chunk_indexing_type == 2
                # Implicit Index
                ImplicitIndexInfo()
            elseif chunk_indexing_type == 3
                # Fixed Array
                FixedArrayInfo(msg.page_bits)
            elseif chunk_indexing_type == 4
                # Extensible Array
                ExtensibleArrayInfo(msg.maxbits, msg.index_elements, msg.minpointers,
                                  msg.minelements, msg.page_bits)
            elseif chunk_indexing_type == 5
                # V2 B-tree
                V2BTreeInfo(msg.node_size, msg.splitpercent, msg.mergepercent)
            else
                throw(UnsupportedFeatureException("Unknown chunk indexing type: $chunk_indexing_type"))
            end

            data_length = chunk_indexing_type == 1 && (msg.flags & 0x02) != 0 ? Int64(msg.data_size) : Int64(0)

            return DataLayout(version, storage_type, data_length, data_offset,
                            msg.dimensionality, chunk_indexing_type, indexing_info, chunk_dimensions)
        elseif version == 3 && storage_type == LcChunked
            # Version 3 chunked layouts don't have data_size field - it's calculated from the B-tree
            data_length = 0  # Will be determined when reading chunks

            chunk_dimensions = Int[msg.dimensions[1:end-1]...] # drop element size as last dimension
            return DataLayout(version, storage_type, data_length, data_offset, msg.dimensionality, 0, chunk_dimensions)
        else
            throw(UnsupportedFeatureException("Unknown data layout"))
        end
    else
        throw(UnsupportedVersionException("Data layout message version $version is not supported"))
    end
end
