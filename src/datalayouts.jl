## Left over header message parsing that does not have a good place.

struct DataLayout
    version::UInt8
    storage_type::LayoutClass
    data_length::Int64
    data_offset::Int64
    dimensionality::UInt8
    chunk_indexing_type::UInt8 # only in version 4
    chunk_dimensions::Vector{UInt64} # only defined if dimensionality > 0
    DataLayout(version, storage_type, data_length, data_offset) = 
        new(version, storage_type, data_length, data_offset, 0, 0)
    DataLayout(version, storage_type, data_length, data_offset, dimensionality, chunk_indexing_type, chunk_dimensions) = 
        new(version, storage_type, data_length, data_offset, dimensionality, chunk_indexing_type, chunk_dimensions)
end

ischunked(dl::DataLayout) = dl.storage_type == LcChunked
isvirtual(dl::DataLayout) = dl.storage_type == LcVirtual
DataLayout(f::JLD2.JLDFile, msg_::Hmessage) =
    DataLayout(f, HmWrap(HmDataLayout, msg_))

function DataLayout(f::JLD2.JLDFile, msg::HmWrap{HmDataLayout})
    version = msg.version::UInt8
    storage_type = msg.layout_class::LayoutClass
    rf = msg.data_address::RelOffset
    data_offset::Int64 = rf != UNDEFINED_ADDRESS ? fileoffset(f, rf) : typemax(Int64)
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
            chunk_indexing_type == 1 || throw(UnsupportedFeatureException("Unknown chunk indexing type"))
            data_length = Int64(msg.data_size)

            #filters = msg.filters#jlread(cio, UInt32)
            chunked_storage = true
            DataLayout(version, storage_type, data_length, data_offset, msg.dimensionality, msg.chunk_indexing_type, chunk_dimensions) 
        elseif version == 3 && storage_type == LcChunked
            data_length = Int64(msg.data_size)

            chunk_dimensions = Int[msg.dimensions[1:end-1]...] # drop element size as last dimension
            chunked_storage = true
            DataLayout(version, storage_type, data_length, data_offset, msg.dimensionality, 0, chunk_dimensions)
        elseif storage_type == LcVirtual
            # Virtual dataset layout
            data_length = -1  # Virtual datasets don't have a fixed data length
            heap_address = msg.data_address
            index = hasfield(typeof(msg), :index) ? msg.index : UInt32(0)
            DataLayout(version, storage_type, data_length, fileoffset(f, heap_address), 0, 0, UInt64[])
        else
            throw(UnsupportedFeatureException("Unknown data layout"))
        end
    else
        throw(UnsupportedVersionException("Data layout message version $version is not supported"))
    end
end
