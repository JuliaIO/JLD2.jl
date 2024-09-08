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
DataLayout(f::JLD2.JLDFile, msg_::Hmessage) =
    DataLayout(f, HmWrap(HmDataLayout, msg_))

function DataLayout(f::JLD2.JLDFile, msg::HmWrap{HmDataLayout})
    version = msg.version::UInt8
    storage_type = msg.layout_class::LayoutClass
    rf = msg.data_address::RelOffset
    data_offset::Int64 = rf != UNDEFINED_ADDRESS ? fileoffset(f, rf) : typemax(Int64)
    if version == 4 || version == 3
        if storage_type == LcCompact
            data_length = Int64(msg.data_size::UInt16)
            return DataLayout(version, storage_type, data_length, data_offset) 
        elseif storage_type == LcContiguous
            data_length = Int64(msg.data_size::Int64)
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
        else
            throw(UnsupportedFeatureException("Unknown data layout"))
        end
    else
        throw(UnsupportedVersionException("Data layout message version $version is not supported"))
    end
end

function FilterPipeline(msg_::Hmessage)
    msg = HmWrap(HmFilterPipeline, msg_)
    version = msg.version
    nfilters = msg.nfilters
    io = msg.m.io
    seek(io, msg.m.address+2)
    version == 1 && skip(io, 6)
    filters = map(1:nfilters) do _
        id = jlread(io, UInt16)
        name_length = (version == 2 && id < 255) ? zero(UInt16) : jlread(io, UInt16)
        flags = jlread(io, UInt16)
        nclient_vals = jlread(io, UInt16)
        name = iszero(name_length) ? "" : read_bytestring(io)
        skip(io, max(0, 8-mod1(name_length, 8)-1))
        client_data = jlread(io, UInt32, nclient_vals)
        (version == 1 && isodd(nclient_vals)) && skip(io, 4)
        Filter(id, flags, name, client_data)
    end
    return FilterPipeline(filters)
end