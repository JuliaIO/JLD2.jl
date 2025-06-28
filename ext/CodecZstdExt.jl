module CodecZstdExt

using JLD2
import JLD2.Filters: Filter, register_filter, filterid, filtername
import JLD2.Filters: compress, decompress, client_values
using CodecZstd

const H5Z_FILTER_ZSTD = UInt16(32015)
const zstd_name = "Zstandard compression: http://www.zstd.net"

struct ZstdFilter <: Filter
    level::Cuint
    ZstdFilter(level=CodecZstd.LibZstd.ZSTD_CLEVEL_DEFAULT) =
        new(clamp(level, 1, CodecZstd.LibZstd.ZSTD_maxCLevel()))
end

filterid(::Type{ZstdFilter}) = H5Z_FILTER_ZSTD
filtername(::Type{ZstdFilter}) = zstd_name
client_values(filter::ZstdFilter) = (filter.level, )


compress(filter::ZstdFilter, buf::Vector{UInt8}, args...) =
    transcode(ZstdCompressor(; filter.level), buf)

decompress(filter::ZstdFilter, buf::Vector{UInt8}, args...) =
    transcode(ZstdDecompressor(), buf)

__init__() = register_filter(ZstdFilter)

end
