module CodecZlibExt

using JLD2
import JLD2.Filters: Filter, register_filter, filterid, filtername, client_values
import JLD2.Filters: compress, decompress
using CodecZlib

const H5Z_FILTER_ZLIB = UInt16(1)
const zlib_name = ""

struct Deflate <: Filter
    level::Cuint
    Deflate(level=5) = new(clamp(level, 0, 9))
end

filterid(::Type{Deflate}) = H5Z_FILTER_ZLIB
filtername(::Type{Deflate}) = zlib_name
client_values(filter::Deflate) = (filter.level, )

function compress(filter::Deflate, buf::Vector{UInt8}, args...)
    transcode(
        ZlibCompressor(; filter.level),
        buf)
end

function decompress(::Deflate, buf::Vector{UInt8}, args...)
    transcode(
        ZlibDecompressor(),
        buf)
end

__init__() = register_filter(Deflate)

end
