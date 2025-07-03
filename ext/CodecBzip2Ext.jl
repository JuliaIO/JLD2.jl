module CodecBzip2Ext

using JLD2
import JLD2.Filters: Filter, register_filter, filterid, filtername
import JLD2.Filters: compress, decompress, client_values
using CodecBzip2

const H5Z_FILTER_BZIP2 = UInt16(307)

const bzip2_name = "HDF5 bzip2 filter; see http://www.hdfgroup.org/services/contributions.html"

struct Bzip2Filter <: Filter
    blocksize100k::Cuint
end
Bzip2Filter() = Bzip2Filter(9)

filterid(::Type{Bzip2Filter}) = H5Z_FILTER_BZIP2
filtername(::Type{Bzip2Filter}) = bzip2_name
client_values(filter::Bzip2Filter) = (filter.blocksize100k, )


compress(filter::Bzip2Filter, buf::Vector{UInt8}, args...) =
    transcode(Bzip2Compressor(; filter.blocksize100k), buf)

decompress(::Bzip2Filter, buf::Vector{UInt8}, args...) =
    transcode(Bzip2Decompressor(), buf)

__init__() = register_filter(Bzip2Filter)

end
