module Lz4Ext

using JLD2
import JLD2.Filters: Filter, register_filter, filterid, filtername
import JLD2.Filters: compress, decompress, client_values
using ChunkCodecLibLz4

const H5Z_FILTER_LZ4 = UInt16(32004)
const DEFAULT_BLOCK_SIZE = 1 << 30
const lz4_name = "HDF5 lz4 filter; see http://www.hdfgroup.org/services/contributions.html"

"""
    Lz4Filter(blocksize)

Apply LZ4 compression. `blocksize` is the main argument. The filter id is 32004.

# External Links
* [LZ4 HDF5 Filter ID 32004](https://portal.hdfgroup.org/display/support/Filters#Filters-32004)
* [LZ4 HDF5 Plugin Repository (C code)](https://github.com/nexusformat/HDF5-External-Filter-Plugins/tree/master/LZ4)
"""
struct Lz4Filter <: Filter
    blocksize::Cuint
end
Lz4Filter() = Lz4Filter(DEFAULT_BLOCK_SIZE)


filterid(::Type{Lz4Filter}) = H5Z_FILTER_LZ4
filtername(::Type{Lz4Filter}) = lz4_name
client_values(filter::Lz4Filter) = (filter.blocksize, )


function compress(filter::Lz4Filter, buf::Vector{UInt8}, args...)
    encode(
        LZ4HDF5EncodeOptions(;
            blockSize=filter.blocksize,
            compressionLevel = 0,
        ),
        buf
    )
end

function decompress(filter::Lz4Filter, buf::Vector{UInt8}, args...)
    decode(LZ4HDF5DecodeOptions(), buf)
end


__init__() = register_filter(Lz4Filter)


end # module CodecLz4Ext
