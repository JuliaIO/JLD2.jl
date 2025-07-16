"""
    JLD2Lz4

Addition package to JLD2 implementing the Lz4 compression filter for JLD2.
Loading this package provides the filter type `Lz4Filter`.
"""
module JLD2Lz4
using JLD2
import JLD2.Filters: Filter, filterid, filtername, filtertype, compress, decompress, client_values
using ChunkCodecLibLz4

const DEFAULT_BLOCK_SIZE = 1 << 30

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


filterid(::Type{Lz4Filter}) = UInt16(32004)
filtername(::Type{Lz4Filter}) = "HDF5 lz4 filter; see http://www.hdfgroup.org/services/contributions.html"
client_values(filter::Lz4Filter) = (filter.blocksize, )
filtertype(::Val{32004}) = Lz4Filter


function compress(filter::Lz4Filter, buf::Vector{UInt8}, args...)
    encode(
        LZ4HDF5EncodeOptions(;
            blockSize=filter.blocksize,
            compressionLevel = 0,
        ),
        buf
    )
end

function decompress(::Lz4Filter, buf::Vector{UInt8}, args...)
    decode(LZ4HDF5DecodeOptions(), buf)
end

export Lz4Filter

end # module JLD2Lz4
