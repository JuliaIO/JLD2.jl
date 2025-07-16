"""
    JLD2Bzip2

Addition package to JLD2 implementing the Bzip compression filter for JLD2.
Loading this package provides the filter type `BzipFilter`.
"""
module JLD2Bzip2

using JLD2: JLD2
import JLD2.Filters: Filter, filterid, filtername, filtertype, compress, decompress, client_values
using ChunkCodecLibBzip2

struct Bzip2Filter <: Filter
    blocksize100k::Cuint
end
Bzip2Filter() = Bzip2Filter(9)

filterid(::Type{Bzip2Filter}) = UInt16(307)
filtername(::Type{Bzip2Filter}) = "HDF5 bzip2 filter; see http://www.hdfgroup.org/services/contributions.html"
client_values(filter::Bzip2Filter) = (filter.blocksize100k, )
filtertype(::Val{307}) = Bzip2Filter

function compress(filter::Bzip2Filter, buf::Vector{UInt8}, args...)
    encode(
        BZ2EncodeOptions(;
            blockSize100k=filter.blocksize100k,
        ),
        buf
    )
end

function decompress(::Bzip2Filter, buf::Vector{UInt8}, args...)
    decode(BZ2DecodeOptions(), buf)
end

export Bzip2Filter

end # module JLD2Bzip2
