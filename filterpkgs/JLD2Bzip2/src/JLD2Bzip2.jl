"""
    JLD2Bzip2

Addition package to JLD2 implementing the Bzip compression filter for JLD2.
Loading this package provides the filter type `BzipFilter`.
"""
module JLD2Bzip2

using JLD2: JLD2, Filters
using ChunkCodecLibBzip2

"""
    Bzip2Filter <: Filter

The Bzip2Filter can be used to compress datasets using the bzip2 compression algorithm.
This is filter id 307.

## Keyword arguments:
- `blocksize100k::Integer = 9`: Specifies the block size to be used for compression.

  It should be a value between 1 and 9 inclusive, and the actual block size used
  is 100000 x this figure. The default 9 gives the best compression but takes the most memory.

# External Links
* [BZIP2 HDF5 Filter ID 307](https://github.com/HDFGroup/hdf5_plugins/blob/master/docs/RegisteredFilterPlugins.md)
* [BZIP2 HDF5 Plugin Repository (C code)](https://github.com/HDFGroup/hdf5_plugins/tree/master/BZIP2/src)
"""
struct Bzip2Filter <: Filters.Filter
    blocksize100k::Cuint
end
Bzip2Filter(; blocksize100k::Integer=9) = Bzip2Filter(blocksize100k)

Filters.filterid(::Type{Bzip2Filter}) = UInt16(307)
Filters.filtername(::Type{Bzip2Filter}) = "BZIP2"
Filters.client_values(filter::Bzip2Filter) = (filter.blocksize100k, )
Filters.filtertype(::Val{307}) = Bzip2Filter

function Filters.apply_filter!(filter::Bzip2Filter, ref::Ref, forward::Bool=true)
    if forward
        ref[] = encode(
            BZ2EncodeOptions(;
                blockSize100k=filter.blocksize100k,
            ),
            ref[]
        )
    else
        ref[] = decode(BZ2DecodeOptions(), ref[])
    end
    return 0
end

export Bzip2Filter

end # module JLD2Bzip2
