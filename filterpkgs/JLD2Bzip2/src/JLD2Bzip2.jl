"""
    JLD2Bzip2

Addition package to JLD2 implementing the Bzip compression filter for JLD2.
Loading this package provides the filter type `BzipFilter`.
"""
module JLD2Bzip2

using JLD2: JLD2, Filters
using ChunkCodecLibBzip2

struct Bzip2Filter <: Filters.Filter
    blocksize100k::Cuint
end
Bzip2Filter() = Bzip2Filter(9)

Filters.filterid(::Type{Bzip2Filter}) = UInt16(307)
Filters.filtername(::Type{Bzip2Filter}) = "HDF5 bzip2 filter; see http://www.hdfgroup.org/services/contributions.html"
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
