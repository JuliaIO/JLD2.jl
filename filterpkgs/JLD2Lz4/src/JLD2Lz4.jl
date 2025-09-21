"""
    JLD2Lz4

Addition package to JLD2 implementing the Lz4 compression filter for JLD2.
Loading this package provides the filter type `Lz4Filter`.
"""
module JLD2Lz4
using JLD2: JLD2, Filters
using ChunkCodecLibLz4

const DEFAULT_BLOCK_SIZE = 1 << 30

"""
    Lz4Filter(blocksize)

Apply LZ4 compression. `blocksize` is the main argument. The filter id is 32004.

# External Links
* [LZ4 HDF5 Filter ID 32004](https://github.com/HDFGroup/hdf5_plugins/blob/master/docs/RegisteredFilterPlugins.md)
* [LZ4 HDF5 Plugin Repository (C code)](https://github.com/HDFGroup/hdf5_plugins/tree/master/LZ4/src)
"""
struct Lz4Filter <: Filters.Filter
    blocksize::Cuint
end
Lz4Filter(; blocksize=DEFAULT_BLOCK_SIZE) = Lz4Filter(blocksize)


Filters.filterid(::Type{Lz4Filter}) = UInt16(32004)
Filters.filtername(::Type{Lz4Filter}) = "LZ4H5"
Filters.client_values(filter::Lz4Filter) = (filter.blocksize, )
Filters.filtertype(::Val{32004}) = Lz4Filter

function Filters.apply_filter!(filter::Lz4Filter, ref::Ref, forward::Bool=true)
    if forward
        buf = ref[]
        # This check is to avoid confusion with LZ4 Frame magic
        # for backwards compatibility with pre 0.6 JLD2 files
        # This is never going to error unless someone can encode about 
        # 300 PB in a single chunk.
        if UInt64(length(buf))>>32 == 0x0422_4D18
            throw(UnsupportedFeatureException("This particular dataset size is disallowed for the Lz4Filter. Please use a different compression filter."))
        end
        ref[] = encode(
            LZ4HDF5EncodeOptions(;
                blockSize=filter.blocksize,
                compressionLevel = 0,
            ),
            ref[]
        )
    else
        buf = ref[]
        # For backwards compatibility with pre 0.6 JLD2 files
        # check if this is actually the LZ4 Frame format
        # Fortunately because LZ4 Frame and LZ4 HDF5 formats have incompatible
        # headers, we can distinguish the formats by looking at the first 4 bytes.
        # https://github.com/HDFGroup/hdf5_plugins/issues/134#issuecomment-2484434118
        if length(buf) ≥ 4 &&
                buf[1] == 0x04 &&
                buf[2] == 0x22 &&
                buf[3] == 0x4D &&
                buf[4] == 0x18
            ref[] = decode(LZ4FrameDecodeOptions(), ref[])
        else
            ref[] = decode(LZ4HDF5DecodeOptions(), ref[])
        end
    end
    return 0
end

export Lz4Filter

end # module JLD2Lz4
