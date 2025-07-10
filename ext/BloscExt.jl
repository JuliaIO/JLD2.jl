module BloscExt

using JLD2
import JLD2.Filters: Filter, register_filter, filterid, filtername
import JLD2.Filters: compress, decompress, client_values

using ChunkCodecLibBlosc
using ChunkCodecLibBlosc: compcode, compname

const H5Z_FILTER_BLOSC = UInt16(32001)
const blosc_name = "blosc"

"""
    BloscFilter(;level=5, shuffle=true, compressor="blosclz")

The Blosc compression filter. Options:

 - `level`: compression level
 - `shuffle`: whether to shuffle data before compressing (this option should be used instead of the [`Shuffle`](@ref) filter). Valid values are:
   - `0`: do not shuffle the data before compressing
   - `1`: shuffle the data at byte level before compressing
   - `2`: shuffle the data at bit level before compressing (slower)
 - `compressor::AbstractString="lz4"`: The string representing the type of compressor to use.

  For example, "blosclz", "lz4", "lz4hc", "zlib", or "zstd".

  # External links
* [What Is Blosc?](https://www.blosc.org/pages/blosc-in-depth/)
* [Blosc HDF5 Filter ID 32001](https://portal.hdfgroup.org/display/support/Filters#Filters-32001)
* [Blosc HDF5 Plugin Repository (C code)](https://github.com/Blosc/hdf5-blosc)
* [ChunkCodecs.jl](https://github.com/JuliaIO/ChunkCodecs.jl)
"""
mutable struct BloscFilter <: Filter
    blosc_version::Cuint
    version_format::Cuint
    typesize::Cuint
    bufsize::Cuint
    level::Cuint
    shuffle::Cuint
    compcode::Cuint
end

function BloscFilter(; level=5, shuffle::Integer=1, compressor="blosclz")
    0 <= shuffle <= 2 || throw(ArgumentError("invalid blosc shuffle value $shuffle"))
    BloscFilter(0, 0, 0, 0, level, shuffle, compcode(compressor))
end

filterid(::Type{BloscFilter}) = H5Z_FILTER_BLOSC
filtername(::Type{BloscFilter}) = blosc_name
client_values(blosc::BloscFilter) = (
    blosc.blosc_version,
    blosc.version_format,
    blosc.typesize,
    blosc.bufsize,
    blosc.level,
    blosc.shuffle,
    blosc.compcode
)

function Base.show(io::IO, blosc::BloscFilter)
    print(
        io,
        "BloscFilter(level=", Int(blosc.level),
        ",shuffle=", Int(blosc.shuffle),
        ",compressor=", compname(blosc.compcode),
        ")"
    )
end

function compress(filter::BloscFilter, buf::Vector{UInt8}, elsize, args...)
    encode(
        BloscEncodeOptions(;
            clevel=filter.level,
            doshuffle=filter.shuffle,
            typesize=elsize,
            compressor=compname(filter.compcode)
        ),
        buf
    )
end

function decompress(::BloscFilter, buf::Vector{UInt8}, args...)
    decode(BloscDecodeOptions(), buf)
end

__init__() = register_filter(BloscFilter)

end # module BloscExt
