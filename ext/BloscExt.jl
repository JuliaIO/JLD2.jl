module BloscExt
# port of https://github.com/Blosc/c-blosc/blob/3a668dcc9f61ad22b5c0a0ab45fe8dad387277fd/hdf5/blosc_filter.c (copyright 2010 Francesc Alted, license: MIT/expat)

using JLD2
import JLD2.Filters: Filter, register_filter, filterid, filtername
import JLD2.Filters: compress, decompress, client_values

using Blosc: Blosc
# Import Blosc shuffle constants
using Blosc: NOSHUFFLE, SHUFFLE, BITSHUFFLE

const H5Z_FILTER_BLOSC = UInt16(32001)
const FILTER_BLOSC_VERSION = 2
const blosc_name = "blosc"

"""
    BloscFilter(;level=5, shuffle=true, compressor="blosclz")

The Blosc compression filter, using [Blosc.jl](https://github.com/JuliaIO/Blosc.jl). Options:

 - `level`: compression level
 - `shuffle`: whether to shuffle data before compressing (this option should be used instead of the [`Shuffle`](@ref) filter)
 - `compressor`: the compression algorithm. Call `Blosc.compressors()` for the available compressors.

# External links
* [What Is Blosc?](https://www.blosc.org/pages/blosc-in-depth/)
* [Blosc HDF5 Filter ID 32001](https://portal.hdfgroup.org/display/support/Filters#Filters-32001)
* [Blosc HDF5 Plugin Repository (C code)](https://github.com/Blosc/hdf5-blosc)
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

function BloscFilter(; level=5, shuffle=SHUFFLE, compressor="blosclz")
    Blosc.isvalidshuffle(shuffle) || throw(ArgumentError("invalid blosc shuffle $shuffle"))
    compcode = Blosc.compcode(compressor)
    BloscFilter(0, 0, 0, 0, level, shuffle, compcode)
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
        BloscFilter,
        "(level=",
        Int(blosc.level),
        ",shuffle=",
        blosc.shuffle == NOSHUFFLE  ? "NOSHUFFLE"  :
        blosc.shuffle == SHUFFLE    ? "SHUFFLE"    :
        blosc.shuffle == BITSHUFFLE ? "BITSHUFFLE" :
        "UNKNOWN",
        ",compressor=",
        Blosc.compname(blosc.compcode),
        ")"
    )
end

function compress(filter::BloscFilter, buf::Vector{UInt8}, elsize, args...)
    # Set dataset specific fields of the filter so that the metadata
    # gets written correctly.
    filter.typesize = elsize
    filter.bufsize = length(buf)

    # Allocate an output buffer exactly as long as the input data; if
    # the result is larger, we simply return 0. The filter is flagged
    # as optional, so HDF5 marks the chunk as uncompressed and proceeds.
    outbuf = Vector{UInt8}(undef, filter.bufsize)

    compname = Blosc.compname(filter.compcode)
    Blosc.set_compressor(compname)

    @GC.preserve buf outbuf begin
        status = Blosc.blosc_compress(
            filter.level,
            filter.shuffle,
            filter.typesize,
            filter.bufsize,
            pointer(buf),
            pointer(outbuf),
            filter.bufsize
        )
    end

    if status > 0
        # resize to the actual compressed size
        resize!(outbuf, status)
        return outbuf
    end

    # Compression failed
    status <= 0 && return buf
end

function decompress(::BloscFilter, buf::Vector{UInt8}, args...)

    @GC.preserve buf begin
        in = pointer(buf)
        outbuf_size, cbytes, blocksize = Blosc.cbuffer_sizes(in)
    end

    outbuf_size <= 0 && return buf # Decompression failed
    out_buf = Vector{UInt8}(undef, outbuf_size)

    @GC.preserve buf out_buf begin
        in = pointer(buf)
        outbuf = pointer(out_buf)
        status = Blosc.blosc_decompress(in, outbuf, outbuf_size)
        status <= 0 && return buf # Decompression failed
    end
    return out_buf
end

__init__() = register_filter(BloscFilter)

end # module BloscExt
