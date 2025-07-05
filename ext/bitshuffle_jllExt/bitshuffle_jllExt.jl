#==
Julia code wrapping the bitshuffle filter for HDF5. A rough translation of
bshuf_h5filter.c by Kiyoshi Masui, see
https://github.com/kiyo-masui/bitshuffle.
Originally authored by "James.Hester <jxh@ansto.gov.au>" as H5Zbitshuffle
==#
"""
The bitshuffle filter for HDF5. See https://portal.hdfgroup.org/display/support/Filters#Filters-32008
and https://github.com/kiyo-masui/bitshuffle for details.
"""
module bitshuffle_jllExt

using JLD2
import JLD2.Filters: Filter, register_filter, filterid, filtername
import JLD2.Filters: compress, decompress, client_values

using bitshuffle_jll

# From bshuf_h5filter.h

const BSHUF_H5_COMPRESS_LZ4 = 2
const BSHUF_H5_COMPRESS_ZSTD = 3
const H5Z_FILTER_BITSHUFFLE = UInt16(32008)

const BSHUF_VERSION_MAJOR = 0
const BSHUF_VERSION_MINOR = 4
const BSHUF_VERSION_POINT = 2

const bitshuffle_name = "HDF5 bitshuffle filter; see https://github.com/kiyo-masui/bitshuffle"

# All information for the filter

mutable struct BitshuffleFilter <: Filter
    major::Cuint
    minor::Cuint
    typesize::Cuint
    blocksize::Cuint
    compcode::Cuint
    comp_lvl::Cuint #Zstd only
end

"""
    BitshuffleFilter(blocksize=0,compressor=:none,comp_lvl=0)

The Bitshuffle filter can optionally include compression :lz4 or :zstd. For :zstd
comp_lvl can be provided. This is ignored for :lz4 compression. If `blocksize`
is zero the default bitshuffle blocksize is used.
"""
function BitshuffleFilter(; blocksize=0, compressor=:none, comp_lvl=0)
    compressor in (:lz4, :zstd, :none) ||
        throw(ArgumentError("Invalid bitshuffle compression $compressor"))
    compcode = 0
    if compressor == :lz4
        compcode = BSHUF_H5_COMPRESS_LZ4
    elseif compressor == :zstd
        compcode = BSHUF_H5_COMPRESS_ZSTD
    end
    BitshuffleFilter(
        BSHUF_VERSION_MAJOR, BSHUF_VERSION_MINOR, 0, blocksize, compcode, comp_lvl
    )
end


filterid(::Type{BitshuffleFilter}) = H5Z_FILTER_BITSHUFFLE
filtername(::Type{BitshuffleFilter}) = bitshuffle_name
client_values(f::BitshuffleFilter) = (f.major, f.minor, f.typesize, f.blocksize, f.compcode, f.comp_lvl)


function compress(filter::BitshuffleFilter, buf::Vector{UInt8}, elsize)
    filter.typesize = elsize

    @GC.preserve buf begin
        in_buf = pointer(buf)
        nbytes_out = 0

        # Get needed information
        (; major, minor, blocksize, compcode, comp_lvl) = filter
        elem_size = filter.typesize

        if blocksize == 0
            blocksize = ccall(
                (:bshuf_default_block_size, libbitshuffle), Csize_t, (Csize_t,), elem_size
            )
        end

        # Work out buffer sizes
        nbytes_uncomp = length(buf)
        if compcode == BSHUF_H5_COMPRESS_LZ4
            buf_size_out =
                ccall(
                    (:bshuf_compress_lz4_bound, libbitshuffle),
                    Csize_t,
                    (Csize_t, Csize_t, Csize_t),
                    nbytes_uncomp ÷ elem_size,
                    elem_size,
                    blocksize
                ) + 12
        elseif compcode == BSHUF_H5_COMPRESS_ZSTD
            buf_size_out =
                ccall(
                    (:bshuf_compress_zstd_bound, libbitshuffle),
                    Csize_t,
                    (Csize_t, Csize_t, Csize_t),
                    nbytes_uncomp ÷ elem_size,
                    elem_size,
                    blocksize
                ) + 12
        else  # No compression required
            nbytes_uncomp = length(buf)
            buf_size_out = length(buf)
        end

        if nbytes_uncomp % elem_size != 0
            error(
                "bitshuffle_h5plugin: Uncompressed size $nbytes_uncomp is not a multiple of $elem_size"
            )
        end

        size = nbytes_uncomp ÷ elem_size
        buf_size_out <= 0 && error(
            "bitshuffle_h5plugin: Non-positive buf_size_out for malloc: $buf_size_out"
        )

        outbuf = Vector{UInt8}(undef, buf_size_out)
        @GC.preserve outbuf begin
            out_buf = pointer(outbuf)
            if compcode != 0
                #shuffle and compress
                ccall(
                    (:bshuf_write_uint64_BE, libbitshuffle),
                    Cvoid,
                    (Ptr{Cvoid}, UInt64),
                    out_buf,
                    nbytes_uncomp
                )
                ccall(
                    (:bshuf_write_uint32_BE, libbitshuffle),
                    Cvoid,
                    (Ptr{Cvoid}, UInt32),
                    out_buf + 8,
                    blocksize * elem_size
                )

                if compcode == BSHUF_H5_COMPRESS_LZ4
                    err = ccall(
                        (:bshuf_compress_lz4, libbitshuffle),
                        Int64,
                        (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t, Csize_t, Csize_t),
                        in_buf,
                        out_buf + 12,
                        size,
                        elem_size,
                        blocksize
                    )
                else
                    err = ccall(
                        (:bshuf_compress_zstd, libbitshuffle),
                        Int64,
                        (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t, Csize_t, Csize_t, Cint),
                        in_buf,
                        out_buf + 12,
                        size,
                        elem_size,
                        blocksize,
                        Cint(comp_lvl)
                    )
                end

                nbytes_out = err + 12
            else # just the shuffle thanks
                err = ccall(
                    (:bshuf_bitshuffle, libbitshuffle),
                    Int64,
                    (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t, Csize_t, Csize_t),
                    in_buf,
                    out_buf,
                    size,
                    elem_size,
                    blocksize
                )
                nbytes_out = length(buf)
            end
        end

        # And wrap it up
        if err < 0
            error("h5plugin_bitshuffle: Error in bitshuffle with code $err")
        end
        resize!(outbuf, nbytes_out)
    end
    return outbuf
end

function decompress(filter::BitshuffleFilter, buf::Vector{UInt8}, args...)
    @GC.preserve buf begin
        in_buf = pointer(buf)


        # Get needed information
        (; major, minor, blocksize, compcode) = filter
        elem_size = filter.typesize
        @info "Bitshuffle decompressing with $major.$minor, blocksize=$blocksize, compcode=$compcode, elem_size=$elem_size"
        @info "Bitshuffle comp_lvl $(filter.comp_lvl)"
        if blocksize == 0
            blocksize = ccall(
                (:bshuf_default_block_size, libbitshuffle), Csize_t, (Csize_t,), elem_size
            )
        end

        # Work out buffer sizes
        if compcode != 0
            # First 8 bytes is number of uncompressed bytes
            nbytes_uncomp = ccall(
                (:bshuf_read_uint64_BE, libbitshuffle), UInt64, (Ptr{Cvoid},), in_buf
            )
            @info "Bitshuffle decompressing: nbytes_uncomp=$nbytes_uncomp"
            # Next 4 bytes are the block size
            blocksize =
                ccall(
                    (:bshuf_read_uint32_BE, libbitshuffle),
                    UInt32,
                    (Ptr{Cvoid},),
                    in_buf + 8
                ) ÷ elem_size
            @info "Bitshuffle decompressing: blocksize=$blocksize"
            in_buf += 12
            buf_size_out = nbytes_uncomp
        else  # No compression required
            nbytes_uncomp = length(buf)
            buf_size_out = length(buf)
        end

        if nbytes_uncomp % elem_size != 0
            error(
                "bitshuffle_h5plugin: Uncompressed size $nbytes_uncomp is not a multiple of $elem_size"
            )
        end

        size = nbytes_uncomp ÷ elem_size
        buf_size_out <= 0 && error(
            "bitshuffle_h5plugin: Non-positive buf_size_out for malloc: $buf_size_out"
        )
        outbuf = Vector{UInt8}(undef, buf_size_out)
        @GC.preserve outbuf begin
            out_buf = pointer(outbuf)

            # Now perform the decompression
            if compcode == BSHUF_H5_COMPRESS_LZ4
                err = ccall(
                    (:bshuf_decompress_lz4, libbitshuffle),
                    Int64,
                    (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t, Csize_t, Csize_t),
                    in_buf,
                    out_buf,
                    size,
                    elem_size,
                    blocksize
                )
            elseif compcode == BSHUF_H5_COMPRESS_ZSTD
                err = ccall(
                    (:bshuf_decompress_zstd, libbitshuffle),
                    Int64,
                    (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t, Csize_t, Csize_t),
                    in_buf,
                    out_buf,
                    size,
                    elem_size,
                    blocksize
                )
            else # just the shuffle
                err = ccall(
                    (:bshuf_bitunshuffle, libbitshuffle),
                    Int64,
                    (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t, Csize_t, Csize_t),
                    in_buf,
                    out_buf,
                    size,
                    elem_size,
                    blocksize
                )
            end
        end
    end

    if err < 0
        error("h5plugin_bitshuffle: Error in bitshuffle with code $err")
    end

    resize!(outbuf, nbytes_uncomp)
    return outbuf
end

__init__() = register_filter(BitshuffleFilter)

end # module bitshuffle_jllExt
