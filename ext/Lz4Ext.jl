#=
This is a port of H5Zlz4.c to Julia
https://github.com/HDFGroup/hdf5_plugins/blob/master/LZ4/src/H5Zlz4.c
https://github.com/nexusformat/HDF5-External-Filter-Plugins/blob/master/LZ4/src/H5Zlz4.c
https://github.com/silx-kit/hdf5plugin/blob/main/src/LZ4/H5Zlz4.c

H5Zlz4 is originally a copyright of HDF Group. License: licenses/H5Zlz4_LICENSE.txt

The following license applies to the Julia port.
Copyright (c) 2021 Mark Kittisopikul and Howard Hughes Medical Institute. License MIT, see LICENSE.txt
=#
module Lz4Ext

using JLD2
import JLD2.Filters: Filter, register_filter, filterid, filtername
import JLD2.Filters: compress, decompress
using CodecLz4

const H5Z_FILTER_LZ4 = UInt16(32004)
const DEFAULT_BLOCK_SIZE = 1 << 30
const lz4_name = "HDF5 lz4 filter; see http://www.hdfgroup.org/services/contributions.html"

"""
    Lz4Filter(blockSize)

Apply LZ4 compression. `blockSize` is the main argument. The filter id is 32004.

# External Links
* [LZ4 HDF5 Filter ID 32004](https://portal.hdfgroup.org/display/support/Filters#Filters-32004)
* [LZ4 HDF5 Plugin Repository (C code)](https://github.com/nexusformat/HDF5-External-Filter-Plugins/tree/master/LZ4)
"""
struct Lz4Filter <: Filter
    blockSize::Cuint
end
Lz4Filter() = Lz4Filter(DEFAULT_BLOCK_SIZE)


filterid(::Type{Lz4Filter}) = H5Z_FILTER_LZ4
filtername(::Type{Lz4Filter}) = lz4_name
client_values(filter::Lz4Filter) = (filter.blocksize, )

const LZ4_AGGRESSION = Ref(1)

function compress(filter::Lz4Filter, buf::Vector{UInt8}, args...)
    nbytes = length(buf)
    blockSize = min(filter.blockSize, nbytes)
    nbytes > typemax(Int32) && error("Can only compress chunks up to 2GB")
    nBlocks = (nbytes - 1) ÷ blockSize + 1
    maxDestSize =
        nBlocks * CodecLz4.LZ4_compressBound(blockSize) + 4 + 8 + nBlocks * 4
    maxDestSize <= 0 &&
        error("H5Zlz4: Non-positive maxDestSize for malloc: $maxDestSize")

    outBuf = Vector{UInt8}(undef, maxDestSize)

    @GC.preserve buf outBuf begin
        rpos = Ptr{UInt8}(pointer(buf))
        roBuf = Ptr{UInt8}(pointer(outBuf))

        # Header
        unsafe_store!(Ptr{UInt64}(roBuf), hton(UInt64(nbytes)))
        roBuf += 8

        unsafe_store!(Ptr{UInt32}(roBuf), hton(UInt32(blockSize)))
        roBuf += 4

        outSize = 12

        for block in 0:(nBlocks - 1)
            # compBlockSize::UInt32
            origWritten = Csize_t(block * blockSize)
            if nbytes - origWritten < blockSize # the last block may be < blockSize
                blockSize = nbytes - origWritten
            end

            # aggression = 1 is the same LZ4_compress_default
            @debug "LZ4_compress_fast args" rpos outBuf roBuf roBuf + 4 blockSize nBlocks CodecLz4.LZ4_compressBound(
                blockSize
            )
            compBlockSize = UInt32(
                CodecLz4.LZ4_compress_fast(
                    rpos,
                    roBuf + 4,
                    blockSize,
                    CodecLz4.LZ4_compressBound(blockSize),
                    LZ4_AGGRESSION[]
                )
            )
            @debug "Compressed block size" compBlockSize

            if compBlockSize == 0
                error("Could not compress block $block")
            end

            if compBlockSize >= blockSize # compression did not save any space, do a memcpy instead
                compBlockSize = blockSize
                unsafe_copyto!(roBuf + 4, rpos, blockSize)
            end

            unsafe_store!(Ptr{UInt32}(roBuf), hton(UInt32(compBlockSize))) # write blocksize
            roBuf += 4

            rpos += blockSize
            roBuf += compBlockSize
            outSize += compBlockSize + 4
        end
    end
    resize!(outBuf, outSize) # resize the output buffer to the actual size
    return outBuf
end

function decompress(filter::Lz4Filter, buf::Vector{UInt8}, args...)
    @GC.preserve buf begin
        roBuf = Ref{UInt8}()
        rpos = Ptr{UInt8}(pointer(buf))
        # Load the first 8 bytes from buffer as a big endian UInt64
        # This is the original size of the buffer
        origSize = ntoh(unsafe_load(Ptr{UInt64}(rpos)))
        rpos += 8 # advance the pointer

        # Next read the next four bytes from the buffer as a big endian UInt32
        # This is the blocksize
        blockSize = ntoh(unsafe_load(Ptr{UInt32}(rpos)))
        rpos += 4
        blockSize = min(blockSize, origSize) # blockSize should not be less than origSize

        # malloc a byte buffer of origSize
        # outBuf = Vector{UInt8}(undef, origSize)
        @debug "OrigSize" origSize
        origSize <= 0 && error("H5Zlz4: Non-positive origSize for malloc: $origSize")
        outBuf = Vector{UInt8}(undef, origSize)
        @GC.preserve outBuf begin
            roBuf = Ptr{UInt8}(pointer(outBuf))
            decompSize = 0
            # Start with the first blockSize
            while decompSize < origSize
                # compressedBlockSize = UInt32(0)
                if origSize - decompSize < blockSize # the last block can be smaller than block size
                    blockSize = origSize - decompSize
                end

                compressedBlockSize = ntoh(unsafe_load(Ptr{UInt32}(rpos)))
                rpos += 4

                if compressedBlockSize == blockSize
                    # There was no compression
                    # memcpy(roBuf, rpos, blockSize)
                    unsafe_copyto!(roBuf, rpos, blockSize)
                    decompressedBytes = blockSize
                else
                    # do the compression
                    # LZ4_decompress_fast, version number 10300 ?
                    @debug "decompress_safe" rpos roBuf compressedBlockSize (
                        origSize - decompSize
                    )
                    decompressedBytes = CodecLz4.LZ4_decompress_safe(
                        rpos, roBuf, compressedBlockSize, origSize - decompSize
                    )
                    @debug "decompressedBytes" decompressedBytes
                end

                rpos += compressedBlockSize
                roBuf += blockSize
                decompSize += decompressedBytes
            end
        end
    end
    return outBuf
end


__init__() = register_filter(Lz4Filter)


end # module Lz4Ext
