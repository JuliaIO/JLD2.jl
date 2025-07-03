#=
This is a port of H5Zlz4.c to Julia
https://github.com/HDFGroup/hdf5_plugins/blob/master/LZ4/src/H5Zlz4.c
https://github.com/nexusformat/HDF5-External-Filter-Plugins/blob/master/LZ4/src/H5Zlz4.c
https://github.com/silx-kit/hdf5plugin/blob/main/src/LZ4/H5Zlz4.c

H5Zlz4 is originally a copyright of HDF Group. License: licenses/H5Zlz4_LICENSE.txt

The following license applies to the Julia port.
Copyright (c) 2021 Mark Kittisopikul and Howard Hughes Medical Institute. License MIT, see LICENSE.txt
=#
module CodecLz4Ext

using JLD2
import JLD2.Filters: Filter, register_filter, filterid, filtername
import JLD2.Filters: compress, decompress, client_values
using CodecLz4

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

const LZ4_AGGRESSION = Ref(1)

function compress(filter::Lz4Filter, buf::Vector{UInt8}, args...)
    nbytes = length(buf)
    blocksize = min(filter.blocksize, nbytes)
    nbytes > typemax(Int32) && error("Can only compress chunks up to 2GB")
    nBlocks = (nbytes - 1) ÷ blocksize + 1
    maxDestSize =
        nBlocks * CodecLz4.LZ4_compressBound(blocksize) + 4 + 8 + nBlocks * 4
    maxDestSize <= 0 &&
        error("H5Zlz4: Non-positive maxDestSize for malloc: $maxDestSize")

    outBuf = Vector{UInt8}(undef, maxDestSize)

    @GC.preserve buf outBuf begin
        rpos = Ptr{UInt8}(pointer(buf))
        roBuf = Ptr{UInt8}(pointer(outBuf))

        # Header
        unsafe_store!(Ptr{UInt64}(roBuf), hton(UInt64(nbytes)))
        roBuf += 8

        unsafe_store!(Ptr{UInt32}(roBuf), hton(UInt32(blocksize)))
        roBuf += 4

        outSize = 12

        for block in 0:(nBlocks - 1)
            # compblocksize::UInt32
            origWritten = Csize_t(block * blocksize)
            if nbytes - origWritten < blocksize # the last block may be < blocksize
                blocksize = nbytes - origWritten
            end

            # aggression = 1 is the same LZ4_compress_default
            @debug "LZ4_compress_fast args" rpos outBuf roBuf roBuf + 4 blocksize nBlocks CodecLz4.LZ4_compressBound(
                blocksize
            )
            compblocksize = UInt32(
                CodecLz4.LZ4_compress_fast(
                    rpos,
                    roBuf + 4,
                    blocksize,
                    CodecLz4.LZ4_compressBound(blocksize),
                    LZ4_AGGRESSION[]
                )
            )
            @debug "Compressed block size" compblocksize

            if compblocksize == 0
                error("Could not compress block $block")
            end

            if compblocksize >= blocksize # compression did not save any space, do a memcpy instead
                compblocksize = blocksize
                unsafe_copyto!(roBuf + 4, rpos, blocksize)
            end

            unsafe_store!(Ptr{UInt32}(roBuf), hton(UInt32(compblocksize))) # write blocksize
            roBuf += 4

            rpos += blocksize
            roBuf += compblocksize
            outSize += compblocksize + 4
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
        blocksize = ntoh(unsafe_load(Ptr{UInt32}(rpos)))
        rpos += 4
        blocksize = min(blocksize, origSize) # blocksize should not be less than origSize

        # malloc a byte buffer of origSize
        # outBuf = Vector{UInt8}(undef, origSize)
        @debug "OrigSize" origSize
        origSize <= 0 && error("H5Zlz4: Non-positive origSize for malloc: $origSize")
        outBuf = Vector{UInt8}(undef, origSize)
        @GC.preserve outBuf begin
            roBuf = Ptr{UInt8}(pointer(outBuf))
            decompSize = 0
            # Start with the first blocksize
            while decompSize < origSize
                # compressedblocksize = UInt32(0)
                if origSize - decompSize < blocksize # the last block can be smaller than block size
                    blocksize = origSize - decompSize
                end

                compressedblocksize = ntoh(unsafe_load(Ptr{UInt32}(rpos)))
                rpos += 4

                if compressedblocksize == blocksize
                    # There was no compression
                    # memcpy(roBuf, rpos, blocksize)
                    unsafe_copyto!(roBuf, rpos, blocksize)
                    decompressedBytes = blocksize
                else
                    # do the compression
                    # LZ4_decompress_fast, version number 10300 ?
                    @debug "decompress_safe" rpos roBuf compressedblocksize (
                        origSize - decompSize
                    )
                    decompressedBytes = CodecLz4.LZ4_decompress_safe(
                        rpos, roBuf, compressedblocksize, origSize - decompSize
                    )
                    @debug "decompressedBytes" decompressedBytes
                end

                rpos += compressedblocksize
                roBuf += blocksize
                decompSize += decompressedBytes
            end
        end
    end
    return outBuf
end


__init__() = register_filter(Lz4Filter)


end # module CodecLz4Ext
