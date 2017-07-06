#
# Superblock
#

const SUPERBLOCK_SIGNATURE = htol(0x0a1a0a0d46444889) # UInt8[0o211, 'H', 'D', 'F', '\r', '\n', 0o032, '\n']

# https://www.hdfgroup.org/HDF5/doc/H5.format.html#FileMetaData
# Superblock (Version 2)
struct Superblock
    file_consistency_flags::UInt8
    base_address::Int64
    superblock_extension_address::RelOffset
    end_of_file_address::Int64
    root_group_object_header_address::RelOffset
end

Base.sizeof(::Union{Type{Superblock},Superblock}) =
    12+sizeof(RelOffset)*4+4

function Base.read(io::IO, ::Type{Superblock})
    cio = begin_checksum_read(io)

    # Signature
    signature = read(cio, UInt64)
    signature == SUPERBLOCK_SIGNATURE || throw(InvalidDataException())

    # Version
    version = read(cio, UInt8)
    version == 2 || throw(UnsupportedVersionException())

    # Size of offsets and size of lengths
    size_of_offsets = read(cio, UInt8)
    size_of_lengths = read(cio, UInt8)
    (size_of_offsets == 8 && size_of_lengths == 8) || throw(UnsupportedFeatureException())

    # File consistency flags
    file_consistency_flags = read(cio, UInt8)

    # Addresses
    base_address = read(cio, Int64)
    superblock_extension_address = read(cio, RelOffset)
    end_of_file_address = read(cio, Int64)
    root_group_object_header_address = read(cio, RelOffset)

    # Checksum
    cs = end_checksum(cio)
    read(io, UInt32) == cs || throw(InvalidDataException())

    Superblock(file_consistency_flags, base_address, superblock_extension_address,
                end_of_file_address, root_group_object_header_address)
end

function Base.write(io::IO, s::Superblock)
    cio = begin_checksum_write(io, 8+4+4*sizeof(RelOffset))
    write(cio, SUPERBLOCK_SIGNATURE::UInt64)    # Signature
    write(cio, UInt8(2))                        # Version
    write(cio, UInt8(8))                        # Size of offsets
    write(cio, UInt8(8))                        # Size of lengths
    write(cio, s.file_consistency_flags::UInt8)
    write(cio, s.base_address::Int64)
    write(cio, s.superblock_extension_address::RelOffset)
    write(cio, s.end_of_file_address::Int64)
    write(cio, s.root_group_object_header_address::RelOffset)
    write(io, end_checksum(cio))
end
