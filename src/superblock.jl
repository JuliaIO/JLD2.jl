#
# Superblock
#

# https://www.hdfgroup.org/HDF5/doc/H5.format.html#FileMetaData
# Superblock (Version 2)
type Superblock
    file_consistency_flags::UInt8
    base_address::FileOffset
    superblock_extension_address::Offset
    end_of_file_address::FileOffset
    root_group_object_header_address::Offset
end

Base.sizeof(::Union(Type{Superblock}, Superblock)) =
    12+sizeof(Offset)*4+4

function Base.read(io::IO, ::Type{Superblock})
    cio = begin_checksum(io)

    # Signature
    signature = read(cio, UInt64)
    signature == SUPERBLOCK_SIGNATURE || throw(InvalidDataException())

    # Version
    version  = read(cio, UInt8)
    version == 2 || throw(UnsupportedVersionException())

    # Size of offsets and size of lengths
    size_of_offsets = read(cio, UInt8)
    size_of_lengths = read(cio, UInt8)
    (size_of_offsets == 8 && size_of_lengths == 8) || throw(UnsupportedFeatureException())

    # File consistency flags
    file_consistency_flags = read(cio, UInt8)

    # Addresses
    base_address = read(cio, FileOffset)
    superblock_extension_address = read(cio, Offset)
    end_of_file_address = read(cio, FileOffset)
    root_group_object_header_address = read(cio, Offset)

    # Checksum
    cs = end_checksum(cio)
    read(io, UInt32) == cs || throw(InvalidDataException())

    Superblock(file_consistency_flags, base_address, superblock_extension_address,
                end_of_file_address, root_group_object_header_address)
end

function Base.write(io::IO, s::Superblock)
    cio = begin_checksum(io, sizeof(s))
    write(cio, SUPERBLOCK_SIGNATURE::UInt64)    # Signature
    write(cio, UInt8(2))                        # Version
    write(cio, UInt8(8))                        # Size of offsets
    write(cio, UInt8(8))                        # Size of lengths
    write(cio, s.file_consistency_flags::UInt8)
    write(cio, s.base_address::FileOffset)
    write(cio, s.superblock_extension_address::Offset)
    write(cio, s.end_of_file_address::FileOffset)
    write(cio, s.root_group_object_header_address::Offset)
    write(io, end_checksum(cio))
end
