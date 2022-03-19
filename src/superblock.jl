#
# Superblock
#

const SUPERBLOCK_SIGNATURE = htol(0x0a1a0a0d46444889) # UInt8[0o211, 'H', 'D', 'F', '\r', '\n', 0o032, '\n']

# Data starts after file header and after superblock
const DATA_START = FILE_HEADER_LENGTH +  (12+8*4+4)


function read_superblock(io::IO)
    cio = begin_checksum_read(io)

    # Signature
    signature = jlread(cio, UInt64)
    signature == SUPERBLOCK_SIGNATURE || throw(InvalidDataException())

    # Version
    version = jlread(cio, UInt8)
    if version == 0
        version_free_space_storage = jlread(cio, UInt8) # has to be zero
        version_root_group_symbol_table_enty = jlread(cio, UInt8) # has to be zero
        jlread(cio, UInt8)
        version_share_header_msg_format = jlread(cio, UInt8) # has to be zero
        size_of_offsets = jlread(cio, UInt8)
        size_of_lengths = jlread(cio, UInt8)
        size_of_lengths == 8 && size_of_offsets == 8 || throw(UnsupportedFeatureException("Only files with length and offset size of 8 bytes are supported."))
        jlread(cio, UInt8)
        group_leaf_node_k = jlread(cio, UInt16) # must be greater than zero
        group_internal_node_k = jlread(cio, UInt16) # must be greater than zero
        # Unused File consistency flags
        jlread(cio, UInt32)
        #indexed_storage_internal_node_k = jlread(cio, UInt16) # must be greater than zero
        #jlread(cio, UInt16)
        base_address = jlread(cio, UInt64) # base adress for offsets within file (also absolute address of superblock)
        adress_free_space_info = jlread(cio, RelOffset)  # Undefined Adress
        end_of_file_address = jlread(cio, UInt64) # absolute adress of first byte past end of data
        driver_info_block_adress = jlread(cio, RelOffset) # undefined of relative adress of driver info block
        #root_group_symbol_table_entry = jlread(cio, UInt32) # symbol table entry of root group

        link_name_offset = jlread(cio, RelOffset)
        root_group_object_header_address = jlread(cio, RelOffset)
        cachetype = jlread(cio, UInt32)
        reserved = jlread(cio, UInt32)
        scratchspace = jlread(cio, UInt128)

        # Discard Checksum
        end_checksum(cio)

        (; version, base_address, end_of_file_address, root_group_object_header_address)
    elseif version == 2 || version == 3
        
        # Size of offsets and size of lengths
        size_of_offsets = jlread(cio, UInt8)
        size_of_lengths = jlread(cio, UInt8)
        (size_of_offsets == 8 && size_of_lengths == 8) || throw(UnsupportedFeatureException("Only files with length and offset size of 8 bytes are supported."))

        # File consistency flags
        file_consistency_flags = jlread(cio, UInt8)

        # Addresses
        base_address = jlread(cio, UInt64)
        superblock_extension_address = jlread(cio, RelOffset)
        end_of_file_address = jlread(cio, UInt64)
        root_group_object_header_address = jlread(cio, RelOffset)

        # Checksum
        cs = end_checksum(cio)
        jlread(io, UInt32) == cs || throw(InvalidDataException())

        (; version, base_address, end_of_file_address, root_group_object_header_address)
    else
        throw(UnsupportedVersionException("superblock version $version is not supported."))
    end
end

function write_superblock(io::IO, f)
    cio = begin_checksum_write(io, 8+4+4*jlsizeof(RelOffset))
    jlwrite(cio, SUPERBLOCK_SIGNATURE::UInt64)    # Signature
    jlwrite(cio, UInt8(2))                        # Version
    jlwrite(cio, UInt8(8))                        # Size of offsets
    jlwrite(cio, UInt8(8))                        # Size of lengths
    jlwrite(cio, UInt8(0))                        # file_consistency_flags
    jlwrite(cio, f.base_address::UInt64)
    jlwrite(cio, UNDEFINED_ADDRESS)
    jlwrite(cio, UInt64(f.end_of_data))
    jlwrite(cio, f.root_group_offset::RelOffset)
    jlwrite(io, end_checksum(cio))
end

function find_superblock(f)
    # Search at 0, 512, 1024, 2048 ...
    for offset in (0, 512, 1024, 2048, 4096)
        seek(f.io, offset)
        # Signature
        signature = jlread(f.io, UInt64)
        if signature == SUPERBLOCK_SIGNATURE
            seek(f.io, offset)
            return read_superblock(f.io)
        end
    end
    throw(InvalidDataException("Did not find a Superblock."))
end
