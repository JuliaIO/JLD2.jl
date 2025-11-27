module BTrees

using ..JLD2: JLDFile, RelOffset, UNDEFINED_ADDRESS, jlwrite, jlread, jlsizeof,
    fileoffset, h5offset, InvalidDataException, UnsupportedFeatureException,
    UnsupportedVersionException, JLDWriteSession, FilterPipeline, ReadRepresentation,
    odr_sizeof, DataLayout, HmDataLayout, HeaderMessageIterator,
    LcChunked, Link, is_hard_link, print_header_messages, jldopen, Length,
    begin_checksum_read, begin_checksum_write, end_checksum, read_nb_uint, size_size, size_size2,
    HmWrap, HmLinkMessage, Message, define_packed

# Import Filters submodule
using ..JLD2.Filters

# Include V1 B-tree implementation files
include("v1btree_types.jl")
include("v1btree_core.jl")
include("v1btree_write.jl")
include("v1btree_read.jl")
include("v1btree_debug.jl")

# Include V2 B-tree implementation files
include("v2btree_types.jl")
include("v2btree_read.jl")
include("v2btree_write.jl")

# Types and constants
export V2BTreeHeader
export V2_BTREE_HEADER_SIGNATURE
export ChunkRecordV2

# Functions needed within JLD2
export write_v2btree_chunked_dataset
export read_v1btree
export read_symbol_table_node
export read_v2btree_header
export read_records_in_node
export read_chunk_record_type10

end # module BTrees
