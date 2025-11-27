module Chunking

# Import parent module to access functions defined later in load order
using ..JLD2

# Import specific symbols we need
using ..JLD2: JLDFile, RelOffset, UNDEFINED_ADDRESS, jlwrite, jlread, jlsizeof,
    fileoffset, h5offset, InvalidDataException, UnsupportedFeatureException,
    UnsupportedVersionException, JLDWriteSession, FilterPipeline, ReadRepresentation,
    odr_sizeof, DataLayout, HmDataLayout, HeaderMessageIterator, ReadDataspace,
    LcChunked, Link, is_hard_link, lookup_link, print_header_messages, jldopen, Length, H5Datatype,
    begin_checksum_read, begin_checksum_write, end_checksum, read_nb_uint, size_size, size_size2,
    HmWrap, HmLinkMessage, Message, define_packed, Group, prewrite, CommittedDatatype,
    pathize, WrittenAttribute, jlconvert, CustomSerialization,
    HmDataspace, HmDatatype, HmFilterPipeline, HmFillValue, write_attribute,
    ObjectStart, size_flag, HmAttribute, write_size, write_header_message,
    write_continuation_placeholder,
    PlaceholderH5Datatype, LcCompact, ischunked, jltype,
    julia_repr, BTrees

# Note: The following are defined later in JLD2 module loading order,
# so we access them via JLD2. prefix:
# - WriteDataspace, numel, payload_size_without_storage_message, CONTINUATION_MSG_SIZE (datasets.jl/dataspaces.jl)
# - objodr, h5type (data/ files)
# - ArrayMemory (data/specialcased_types.jl)
# - datamode, write_data (data/writing_datatypes.jl)

# Import Filters submodule
using ..JLD2.Filters
using ..JLD2.Filters: iscompressed

const H5S_UNLIMITED = 0xFFFFFFFFFFFFFFFF
const CHUNK_TARGET_BYTES = 32 * 1024
const V2_BTREE_NODE_SIZE = UInt32(2048)
const V2_BTREE_SPLIT_PERCENT = UInt8(100)
const V2_BTREE_MERGE_PERCENT = UInt8(40)

# Note: BTrees is loaded before Chunking in JLD2.jl.
# Import BTrees functions and types needed for chunk indexing
using ..JLD2.BTrees: read_chunk_record_type10, ChunkRecordV2

# Include chunking implementation files
include("chunking_helpers.jl")
include("chunked_array.jl")
include("read_index_types.jl")
include("write_index_types.jl")
include("write_api.jl")
include("argument_validation.jl")

# Export types
export WriteChunkedArray

# Export API functions
export write_chunked
# Export internal functions needed by datasets.jl and other internal modules
export read_chunked_array

end # module Chunking
