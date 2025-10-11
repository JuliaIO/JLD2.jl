module Chunking

# Import parent module to access functions defined later in load order
using ..JLD2

# Import specific symbols we need
using ..JLD2: JLDFile, RelOffset, UNDEFINED_ADDRESS, jlwrite, jlread, jlsizeof,
    fileoffset, h5offset, InvalidDataException, UnsupportedFeatureException,
    UnsupportedVersionException, JLDWriteSession, FilterPipeline, ReadRepresentation,
    odr_sizeof, DataLayout, HmDataLayout, HeaderMessageIterator, ReadDataspace,
    LcChunked, HardLink, print_header_messages, jldopen, Length, H5Datatype,
    begin_checksum_read, begin_checksum_write, end_checksum, read_nb_uint, size_size, size_size2,
    HmWrap, HmLinkMessage, Message, define_packed, Group, prewrite, CommittedDatatype,
    pathize, WrittenAttribute, jlconvert, CustomSerialization,
    HmDataspace, HmDatatype, HmFilterPipeline, HmFillValue, write_attribute,
    ObjectStart, size_flag, HmAttribute, write_size, write_header_message,
    write_continuation_placeholder,
    PlaceholderH5Datatype, LcCompact, ischunked, jltype,
    julia_repr, datamode, write_data, BTrees

# Note: The following are defined later in JLD2 module loading order,
# so we access them via JLD2. prefix:
# - WriteDataspace, numel, payload_size_without_storage_message, CONTINUATION_MSG_SIZE (datasets.jl/dataspaces.jl)
# - objodr, h5type (data/ files)
# - ArrayMemory (data/specialcased_types.jl)

# Import Filters submodule
using ..JLD2.Filters
using ..JLD2.Filters: iscompressed

# Note: BTrees is loaded before Chunking in JLD2.jl.
# BTrees functions are accessed via JLD2.BTrees.* prefix.

# Include chunking implementation files
include("chunking_helpers.jl")
include("chunked_array.jl")
include("implicit_index.jl")
include("fixed_array.jl")
include("extensible_array.jl")
include("v2btree_chunk_index.jl")
include("chunked_writing_api.jl")

# Export types
export ChunkedArray, Chunk, ChunkInfo
export WriteChunkedArray

# Export API functions
export write_chunked
export get_chunked_array, chunk_dimensions, num_chunks, chunk_grid_size

# Export helper functions for use by BTrees and other modules
export extract_chunk_region

# Export internal functions needed by datasets.jl and other internal modules
export read_chunked_array
export read_implicit_index_chunks
export read_fixed_array_chunks
export read_extensible_array_chunks
export read_v2btree_chunks

end # module Chunking
