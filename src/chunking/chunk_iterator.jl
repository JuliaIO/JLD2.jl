# Chunk Iterator for Writing
# Provides an iterator interface for processing chunks of data

"""
    ProcessedChunk

A single processed chunk ready for writing to a B-tree.

# Fields
- `chunk_idx::Tuple` - 1-based Julia coordinates of the chunk
- `chunk_data::Vector{UInt8}` - Processed (potentially compressed) chunk bytes
- `filter_mask::UInt32` - Filter mask indicating which filters were applied
"""
struct ProcessedChunk
    chunk_idx::Tuple
    chunk_data::Vector{UInt8}
    filter_mask::UInt32
end

"""
    ChunkIterator{T,N}

Iterator that extracts, processes, and yields chunks from an array.

# Fields
- `data::AbstractArray{T,N}` - Source data array
- `chunk_dims::NTuple{N,Int}` - Dimensions of each chunk
- `odr` - Object data representation
- `filters::FilterPipeline` - Filters to apply to each chunk
- `f::JLDFile` - File handle for compression context
- `wsession::JLDWriteSession` - Write session for serialization

# Iteration
Yields `ProcessedChunk` objects containing:
- `chunk_idx`: 1-based Julia chunk coordinates
- `chunk_data`: Compressed/filtered chunk bytes ready to write
- `filter_mask`: Filter mask for the chunk

# Example
```julia
iter = ChunkIterator(data, chunk_dims, odr, filters, f, wsession)
for chunk in iter
    # chunk.chunk_idx: (1, 1), (1, 2), ...
    # chunk.chunk_data: compressed bytes
    # chunk.filter_mask: 0x00000000
    write_to_btree(chunk)
end
```
"""
struct ChunkIterator{T,N}
    data::AbstractArray{T,N}
    chunk_dims::NTuple{N,Int}
    odr
    filters::FilterPipeline
    f::JLDFile
    wsession::JLDWriteSession
end

"""
    ChunkIterator(data, chunk_dims, odr, filters, f, wsession)

Create a chunk iterator for processing array data into chunks.

Chunks are extracted, optionally compressed/filtered, and yielded one at a time.
"""
function ChunkIterator(data::AbstractArray{T,N}, chunk_dims, odr, filters, f, wsession) where {T,N}
    chunk_dims_tuple = Tuple(chunk_dims)
    ChunkIterator{T,N}(data, chunk_dims_tuple, odr, filters, f, wsession)
end

"""
    Base.iterate(iter::ChunkIterator)
    Base.iterate(iter::ChunkIterator, state)

Iterate over chunks in the array.

The state is an iterator over chunk starting indices.
"""
function Base.iterate(iter::ChunkIterator{T,N}) where {T,N}
    data_size = size(iter.data)

    # Create iterator over chunk starting indices (1-based Julia coordinates)
    chunk_ranges = StepRange.(1, iter.chunk_dims, data_size)
    chunk_indices_iter = Iterators.product(chunk_ranges...)

    # Start iteration
    result = iterate(chunk_indices_iter)
    if result === nothing
        return nothing
    end

    chunk_idx, next_state = result
    chunk = process_chunk(iter, chunk_idx)

    return (chunk, (chunk_indices_iter, next_state))
end

function Base.iterate(iter::ChunkIterator{T,N}, state) where {T,N}
    chunk_indices_iter, prev_state = state

    result = iterate(chunk_indices_iter, prev_state)
    if result === nothing
        return nothing
    end

    chunk_idx, next_state = result
    chunk = process_chunk(iter, chunk_idx)

    return (chunk, (chunk_indices_iter, next_state))
end

"""
    process_chunk(iter::ChunkIterator, chunk_idx::Tuple) -> ProcessedChunk

Extract and process a single chunk at the given index.
"""
function process_chunk(iter::ChunkIterator, chunk_idx::Tuple)
    # Extract raw chunk data
    chunk_data_array = extract_chunk(iter.data, chunk_idx, iter.chunk_dims)

    # Apply filters/compression
    compressed_chunk, filter_mask = Filters.compress(
        iter.filters, chunk_data_array, iter.odr, iter.f, iter.wsession
    )

    return ProcessedChunk(chunk_idx, compressed_chunk, filter_mask)
end

"""
    Base.length(iter::ChunkIterator)

Return the total number of chunks.
"""
function Base.length(iter::ChunkIterator{T,N}) where {T,N}
    data_size = size(iter.data)
    grid_dims = cld.(data_size, iter.chunk_dims)
    return prod(grid_dims)
end

"""
    Base.eltype(::Type{ChunkIterator{T,N}}) where {T,N}

Return the element type of the iterator (ProcessedChunk).
"""
Base.eltype(::Type{ChunkIterator{T,N}}) where {T,N} = ProcessedChunk
