"""Processed chunk ready for writing: (chunk_idx, chunk_data, filter_mask)."""
struct ProcessedChunk
    chunk_idx::Tuple
    chunk_data::Vector{UInt8}
    filter_mask::UInt32
end

"""Iterator yielding `ProcessedChunk` objects from an array with optional compression."""
struct ChunkIterator{T,N}
    data::AbstractArray{T,N}
    chunk_dims::NTuple{N,Int}
    odr
    filters::FilterPipeline
    f::JLDFile
    wsession::JLDWriteSession
end

ChunkIterator(data::AbstractArray{T,N}, chunk_dims, odr, filters, f, wsession) where {T,N} =
    ChunkIterator{T,N}(data, Tuple(chunk_dims), odr, filters, f, wsession)

function Base.iterate(iter::ChunkIterator{T,N}) where {T,N}
    chunk_indices_iter = Iterators.product(StepRange.(1, iter.chunk_dims, size(iter.data))...)
    result = iterate(chunk_indices_iter)
    isnothing(result) && return nothing
    chunk_idx, next_state = result
    return (process_chunk(iter, chunk_idx), (chunk_indices_iter, next_state))
end

function Base.iterate(iter::ChunkIterator{T,N}, state) where {T,N}
    chunk_indices_iter, prev_state = state
    result = iterate(chunk_indices_iter, prev_state)
    isnothing(result) && return nothing
    chunk_idx, next_state = result
    return (process_chunk(iter, chunk_idx), (chunk_indices_iter, next_state))
end

"""Extract and compress chunk at given index."""
function process_chunk(iter::ChunkIterator, chunk_idx::Tuple)
    chunk_data_array = extract_chunk(iter.data, chunk_idx, iter.chunk_dims)
    compressed_chunk, filter_mask = Filters.compress(iter.filters, chunk_data_array,
                                                      iter.odr, iter.f, iter.wsession)
    return ProcessedChunk(chunk_idx, compressed_chunk, filter_mask)
end

Base.length(iter::ChunkIterator) = prod(cld.(size(iter.data), iter.chunk_dims))
Base.eltype(::Type{ChunkIterator{T,N}}) where {T,N} = ProcessedChunk
