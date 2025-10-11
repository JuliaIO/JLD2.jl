"""
Read chunks using Implicit indexing (type 2). Chunks are stored contiguously:
chunk_address = base_address + (chunk_size_bytes × chunk_index)
"""
function read_implicit_index_chunks(f::JLDFile, v::Array{T}, dataspace::ReadDataspace,
                                   @nospecialize(rr::ReadRepresentation), layout::DataLayout,
                                   filters::FilterPipeline, header_offset::RelOffset,
                                   ndims::Int) where T
    chunk_dims_julia = reverse(layout.chunk_dimensions[1:ndims])
    base_address = Int64(layout.data_offset)
    chunk_size_bytes = Int(prod(chunk_dims_julia) * sizeof(T))

    for (chunk_grid_idx, linear_idx) in ChunkGrid(size(v), chunk_dims_julia)
        chunk_address = base_address + (linear_idx * chunk_size_bytes)
        chunk_start = chunk_start_from_index(chunk_grid_idx, chunk_dims_julia)
        read_and_assign_chunk!(f, v, chunk_start, RelOffset(chunk_address),
                              chunk_size_bytes, chunk_dims_julia, rr, filters, 0)
    end

    track_weakref!(f, header_offset, v)
    return v
end
