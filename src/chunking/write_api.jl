"""
    WriteChunkedArray{T,N,A<:AbstractArray{T,N}} <: AbstractArray{T,N}

Wrapper type for arrays that should be written with chunking to JLD2 files.

The chunk index type (Single Chunk, Fixed Array, Extensible Array, V2 B-tree or V1 B-tree)
must be manually specified using the `indexing` parameter. If not specified, v1 b-tree is used.

# Examples

```julia
using JLD2

data = rand(Float32, 100, 50)

ca1 = WriteChunkedArray(data, chunks=(10, 10))
ca2 = WriteChunkedArray(data, chunks=(10, 10), maxshape=(nothing, 50))
ca3 = WriteChunkedArray(data, chunks=(10, 10), maxshape=(nothing, nothing))
ca4 = WriteChunkedArray(data, chunks=(10, 10), filters=Deflate())
ca5 = WriteChunkedArray(data, chunks=(10, 10), indexing=:v2btree)

jldsave("output.jld2"; data1=ca1, data2=ca2, data3=ca3)
```
"""
struct WriteChunkedArray{T,N,A<:AbstractArray{T,N}} <: AbstractArray{T,N}
    data::A
    chunk::NTuple{N,Int}
    maxshape::Union{Nothing, NTuple{N,Union{Int,Nothing}}}
    fill_value::Union{Nothing, T}
    indexing::Union{Symbol, Nothing}
    filters

    function WriteChunkedArray(data::AbstractArray{T,N};
                         chunk::NTuple{N,Int},
                         maxshape::Union{Nothing, NTuple{N,Union{Int,Nothing}}}=nothing,
                         fill_value::Union{Nothing, T}=nothing,
                         indexing::Union{Symbol, Nothing}=nothing,
                         filters=nothing) where {T,N}

        validate_chunks(chunk, size(data))
        validate_maxshape(maxshape, size(data))
        validate_fill_value(fill_value, T)
        if !isnothing(indexing)
            validate_index_type(indexing)
        end

        new{T,N,typeof(data)}(data, chunk, maxshape, fill_value, indexing, filters)
    end
end

Base.size(ca::WriteChunkedArray) = size(ca.data)
Base.getindex(ca::WriteChunkedArray, i...) = getindex(ca.data, i...)
Base.IndexStyle(::Type{<:WriteChunkedArray}) = IndexLinear()
Base.length(ca::WriteChunkedArray) = length(ca.data)
Base.eltype(::Type{WriteChunkedArray{T,N,A}}) where {T,N,A} = T

write_chunked(f::JLDFile, name::String, wca::WriteChunkedArray) =
    write_chunked(f, name, wca.data; wca.chunk, wca.maxshape, wca.fill_value,
        wca.indexing, wca.filters
    )

"""
    write_chunked(f::JLDFile, name::String, data; kwargs...)
    write_chunked(g::Group, name::String, data; kwargs...)

Write a chunked dataset to a JLD2 file.

# Arguments
- `f::JLDFile` or `g::Group` - Open JLD2 file/group in write mode
- `name::String` - Dataset name
- `data::AbstractArray{T,N}` - Data to write

# Keyword Arguments
- `chunks::Union{NTuple{N,Int}, Symbol}` - Chunk dimensions (required) or `:auto`
- `maxshape::Union{Nothing, NTuple{N,Union{Int,Nothing}}}=nothing` - Maximum dimensions
- `fill_value::Union{Nothing, T}=nothing` - Fill value for unallocated chunks
- `indexing::Union{Symbol, Nothing}=nothing` - Manual chunk index type override
- `filters=nothing` - Compression filters

# Examples

```julia
jldopen("file.jld2", "w") do f
    data = rand(Float32, 1000, 1000)
    write_chunked(f, "data1", data; chunk=(100, 100))
    write_chunked(f, "data2", data; chunk=(100, 100), maxshape=(nothing, 1000))
    write_chunked(f, "data3", data; chunk=(100, 100), filters=Deflate())
end
```
"""
function write_chunked(g::Union{JLDFile,Group}, name::String, data::AbstractArray{T,N};
                      chunk::NTuple{N,Int},
                      maxshape::Union{Nothing, NTuple{N,Union{Int,Nothing}}}=nothing,
                      fill_value::Union{Nothing, T}=nothing,
                      indexing::Union{Symbol, Nothing}=nothing,
                      filters=nothing) where {T,N}

    # Get file from group if necessary
    f = g isa JLDFile ? g : g.f

    !f.writable && throw(ArgumentError("File must be opened in write mode"))

    validate_chunks(chunk, size(data))
    validate_maxshape(maxshape, size(data))
    validate_fill_value(fill_value, T)

    index_type = if !isnothing(indexing)
        validate_index_type(indexing)
        indexing
    else
        :v1btree
    end

    odr = JLD2.objodr(data)
    datatype = JLD2.h5type(f, data)
    dataspace = JLD2.WriteDataspace(f, data, odr)
    filter_pipeline = prepare_filter_pipeline(filters, odr, dataspace)
    wsession = JLDWriteSession()

    index_metadata = if index_type == :single_chunk
        write_single_chunk_index(f, data, chunk, odr, filter_pipeline, wsession)
    elseif index_type == :implicit_index
        write_implicit_index(f, data, chunk, fill_value, odr, filter_pipeline, wsession)
    elseif index_type == :fixed_array
        write_fixed_array_index(f, data, chunk, odr, filter_pipeline, wsession)
    elseif index_type == :extensible_array
        write_extensible_array_index(f, data, chunk, maxshape, odr, filter_pipeline, wsession)
    elseif index_type == :v2btree
        write_v2btree_index(f, data, chunk, maxshape, odr, filter_pipeline, wsession)
    elseif index_type == :v1btree
        write_v1btree_index(f, data, chunk, odr, filter_pipeline, wsession)
    else
        throw(ArgumentError("Unknown chunk index type: $index_type"))
    end

    dspace_params = (;
        dataspace.dataspace_type,
        dimensions=dataspace.size,
        flags=UInt8(index_metadata.requires_maxshape),
        max_dimension_size = convert_maxshape_to_hdf5(maxshape)
        )

    # Compute optimal dim_size for chunk dimensions
    # Use smallest integer type that can hold all dimension values
    all_dims = (reverse(chunk)..., odr_sizeof(odr))
    max_dim = maximum(all_dims)
    dim_size = if max_dim <= typemax(UInt8)
        UInt8(1)
    elseif max_dim <= typemax(UInt16)
        UInt8(2)
    elseif max_dim <= typemax(UInt32)
        UInt8(4)
    else
        UInt8(8)
    end

    # Convert dimensions to appropriate size tuple
    dim_values = if dim_size == 1
        Tuple(UInt8.(all_dims))
    elseif dim_size == 2
        Tuple(UInt16.(all_dims))
    elseif dim_size == 4
        Tuple(UInt32.(all_dims))
    else
        Tuple(UInt64.(all_dims))
    end

    layout_params = (;
        version = index_metadata.layout_version,
        layout_class = LcChunked,
        dimensionality = UInt8(N + 1),
        dim_size = dim_size,
        dimensions = dim_values,
        index_metadata.chunk_indexing_type,
        data_address = index_metadata.data_address,
        index_metadata.layout_params...
    )

    # Calculate header size
    psz = jlsizeof(Val(HmDataspace); dspace_params...)
    psz += jlsizeof(Val(HmDatatype), 1; dt=datatype)
    if index_metadata.requires_fill_value
        psz += jlsizeof(Val(HmFillValue); index_metadata.fill_value_params...)
    end
    psz += Filters.pipeline_message_size(filter_pipeline)
    psz += jlsizeof(Val(HmDataLayout); layout_params...)
    psz += JLD2.CONTINUATION_MSG_SIZE
    fullsz = jlsizeof(ObjectStart) + size_size(psz) + psz + 4

    # Write object header
    obj_header_offset = f.end_of_data
    seek(f.io, obj_header_offset)
    f.end_of_data = obj_header_offset + fullsz

    cio = begin_checksum_write(f.io, fullsz - 4)
    jlwrite(cio, ObjectStart(size_flag(psz)))
    write_size(cio, psz)

    # Write header messages
    if index_metadata.requires_fill_value
        write_header_message(cio, Val(HmFillValue); index_metadata.fill_value_params...)
    end
    write_header_message(cio, Val(HmDataspace); dspace_params...)
    for attr in dataspace.attributes
        write_header_message(cio, f, attr)
    end
    write_header_message(cio, Val(HmDatatype), 1 | (2*isa(datatype, CommittedDatatype)); dt=datatype)
    Filters.write_filter_pipeline_message(cio, filter_pipeline)
    write_header_message(cio, Val(HmDataLayout); layout_params...)
    write_continuation_placeholder(cio)
    jlwrite(f.io, end_checksum(cio))

    dataset_offset = h5offset(f, obj_header_offset)
    # Register dataset with the appropriate group
    target_group = g isa JLDFile ? g.root_group : g
    target_group[name] = dataset_offset
    return dataset_offset
end
