# Chunked Array Writing API for JLD2
# This module provides high-level API for writing chunked datasets with automatic
# chunk index type selection matching h5py semantics.

#=============================================================================
# Constants
=============================================================================#

# HDF5 constant for unlimited dimension size
const H5S_UNLIMITED = 0xFFFFFFFFFFFFFFFF

"""
    WriteChunkedArray{T,N,A<:AbstractArray{T,N}} <: AbstractArray{T,N}

Wrapper type for arrays that should be written with chunking to JLD2 files.

The chunk index type (Single Chunk, Fixed Array, Extensible Array, or V2 B-tree)
is automatically selected based on the `maxshape` parameter, matching h5py's behavior.

# Fields
- `data::AbstractArray{T,N}` - The array data to write
- `chunks::NTuple{N,Int}` - Chunk dimensions (must match array dimensionality)
- `maxshape::Union{Nothing, NTuple{N,Union{Int,Nothing}}}` - Maximum dimensions
  - `nothing` for fixed-size dataset
  - `nothing` in tuple for unlimited dimension (e.g., `(nothing, 100)`)
- `fill_value::Union{Nothing, T}` - Fill value for unallocated chunks
- `indexing::Union{Symbol, Nothing}` - Manual chunk index type override
  - Valid values: `:single_chunk`, `:implicit_index`, `:fixed_array`,
    `:extensible_array`, `:v2btree`
  - `nothing` for automatic selection
- `filters::Union{Nothing, FilterPipeline}` - Compression filters to apply

# Examples

```julia
using JLD2

data = rand(Float32, 100, 50)

# Basic chunking (automatic Fixed Array indexing)
ca1 = WriteChunkedArray(data, chunks=(10, 10))

# Extensible in first dimension (automatic Extensible Array indexing)
ca2 = WriteChunkedArray(data, chunks=(10, 10), maxshape=(nothing, 50))

# Extensible in all dimensions (automatic V2 B-tree indexing)
ca3 = WriteChunkedArray(data, chunks=(10, 10), maxshape=(nothing, nothing))

# With compression
ca4 = WriteChunkedArray(data, chunks=(10, 10), filters=:gzip)

# Manual index type override
ca5 = WriteChunkedArray(data, chunks=(10, 10), indexing=:v2btree)

# Save to file
jldsave("output.jld2"; data1=ca1, data2=ca2, data3=ca3)
```
"""
struct WriteChunkedArray{T,N,A<:AbstractArray{T,N}} <: AbstractArray{T,N}
    data::A
    chunks::NTuple{N,Int}
    maxshape::Union{Nothing, NTuple{N,Union{Int,Nothing}}}
    fill_value::Union{Nothing, T}
    indexing::Union{Symbol, Nothing}
    filters  # Union{Nothing, FilterPipeline} - leave untyped for now

    function WriteChunkedArray(data::AbstractArray{T,N};
                         chunks::Union{NTuple{N,Int}, Symbol},
                         maxshape::Union{Nothing, NTuple{N,Union{Int,Nothing}}}=nothing,
                         fill_value::Union{Nothing, T}=nothing,
                         indexing::Union{Symbol, Nothing}=nothing,
                         filters=nothing) where {T,N}

        # Handle auto-chunking
        chunk_dims = if chunks === :auto
            auto_chunk_size(size(data), sizeof(T))
        else
            chunks
        end

        # Validate parameters
        validate_chunks(chunk_dims, size(data))
        validate_maxshape(maxshape, size(data))
        validate_fill_value(fill_value, T)
        if !isnothing(indexing)
            validate_index_type(indexing)
        end

        new{T,N,typeof(data)}(data, chunk_dims, maxshape, fill_value, indexing, filters)
    end
end

# Implement AbstractArray interface so WriteChunkedArray can be used like a regular array
Base.size(ca::WriteChunkedArray) = size(ca.data)
Base.getindex(ca::WriteChunkedArray, i...) = getindex(ca.data, i...)
Base.IndexStyle(::Type{<:WriteChunkedArray}) = IndexLinear()
Base.length(ca::WriteChunkedArray) = length(ca.data)
Base.eltype(::Type{WriteChunkedArray{T,N,A}}) where {T,N,A} = T

#-------------------------------------------------------------------------------
# Validation Functions
#-------------------------------------------------------------------------------

"""
    validate_chunks(chunks, data_size)

Validate chunk dimensions against data size.

Throws ArgumentError if:
- Chunk dimensionality doesn't match data dimensionality
- Any chunk dimension is non-positive
"""
function validate_chunks(chunks::NTuple{N,Int}, data_size::NTuple{N,Int}) where N
    # Dimensionality check is automatic via NTuple{N,Int} type constraint

    # Check for positive values
    for (i, chunk_dim) in enumerate(chunks)
        if chunk_dim <= 0
            throw(ArgumentError("Chunk size must be positive, got chunks[$i]=$chunk_dim"))
        end
    end

    # Warn if chunks are larger than data (not an error, but unusual)
    for (i, (chunk_dim, data_dim)) in enumerate(zip(chunks, data_size))
        if chunk_dim > data_dim
            @warn "Chunk dimension $i ($chunk_dim) is larger than data dimension ($data_dim). " *
                  "This may not be optimal." maxlog=1
        end
    end

    return nothing
end

function validate_chunks(chunks::NTuple{M,Int}, data_size::NTuple{N,Int}) where {M,N}
    throw(ArgumentError(
        "Chunk dimensions ($M) must match data dimensions ($N). " *
        "Got chunks=$chunks for data of size $data_size"
    ))
end

"""
    validate_maxshape(maxshape, data_size)

Validate maximum shape specification against current data size.

Throws ArgumentError if:
- maxshape dimensionality doesn't match data dimensionality
- Any fixed maxshape dimension is less than corresponding data dimension
"""
function validate_maxshape(maxshape::Nothing, data_size)
    # Fixed-size dataset, always valid
    return nothing
end

function validate_maxshape(maxshape::NTuple{N,Union{Int,Nothing}}, data_size::NTuple{N,Int}) where N
    for (i, (max_dim, data_dim)) in enumerate(zip(maxshape, data_size))
        if !isnothing(max_dim) && max_dim < data_dim
            throw(ArgumentError(
                "maxshape[$i]=$max_dim is less than current data size $data_dim. " *
                "maxshape must be >= current size or nothing (unlimited)."
            ))
        end
    end
    return nothing
end

function validate_maxshape(maxshape::NTuple{M,Union{Int,Nothing}}, data_size::NTuple{N,Int}) where {M,N}
    throw(ArgumentError(
        "maxshape dimensions ($M) must match data dimensions ($N). " *
        "Got maxshape=$maxshape for data of size $data_size"
    ))
end

"""
    convert_maxshape_to_hdf5(maxshape::NTuple{N,Union{Int,Nothing}})

Convert Julia maxshape format to HDF5 max dimension format.

In Julia (column-major):
- `nothing` means unlimited dimension
- Dimensions are in Julia order (fastest-varying first)

In HDF5:
- `H5S_UNLIMITED` (0xFFFFFFFFFFFFFFFF) means unlimited dimension
- Dimensions are reversed (slowest-varying first)

# Example
```julia
convert_maxshape_to_hdf5((nothing, 100))  # Returns (100, H5S_UNLIMITED)
convert_maxshape_to_hdf5((10, nothing))   # Returns (H5S_UNLIMITED, 10)
```
"""
function convert_maxshape_to_hdf5(maxshape::NTuple{N,Union{Int,Nothing}}) where N
    hdf5_max = map(maxshape) do dim
        # H5S_UNLIMITED is 0xFFFFFFFFFFFFFFFF, which is -1 when reinterpreted as Int64
        isnothing(dim) ? reinterpret(Int64, UInt64(H5S_UNLIMITED)) : Int64(dim)
    end
    return reverse(hdf5_max)  # Reverse for HDF5 dimension ordering
end

"""
    validate_fill_value(fill_value, data_eltype)

Validate that fill_value type matches data element type.
"""
function validate_fill_value(fill_value::Nothing, data_eltype)
    return nothing
end

function validate_fill_value(fill_value, data_eltype)
    if !(fill_value isa data_eltype)
        throw(ArgumentError(
            "fill_value type $(typeof(fill_value)) doesn't match data element type $data_eltype. " *
            "fill_value must be of the same type as array elements."
        ))
    end
    return nothing
end

"""
    validate_index_type(index_type)

Validate chunk index type symbol.
"""
function validate_index_type(index_type::Symbol)
    valid_types = [:single_chunk, :implicit_index, :fixed_array, :extensible_array, :v2btree, :v1btree]
    if !(index_type in valid_types)
        throw(ArgumentError(
            "Invalid chunk index type: $index_type. " *
            "Must be one of: $(join(valid_types, ", ", " or "))"
        ))
    end
    return nothing
end

#-------------------------------------------------------------------------------
# Chunk Index Type Selection
#-------------------------------------------------------------------------------

"""
    select_chunk_index_type(data_size, chunks, maxshape, fill_value) -> Symbol

Automatically select the optimal chunk indexing type based on dataset characteristics.

This matches h5py's selection logic:

# Selection Rules

1. **Single Chunk (Type 1)**: If `chunks == data_size`
   - Entire dataset stored as one chunk
   - Most efficient for small arrays that fit in memory

2. **Fixed Array (Type 3)**: If no unlimited dimensions
   - Default for fixed-size chunked arrays
   - Simple and efficient index structure

3. **Implicit Index (Type 2)**: If fixed size AND fill_value specified
   - Currently NOT used to match h5py behavior
   - Could be enabled in future for optimization

4. **Extensible Array (Type 4)**: If exactly 1 unlimited dimension
   - Efficient for datasets that grow along one axis (e.g., time series)

5. **V2 B-tree (Type 5)**: If 2 or more unlimited dimensions
   - Most flexible for multi-dimensional growth
   - More overhead but handles sparse access patterns

# Arguments
- `data_size::NTuple{N,Int}` - Current data dimensions
- `chunks::NTuple{N,Int}` - Chunk dimensions
- `maxshape::Union{Nothing, NTuple{N,Union{Int,Nothing}}}` - Maximum dimensions
- `fill_value` - Fill value for unallocated chunks

# Returns
Symbol indicating chunk index type: `:single_chunk`, `:implicit_index`,
`:fixed_array`, `:extensible_array`, or `:v2btree`

# Examples

```julia
# Single chunk
select_chunk_index_type((100, 50), (100, 50), nothing, nothing)  # :single_chunk

# Fixed array
select_chunk_index_type((100, 50), (10, 10), nothing, nothing)  # :fixed_array

# Extensible array
select_chunk_index_type((100, 50), (10, 10), (nothing, 50), nothing)  # :extensible_array

# V2 B-tree
select_chunk_index_type((100, 50), (10, 10), (nothing, nothing), nothing)  # :v2btree
```
"""
function select_chunk_index_type(data_size::NTuple{N,Int},
                                 chunks::NTuple{N,Int},
                                 maxshape::Union{Nothing, NTuple{N,Union{Int,Nothing}}},
                                 fill_value) where N

    # Rule 1: Single chunk optimization
    if chunks == data_size
        return :single_chunk
    end

    # Count unlimited dimensions
    n_unlimited = if isnothing(maxshape)
        0  # Fixed size dataset
    else
        count(isnothing, maxshape)
    end

    # Rules 2-5: Select based on unlimited dimensions
    if n_unlimited == 0
        # Fixed size dataset
        # Note: h5py doesn't use implicit index by default even with fill_value
        # We match this behavior for compatibility
        return :fixed_array
    elseif n_unlimited == 1
        return :extensible_array
    else  # 2 or more unlimited
        return :v2btree
    end
end

#-------------------------------------------------------------------------------
# Auto-Chunking
#-------------------------------------------------------------------------------

"""
    auto_chunk_size(data_size, element_size) -> NTuple{N,Int}

Automatically determine chunk size for a dataset.

Uses a target chunk size of 32KB and attempts to create balanced chunks
that divide the array dimensions evenly.

# Arguments
- `data_size::NTuple{N,Int}` - Array dimensions
- `element_size::Int` - Size of each element in bytes

# Returns
Tuple of chunk dimensions

# Examples

```julia
# Small array - use single chunk
auto_chunk_size((100, 50), 4)  # (100, 50)

# Large array - divide into ~32KB chunks
auto_chunk_size((10000, 10000), 4)  # e.g., (100, 100) depending on heuristic
```
"""
function auto_chunk_size(data_size::NTuple{N,Int}, element_size::Int) where N
    target_bytes = 32 * 1024  # 32KB target
    total_elements = prod(data_size)
    target_elements = max(1, target_bytes ÷ element_size)

    # If data fits in target size, use single chunk
    if total_elements <= target_elements
        return data_size
    end

    # Balanced division: divide each dimension by approximately same factor
    # This gives roughly cubic/square chunks which are usually optimal
    scale_factor = (total_elements / target_elements) ^ (1/N)

    chunk_size = ntuple(N) do i
        # Divide each dimension, but keep at least 1
        max(1, round(Int, data_size[i] / scale_factor))
    end

    return chunk_size
end

#-------------------------------------------------------------------------------
# Write Functions (Stubs for now)
#-------------------------------------------------------------------------------

"""
    write_chunked(f::JLDFile, name::String, data; kwargs...)

Write a chunked dataset to a JLD2 file.

This is the low-level function for writing chunked datasets. For most use cases,
prefer using `ChunkedArray` with `jldsave`.

# Arguments
- `f::JLDFile` - Open JLD2 file in write mode
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

    # Basic chunking
    write_chunked(f, "data1", data; chunks=(100, 100))

    # With unlimited dimension
    write_chunked(f, "data2", data; chunks=(100, 100), maxshape=(nothing, 1000))

    # With compression
    write_chunked(f, "data3", data; chunks=(100, 100), filters=:gzip)

    # Auto chunking
    write_chunked(f, "data4", data; chunks=:auto)
end
```
"""
function write_chunked(f::JLDFile, name::String, data::AbstractArray{T,N};
                      chunks::Union{NTuple{N,Int}, Symbol},
                      maxshape::Union{Nothing, NTuple{N,Union{Int,Nothing}}}=nothing,
                      fill_value::Union{Nothing, T}=nothing,
                      indexing::Union{Symbol, Nothing}=nothing,
                      filters=nothing) where {T,N}

    !f.writable && throw(ArgumentError("File must be opened in write mode"))

    # Handle auto-chunking
    chunk_dims = if chunks === :auto
        auto_chunk_size(size(data), sizeof(T))
    else
        chunks
    end

    # Validate parameters
    validate_chunks(chunk_dims, size(data))
    validate_maxshape(maxshape, size(data))
    validate_fill_value(fill_value, T)

    # Select or validate chunk index type
    index_type = if !isnothing(indexing)
        validate_index_type(indexing)
        indexing
    else
        select_chunk_index_type(size(data), chunk_dims, maxshape, fill_value)
    end

    # Dispatch to appropriate writer
    _write_chunked_dispatch(f, name, data, chunk_dims, maxshape, fill_value,
                           index_type, filters)
end

"""
    write_chunked(f::JLDFile, name::String, wca::WriteChunkedArray)

Convenience method to write a WriteChunkedArray directly.

Extracts the configuration from the WriteChunkedArray and calls the main write_chunked function.
"""
function write_chunked(f::JLDFile, name::String, wca::WriteChunkedArray)
    write_chunked(f, name, wca.data;
                 chunks=wca.chunks,
                 maxshape=wca.maxshape,
                 fill_value=wca.fill_value,
                 indexing=wca.indexing,
                 filters=wca.filters)
end

"""
    _write_chunked_dispatch(f, name, data, chunks, maxshape, fill_value, index_type, filters)

Internal dispatch function that computes common structures, routes to the appropriate
chunk index writer, and then writes the unified object header.

This centralizes all common computations (datatype, dataspace, filter pipeline, ODR)
to eliminate code duplication across different indexing types.
"""
function _write_chunked_dispatch(f, name, data::AbstractArray{T,N}, chunks, maxshape, fill_value, index_type, filters) where {T,N}
    # Common computations done once upfront
    odr = JLD2.objodr(data)
    datatype = JLD2.h5type(f, data)
    dataspace = JLD2.WriteDataspace(f, data, odr)
    filter_pipeline = prepare_filter_pipeline(filters, odr, dataspace)
    wsession = JLDWriteSession()

    # Dispatch to appropriate chunk index writer (returns metadata only)
    index_metadata = if index_type == :single_chunk
        write_single_chunk_index(f, data, chunks, odr, filter_pipeline, wsession)
    elseif index_type == :implicit_index
        write_implicit_index(f, data, chunks, fill_value, odr, filter_pipeline, wsession)
    elseif index_type == :fixed_array
        write_fixed_array_index(f, data, chunks, odr, filter_pipeline, wsession)
    elseif index_type == :extensible_array
        write_extensible_array_index(f, data, chunks, maxshape, odr, filter_pipeline, wsession)
    elseif index_type == :v2btree
        write_v2btree_index(f, data, chunks, maxshape, odr, filter_pipeline, wsession)
    elseif index_type == :v1btree
        write_v1btree_index(f, data, chunks, odr, filter_pipeline, wsession)
    else
        throw(ArgumentError("Unknown chunk index type: $index_type"))
    end

    # Write unified object header using metadata from index writer
    return write_chunked_dataset_object(f, name, data, chunks, maxshape, odr, datatype,
                                        dataspace, filter_pipeline, index_metadata)
end

#-------------------------------------------------------------------------------
# Refactored Architecture: Chunk Index Writers (Layer 1)
#-------------------------------------------------------------------------------
# These functions write chunks + index structures and return metadata for
# the object header writer. They don't write object headers themselves.

"""
    ChunkIndexMetadata

Metadata returned by chunk index writers for use by the unified header writer.

# Fields
- `data_address::RelOffset` - Address of chunk index structure
- `layout_version::UInt8` - DataLayout message version (3 or 4)
- `chunk_indexing_type::Union{UInt8,Nothing}` - Type code (1-5, or nothing for v3)
- `layout_params::NamedTuple` - Index-specific DataLayout parameters
- `requires_maxshape::Bool` - Whether dataspace needs max dimensions
- `requires_fill_value::Bool` - Whether fill value message is needed
- `fill_value_params::Union{NamedTuple,Nothing}` - Fill value message parameters
"""
struct ChunkIndexMetadata
    data_address::RelOffset
    layout_version::UInt8
    chunk_indexing_type::Union{UInt8,Nothing}
    layout_params::NamedTuple
    requires_maxshape::Bool
    requires_fill_value::Bool
    fill_value_params::Union{NamedTuple,Nothing}
end

#-------------------------------------------------------------------------------
# Layer 1: Chunk Index Writers (Return Metadata Only)
#-------------------------------------------------------------------------------

"""
    write_single_chunk_index(f::JLDFile, data::AbstractArray{T,N}, chunks, odr, filter_pipeline, wsession) where {T,N}

Write chunks using Single Chunk indexing and return metadata.

Returns `ChunkIndexMetadata` for use by `write_chunked_dataset_object`.
"""
function write_single_chunk_index(f::JLDFile, data::AbstractArray{T,N}, chunks, odr, filter_pipeline, wsession) where {T,N}
    @assert chunks == size(data) "Single chunk requires chunks == size(data)"

    # Prepare chunk data (apply filters if specified)
    chunk_data, chunk_size = if !iscompressed(filter_pipeline)
        io_buf = IOBuffer()
        write_data(io_buf, f, data, odr, datamode(odr), wsession)
        raw_data = take!(io_buf)
        (raw_data, sizeof(raw_data))
    else
        compressed, retcodes = Filters.compress(filter_pipeline, data, odr, f, wsession)
        (compressed, sizeof(compressed))
    end

    # Write chunk data
    chunk_offset = f.end_of_data
    seek(f.io, chunk_offset)
    write(f.io, chunk_data)
    f.end_of_data = chunk_offset + chunk_size

    # Return metadata for DataLayout message
    layout_params = (
        flags = iscompressed(filter_pipeline) ? UInt8(0x02) : UInt8(0x00),
        data_size = chunk_size,
        filters = UInt32(0)
    )

    return ChunkIndexMetadata(
        h5offset(f, chunk_offset),
        UInt8(4),  # version 4
        UInt8(1),  # type 1
        layout_params,
        false,     # no maxshape needed
        false,     # no fill value needed
        nothing
    )
end

"""
    write_implicit_index(f::JLDFile, data::AbstractArray{T,N}, chunks, fill_value, odr, filter_pipeline, wsession) where {T,N}

Write chunks using Implicit Index and return metadata.

Implicit Index stores chunks contiguously in the file starting at a base address.
No explicit index structure is needed - chunk addresses are calculated as:
    chunk_address = base_address + (chunk_index × chunk_size_bytes)

This is the simplest chunk indexing type and is most memory-efficient for sparse datasets
where a fill value is used for unallocated chunks.

# Validation
- Requires fill_value (HDF5 format requirement)
- Does not support filters/compression (HDF5 format requirement)

Returns `ChunkIndexMetadata` for use by `write_chunked_dataset_object`.
"""
function write_implicit_index(f::JLDFile, data::AbstractArray{T,N}, chunks, fill_value, odr, filter_pipeline, wsession) where {T,N}
    # Validate specific requirements for implicit index
    if isnothing(fill_value)
        throw(ArgumentError(
            "Implicit index (Type 2) requires a fill_value. " *
            "Please specify fill_value in WriteChunkedArray constructor."
        ))
    end

    if iscompressed(filter_pipeline)
        throw(UnsupportedFeatureException(
            "Compression/filters are not supported for Implicit Index (Type 2) chunk indexing. " *
            "This is an HDF5 format requirement. Use Fixed Array (Type 3) for filtered chunks."
        ))
    end

    # Calculate chunk grid and allocate contiguous space
    grid_dims = cld.(size(data), chunks)
    n_chunks = prod(grid_dims)
    chunk_size_bytes = prod(chunks) * odr_sizeof(odr)

    chunks_start_offset = f.end_of_data
    f.end_of_data = chunks_start_offset + n_chunks * chunk_size_bytes

    # Write chunks in linear order
    indexer = ChunkLinearIndexer(grid_dims)
    for julia_chunk_idx in CartesianIndices(grid_dims)
        linear_idx = compute_linear_index(indexer, julia_chunk_idx)
        chunk_offset = chunks_start_offset + linear_idx * chunk_size_bytes

        # Extract and pad chunk
        chunk_data_partial, _, _ = extract_chunk_region(data, julia_chunk_idx, chunks)
        chunk_data = pad_chunk_data(chunk_data_partial, chunks, fill_value)

        # Write directly to calculated position
        seek(f.io, chunk_offset)
        io_buf = IOBuffer()
        write_data(io_buf, f, chunk_data, odr, datamode(odr), wsession)
        write(f.io, take!(io_buf))
    end

    # Prepare fill value message parameters
    fill_value_size = odr_sizeof(odr)
    fill_value_bytes = reinterpret(UInt8, [fill_value])
    fill_params = (
        version = 3,
        flags = UInt8(0x20),
        size = UInt32(fill_value_size),
        fill_value = fill_value_bytes
    )

    # Return metadata
    layout_params = (flags = UInt8(0x00),)

    return ChunkIndexMetadata(
        h5offset(f, chunks_start_offset),
        UInt8(4),  # version 4
        UInt8(2),  # type 2
        layout_params,
        false,     # no maxshape needed
        true,      # fill value needed
        fill_params
    )
end

"""
    write_fixed_array_index(f::JLDFile, data::AbstractArray{T,N}, chunks, odr, filter_pipeline, wsession) where {T,N}

Write chunks using Fixed Array indexing and return metadata.

Returns `ChunkIndexMetadata` for use by `write_chunked_dataset_object`.
"""
function write_fixed_array_index(f::JLDFile, data::AbstractArray{T,N}, chunks, odr, filter_pipeline, wsession) where {T,N}
    # Write all chunks in linear order
    chunk_addresses_linear, chunk_sizes_linear, _ = write_all_chunks_linear(
        f, data, chunks, odr, filter_pipeline, wsession; pad_chunks=true, fill_value=zero(T)
    )
    n_chunks = length(chunk_addresses_linear)

    # Write Fixed Array data block
    entry_size = iscompressed(filter_pipeline) ? UInt8(20) : UInt8(8)
    db_size = 4 + 1 + 1 + 8 + n_chunks * entry_size + 4

    data_block_offset = f.end_of_data
    seek(f.io, data_block_offset)
    f.end_of_data = data_block_offset + db_size

    db_cio = begin_checksum_write(f.io, db_size - 4)
    jlwrite(db_cio, FIXED_ARRAY_DATABLOCK_SIGNATURE)
    jlwrite(db_cio, UInt8(0))  # version

    client_id = iscompressed(filter_pipeline) ? UInt8(1) : UInt8(0)
    jlwrite(db_cio, client_id)

    header_addr_pos = position(f.io)
    jlwrite(db_cio, RelOffset(0))  # placeholder

    # Write chunk entries
    for i in 1:n_chunks
        jlwrite(db_cio, chunk_addresses_linear[i])
        if iscompressed(filter_pipeline)
            jlwrite(db_cio, UInt64(chunk_sizes_linear[i]))
            jlwrite(db_cio, UInt32(0))  # filter_mask
        end
    end

    jlwrite(f.io, end_checksum(db_cio))

    # Write Fixed Array header
    header_pos = f.end_of_data
    header_offset = h5offset(f, header_pos)
    seek(f.io, header_pos)

    hdr = FixedArrayHeader(
        UInt8(0), client_id, entry_size, UInt8(0),
        Int64(n_chunks), h5offset(f, data_block_offset)
    )
    header_size = write_fixed_array_header(f.io, hdr)
    f.end_of_data = header_pos + header_size

    # Update data block header address
    current_pos = position(f.io)
    seek(f.io, header_addr_pos)
    jlwrite(f.io, header_offset)
    seek(f.io, current_pos)

    # Return metadata
    layout_params = (
        flags = UInt8(0x00),
        page_bits = UInt8(0)
    )

    return ChunkIndexMetadata(
        header_offset,
        UInt8(4),  # version 4
        UInt8(3),  # type 3
        layout_params,
        false,     # no maxshape needed
        false,     # no fill value needed
        nothing
    )
end

"""
    write_extensible_array_index(f::JLDFile, data::AbstractArray{T,N}, chunks, maxshape, odr, filter_pipeline, wsession) where {T,N}

Write chunks using Extensible Array indexing and return metadata.

Extensible Array is used for datasets with exactly one unlimited dimension.
It uses a multi-level index structure (Header → Index Block → Data Blocks) that can
grow dynamically as the dataset is extended.

For initial implementation, we support cases where all chunks fit in the index block directly
(no data blocks needed).

# Validation
- Requires maxshape with at least one unlimited dimension

Returns `ChunkIndexMetadata` for use by `write_chunked_dataset_object`.
"""
function write_extensible_array_index(f::JLDFile, data::AbstractArray{T,N}, chunks, maxshape, odr, filter_pipeline, wsession) where {T,N}
    # Validate specific requirements for extensible array
    if isnothing(maxshape)
        throw(ArgumentError(
            "Extensible array (Type 4) requires at least one unlimited dimension. " *
            "Provide maxshape with at least one nothing element (e.g., maxshape=(nothing, 100))."
        ))
    end

    n_unlimited = count(isnothing, maxshape)
    if n_unlimited == 0
        throw(ArgumentError(
            "Extensible array (Type 4) requires at least one unlimited dimension. " *
            "Use Fixed Array (Type 3) for fixed-size datasets."
        ))
    end

    # Calculate chunk grid
    grid_dims = cld.(size(data), chunks)
    n_chunks = prod(grid_dims)

    # Determine index block elements
    index_blk_elmts = UInt8(min(n_chunks, 64))
    if n_chunks > Int(index_blk_elmts)
        index_blk_elmts = UInt8(min(n_chunks, 255))
        if n_chunks > 255
            throw(UnsupportedFeatureException(
                "Extensible arrays with >255 chunks require data block support"
            ))
        end
    end

    element_size = iscompressed(filter_pipeline) ? UInt8(20) : UInt8(8)
    client_id = iscompressed(filter_pipeline) ? UInt8(1) : UInt8(0)
    num_data_blks = UInt64(0)

    # Write all chunks
    chunk_addresses, chunk_sizes, _ = write_all_chunks_linear(
        f, data, chunks, odr, filter_pipeline, wsession; pad_chunks=false
    )

    # Write Index Block
    index_block_size = 4 + 1 + 1 + 8 + Int(index_blk_elmts) * Int(element_size) + 4

    index_block_pos = f.end_of_data
    seek(f.io, index_block_pos)
    f.end_of_data = index_block_pos + index_block_size

    header_pos = f.end_of_data
    header_offset = h5offset(f, header_pos)

    ib_cio = begin_checksum_write(f.io, index_block_size - 4)
    jlwrite(ib_cio, EXTENSIBLE_ARRAY_INDEX_BLOCK_SIGNATURE)
    jlwrite(ib_cio, UInt8(0))  # version
    jlwrite(ib_cio, UInt8(client_id))
    jlwrite(ib_cio, header_offset)

    # Write chunk addresses
    for i in 1:Int(index_blk_elmts)
        if i <= n_chunks
            jlwrite(ib_cio, chunk_addresses[i])
            if iscompressed(filter_pipeline)
                jlwrite(ib_cio, UInt64(chunk_sizes[i]))
                jlwrite(ib_cio, UInt32(0))  # filter_mask
            end
        else
            jlwrite(ib_cio, RelOffset(typemax(UInt64)))
            if iscompressed(filter_pipeline)
                jlwrite(ib_cio, UInt64(0))
                jlwrite(ib_cio, UInt32(0))
            end
        end
    end

    jlwrite(f.io, end_checksum(ib_cio))

    # Write Extensible Array Header
    seek(f.io, header_pos)

    hdr = ExtensibleArrayHeader(
        UInt8(0), client_id, element_size, UInt8(32), index_blk_elmts,
        UInt8(16), UInt8(4), UInt8(10),
        UInt64(0), UInt64(0), UInt64(0), UInt64(0),
        UInt64(n_chunks), UInt64(n_chunks),
        h5offset(f, index_block_pos)
    )

    cio = begin_checksum_write(f.io, 4 + jlsizeof(ExtensibleArrayHeader) )
    jlwrite(cio, EXTENSIBLE_ARRAY_HEADER_SIGNATURE)
    jlwrite(cio, hdr)
    jlwrite(f.io, end_checksum(cio))
    f.end_of_data = header_pos + 4 + jlsizeof(ExtensibleArrayHeader) + 4

    # Return metadata
    layout_params = (
        flags = iscompressed(filter_pipeline) ? UInt8(0x02) : UInt8(0x00),
        maxbits = UInt8(32),
        index_elements = index_blk_elmts,
        minpointers = UInt8(4),
        minelements = UInt8(16),
        page_bits = UInt8(10)
    )

    return ChunkIndexMetadata(
        header_offset,
        UInt8(4),  # version 4
        UInt8(4),  # type 4
        layout_params,
        true,      # requires maxshape
        false,     # no fill value needed
        nothing
    )
end

"""
    write_v2btree_index(f::JLDFile, data::AbstractArray{T,N}, chunks, maxshape, odr, filter_pipeline, wsession) where {T,N}

Write chunks using V2 B-tree indexing and return metadata.

V2 B-tree indexing is used for:
- Datasets with 2 or more unlimited dimensions
- Very large datasets with >64K chunks

# Implementation Notes

Simplified implementation with depth=0 (single leaf node):
- All chunk records stored in root leaf node
- Node size: 2048 bytes (matching h5py)
- Record size: 8 + 8*ndims (address + chunk indices)
- Split/merge percentages: 100/40 (matching h5py)

Structure written:
1. Chunks (unpadded, like Extensible Array)
2. Leaf node with chunk records
3. B-tree header
4. Object header with DataLayout message (version 4, type 5)

# Validation
- Requires maxshape with at least one unlimited dimension (typically 2+)

Returns `ChunkIndexMetadata` for use by `write_chunked_dataset_object`.
"""
function write_v2btree_index(f::JLDFile, data::AbstractArray{T,N}, chunks, maxshape, odr, filter_pipeline, wsession) where {T,N}
    # Validate specific requirements for V2 B-tree
    if isnothing(maxshape)
        throw(ArgumentError(
            "V2 B-tree (Type 5) is typically used for datasets with multiple unlimited dimensions. " *
            "Provide maxshape with at least two nothing elements (e.g., maxshape=(nothing, nothing))."
        ))
    end

    n_unlimited = count(isnothing, maxshape)
    if n_unlimited == 0
        throw(ArgumentError(
            "V2 B-tree (Type 5) requires at least one unlimited dimension. " *
            "Use Fixed Array (Type 3) for fixed-size datasets."
        ))
    end

    # Write all chunks and collect records
    chunk_records = write_all_chunks_as_records(f, data, chunks, odr, filter_pipeline, wsession)

    # Write V2 B-tree structure
    header_offset = JLD2.BTrees.write_v2btree_chunked_dataset(
        f, chunk_records, iscompressed(filter_pipeline)
    )

    # Return metadata
    layout_params = (
        flags = iscompressed(filter_pipeline) ? UInt8(0x02) : UInt8(0x00),
        node_size = UInt32(2048),
        splitpercent = UInt8(100),
        mergepercent = UInt8(40)
    )

    return ChunkIndexMetadata(
        header_offset,
        UInt8(4),  # version 4
        UInt8(5),  # type 5
        layout_params,
        true,      # requires maxshape
        false,     # no fill value needed
        nothing
    )
end

"""
    write_v1btree_index(f::JLDFile, data::AbstractArray{T,N}, chunks, odr, datatype, filter_pipeline, wsession) where {T,N}

Write chunks using V1 B-tree indexing and return metadata.

Note: V1 B-tree uses version 3 DataLayout, so this includes writing the object header.
This is kept for compatibility but doesn't follow the new refactored pattern completely.

Returns `ChunkIndexMetadata` for use by `write_chunked_dataset_object`.
"""
function write_v1btree_index(f::JLDFile, data::AbstractArray{T,N}, chunks, odr, filter_pipeline, wsession) where {T,N}
    # Write chunks using V1 B-tree
    chunk_iter = ChunkIterator(data, chunks, odr, filter_pipeline, f, wsession)

    btree, total_chunk_size, num_chunks = JLD2.BTrees.write_chunked_dataset_with_v1btree(
        f, chunk_iter, odr, size(data), chunks
    )

    # Return metadata
    layout_params = (address = btree.root,)

    return ChunkIndexMetadata(
        btree.root,
        UInt8(3),  # version 3
        nothing,   # no type field in v3
        layout_params,
        false,     # no maxshape needed
        true,      # fill value needed for v1 btree
        (flags = UInt8(0x09),)  # fill value flags
    )
end

#-------------------------------------------------------------------------------
# Layer 2: Unified Object Header Writer
#-------------------------------------------------------------------------------

"""
    write_chunked_dataset_object(f::JLDFile, name::String, data::AbstractArray{T,N},
                                  chunks, maxshape, odr, datatype, dataspace,
                                  filter_pipeline, index_metadata::ChunkIndexMetadata) where {T,N}

Write the object header for a chunked dataset using metadata from a chunk index writer.

This is the unified function that handles all object header writing for chunked datasets,
eliminating duplication across different indexing types.

# Arguments
- `f::JLDFile` - File handle
- `name::String` - Dataset name
- `data::AbstractArray{T,N}` - Original data
- `chunks::NTuple{N,Int}` - Chunk dimensions
- `maxshape` - Maximum shape (or nothing)
- `odr` - Object data representation
- `datatype` - HDF5 datatype
- `dataspace` - Write dataspace
- `filter_pipeline` - Filter pipeline
- `index_metadata::ChunkIndexMetadata` - Metadata from chunk index writer

# Returns
Dataset offset (RelOffset)
"""
function write_chunked_dataset_object(f::JLDFile, name::String, data::AbstractArray{T,N},
                                       chunks, maxshape, odr, datatype, dataspace,
                                       filter_pipeline, index_metadata::ChunkIndexMetadata) where {T,N}
    # Calculate payload size for object header
    psz = if index_metadata.requires_maxshape
        # Convert maxshape to HDF5 format
        hdf5_maxshape = convert_maxshape_to_hdf5(maxshape)
        jlsizeof(Val(HmDataspace);
            dataspace.dataspace_type,
            dimensions=dataspace.size,
            flags=0x01,
            max_dimension_size=hdf5_maxshape)
    else
        jlsizeof(Val(HmDataspace); dataspace.dataspace_type, dimensions=dataspace.size)
    end

    psz += jlsizeof(Val(HmDatatype), 1; dt=datatype)

    # Add fill value message if needed
    if index_metadata.requires_fill_value
        psz += jlsizeof(Val(HmFillValue); index_metadata.fill_value_params...)
    end

    # Add filter pipeline message size if present
    if iscompressed(filter_pipeline)
        psz += Filters.pipeline_message_size(filter_pipeline)
    end

    # Add DataLayout message size
    if index_metadata.layout_version == 4
        # Version 4 DataLayout
        dimensionality = UInt8(N + 1)
        dimensions = UInt64.((reverse(chunks)..., odr_sizeof(odr)))

        psz += jlsizeof(Val(HmDataLayout);
            version = 4,
            layout_class = LcChunked,
            dimensionality = dimensionality,
            dimensions = dimensions,
            chunk_indexing_type = index_metadata.chunk_indexing_type,
            data_address = index_metadata.data_address,
            index_metadata.layout_params...
        )
    else
        # Version 3 DataLayout (V1 B-tree)
        dimensionality = UInt8(N + 1)
        dl_dims = UInt32.([reverse(chunks)..., odr_sizeof(odr)])

        psz += jlsizeof(Val(HmDataLayout);
            version = 3,
            layout_class = LcChunked,
            dimensionality = dimensionality,
            dimensions = dl_dims,
            data_address = index_metadata.layout_params.address
        )
    end

    # Add continuation message size
    psz += JLD2.CONTINUATION_MSG_SIZE

    # Calculate full object size
    fullsz = jlsizeof(ObjectStart) + size_size(psz) + psz + 4

    # Allocate space for object header
    obj_header_offset = f.end_of_data
    seek(f.io, obj_header_offset)
    f.end_of_data = obj_header_offset + fullsz

    # Write object header
    cio = begin_checksum_write(f.io, fullsz - 4)
    jlwrite(cio, ObjectStart(size_flag(psz)))
    write_size(cio, psz)

    # Write fill value message first (if needed, for v1 btree compatibility)
    if index_metadata.requires_fill_value
        write_header_message(cio, Val(HmFillValue); index_metadata.fill_value_params...)
    end

    # Write dataspace message
    if index_metadata.requires_maxshape
        hdf5_maxshape = convert_maxshape_to_hdf5(maxshape)
        write_header_message(cio, Val(HmDataspace);
            dataspace.dataspace_type,
            dimensions=dataspace.size,
            flags=0x01,
            max_dimension_size=hdf5_maxshape)
    else
        write_header_message(cio, Val(HmDataspace);
            dataspace.dataspace_type,
            dimensions=dataspace.size)
    end

    # Write dataspace attributes if any
    for attr in dataspace.attributes
        write_header_message(cio, f, attr)
    end

    # Write datatype message
    write_header_message(cio, Val(HmDatatype), 1 | (2*isa(datatype, CommittedDatatype)); dt=datatype)

    # Write filter pipeline message if present
    if iscompressed(filter_pipeline)
        Filters.write_filter_pipeline_message(cio, filter_pipeline)
    end

    # Write DataLayout message
    if index_metadata.layout_version == 4
        dimensionality = UInt8(N + 1)
        dimensions = UInt64.((reverse(chunks)..., odr_sizeof(odr)))

        write_header_message(cio, Val(HmDataLayout);
            version = 4,
            layout_class = LcChunked,
            dimensionality = dimensionality,
            dimensions = dimensions,
            chunk_indexing_type = index_metadata.chunk_indexing_type,
            data_address = index_metadata.data_address,
            index_metadata.layout_params...
        )
    else
        # Version 3 DataLayout (V1 B-tree)
        dimensionality = UInt8(N + 1)
        dl_dims = UInt32.([reverse(chunks)..., odr_sizeof(odr)])

        write_header_message(cio, Val(HmDataLayout);
            version = 3,
            layout_class = LcChunked,
            dimensionality = dimensionality,
            dimensions = dl_dims,
            data_address = index_metadata.layout_params.address
        )
    end

    # Write continuation placeholder and checksum
    write_continuation_placeholder(cio)
    jlwrite(f.io, end_checksum(cio))

    # Link dataset to file hierarchy
    parent_group = f.root_group
    dataset_offset = h5offset(f, obj_header_offset)

    if haskey(parent_group, name)
        @warn "Overwriting existing dataset" name
    end
    parent_group.unwritten_links[name] = HardLink(dataset_offset)

    return dataset_offset
end

#-------------------------------------------------------------------------------
# Architecture Note:
#
# The refactored architecture eliminates the need for separate _write_* wrapper
# functions. Instead:
#
# 1. _write_chunked_dispatch computes all common structures (ODR, datatype,
#    dataspace, filter pipeline) once upfront
#
# 2. It dispatches to the appropriate write_*_index function which writes chunks
#    and index structures, returning ChunkIndexMetadata
#
# 3. _write_chunked_dispatch then calls write_chunked_dataset_object once to
#    write the unified object header
#
# This eliminates ~280 lines of duplicated code across 6 wrapper functions.
# All validation and special logic for each indexing type is now in the
# corresponding write_*_index function.
#-------------------------------------------------------------------------------
