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
    valid_types = [:single_chunk, :implicit_index, :fixed_array, :extensible_array, :v2btree]
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

    # Log selection for debugging (will be removed in production)
    @info "write_chunked: $(name)" size=size(data) chunks=chunk_dims index_type filters

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

Internal dispatch function that routes to the appropriate chunk index writer.
"""
function _write_chunked_dispatch(f, name, data, chunks, maxshape, fill_value, index_type, filters)
    if index_type == :single_chunk
        _write_single_chunk(f, name, data, chunks, filters)
    elseif index_type == :implicit_index
        _write_implicit_index(f, name, data, chunks, maxshape, fill_value, filters)
    elseif index_type == :fixed_array
        _write_fixed_array(f, name, data, chunks, filters)
    elseif index_type == :extensible_array
        _write_extensible_array(f, name, data, chunks, maxshape, filters)
    elseif index_type == :v2btree
        _write_v2btree(f, name, data, chunks, maxshape, filters)
    else
        throw(ArgumentError("Unknown chunk index type: $index_type"))
    end
end

#-------------------------------------------------------------------------------
# Stub Implementations (to be filled in later phases)
#-------------------------------------------------------------------------------

"""
    _write_single_chunk(f::JLDFile, name::String, data::AbstractArray, chunks, filters)

Write a dataset using Single Chunk indexing (Type 1).

Single Chunk is used when the chunk dimensions equal the dataset dimensions,
storing the entire dataset as a single contiguous block.

# Implementation Notes

Creates an HDF5 v4 DataLayout message with:
- Layout Class: Chunked (2)
- Version: 4
- Chunk Index Type: 1 (Single Chunk)
- Flags: 0x02 if filtered, 0x00 otherwise
- Chunk address and size directly in layout message

# Arguments
- `f::JLDFile` - File handle (must be writable)
- `name::String` - Dataset name
- `data::AbstractArray` - Data to write
- `chunks` - Chunk dimensions (must equal size(data))
- `filters` - Optional compression filters
"""
function _write_single_chunk(f::JLDFile, name::String, data::AbstractArray{T,N},
                             chunks, filters) where {T,N}
    # Validate inputs
    @assert chunks == size(data) "Single chunk requires chunks == size(data)"
    @assert f.writable "File must be writable"

    # Get datatype and object data representation
    odr = JLD2.objodr(data)
    datatype = JLD2.h5type(f, data)
    dataspace = JLD2.WriteDataspace(f, data, odr)

    # Prepare filter pipeline
    filter_pipeline = prepare_filter_pipeline(filters, odr, dataspace)

    # Prepare chunk data (apply filters if specified)
    chunk_data, chunk_size = if !iscompressed(filter_pipeline)
        # No compression - write data directly
        io_buf = IOBuffer()
        write_data(io_buf, f, data, odr, datamode(odr), JLDWriteSession())
        raw_data = take!(io_buf)
        (raw_data, sizeof(raw_data))
    else
        # Apply compression filters
        compressed, retcodes = Filters.compress(filter_pipeline, data, odr, f, JLDWriteSession())
        (compressed, sizeof(compressed))
    end

    # Allocate space for chunk data and write it
    chunk_offset = f.end_of_data
    seek(f.io, chunk_offset)
    write(f.io, chunk_data)
    f.end_of_data = chunk_offset + chunk_size

    # Calculate payload size for object header
    psz = jlsizeof(Val(HmDataspace); dataspace.dataspace_type, dimensions=dataspace.size)
    psz += jlsizeof(Val(HmDatatype), 1; dt=datatype)  # 1 for committed flag check

    # DataLayout message size (version 4, type 1)
    layout_flags = iscompressed(filter_pipeline) ? UInt8(0x02) : UInt8(0x00)  # 0x02 = has filters
    dimensionality = UInt8(N + 1)  # +1 for element size dimension

    psz += jlsizeof(Val(HmDataLayout);
        version = 4,
        layout_class = LcChunked,
        flags = layout_flags,
        dimensionality = dimensionality,
        dimensions = UInt64.((reverse(size(data))..., odr_sizeof(odr))),
        chunk_indexing_type = 1,
        data_address = h5offset(f, chunk_offset),
        data_size = chunk_size,
        filters = UInt32(0)  # Filter mask (0 = no disabled filters)
    )

    # Add filter pipeline message size if present
    if iscompressed(filter_pipeline)
        psz += Filters.pipeline_message_size(filter_pipeline)
    end

    # Add continuation message size
    psz += JLD2.CONTINUATION_MSG_SIZE

    # Calculate full object size
    fullsz = jlsizeof(ObjectStart) + size_size(psz) + psz + 4

    # Allocate space for object header and write it
    header_offset = f.end_of_data
    seek(f.io, header_offset)
    f.end_of_data = header_offset + fullsz

    # Write object header
    cio = begin_checksum_write(f.io, fullsz - 4)
    jlwrite(cio, ObjectStart(size_flag(psz)))
    write_size(cio, psz)

    # Write header messages
    write_header_message(cio, Val(HmDataspace);
        dataspace.dataspace_type,
        dimensions=dataspace.size)

    write_header_message(cio, Val(HmDatatype), 1; dt=datatype)

    # Write filter pipeline message if present
    if iscompressed(filter_pipeline)
        Filters.write_filter_pipeline_message(cio, filter_pipeline)
    end

    # Write DataLayout message (version 4, type 1)
    write_header_message(cio, Val(HmDataLayout);
        version = 4,
        layout_class = LcChunked,
        flags = layout_flags,
        dimensionality = dimensionality,
        dimensions = UInt64.((reverse(size(data))..., odr_sizeof(odr))),
        chunk_indexing_type = 1,
        data_address = h5offset(f, chunk_offset),
        data_size = chunk_size,
        filters = UInt32(0)
    )

    # Write continuation placeholder and checksum
    write_continuation_placeholder(cio)
    jlwrite(f.io, end_checksum(cio))

    # Link dataset to file hierarchy
    # Get or create parent group
    parent_group = f.root_group
    dataset_offset = h5offset(f, header_offset)

    # Add link to parent group (must be wrapped in HardLink)
    # Use unwritten_links which gets persisted on file close
    if haskey(parent_group, name)
        @warn "Overwriting existing dataset" name
    end
    parent_group.unwritten_links[name] = HardLink(dataset_offset)

    return dataset_offset
end

"""
    _write_implicit_index(f::JLDFile, name::String, data::AbstractArray, chunks, maxshape, fill_value, filters)

Write a dataset using Implicit Index indexing (Type 2).

Implicit Index stores chunks contiguously in the file starting at a base address.
No explicit index structure is needed - chunk addresses are calculated as:
    chunk_address = base_address + (chunk_index × chunk_size_bytes)

This is the simplest chunk indexing type and is most memory-efficient for sparse datasets
where a fill value is used for unallocated chunks.

# Arguments
- `f::JLDFile` - File handle (must be writable)
- `name::String` - Dataset name
- `data::AbstractArray` - Data to write
- `chunks` - Chunk dimensions
- `maxshape` - Maximum shape (must match data size for Type 2)
- `fill_value` - Fill value for unallocated regions (required for Type 2)
- `filters` - Optional compression filters (not supported for Type 2)
"""
function _write_implicit_index(f::JLDFile, name::String, data::AbstractArray{T,N},
                                chunks, maxshape, fill_value, filters) where {T,N}
    # Validate inputs
    @assert f.writable "File must be writable"
    @assert all(chunks .<= size(data)) "Chunk dimensions must be <= data dimensions"

    # Implicit index requires fill_value
    if isnothing(fill_value)
        throw(ArgumentError(
            "Implicit index (Type 2) requires a fill_value. " *
            "Please specify fill_value in WriteChunkedArray constructor."
        ))
    end

    # Implicit index doesn't support extensible dimensions
    if !isnothing(maxshape) && maxshape != size(data)
        throw(ArgumentError(
            "Implicit index (Type 2) does not support extensible dimensions. " *
            "For extensible datasets, use Extensible Array (Type 4) or V2 B-tree (Type 5)."
        ))
    end

    # Filters not supported for implicit index (HDF5 spec requirement)
    filter_pipeline = Filters.normalize_filters(filters)
    if iscompressed(filter_pipeline)
        throw(UnsupportedFeatureException(
            "Compression/filters are not supported for Implicit Index (Type 2) chunk indexing. " *
            "This is an HDF5 format requirement. Use Fixed Array (Type 3) for filtered chunks."
        ))
    end

    # Get datatype and object data representation
    odr = JLD2.objodr(data)
    datatype = JLD2.h5type(f, data)
    dataspace = JLD2.WriteDataspace(f, data, odr)

    # Calculate chunk grid and allocate contiguous space
    grid_dims = cld.(size(data), chunks)
    n_chunks = prod(grid_dims)
    chunk_size_bytes = prod(chunks) * odr_sizeof(odr)

    chunks_start_offset = f.end_of_data
    f.end_of_data = chunks_start_offset + n_chunks * chunk_size_bytes

    # Write chunks in linear order to pre-allocated space
    wsession = JLDWriteSession()
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

    # Calculate payload size for object header
    psz = jlsizeof(Val(HmDataspace); dataspace.dataspace_type, dimensions=dataspace.size)
    psz += jlsizeof(Val(HmDatatype), 1; dt=datatype)

    # Add fill value message size
    # Version 3 format with flags 0x20 (bit 5 set = fill value defined)
    fill_value_size = odr_sizeof(odr)
    fill_value_bytes = reinterpret(UInt8, [fill_value])
    psz += jlsizeof(Val(HmFillValue); version=3, flags=UInt8(0x20),
                    size=UInt32(fill_value_size), fill_value=fill_value_bytes)

    # DataLayout message size (version 4, type 2)
    # For Implicit Index, dimensions field contains CHUNK dimensions
    layout_flags = UInt8(0x00)  # No filters
    dimensionality = UInt8(N + 1)  # +1 for element size dimension

    psz += jlsizeof(Val(HmDataLayout);
        version = 4,
        layout_class = LcChunked,
        flags = layout_flags,
        dimensionality = dimensionality,
        dimensions = UInt64.((reverse(chunks)..., odr_sizeof(odr))),  # CHUNK dimensions
        chunk_indexing_type = 2,
        data_address = h5offset(f, chunks_start_offset)
    )

    # Add continuation message size
    psz += JLD2.CONTINUATION_MSG_SIZE

    # Calculate full object size
    fullsz = jlsizeof(ObjectStart) + size_size(psz) + psz + 4

    # Allocate space for object header and write it
    obj_header_offset = f.end_of_data
    seek(f.io, obj_header_offset)
    f.end_of_data = obj_header_offset + fullsz

    # Write object header
    cio = begin_checksum_write(f.io, fullsz - 4)
    jlwrite(cio, ObjectStart(size_flag(psz)))
    write_size(cio, psz)

    # Write header messages
    write_header_message(cio, Val(HmDataspace);
        dataspace.dataspace_type,
        dimensions=dataspace.size)

    write_header_message(cio, Val(HmDatatype), 1; dt=datatype)

    # Write fill value message (version 3 format)
    # Flags: 0x20 = bit 5 set (fill value defined)
    write_header_message(cio, Val(HmFillValue);
        version=3,
        flags=UInt8(0x20),
        size=UInt32(fill_value_size),
        fill_value=fill_value_bytes)

    # Write DataLayout message (version 4, type 2)
    write_header_message(cio, Val(HmDataLayout);
        version = 4,
        layout_class = LcChunked,
        flags = layout_flags,
        dimensionality = dimensionality,
        dimensions = UInt64.((reverse(chunks)..., odr_sizeof(odr))),  # CHUNK dimensions
        chunk_indexing_type = 2,
        data_address = h5offset(f, chunks_start_offset)
    )

    # Write continuation message
    write_continuation_placeholder(cio)

    # Write checksum
    jlwrite(f.io, end_checksum(cio))

    # Commit the dataset to the parent group
    dataset_offset = h5offset(f, obj_header_offset)
    parent_group = f.root_group

    # Use unwritten_links which gets persisted on file close
    if haskey(parent_group, name)
        @warn "Overwriting existing dataset" name
    end
    parent_group.unwritten_links[name] = HardLink(dataset_offset)

    return dataset_offset
end

"""
    _write_fixed_array(f::JLDFile, name::String, data::AbstractArray, chunks, filters)

Write a dataset using Fixed Array indexing (Type 3).

Fixed Array is used for fixed-size chunked datasets where multiple chunks are needed.
It stores chunk addresses in a pre-allocated index array for direct lookup.

# Arguments
- `f::JLDFile` - File handle (must be writable)
- `name::String` - Dataset name
- `data::AbstractArray` - Data to write
- `chunks` - Chunk dimensions
- `filters` - Optional compression filters
"""
function _write_fixed_array(f::JLDFile, name::String, data::AbstractArray{T,N},
                            chunks, filters) where {T,N}
    # Validate inputs
    @assert f.writable "File must be writable"
    @assert all(chunks .<= size(data)) "Chunk dimensions must be <= data dimensions"

    # Get datatype and object data representation
    odr = JLD2.objodr(data)
    datatype = JLD2.h5type(f, data)
    dataspace = JLD2.WriteDataspace(f, data, odr)

    # Prepare filter pipeline
    filter_pipeline = prepare_filter_pipeline(filters, odr, dataspace)

    # Write all chunks in linear order
    wsession = JLDWriteSession()
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

    # Write chunk entries in linear order
    for i in 1:n_chunks
        jlwrite(db_cio, chunk_addresses_linear[i])
        if iscompressed(filter_pipeline)
            jlwrite(db_cio, UInt64(chunk_sizes_linear[i]))
            jlwrite(db_cio, UInt32(0))  # filter_mask
        end
    end

    # Write checksum
    jlwrite(f.io, end_checksum(db_cio))

    # Write Fixed Array header
    header_offset = f.end_of_data
    seek(f.io, header_offset)

    hdr = FixedArrayHeader(
        UInt8(0),       # version
        client_id,
        entry_size,
        UInt8(0),       # page_bits (0 = no paging, elements stored directly)
        Int64(n_chunks),
        h5offset(f, data_block_offset)
    )
    header_size = write_fixed_array_header(f.io, hdr)
    f.end_of_data = header_offset + header_size

    # Update data block header address (now that we know it)
    current_pos = position(f.io)
    seek(f.io, header_addr_pos)
    jlwrite(f.io, h5offset(f, header_offset))
    seek(f.io, current_pos)

    # Calculate payload size for object header
    psz = jlsizeof(Val(HmDataspace); dataspace.dataspace_type, dimensions=dataspace.size)
    psz += jlsizeof(Val(HmDatatype), 1; dt=datatype)

    # Add filter pipeline message size if present
    if iscompressed(filter_pipeline)
        psz += Filters.pipeline_message_size(filter_pipeline)
    end

    # DataLayout message size (version 4, type 3)
    # For Fixed Array, dimensions field contains CHUNK dimensions, not array dimensions
    psz += jlsizeof(Val(HmDataLayout);
        version = 4,
        layout_class = LcChunked,
        flags = UInt8(0x00),           # no special flags for Fixed Array
        dimensionality = UInt8(N + 1), # +1 for element size dimension
        dimensions = UInt64.((reverse(chunks)..., odr_sizeof(odr))),  # CHUNK dimensions
        chunk_indexing_type = 3,
        page_bits = UInt8(0),          # no paging
        data_address = h5offset(f, header_offset)
    )

    # Add continuation message size
    psz += JLD2.CONTINUATION_MSG_SIZE

    # Calculate full object size
    fullsz = jlsizeof(ObjectStart) + size_size(psz) + psz + 4

    # Allocate space for object header and write it
    obj_header_offset = f.end_of_data
    seek(f.io, obj_header_offset)
    f.end_of_data = obj_header_offset + fullsz

    # Write object header
    cio = begin_checksum_write(f.io, fullsz - 4)
    jlwrite(cio, ObjectStart(size_flag(psz)))
    write_size(cio, psz)

    # Write header messages
    write_header_message(cio, Val(HmDataspace);
        dataspace.dataspace_type,
        dimensions=dataspace.size)

    write_header_message(cio, Val(HmDatatype), 1; dt=datatype)

    # Write filter pipeline message if present
    if iscompressed(filter_pipeline)
        Filters.write_filter_pipeline_message(cio, filter_pipeline)
    end

    # Write DataLayout message (version 4, type 3)
    # For Fixed Array, dimensions field contains CHUNK dimensions, not array dimensions
    write_header_message(cio, Val(HmDataLayout);
        version = 4,
        layout_class = LcChunked,
        flags = UInt8(0x00),           # no special flags for Fixed Array
        dimensionality = UInt8(N + 1), # +1 for element size dimension
        dimensions = UInt64.((reverse(chunks)..., odr_sizeof(odr))),  # CHUNK dimensions
        chunk_indexing_type = 3,
        page_bits = UInt8(0),          # no paging
        data_address = h5offset(f, header_offset)
    )

    # Write continuation placeholder and checksum
    write_continuation_placeholder(cio)
    jlwrite(f.io, end_checksum(cio))

    # Link dataset to file hierarchy
    parent_group = f.root_group
    dataset_offset = h5offset(f, obj_header_offset)

    # Add link to parent group
    if haskey(parent_group, name)
        @warn "Overwriting existing dataset" name
    end
    parent_group.unwritten_links[name] = HardLink(dataset_offset)

    return dataset_offset
end

"""
    _write_extensible_array(f::JLDFile, name::String, data::AbstractArray, chunks, maxshape, filters)

Write a dataset using Extensible Array indexing (Type 4).

Extensible Array is used for datasets with exactly one unlimited dimension.
It uses a multi-level index structure (Header → Index Block → Data Blocks) that can
grow dynamically as the dataset is extended.

For initial implementation, we support cases where all chunks fit in the index block directly
(no data blocks needed).

# Arguments
- `f::JLDFile` - File handle (must be writable)
- `name::String` - Dataset name
- `data::AbstractArray` - Data to write
- `chunks` - Chunk dimensions
- `maxshape` - Maximum shape (must have exactly 1 unlimited dimension)
- `filters` - Optional compression filters (not yet supported)
"""
function _write_extensible_array(f::JLDFile, name::String, data::AbstractArray{T,N},
                                  chunks, maxshape, filters) where {T,N}
    # Validate inputs
    @assert f.writable "File must be writable"
    @assert all(chunks .<= size(data)) "Chunk dimensions must be <= data dimensions"

    # Extensible array requires exactly 1 unlimited dimension
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

    # Get datatype and object data representation
    odr = JLD2.objodr(data)
    datatype = JLD2.h5type(f, data)
    dataspace = JLD2.WriteDataspace(f, data, odr)

    # Prepare filter pipeline for chunk compression
    filter_pipeline = prepare_filter_pipeline(filters, odr, dataspace)

    # Calculate chunk grid dimensions
    grid_dims = cld.(size(data), chunks)
    n_chunks = prod(grid_dims)

    # Calculate chunk size in bytes
    chunk_size_bytes = prod(chunks) * odr_sizeof(odr)

    # Determine index block elements (must fit all chunks in simplified implementation)
    index_blk_elmts = UInt8(min(n_chunks, 64))
    if n_chunks > Int(index_blk_elmts)
        index_blk_elmts = UInt8(min(n_chunks, 255))
        if n_chunks > 255
            throw(UnsupportedFeatureException(
                "Extensible arrays with >255 chunks require data block support, " *
                "which is not yet implemented. Current chunk count: $n_chunks"
            ))
        end
    end

    # Calculate element size and client ID based on filtering
    element_size = iscompressed(filter_pipeline) ? UInt8(16) : UInt8(8)
    client_id = iscompressed(filter_pipeline) ? UInt8(1) : UInt8(0)

    # No data blocks in simplified implementation
    num_data_blks = UInt64(0)

    # Convert maxshape to HDF5 format (needed before psz calculation)
    hdf5_maxshape = convert_maxshape_to_hdf5(maxshape)

    # Step 1: Write all chunks in linear order (unpadded for extensible arrays)
    wsession = JLDWriteSession()
    chunk_addresses, chunk_sizes, _ = write_all_chunks_linear(
        f, data, chunks, odr, filter_pipeline, wsession; pad_chunks=false
    )

    # Step 2: Write Index Block
    index_block_size = 4 +  # signature
                       1 +  # version
                       1 +  # client_id
                       8 +  # header address (back-reference)
                       Int(index_blk_elmts) * Int(element_size) +  # direct elements
                       4    # checksum (no data block addresses in simplified implementation)

    index_block_offset = f.end_of_data
    seek(f.io, index_block_offset)
    f.end_of_data = index_block_offset + index_block_size

    # Object header will follow here! (directly after)
    header_offset = f.end_of_data

    # Write index block
    ib_cio = begin_checksum_write(f.io, index_block_size - 4)

    # Signature
    jlwrite(ib_cio, EXTENSIBLE_ARRAY_INDEX_BLOCK_SIGNATURE)

    # Version
    jlwrite(ib_cio, UInt8(0))

    # Client ID
    jlwrite(ib_cio, UInt8(client_id))

    # Header address (placeholder, will be updated)
    jlwrite(ib_cio, h5offset(f, header_offset))

    # Write direct chunk addresses (and sizes + filter_mask if filtered) in linear order
    for i in 1:Int(index_blk_elmts)
        if i <= n_chunks
            jlwrite(ib_cio, chunk_addresses[i])
            if iscompressed(filter_pipeline)
                jlwrite(ib_cio, UInt32(chunk_sizes[i]))  # Write size as UInt32
                jlwrite(ib_cio, UInt32(0))  # filter_mask (0 = all filters applied)
            end
        else
            # Undefined chunks (shouldn't happen with our current logic)
            jlwrite(ib_cio, RelOffset(typemax(UInt64)))
            if iscompressed(filter_pipeline)
                jlwrite(ib_cio, UInt32(0))  # size
                jlwrite(ib_cio, UInt32(0))  # filter_mask
            end
        end
    end

    # Write data block addresses (none in simple case)
    for i in 1:Int(num_data_blks)
        jlwrite(ib_cio, RelOffset(typemax(UInt64)))
    end
    @info index_block_size position(ib_cio) index_block_offset
    # Write checksum
    jlwrite(f.io, end_checksum(ib_cio))

    # Step 3: Write Extensible Array Header
    seek(f.io, header_offset)

    hdr = ExtensibleArrayHeader(
        UInt8(0),           # version
        client_id,
        element_size,
        UInt8(32),          # max_nelmts_bits (support up to 2^32 chunks)
        index_blk_elmts,
        UInt8(16),          # data_blk_min_elmts
        UInt8(4),           # secondary_blk_min_data_ptrs
        UInt8(10),          # max_dblk_page_nelmts_bits
        UInt64(0),          # num_secondary_blks (no secondary blocks)
        UInt64(0),          # secondary_blk_size
        UInt64(0),          # num_data_blks (no data blocks)
        UInt64(0),          # data_blk_size
        UInt64(n_chunks),   # max_index_set
        UInt64(n_chunks),   # nelmts (all chunks allocated)
        h5offset(f, index_block_offset)
    )
    header_size = write_extensible_array_header(f.io, hdr)
    f.end_of_data = header_offset + header_size

    # Update index block header address (now that we know it)
    #current_pos = position(f.io)
    #seek(f.io, header_addr_pos)
    #jlwrite(f.io, h5offset(f, header_offset))
    # Update checksum !!
    #update_checksum(f.io, index_block_offset, index_block_offset + index_block_size-4)
    #seek(f.io, current_pos)

    # Step 4: Write Object Header with DataLayout message
    # Calculate payload size for object header
    # Extensible array requires max dimensions in dataspace (flags bit 0 set)
    psz = jlsizeof(Val(HmDataspace);
        dataspace.dataspace_type,
        dimensions=dataspace.size,
        flags=0x01,  # Bit 0 indicates max dimensions are present
        max_dimension_size=hdf5_maxshape)
    psz += jlsizeof(Val(HmDatatype), 1; dt=datatype)

    # Add filter pipeline message size if present
    if iscompressed(filter_pipeline)
        psz += Filters.pipeline_message_size(filter_pipeline)
    end

    # DataLayout message size (version 4, type 4)
    psz += jlsizeof(Val(HmDataLayout);
        version = 4,
        layout_class = LcChunked,
        flags = iscompressed(filter_pipeline) ? UInt8(0x02) : UInt8(0x00),  # 0x02 indicates filters
        dimensionality = UInt8(N + 1),  # +1 for element size dimension
        dimensions = UInt64.((reverse(chunks)..., odr_sizeof(odr))),  # CHUNK dimensions
        chunk_indexing_type = 4,
        maxbits = UInt8(32),           # max_nelmts_bits
        index_elements = index_blk_elmts,
        minpointers = UInt8(4),        # secondary_blk_min_data_ptrs
        minelements = UInt8(16),       # data_blk_min_elmts
        page_bits = UInt8(10),         # max_dblk_page_nelmts_bits
        data_address = h5offset(f, header_offset)
    )

    # Add continuation message size
    psz += JLD2.CONTINUATION_MSG_SIZE

    # Calculate full object size
    fullsz = jlsizeof(ObjectStart) + size_size(psz) + psz + 4

    # Allocate space for object header and write it
    obj_header_offset = f.end_of_data
    seek(f.io, obj_header_offset)
    f.end_of_data = obj_header_offset + fullsz

    # Write object header
    cio = begin_checksum_write(f.io, fullsz - 4)
    jlwrite(cio, ObjectStart(size_flag(psz)))
    write_size(cio, psz)

    # Write header messages
    # Extensible array requires max dimensions in dataspace
    write_header_message(cio, Val(HmDataspace);
        dataspace.dataspace_type,
        dimensions=dataspace.size,
        flags=0x01,  # Bit 0 indicates max dimensions are present
        max_dimension_size=hdf5_maxshape)

    write_header_message(cio, Val(HmDatatype), 1; dt=datatype)

    # Write filter pipeline message if present
    if iscompressed(filter_pipeline)
        Filters.write_filter_pipeline_message(cio, filter_pipeline)
    end

    # Write DataLayout message (version 4, type 4)
    write_header_message(cio, Val(HmDataLayout);
        version = 4,
        layout_class = LcChunked,
        flags = iscompressed(filter_pipeline) ? UInt8(0x02) : UInt8(0x00),  # 0x02 indicates filters
        dimensionality = UInt8(N + 1),  # +1 for element size dimension
        dimensions = UInt64.((reverse(chunks)..., odr_sizeof(odr))),  # CHUNK dimensions
        chunk_indexing_type = 4,
        maxbits = UInt8(32),           # max_nelmts_bits
        index_elements = index_blk_elmts,
        minpointers = UInt8(4),        # secondary_blk_min_data_ptrs
        minelements = UInt8(16),       # data_blk_min_elmts
        page_bits = UInt8(10),         # max_dblk_page_nelmts_bits
        data_address = h5offset(f, header_offset)
    )

    # Write continuation placeholder and checksum
    write_continuation_placeholder(cio)
    jlwrite(f.io, end_checksum(cio))

    # Link dataset to file hierarchy
    parent_group = f.root_group
    dataset_offset = h5offset(f, obj_header_offset)

    # Add link to parent group
    if haskey(parent_group, name)
        @warn "Overwriting existing dataset" name
    end
    parent_group.unwritten_links[name] = HardLink(dataset_offset)

    return dataset_offset
end

"""
    _write_v2btree(f::JLDFile, name::String, data::AbstractArray, chunks, maxshape, filters)

Write a chunked array with V2 B-tree indexing (Type 5).

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

# Chunk Record Format

Each record contains:
- chunk_address (UInt64) - file offset
- chunk_index_dim_N (UInt64) for each dimension - in HDF5 order, 0-based

For a 2D dataset, record_size = 24 bytes:
- chunk_address (8 bytes)
- chunk_index_row (8 bytes) - in HDF5 order
- chunk_index_col (8 bytes)
"""
function _write_v2btree(f::JLDFile, name::String, data::AbstractArray{T,N},
                        chunks, maxshape, filters) where {T,N}
    # Validate inputs
    @assert f.writable "File must be writable"
    # Note: chunks can be larger than data dimensions (partial chunks allowed)

    # V2 B-tree typically used for 2+ unlimited dimensions
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


    # Get datatype and object data representation
    odr = JLD2.objodr(data)
    datatype = JLD2.h5type(f, data)
    dataspace = JLD2.WriteDataspace(f, data, odr)

    # Prepare filter pipeline for chunk compression
    filter_pipeline = prepare_filter_pipeline(filters, odr, dataspace)

    # Calculate chunk grid dimensions
    grid_dims = cld.(size(data), chunks)
    n_chunks = prod(grid_dims)

    # Calculate chunk size in bytes
    chunk_size_bytes = prod(chunks) * odr_sizeof(odr)

    # Convert maxshape to HDF5 format (needed before psz calculation)
    hdf5_maxshape = convert_maxshape_to_hdf5(maxshape)

    # Step 1: Write all chunks and collect records
    wsession = JLDWriteSession()
    chunk_records = write_all_chunks_as_records(f, data, chunks, odr, filter_pipeline, wsession)

    # Step 2: Write V2 B-tree structure using BTrees module
    header_offset = JLD2.BTrees.write_v2btree_chunked_dataset(
        f, chunk_records, iscompressed(filter_pipeline)
    )

    # Step 3: Write Object Header with DataLayout message
    # Calculate payload size for object header
    # V2 B-tree requires max dimensions in dataspace (flags bit 0 set)
    psz = jlsizeof(Val(HmDataspace);
        dataspace.dataspace_type,
        dimensions=dataspace.size,
        flags=0x01,  # Bit 0 indicates max dimensions are present
        max_dimension_size=hdf5_maxshape)
    psz += jlsizeof(Val(HmDatatype), 1; dt=datatype)

    # DataLayout message size (version 4, type 5)
    psz += jlsizeof(Val(HmDataLayout);
        version = 4,
        layout_class = LcChunked,
        flags = iscompressed(filter_pipeline) ? UInt8(0x02) : UInt8(0x00),  # 0x02 indicates filters
        dimensionality = UInt8(N + 1),  # +1 for element size dimension
        dimensions = UInt64.((reverse(chunks)..., odr_sizeof(odr))),  # CHUNK dimensions
        chunk_indexing_type = 5,
        node_size = UInt32(2048),      # matching h5py
        splitpercent = UInt8(100),     # matching h5py
        mergepercent = UInt8(40),      # matching h5py
        data_address = h5offset(f, header_offset)
    )

    # Add filter pipeline message size if filters are present
    if iscompressed(filter_pipeline)
        psz += Filters.pipeline_message_size(filter_pipeline)
    end

    # Add continuation message size
    psz += JLD2.CONTINUATION_MSG_SIZE

    # Calculate full object size
    fullsz = jlsizeof(ObjectStart) + size_size(psz) + psz + 4

    # Allocate space for object header and write it
    obj_header_offset = f.end_of_data
    seek(f.io, obj_header_offset)
    f.end_of_data = obj_header_offset + fullsz

    # Write object header
    cio = begin_checksum_write(f.io, fullsz - 4)
    jlwrite(cio, ObjectStart(size_flag(psz)))
    write_size(cio, psz)

    # Write header messages
    # V2 B-tree requires max dimensions in dataspace
    write_header_message(cio, Val(HmDataspace);
        dataspace.dataspace_type,
        dimensions=dataspace.size,
        flags=0x01,  # Bit 0 indicates max dimensions are present
        max_dimension_size=hdf5_maxshape)

    write_header_message(cio, Val(HmDatatype), 1; dt=datatype)

    # Write DataLayout message (version 4, type 5)
    write_header_message(cio, Val(HmDataLayout);
        version = 4,
        layout_class = LcChunked,
        flags = iscompressed(filter_pipeline) ? UInt8(0x02) : UInt8(0x00),  # 0x02 indicates filters
        dimensionality = UInt8(N + 1),  # +1 for element size dimension
        dimensions = UInt64.((reverse(chunks)..., odr_sizeof(odr))),  # CHUNK dimensions
        chunk_indexing_type = 5,
        node_size = UInt32(2048),      # matching h5py
        splitpercent = UInt8(100),     # matching h5py
        mergepercent = UInt8(40),      # matching h5py
        data_address = h5offset(f, header_offset)
    )

    # Write filter pipeline message if filters are present
    if iscompressed(filter_pipeline)
        Filters.write_filter_pipeline_message(cio, filter_pipeline)
    end

    # Write continuation placeholder and checksum
    write_continuation_placeholder(cio)
    jlwrite(f.io, end_checksum(cio))

    # Link dataset to file hierarchy
    parent_group = f.root_group
    dataset_offset = h5offset(f, obj_header_offset)

    # Add link to parent group
    if haskey(parent_group, name)
        @warn "Overwriting existing dataset" name
    end
    parent_group.unwritten_links[name] = HardLink(dataset_offset)

    return dataset_offset
end
