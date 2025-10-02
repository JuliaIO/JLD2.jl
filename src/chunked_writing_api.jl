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
    odr = objodr(data)
    datatype = h5type(f, data)
    dataspace = WriteDataspace(f, data, odr)

    # Normalize filters (handles symbols like :gzip, booleans, etc.)
    normalized_filters = isnothing(filters) ? nothing : Filters.normalize_filters(filters)

    # Prepare chunk data (apply filters if specified)
    chunk_data, chunk_size, filter_pipeline = if isnothing(normalized_filters)
        # No compression - write data directly
        io_buf = IOBuffer()
        write_data(io_buf, f, data, odr, datamode(odr), JLDWriteSession())
        raw_data = take!(io_buf)
        (raw_data, sizeof(raw_data), nothing)
    else
        # Apply compression filters
        local_filters = FilterPipeline(map(normalized_filters) do filter
            Filters.set_local(filter, odr, dataspace, ())
        end)
        compressed, retcodes = Filters.compress(local_filters, data, odr, f, JLDWriteSession())
        (compressed, sizeof(compressed), local_filters)
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
    layout_flags = isnothing(filter_pipeline) ? UInt8(0x00) : UInt8(0x02)  # 0x02 = has filters
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
    if !isnothing(filter_pipeline)
        psz += Filters.pipeline_message_size(filter_pipeline)
    end

    # Add continuation message size
    psz += CONTINUATION_MSG_SIZE

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
    if !isnothing(filter_pipeline)
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
    if !isnothing(filters)
        throw(UnsupportedFeatureException(
            "Compression/filters are not supported for Implicit Index (Type 2) chunk indexing. " *
            "This is an HDF5 format requirement. Use Fixed Array (Type 3) for filtered chunks."
        ))
    end

    # Get datatype and object data representation
    odr = objodr(data)
    datatype = h5type(f, data)
    dataspace = WriteDataspace(f, data, odr)

    # Calculate chunk grid dimensions
    grid_dims = cld.(size(data), chunks)
    n_chunks = prod(grid_dims)

    # Calculate chunk size in bytes
    chunk_size_bytes = prod(chunks) * odr_sizeof(odr)

    # Allocate contiguous space for all chunks
    chunks_start_offset = f.end_of_data
    total_chunks_size = n_chunks * chunk_size_bytes
    f.end_of_data = chunks_start_offset + total_chunks_size

    # Write session for tracking
    wsession = JLDWriteSession()

    # Convert grid_dims to HDF5 order for linear indexing
    grid_dims_hdf5 = reverse(grid_dims)
    ndims_hdf5 = length(grid_dims_hdf5)

    # Precompute down_chunks for HDF5 ordering
    # This matches compute_chunk_index from fixed_array.jl
    down_chunks = zeros(Int, ndims_hdf5)
    acc = 1
    for i in ndims_hdf5:-1:1
        down_chunks[i] = acc
        acc *= grid_dims_hdf5[i]
    end

    # Iterate through all chunks in Julia order, but write them in HDF5 linear order
    for julia_chunk_idx in CartesianIndices(grid_dims)
        # Convert Julia chunk coordinates to HDF5 coordinates (0-based, reversed)
        hdf5_coords = reverse(Tuple(julia_chunk_idx) .- 1)

        # Compute linear index using same logic as compute_chunk_index
        linear_idx = 0
        for i in 1:ndims_hdf5
            linear_idx += down_chunks[i] * hdf5_coords[i]
        end

        # Calculate chunk address based on linear index
        chunk_offset = chunks_start_offset + linear_idx * chunk_size_bytes

        # Calculate data range for this chunk in Julia coordinates
        chunk_coords = Tuple(julia_chunk_idx)
        start_idx = (chunk_coords .- 1) .* chunks .+ 1
        end_idx = min.(start_idx .+ chunks .- 1, size(data))

        # Extract chunk data
        chunk_data_partial = data[map(:, start_idx, end_idx)...]

        # Pad partial chunks to full chunk size with fill_value
        # HDF5 requires all chunks to be the same size
        actual_chunk_size = size(chunk_data_partial)
        chunk_data = if actual_chunk_size == chunks
            chunk_data_partial
        else
            # Create full-size chunk filled with fill_value
            full_chunk = fill(fill_value, chunks)
            # Copy actual data into the full chunk
            ranges = ntuple(i -> 1:actual_chunk_size[i], N)
            full_chunk[ranges...] = chunk_data_partial
            full_chunk
        end

        # Write chunk to its calculated position
        seek(f.io, chunk_offset)
        io_buf = IOBuffer()
        write_data(io_buf, f, chunk_data, odr, datamode(odr), wsession)
        chunk_bytes = take!(io_buf)
        write(f.io, chunk_bytes)
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
    psz += CONTINUATION_MSG_SIZE

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
    odr = objodr(data)
    datatype = h5type(f, data)
    dataspace = WriteDataspace(f, data, odr)

    # Normalize filters (convert symbols like :gzip to FilterPipeline)
    normalized_filters = isnothing(filters) ? nothing : Filters.normalize_filters(filters)

    # Calculate chunk grid dimensions
    grid_dims = cld.(size(data), chunks)
    n_chunks = prod(grid_dims)

    # Prepare filter pipeline for chunk compression
    filter_pipeline = if isnothing(normalized_filters)
        nothing
    else
        FilterPipeline(map(normalized_filters) do filter
            Filters.set_local(filter, odr, dataspace, ())
        end)
    end

    # Write all chunks and collect their addresses and sizes
    chunk_addresses = Array{RelOffset,N}(undef, grid_dims...)
    chunk_sizes = isnothing(filter_pipeline) ? nothing : Array{UInt32,N}(undef, grid_dims...)
    wsession = JLDWriteSession()

    for chunk_idx in CartesianIndices(grid_dims)
        # Calculate data range for this chunk
        chunk_coords = Tuple(chunk_idx)
        start_idx = (chunk_coords .- 1) .* chunks .+ 1
        end_idx = min.(start_idx .+ chunks .- 1, size(data))

        # Extract chunk data
        chunk_data_partial = data[map(:, start_idx, end_idx)...]

        # Pad partial chunks to full chunk size with zeros
        # HDF5 requires all chunks to be the same size
        actual_chunk_size = size(chunk_data_partial)
        chunk_data = if actual_chunk_size == chunks
            chunk_data_partial
        else
            # Create full-size chunk filled with zeros
            full_chunk = zeros(T, chunks)
            # Copy actual data into the full chunk
            ranges = ntuple(i -> 1:actual_chunk_size[i], N)
            full_chunk[ranges...] = chunk_data_partial
            full_chunk
        end

        # Prepare chunk bytes (with optional compression)
        chunk_bytes = if isnothing(filter_pipeline)
            # No compression - serialize directly
            io_buf = IOBuffer()
            write_data(io_buf, f, chunk_data, odr, datamode(odr), wsession)
            take!(io_buf)
        else
            # Apply compression
            compressed, _ = Filters.compress(filter_pipeline, chunk_data, odr, f, wsession)
            compressed
        end

        # Write chunk to file
        chunk_offset = f.end_of_data
        seek(f.io, chunk_offset)
        write(f.io, chunk_bytes)
        f.end_of_data = chunk_offset + sizeof(chunk_bytes)

        # Store chunk address and size
        chunk_addresses[chunk_idx] = h5offset(f, chunk_offset)
        if !isnothing(filter_pipeline)
            chunk_sizes[chunk_idx] = UInt32(sizeof(chunk_bytes))
        end
    end

    # Write Fixed Array data block
    # Calculate data block size
    # Entry size: 8 bytes (address) for unfiltered, 12 bytes (address + size) for filtered
    entry_size = isnothing(filter_pipeline) ? UInt8(8) : UInt8(12)
    db_size = 4 +  # signature
              1 +  # version
              1 +  # client_id
              8 +  # header address (will be filled later)
              n_chunks * entry_size +  # chunk addresses (and sizes if filtered)
              4    # checksum

    data_block_offset = f.end_of_data
    seek(f.io, data_block_offset)
    f.end_of_data = data_block_offset + db_size

    # Write data block (will update header address later)
    db_cio = begin_checksum_write(f.io, db_size - 4)

    # Signature
    jlwrite(db_cio, FIXED_ARRAY_DATABLOCK_SIGNATURE)

    # Version
    jlwrite(db_cio, UInt8(0))

    # Client ID (0 = non-filtered, 1 = filtered chunks)
    client_id = isnothing(filter_pipeline) ? UInt8(0) : UInt8(1)
    jlwrite(db_cio, client_id)

    # Header address (placeholder, will be updated)
    header_addr_pos = position(f.io)
    jlwrite(db_cio, RelOffset(0))

    # Write chunk addresses in HDF5 linear order
    # HDF5 uses row-major-like ordering where the first dimension varies fastest
    # We need to write chunks in the correct linear order that matches compute_chunk_index
    chunk_addresses_linear = Vector{RelOffset}(undef, n_chunks)

    # Convert grid_dims to HDF5 order (reversed)
    grid_dims_hdf5 = reverse(grid_dims)

    # Compute linear index for each chunk and store at that position
    # This must match the compute_chunk_index function in fixed_array.jl
    ndims_hdf5 = length(grid_dims_hdf5)

    # Precompute down_chunks for HDF5 ordering
    down_chunks = zeros(Int, ndims_hdf5)
    acc = 1
    for i in ndims_hdf5:-1:1
        down_chunks[i] = acc
        acc *= grid_dims_hdf5[i]
    end

    # Also create linear array for sizes if filtered
    chunk_sizes_linear = isnothing(filter_pipeline) ? nothing : Vector{UInt32}(undef, n_chunks)

    for julia_idx in CartesianIndices(grid_dims)
        # Convert Julia chunk coordinates to HDF5 coordinates (0-based, reversed)
        hdf5_coords = reverse(Tuple(julia_idx) .- 1)

        # Compute linear index using same logic as compute_chunk_index
        linear_idx = 0
        for i in 1:ndims_hdf5
            linear_idx += down_chunks[i] * hdf5_coords[i]
        end

        chunk_addresses_linear[linear_idx + 1] = chunk_addresses[julia_idx]
        if !isnothing(filter_pipeline)
            chunk_sizes_linear[linear_idx + 1] = chunk_sizes[julia_idx]
        end
    end

    # Write addresses and sizes in linear order
    for i in 1:n_chunks
        jlwrite(db_cio, chunk_addresses_linear[i])
        if !isnothing(filter_pipeline)
            jlwrite(db_cio, chunk_sizes_linear[i])
        end
    end

    # Write checksum
    jlwrite(f.io, end_checksum(db_cio))

    # Write Fixed Array header
    header_offset = f.end_of_data
    header_size = 4 +  # signature
                  1 +  # version
                  1 +  # client_id
                  1 +  # entry_size
                  1 +  # page_bits
                  8 +  # max_num_entries (Length type)
                  8 +  # data_block_address
                  4    # checksum

    seek(f.io, header_offset)
    f.end_of_data = header_offset + header_size

    hdr_cio = begin_checksum_write(f.io, header_size - 4)

    # Signature
    jlwrite(hdr_cio, FIXED_ARRAY_HEADER_SIGNATURE)

    # Version
    jlwrite(hdr_cio, UInt8(0))

    # Client ID
    jlwrite(hdr_cio, client_id)

    # Entry size (8 bytes for RelOffset)
    jlwrite(hdr_cio, entry_size)

    # Page bits (0 = no paging for simplicity)
    page_bits = UInt8(0)
    jlwrite(hdr_cio, page_bits)

    # Max num entries (total number of chunks)
    jlwrite(hdr_cio, Length(n_chunks))

    # Data block address
    jlwrite(hdr_cio, h5offset(f, data_block_offset))

    # Write checksum
    jlwrite(f.io, end_checksum(hdr_cio))

    # Update data block header address (now that we know it)
    current_pos = position(f.io)
    seek(f.io, header_addr_pos)
    jlwrite(f.io, h5offset(f, header_offset))
    seek(f.io, current_pos)

    # Calculate payload size for object header
    psz = jlsizeof(Val(HmDataspace); dataspace.dataspace_type, dimensions=dataspace.size)
    psz += jlsizeof(Val(HmDatatype), 1; dt=datatype)

    # Add filter pipeline message size if present
    if !isnothing(filter_pipeline)
        psz += Filters.pipeline_message_size(filter_pipeline)
    end

    # DataLayout message size (version 4, type 3)
    # For Fixed Array, dimensions field contains CHUNK dimensions, not array dimensions
    layout_flags = isnothing(filter_pipeline) ? UInt8(0x00) : UInt8(0x02)
    dimensionality = UInt8(N + 1)  # +1 for element size dimension

    psz += jlsizeof(Val(HmDataLayout);
        version = 4,
        layout_class = LcChunked,
        flags = layout_flags,
        dimensionality = dimensionality,
        dimensions = UInt64.((reverse(chunks)..., odr_sizeof(odr))),  # CHUNK dimensions, not array dimensions
        chunk_indexing_type = 3,
        page_bits = page_bits,
        data_address = h5offset(f, header_offset)
    )

    # Add continuation message size
    psz += CONTINUATION_MSG_SIZE

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
    if !isnothing(filter_pipeline)
        Filters.write_filter_pipeline_message(cio, filter_pipeline)
    end

    # Write DataLayout message (version 4, type 3)
    # For Fixed Array, dimensions field contains CHUNK dimensions, not array dimensions
    write_header_message(cio, Val(HmDataLayout);
        version = 4,
        layout_class = LcChunked,
        flags = layout_flags,
        dimensionality = dimensionality,
        dimensions = UInt64.((reverse(chunks)..., odr_sizeof(odr))),  # CHUNK dimensions, not array dimensions
        chunk_indexing_type = 3,
        page_bits = page_bits,
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
    odr = objodr(data)
    datatype = h5type(f, data)
    dataspace = WriteDataspace(f, data, odr)

    # Normalize filters (convert symbols like :gzip to FilterPipeline)
    normalized_filters = isnothing(filters) ? nothing : Filters.normalize_filters(filters)

    # Prepare filter pipeline for chunk compression
    filter_pipeline = if isnothing(normalized_filters)
        nothing
    else
        FilterPipeline(map(normalized_filters) do filter
            Filters.set_local(filter, odr, dataspace, ())
        end)
    end

    # Calculate chunk grid dimensions
    grid_dims = cld.(size(data), chunks)
    n_chunks = prod(grid_dims)

    # Calculate chunk size in bytes
    chunk_size_bytes = prod(chunks) * odr_sizeof(odr)

    # Extensible Array parameters (matching h5py choices)
    # These determine the index structure size and growth behavior
    max_nelmts_bits = UInt8(32)  # Support up to 2^32 chunks
    index_blk_elmts = UInt8(min(n_chunks, 64))  # Direct elements in index block
    data_blk_min_elmts = UInt8(16)  # Minimum elements per data block
    secondary_blk_min_data_ptrs = UInt8(4)  # Minimum pointers per secondary block
    max_dblk_page_nelmts_bits = UInt8(10)  # Page size bits

    # For initial implementation, ensure all chunks fit in index block
    if n_chunks > Int(index_blk_elmts)
        # Could implement data blocks here in future
        # For now, increase index_blk_elmts to fit all chunks
        index_blk_elmts = UInt8(min(n_chunks, 255))  # Max UInt8
        if n_chunks > 255
            throw(UnsupportedFeatureException(
                "Extensible arrays with >255 chunks require data block support, " *
                "which is not yet implemented. Current chunk count: $n_chunks"
            ))
        end
    end

    # Current state (all chunks allocated)
    nelmts = UInt64(n_chunks)
    max_index_set = UInt64(n_chunks - 1)  # 0-based
    num_data_blks = UInt64(0)  # No data blocks in simple case
    num_secondary_blks = UInt64(0)  # No secondary blocks
    data_blk_size = UInt64(0)
    secondary_blk_size = UInt64(0)

    # Element size for index: 8 bytes (address) for unfiltered,
    # 20 bytes (address 8 + size 8 + filter_mask 4) for filtered
    element_size = isnothing(filter_pipeline) ? UInt8(8) : UInt8(20)

    # Client ID: 0 for non-filtered chunks, 1 for filtered
    client_id = isnothing(filter_pipeline) ? UInt8(0) : UInt8(1)

    # Convert maxshape to HDF5 format (needed before psz calculation)
    hdf5_maxshape = convert_maxshape_to_hdf5(maxshape)

    # Write session for tracking
    wsession = JLDWriteSession()

    # Step 1: Write all chunks and collect addresses and sizes
    # Similar to Fixed Array, but we'll store in linear order
    chunk_addresses = Vector{RelOffset}(undef, n_chunks)
    chunk_sizes = isnothing(filter_pipeline) ? nothing : Vector{UInt32}(undef, n_chunks)

    # Calculate grid dims in HDF5 order for linear indexing
    grid_dims_hdf5 = reverse(grid_dims)
    ndims_hdf5 = length(grid_dims_hdf5)

    # Precompute down_chunks for HDF5 ordering
    down_chunks = zeros(Int, ndims_hdf5)
    acc = 1
    for i in ndims_hdf5:-1:1
        down_chunks[i] = acc
        acc *= grid_dims_hdf5[i]
    end

    # Write chunks in Julia order, but calculate HDF5 linear index
    for julia_chunk_idx in CartesianIndices(grid_dims)
        # Calculate data range for this chunk
        chunk_coords = Tuple(julia_chunk_idx)
        start_idx = (chunk_coords .- 1) .* chunks .+ 1
        end_idx = min.(start_idx .+ chunks .- 1, size(data))

        # Extract chunk data
        chunk_data_partial = data[map(:, start_idx, end_idx)...]

        # For extensible arrays, chunks are stored unpadded (actual size)
        # The reading code expects this and sizes buffers accordingly
        chunk_data = chunk_data_partial

        # Prepare chunk bytes (with optional compression)
        chunk_bytes = if isnothing(filter_pipeline)
            # No compression - serialize directly
            io_buf = IOBuffer()
            write_data(io_buf, f, chunk_data, odr, datamode(odr), wsession)
            take!(io_buf)
        else
            # Apply compression
            compressed, _ = Filters.compress(filter_pipeline, chunk_data, odr, f, wsession)
            compressed
        end

        # Write chunk to file
        chunk_offset = f.end_of_data
        seek(f.io, chunk_offset)
        write(f.io, chunk_bytes)
        f.end_of_data = chunk_offset + sizeof(chunk_bytes)

        # Calculate HDF5 linear index for this chunk
        hdf5_coords = reverse(Tuple(julia_chunk_idx) .- 1)  # 0-based, reversed
        linear_idx = 0
        for i in 1:ndims_hdf5
            linear_idx += down_chunks[i] * hdf5_coords[i]
        end

        # Store chunk address and size at linear index position
        chunk_addresses[linear_idx + 1] = h5offset(f, chunk_offset)
        if !isnothing(filter_pipeline)
            chunk_sizes[linear_idx + 1] = UInt32(sizeof(chunk_bytes))  # Store as UInt32, write as UInt64
        end
    end

    # Step 2: Write Index Block
    # Calculate index block size
    index_block_size = 4 +  # signature
                       1 +  # version
                       1 +  # client_id
                       8 +  # header address (back-reference)
                       Int(index_blk_elmts) * Int(element_size) +  # direct elements (with sizes if filtered)
                       Int(num_data_blks) * jlsizeof(RelOffset) +  # data block addresses
                       4    # checksum

    index_block_offset = f.end_of_data
    seek(f.io, index_block_offset)
    f.end_of_data = index_block_offset + index_block_size

    # Write index block (will update header address later)
    ib_cio = begin_checksum_write(f.io, index_block_size - 4)

    # Signature
    jlwrite(ib_cio, EXTENSIBLE_ARRAY_INDEX_BLOCK_SIGNATURE)

    # Version
    jlwrite(ib_cio, UInt8(0))

    # Client ID
    jlwrite(ib_cio, client_id)

    # Header address (placeholder, will be updated)
    header_addr_pos = position(f.io)
    jlwrite(ib_cio, RelOffset(0))

    # Write direct chunk addresses (and sizes + filter_mask if filtered) in linear order
    for i in 1:Int(index_blk_elmts)
        if i <= n_chunks
            jlwrite(ib_cio, chunk_addresses[i])
            if !isnothing(filter_pipeline)
                jlwrite(ib_cio, UInt64(chunk_sizes[i]))  # Write size as UInt64
                jlwrite(ib_cio, UInt32(0))  # filter_mask (0 = all filters applied)
            end
        else
            # Undefined chunks (shouldn't happen with our current logic)
            jlwrite(ib_cio, RelOffset(typemax(UInt64)))
            if !isnothing(filter_pipeline)
                jlwrite(ib_cio, UInt64(0))  # size
                jlwrite(ib_cio, UInt32(0))  # filter_mask
            end
        end
    end

    # Write data block addresses (none in simple case)
    for i in 1:Int(num_data_blks)
        jlwrite(ib_cio, RelOffset(typemax(UInt64)))
    end

    # Write checksum
    jlwrite(f.io, end_checksum(ib_cio))

    # Step 3: Write Extensible Array Header
    header_size = 4 +  # signature
                  1 +  # version
                  1 +  # client_id
                  1 +  # element_size
                  1 +  # max_nelmts_bits
                  1 +  # index_blk_elmts
                  1 +  # data_blk_min_elmts
                  1 +  # secondary_blk_min_data_ptrs
                  1 +  # max_dblk_page_nelmts_bits
                  8 +  # num_secondary_blks
                  8 +  # secondary_blk_size
                  8 +  # num_data_blks
                  8 +  # data_blk_size
                  8 +  # max_index_set
                  8 +  # nelmts
                  8 +  # index_blk_addr
                  4    # checksum

    header_offset = f.end_of_data
    seek(f.io, header_offset)
    f.end_of_data = header_offset + header_size

    hdr_cio = begin_checksum_write(f.io, header_size - 4)

    # Signature
    jlwrite(hdr_cio, EXTENSIBLE_ARRAY_HEADER_SIGNATURE)

    # Version
    jlwrite(hdr_cio, UInt8(0))

    # Client ID
    jlwrite(hdr_cio, client_id)

    # Element size
    jlwrite(hdr_cio, element_size)

    # Max nelmts bits
    jlwrite(hdr_cio, max_nelmts_bits)

    # Index block elmts
    jlwrite(hdr_cio, index_blk_elmts)

    # Data block min elmts
    jlwrite(hdr_cio, data_blk_min_elmts)

    # Secondary block min data ptrs
    jlwrite(hdr_cio, secondary_blk_min_data_ptrs)

    # Max dblk page nelmts bits
    jlwrite(hdr_cio, max_dblk_page_nelmts_bits)

    # Num secondary blocks
    jlwrite(hdr_cio, num_secondary_blks)

    # Secondary block size
    jlwrite(hdr_cio, secondary_blk_size)

    # Num data blocks
    jlwrite(hdr_cio, num_data_blks)

    # Data block size
    jlwrite(hdr_cio, data_blk_size)

    # Max index set
    jlwrite(hdr_cio, max_index_set)

    # Nelmts (current number of elements)
    jlwrite(hdr_cio, nelmts)

    # Index block address
    jlwrite(hdr_cio, h5offset(f, index_block_offset))

    # Write checksum
    jlwrite(f.io, end_checksum(hdr_cio))

    # Update index block header address (now that we know it)
    current_pos = position(f.io)
    seek(f.io, header_addr_pos)
    jlwrite(f.io, h5offset(f, header_offset))
    seek(f.io, current_pos)

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
    if !isnothing(filter_pipeline)
        psz += Filters.pipeline_message_size(filter_pipeline)
    end

    # DataLayout message size (version 4, type 4)
    # Layout flags - 0x02 indicates filters present
    layout_flags = isnothing(filter_pipeline) ? UInt8(0x00) : UInt8(0x02)
    dimensionality = UInt8(N + 1)  # +1 for element size dimension

    psz += jlsizeof(Val(HmDataLayout);
        version = 4,
        layout_class = LcChunked,
        flags = layout_flags,
        dimensionality = dimensionality,
        dimensions = UInt64.((reverse(chunks)..., odr_sizeof(odr))),  # CHUNK dimensions
        chunk_indexing_type = 4,
        maxbits = max_nelmts_bits,
        index_elements = index_blk_elmts,
        minpointers = secondary_blk_min_data_ptrs,
        minelements = data_blk_min_elmts,
        page_bits = max_dblk_page_nelmts_bits,
        data_address = h5offset(f, header_offset)
    )

    # Add continuation message size
    psz += CONTINUATION_MSG_SIZE

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
    if !isnothing(filter_pipeline)
        Filters.write_filter_pipeline_message(cio, filter_pipeline)
    end

    # Write DataLayout message (version 4, type 4)
    write_header_message(cio, Val(HmDataLayout);
        version = 4,
        layout_class = LcChunked,
        flags = layout_flags,
        dimensionality = dimensionality,
        dimensions = UInt64.((reverse(chunks)..., odr_sizeof(odr))),  # CHUNK dimensions
        chunk_indexing_type = 4,
        maxbits = max_nelmts_bits,
        index_elements = index_blk_elmts,
        minpointers = secondary_blk_min_data_ptrs,
        minelements = data_blk_min_elmts,
        page_bits = max_dblk_page_nelmts_bits,
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

    # Normalize filters (convert symbols like :gzip to FilterPipeline)
    normalized_filters = isnothing(filters) ? nothing : Filters.normalize_filters(filters)

    # Get datatype and object data representation
    odr = objodr(data)
    datatype = h5type(f, data)
    dataspace = WriteDataspace(f, data, odr)

    # Prepare filter pipeline for chunk compression
    filter_pipeline = if isnothing(normalized_filters)
        nothing
    else
        FilterPipeline(map(normalized_filters) do filter
            Filters.set_local(filter, odr, dataspace, ())
        end)
    end

    # Calculate chunk grid dimensions
    grid_dims = cld.(size(data), chunks)
    n_chunks = prod(grid_dims)

    # Calculate chunk size in bytes
    chunk_size_bytes = prod(chunks) * odr_sizeof(odr)

    # V2 B-tree parameters (matching h5py)
    node_size = UInt32(2048)  # h5py uses 2048 bytes
    split_percent = UInt8(100)
    merge_percent = UInt8(40)
    type_byte = UInt8(10)  # Type 10 for chunked dataset storage

    # Record size calculation
    # Unfiltered: 8 (address) + 8*N (chunk indices)
    # Filtered: 8 (address) + 4 (size) + 4 (filter_mask) + 8*N (chunk indices)
    record_size = if !isnothing(filter_pipeline)
        UInt16(8 + 4 + 4 + 8 * N)
    else
        UInt16(8 + 8 * N)
    end

    # For simplified implementation: depth=0 (single leaf node)
    depth = UInt16(0)

    # Calculate max records that fit in leaf node
    # node_size = 4 (sig) + 1 (ver) + 1 (type) + num_records * record_size + 4 (checksum)
    max_records_per_node = (node_size - 10) ÷ record_size

    if n_chunks > max_records_per_node
        throw(UnsupportedFeatureException(
            "V2 B-tree with >$max_records_per_node chunks requires internal nodes (depth>0), " *
            "which is not yet implemented. Current chunk count: $n_chunks. " *
            "Consider using larger chunks to reduce the number of chunks."
        ))
    end

    # Convert maxshape to HDF5 format (needed before psz calculation)
    hdf5_maxshape = convert_maxshape_to_hdf5(maxshape)

    # Write session for tracking
    wsession = JLDWriteSession()

    # Step 1: Write all chunks and collect addresses
    # For unfiltered: (chunk_address, chunk_indices_hdf5_0based)
    # For filtered: (chunk_address, chunk_size, filter_mask, chunk_indices_hdf5_0based)
    if !isnothing(filter_pipeline)
        chunk_records = Vector{Tuple{RelOffset, UInt32, UInt32, Vector{UInt64}}}(undef, n_chunks)
    else
        chunk_records = Vector{Tuple{RelOffset, Vector{UInt64}}}(undef, n_chunks)
    end

    # Calculate grid dims in HDF5 order for linear indexing
    grid_dims_hdf5 = reverse(grid_dims)
    ndims_hdf5 = length(grid_dims_hdf5)

    # Precompute down_chunks for HDF5 ordering
    down_chunks = zeros(Int, ndims_hdf5)
    acc = 1
    for i in ndims_hdf5:-1:1
        down_chunks[i] = acc
        acc *= grid_dims_hdf5[i]
    end

    # Write chunks in Julia order, calculate HDF5 linear index
    for julia_chunk_idx in CartesianIndices(grid_dims)
        # Calculate data range for this chunk
        chunk_coords = Tuple(julia_chunk_idx)
        start_idx = (chunk_coords .- 1) .* chunks .+ 1
        end_idx = min.(start_idx .+ chunks .- 1, size(data))

        # Extract chunk data
        chunk_data_partial = data[map(:, start_idx, end_idx)...]

        # For V2 B-tree, chunks are stored unpadded (like Extensible Array)
        chunk_data = chunk_data_partial

        # Apply filters if specified
        chunk_bytes = if isnothing(filter_pipeline)
            io_buf = IOBuffer()
            write_data(io_buf, f, chunk_data, odr, datamode(odr), wsession)
            take!(io_buf)
        else
            compressed, _ = Filters.compress(filter_pipeline, chunk_data, odr, f, wsession)
            compressed
        end

        # Write chunk to file
        chunk_offset = f.end_of_data
        seek(f.io, chunk_offset)
        write(f.io, chunk_bytes)
        f.end_of_data = chunk_offset + sizeof(chunk_bytes)

        # Calculate HDF5 chunk indices (0-based, reversed)
        hdf5_coords = reverse(Tuple(julia_chunk_idx) .- 1)

        # Calculate HDF5 linear index for sorting
        linear_idx = 0
        for i in 1:ndims_hdf5
            linear_idx += down_chunks[i] * hdf5_coords[i]
        end

        # Store chunk record at linear index position
        if !isnothing(filter_pipeline)
            chunk_size = UInt32(sizeof(chunk_bytes))
            filter_mask = UInt32(0)  # All filters applied
            chunk_records[linear_idx + 1] = (h5offset(f, chunk_offset), chunk_size, filter_mask, collect(UInt64.(hdf5_coords)))
        else
            chunk_records[linear_idx + 1] = (h5offset(f, chunk_offset), collect(UInt64.(hdf5_coords)))
        end
    end

    # Step 2: Write Leaf Node
    # Calculate leaf node size
    leaf_node_size = 4 +  # signature
                     1 +  # version
                     1 +  # type
                     n_chunks * Int(record_size) +  # chunk records
                     4    # checksum

    leaf_node_offset = f.end_of_data
    seek(f.io, leaf_node_offset)
    f.end_of_data = leaf_node_offset + leaf_node_size

    # Write leaf node
    leaf_cio = begin_checksum_write(f.io, leaf_node_size - 4)

    # Signature "BTLF" (0x464C5442)
    jlwrite(leaf_cio, htol(0x464C5442))

    # Version
    jlwrite(leaf_cio, UInt8(0))

    # Type
    jlwrite(leaf_cio, type_byte)

    # Write chunk records in sorted order
    if !isnothing(filter_pipeline)
        # Filtered format: address, size, filter_mask, indices
        for record in chunk_records
            chunk_addr, chunk_size, filter_mask, chunk_indices = record
            jlwrite(leaf_cio, chunk_addr.offset)  # UInt64
            jlwrite(leaf_cio, chunk_size)          # UInt32
            jlwrite(leaf_cio, filter_mask)         # UInt32
            for idx in chunk_indices
                jlwrite(leaf_cio, idx)              # UInt64
            end
        end
    else
        # Unfiltered format: address, indices
        for record in chunk_records
            chunk_addr, chunk_indices = record
            jlwrite(leaf_cio, chunk_addr.offset)  # UInt64
            for idx in chunk_indices
                jlwrite(leaf_cio, idx)              # UInt64
            end
        end
    end

    # Write checksum
    jlwrite(f.io, end_checksum(leaf_cio))

    # Step 3: Write B-tree Header
    header_size = 4 +  # signature
                  1 +  # version
                  1 +  # type
                  4 +  # node_size
                  2 +  # record_size
                  2 +  # depth
                  1 +  # split_percent
                  1 +  # merge_percent
                  8 +  # root_node_address
                  2 +  # num_records_root
                  8 +  # total_records
                  4    # checksum

    header_offset = f.end_of_data
    seek(f.io, header_offset)
    f.end_of_data = header_offset + header_size

    hdr_cio = begin_checksum_write(f.io, header_size - 4)

    # Signature "BTHD" (0x44485442)
    jlwrite(hdr_cio, htol(0x44485442))

    # Version
    jlwrite(hdr_cio, UInt8(0))

    # Type
    jlwrite(hdr_cio, type_byte)

    # Node size
    jlwrite(hdr_cio, node_size)

    # Record size
    jlwrite(hdr_cio, record_size)

    # Depth
    jlwrite(hdr_cio, depth)

    # Split percent
    jlwrite(hdr_cio, split_percent)

    # Merge percent
    jlwrite(hdr_cio, merge_percent)

    # Root node address (absolute file offset)
    jlwrite(hdr_cio, UInt64(leaf_node_offset))

    # Num records in root
    jlwrite(hdr_cio, UInt16(n_chunks))

    # Total records
    jlwrite(hdr_cio, UInt64(n_chunks))

    # Write checksum
    jlwrite(f.io, end_checksum(hdr_cio))

    # Step 4: Write Object Header with DataLayout message
    # Calculate payload size for object header
    # V2 B-tree requires max dimensions in dataspace (flags bit 0 set)
    psz = jlsizeof(Val(HmDataspace);
        dataspace.dataspace_type,
        dimensions=dataspace.size,
        flags=0x01,  # Bit 0 indicates max dimensions are present
        max_dimension_size=hdf5_maxshape)
    psz += jlsizeof(Val(HmDatatype), 1; dt=datatype)

    # DataLayout message size (version 4, type 5)
    layout_flags = isnothing(filter_pipeline) ? UInt8(0x00) : UInt8(0x02)  # 0x02 indicates filters
    dimensionality = UInt8(N + 1)  # +1 for element size dimension

    psz += jlsizeof(Val(HmDataLayout);
        version = 4,
        layout_class = LcChunked,
        flags = layout_flags,
        dimensionality = dimensionality,
        dimensions = UInt64.((reverse(chunks)..., odr_sizeof(odr))),  # CHUNK dimensions
        chunk_indexing_type = 5,
        node_size = node_size,
        splitpercent = split_percent,
        mergepercent = merge_percent,
        data_address = h5offset(f, header_offset)
    )

    # Add filter pipeline message size if filters are present
    if !isnothing(filter_pipeline)
        psz += Filters.pipeline_message_size(filter_pipeline)
    end

    # Add continuation message size
    psz += CONTINUATION_MSG_SIZE

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
        flags = layout_flags,
        dimensionality = dimensionality,
        dimensions = UInt64.((reverse(chunks)..., odr_sizeof(odr))),  # CHUNK dimensions
        chunk_indexing_type = 5,
        node_size = node_size,
        splitpercent = split_percent,
        mergepercent = merge_percent,
        data_address = h5offset(f, header_offset)
    )

    # Write filter pipeline message if filters are present
    if !isnothing(filter_pipeline)
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
