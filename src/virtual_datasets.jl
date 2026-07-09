# Virtual Dataset Support for JLD2.jl
# Implements proper HDF5 Virtual Dataset (VDS) format

# H5S_UNLIMITED: Use -1 for unlimited dimensions
# Note: Int64(-1) and UInt64(0xffffffffffffffff) have identical byte representation
# so no conversion is needed between signed/unsigned contexts
const H5S_UNLIMITED = Int64(-1)

# Check if a dimension/count is unlimited
is_unlimited(x::Integer) = (x % Int64) == -1

# Abstract base type for all dataspace selection formats
abstract type DataspaceSelection end

# Hyperslab selection structure for virtual dataset mappings (version 2 format)
# Represents regular patterns with start/stride/count/block
struct HyperslabSelection <: DataspaceSelection
    start::Vector{UInt64}
    stride::Vector{UInt64}
    count::Vector{UInt64}
    block::Vector{UInt64}
end

# Irregular hyperslab selection (version 1 format with multiple blocks)
# Stores explicit list of blocks that may not follow a regular pattern
struct IrregularHyperslabSelection <: DataspaceSelection
    blocks::Vector{Tuple{Vector{UInt64}, Vector{UInt64}}}  # (start, end) for each block
end

# Check if a selection represents the entire dataset
is_all_selection(selection::HyperslabSelection) =
    isempty(selection.start) && isempty(selection.stride) &&
    isempty(selection.count) && isempty(selection.block)

is_all_selection(selection::IrregularHyperslabSelection) = false  # Irregular selections are never "all"

# Serialization methods for DataspaceSelection types
function jlread(io::IO, ::Type{DataspaceSelection})
    # Read selection type (4 bytes)
    sel_type = jlread(io, UInt32)

    if sel_type == 0  # H5S_SEL_NONE
        # No additional data
        return HyperslabSelection(UInt64[], UInt64[], UInt64[], UInt64[])
    elseif sel_type == 1  # H5S_SEL_POINTS
        throw(UnsupportedFeatureException("Point selection not yet supported for virtual datasets"))
    elseif sel_type == 2  # H5S_SEL_HYPERSLABS
        # Read version
        version = jlread(io, UInt32)

        if version == 1
            # Version 1: Reserved, Length, Rank, NumBlocks, then start/end offsets
            reserved = jlread(io, UInt32)
            length_field = jlread(io, UInt32)
            rank = jlread(io, UInt32)
            num_blocks = jlread(io, UInt32)

            if num_blocks == 1
                # Single block - can represent as regular hyperslab
                # Read start offsets
                start = [jlread(io, UInt32) for _ in 1:rank]
                # Read end offsets
                end_offsets = [jlread(io, UInt32) for _ in 1:rank]

                # Convert to start/stride/count/block format
                # For a simple block: stride=1, count=1, block=end-start+1
                stride = ones(UInt64, rank)
                count = ones(UInt64, rank)
                block = UInt64[end_offsets[i] - start[i] + 1 for i in 1:rank]

                return HyperslabSelection(UInt64.(start), stride, count, block)
            else
                # Multiple blocks - store as irregular selection
                # Don't try to convert to regular pattern as blocks may be arbitrarily placed
                blocks = Vector{Tuple{Vector{UInt64}, Vector{UInt64}}}(undef, num_blocks)

                for b in 1:num_blocks
                    start = UInt64.([jlread(io, UInt32) for _ in 1:rank])
                    end_offset = UInt64.([jlread(io, UInt32) for _ in 1:rank])
                    blocks[b] = (start, end_offset)
                end

                return IrregularHyperslabSelection(blocks)
            end
        elseif version == 2
            # Version 2: Flags, Length, Rank, then Start/Stride/Count/Block INTERLEAVED
            # For each dimension: start[i], stride[i], count[i], block[i]
            flags = jlread(io, UInt8)
            length_field = jlread(io, UInt32)
            rank = jlread(io, UInt32)

            start = Vector{UInt64}(undef, rank)
            stride = Vector{UInt64}(undef, rank)
            count = Vector{UInt64}(undef, rank)
            block = Vector{UInt64}(undef, rank)

            # Read interleaved fields
            for i in 1:rank
                start[i] = jlread(io, UInt64)
                stride[i] = jlread(io, UInt64)
                count[i] = jlread(io, UInt64)
                block[i] = jlread(io, UInt64)
            end

            return HyperslabSelection(start, stride, count, block)
        else
            throw(UnsupportedVersionException("Unsupported hyperslab selection version $version"))
        end
    elseif sel_type == 3  # H5S_SEL_ALL
        # H5S_SEL_ALL format: Version (4 bytes) + Reserved (8 bytes)
        version = jlread(io, UInt32)
        reserved = jlread(io, UInt64)  # 8 bytes reserved, should be zero
        # H5S_SEL_ALL means select the entire dataset
        return HyperslabSelection(UInt64[], UInt64[], UInt64[], UInt64[])
    else
        throw(UnsupportedFeatureException("Unknown selection type $sel_type"))
    end
end

function jlwrite(io::IO, selection::HyperslabSelection)
    if is_all_selection(selection)
        # H5S_SEL_ALL: type=3, version=1, reserved=8 bytes
        jlwrite(io, UInt32(3))
        jlwrite(io, UInt32(1))
        jlwrite(io, UInt64(0))
    else
        # H5S_SEL_HYPERSLABS version 2 (regular hyperslab with start/stride/count/block)
        rank = length(selection.start)
        length_field = 4 + rank * 4 * 8  # Rank (4 bytes) + 4 fields * rank * 8 bytes

        jlwrite(io, UInt32(2))      # Selection type: H5S_SEL_HYPERSLABS
        jlwrite(io, UInt32(2))      # Version 2
        jlwrite(io, UInt8(0x01))    # Flags: regular hyperslab
        jlwrite(io, UInt32(length_field))
        jlwrite(io, UInt32(rank))

        # Write interleaved start, stride, count, block for each dimension
        for i in 1:rank
            jlwrite(io, selection.start[i])
            jlwrite(io, selection.stride[i])
            jlwrite(io, selection.count[i])
            jlwrite(io, selection.block[i])
        end
    end
end

# Virtual Dataset Mapping Entry according to HDF5 Global Heap Block format
struct VirtualMapping
    source_filename::String
    source_dataset_name::String
    src_selection::DataspaceSelection
    vds_selection::DataspaceSelection
end


# Convenience constructor for selecting entire dataset
all_selection() = HyperslabSelection(UInt64[], UInt64[], UInt64[], UInt64[])

# Constructor from Julia ranges (1-based) to HDF5 HyperslabSelection (0-based, reversed order)
#
# HDF5 hyperslab semantics: Elements selected are those at positions
#   start + i*stride + j  where i ∈ [0, count-1], j ∈ [0, block-1]
#
# Two canonical encodings:
#   1. Contiguous (UnitRange): stride=1, count=1, block=N  → single block of N elements
#   2. Strided (StepRange):    stride=S, count=N, block=1  → N individual elements, S apart
#
# These are semantically different even when they select the same elements!
# Example: Both select [3,4,5,6,7] but encode differently:
#   - UnitRange(3:7):   start=2, stride=1, count=1, block=5  (h5py standard)
#   - Equivalent alt:   start=2, stride=1, count=5, block=1  (NOT used by h5py/HDF5)
#
# The distinction matters for:
#   - Format compliance with h5py and HDF5 tools
#   - Semantic clarity (one block vs multiple blocks)
#   - Potential optimizations in readers
function HyperslabSelection(ranges::Tuple)
    ndims = length(ranges)

    start = Vector{UInt64}(undef, ndims)
    stride = Vector{UInt64}(undef, ndims)
    count = Vector{UInt64}(undef, ndims)
    block = Vector{UInt64}(undef, ndims)

    # Process each dimension in Julia order
    for (julia_idx, range) in enumerate(ranges)
        # HDF5 dimension index (reversed)
        hdf5_idx = ndims - julia_idx + 1

        # Convert range to HDF5 hyperslab parameters
        if range isa UnitRange
            # Contiguous block: encode as single block (h5py compatible)
            # Example: 3:7 → start=2, stride=1, count=1, block=5
            start[hdf5_idx] = UInt64(first(range) - 1)  # Convert to 0-based
            stride[hdf5_idx] = UInt64(1)
            count[hdf5_idx] = UInt64(1)
            block[hdf5_idx] = UInt64(length(range))
        elseif range isa StepRange
            # Strided selection: encode as multiple single elements (h5py compatible)
            # Example: 1:2:9 → start=0, stride=2, count=5, block=1
            start[hdf5_idx] = UInt64(first(range) - 1)  # Convert to 0-based
            stride[hdf5_idx] = UInt64(step(range))
            count[hdf5_idx] = UInt64(length(range))
            block[hdf5_idx] = UInt64(1)
        elseif range isa Int
            # Single element: special case of contiguous block with size 1
            start[hdf5_idx] = UInt64(range - 1)  # Convert to 0-based
            stride[hdf5_idx] = UInt64(1)
            count[hdf5_idx] = UInt64(1)
            block[hdf5_idx] = UInt64(1)
        else
            throw(ArgumentError("Unsupported range type: $(typeof(range)). Use UnitRange, StepRange, or Int."))
        end
    end

    return HyperslabSelection(start, stride, count, block)
end

# Constructor from root index + shape to HDF5 HyperslabSelection
function HyperslabSelection(; root::Tuple, shape::Tuple)
    length(root) == length(shape) || throw(ArgumentError("root and shape must have same length"))
    # Convert root + shape to ranges: root[i]:(root[i] + shape[i] - 1)
    HyperslabSelection(map((r, s) -> r:(r + s - 1), root, shape))
end


# Extract subset from source data based on selection
function extract_subset(source_data, selection::DataspaceSelection)
    is_all_selection(selection) && return source_data

    src_dims = size(source_data)
    indices = to_indices(selection, src_dims)

    selection isa IrregularHyperslabSelection ?
        [source_data[idx] for idx in indices] : source_data[indices...]
end

# Assign source subset to virtual dataset result array
function assign_to_vds!(result, source_subset, selection::DataspaceSelection, vds_dims)
    if selection isa IrregularHyperslabSelection
        # Irregular selection - assign element by element
        vds_indices = to_indices(selection, vds_dims)
        source_flat = vec(source_subset)

        length(vds_indices) == length(source_flat) || throw(InvalidDataException(
            "VDS selection size ($(length(vds_indices))) does not match source size ($(length(source_flat)))"))

        for (i, idx) in enumerate(vds_indices)
            result[idx] = source_flat[i]
        end
    else
        # Regular selection - assign as block
        vds_ranges = to_indices(selection, vds_dims)
        result[vds_ranges...] = source_subset
    end
end

# Combine virtual mappings into a single dataset
function combine_virtual_mappings(f::JLDFile, mappings::Vector{VirtualMapping},
                                dataspace::ReadDataspace, dt::H5Datatype)
    isempty(mappings) && throw(InvalidDataException("Virtual dataset has no mappings"))

    # Read virtual dataset dimensions from dataspace
    io = f.io
    ndims = Int(dataspace.dimensionality)
    seek(io, dataspace.dimensions_offset)
    vds_dims_hdf5 = ntuple(i -> jlread(io, Int64), ndims)  # HDF5 order

    # Check if we need to calculate dynamic dimensions from pattern files
    # This happens when mappings use file patterns (%b) with H5S_UNLIMITED count
    vds_dims_julia = if any(m -> occursin("%b", m.source_filename) &&
                                  any(is_unlimited, m.vds_selection.count), mappings)
        # Calculate actual dimensions based on pattern expansion
        calculate_dynamic_vds_dims(f, mappings, vds_dims_hdf5)
    else
        # Use static dimensions from dataspace
        reverse(Tuple(vds_dims_hdf5))  # HDF5→Julia order
    end

    # Infer element type and create output array
    element_type = julia_repr(jltype(f, dt))
    result = Array{element_type}(undef, vds_dims_julia...)

    # Process each mapping
    for mapping in mappings
        # Determine file list and corresponding selections
        is_pattern = occursin("%b", mapping.source_filename)
        file_paths = is_pattern ? expand_file_pattern(mapping.source_filename, f) :
                                 [joinpath(dirname(f.path), mapping.source_filename)]

        # Process each file in the mapping
        for (idx, file_path) in enumerate(file_paths)
            isfile(file_path) || (@warn("Source file not found: $file_path"); continue)

            # Load and extract source data
            source_data = jldopen(file_path, "r") do src_f
                haskey(src_f, mapping.source_dataset_name) ||
                    throw(InvalidDataException("Dataset '$(mapping.source_dataset_name)' not found in $file_path"))
                src_f[mapping.source_dataset_name]
            end
            source_subset = extract_subset(source_data, mapping.src_selection)

            # Calculate appropriate VDS selection (pattern uses block index, non-pattern uses selection as-is)
            vds_sel = is_pattern ? calculate_block_selection(mapping.vds_selection, idx - 1) :
                                  mapping.vds_selection

            assign_to_vds!(result, source_subset, vds_sel, vds_dims_julia)
        end
    end

    return result
end

"""
    to_indices(selection::HyperslabSelection, julia_dims::Tuple) -> Tuple

Convert HDF5 HyperslabSelection (0-based, reversed order) to Julia ranges (1-based).
Returns tuple of ranges for indexing into Julia array, or full ranges if `all_selection()`.
"""
function to_indices(selection::HyperslabSelection, julia_dims::Tuple)
    ndims_julia = length(julia_dims)
    rank = length(selection.start)

    # Handle all_selection() - select entire array
    if is_all_selection(selection)
        return ntuple(i -> 1:julia_dims[i], ndims_julia)
    end

    if rank != ndims_julia
        throw(InvalidDataException("Hyperslab rank ($rank) does not match array dimensions ($ndims_julia)"))
    end

    # Convert each HDF5 dimension to a Julia range
    ranges = Vector{Any}(undef, ndims_julia)  # Use Any to allow both UnitRange and StepRange

    for i in 1:rank
        # HDF5 dimension i corresponds to Julia dimension (ndims - i + 1)
        julia_dim_idx = ndims_julia - i + 1

        # Convert from 0-based to 1-based indexing
        start_0based = Int(selection.start[i])
        stride_val = Int(selection.stride[i])
        block_val = Int(selection.block[i])

        # H5S_UNLIMITED means count is determined by actual VDS extent
        # Calculate actual count based on dimension size and stride
        count_val = if is_unlimited(selection.count[i])
            # VDS dimension was calculated to fit all blocks
            # Each file contributes stride elements, so count = dim_size / stride
            cld(julia_dims[julia_dim_idx], stride_val)
        else
            Int(selection.count[i])
        end

        # Calculate the range - various special cases simplify to common patterns
        start_1based = start_0based + 1

        # Determine if this is a contiguous range or requires stepping
        is_contiguous = (stride_val == 1 && count_val == 1) || (stride_val == block_val)

        if is_contiguous
            # Contiguous block: stride=1,count=1,block=N OR stride=block (adjacent blocks)
            end_1based = start_1based + count_val * block_val - 1
            ranges[julia_dim_idx] = start_1based:end_1based
        elseif block_val == 1
            # Single elements with stride - simple strided range
            end_1based = start_1based + (count_val - 1) * stride_val
            ranges[julia_dim_idx] = start_1based:stride_val:end_1based
        else
            # Complex case: stride != block, block > 1
            throw(UnsupportedFeatureException("Complex hyperslab patterns with stride != block not yet supported"))
        end
    end

    return Tuple(ranges)
end

"""
    to_indices(selection::IrregularHyperslabSelection, julia_dims::Tuple) -> Vector{CartesianIndex}

Convert irregular hyperslab selection to vector of CartesianIndex in Julia order.
Used when blocks don't follow regular stride patterns.
"""
function to_indices(selection::IrregularHyperslabSelection, julia_dims::Tuple)
    ndims_julia = length(julia_dims)

    # Convert each block to ranges and collect all CartesianIndex values
    mapreduce(vcat, selection.blocks) do (start_hdf5, end_hdf5)
        rank = length(start_hdf5)
        rank == ndims_julia || throw(InvalidDataException(
            "Hyperslab rank ($rank) does not match array dimensions ($ndims_julia)"))

        # Convert HDF5 block (0-based, reversed) to Julia ranges (1-based)
        ranges = ntuple(ndims_julia) do julia_dim_idx
            i = ndims_julia - julia_dim_idx + 1  # HDF5 dimension index
            Int(start_hdf5[i]) + 1 : Int(end_hdf5[i]) + 1
        end

        # Generate all Cartesian indices for this block
        collect(CartesianIndices(ranges))
    end
end

# Expand file pattern like "./sub-%b.hdf5" to actual filenames
function expand_file_pattern(pattern::String, f::JLDFile)
    vds_dir = dirname(f.path)
    filepath = joinpath(vds_dir, pattern)

    # For patterns like "./sub-%b.h5" or "f-%b.h5", expand %b to 0, 1, 2, ...
    if occursin("%b", pattern)
        files = String[]
        i = 0
        while true
            expanded_file = replace(filepath, "%b" => string(i))
            isfile(expanded_file) || break
            push!(files, expanded_file)
            i += 1
        end
        files
    else
        [filepath]
    end
end

#
# JLD2 Virtual Dataset Creation API
#

"""
    VirtualMapping(source_file, source_dataset; [src_selection], [vds_selection],
                   [src_indices], [vds_indices], [src_root], [src_shape],
                   [vds_root], [vds_shape])

Create a mapping from source file dataset to virtual dataset region.

# Arguments
- `source_file::String`: Path to source file (relative to VDS file)
- `source_dataset::String`: Dataset name within source file

# Selection Methods (choose one for source and one for VDS)
- **Direct**: `src_selection`/`vds_selection` as `HyperslabSelection` (advanced)
- **Ranges**: `src_indices`/`vds_indices` as tuples of Julia ranges (recommended)
- **Root+Shape**: `src_root`+`src_shape` / `vds_root`+`vds_shape` as dimension tuples (intuitive)

If no selection specified, uses entire dataset (`all_selection()`).
"""
function VirtualMapping(source_file::String, source_dataset::String;
                       src_selection::Union{HyperslabSelection,Nothing}=nothing,
                       vds_selection::Union{HyperslabSelection,Nothing}=nothing,
                       src_indices::Union{Tuple,Nothing}=nothing,
                       vds_indices::Union{Tuple,Nothing}=nothing,
                       src_root::Union{Tuple,Nothing}=nothing,
                       src_shape::Union{Tuple,Nothing}=nothing,
                       vds_root::Union{Tuple,Nothing}=nothing,
                       vds_shape::Union{Tuple,Nothing}=nothing)

    # Helper to process selection parameters
    function process_selection(selection, indices, root, shape, prefix)
        methods = (!isnothing(selection), !isnothing(indices), !isnothing(root) || !isnothing(shape))
        count(identity, methods) > 1 && throw(ArgumentError(
            "Specify only one method for $prefix selection: $(prefix)_selection, $(prefix)_indices, or ($(prefix)_root, $(prefix)_shape)"))

        if !isnothing(selection)
            selection
        elseif !isnothing(indices)
            HyperslabSelection(indices)
        elseif !isnothing(root) && !isnothing(shape)
            HyperslabSelection(; root, shape)
        elseif !isnothing(root) || !isnothing(shape)
            throw(ArgumentError("Both $(prefix)_root and $(prefix)_shape must be specified together"))
        else
            all_selection()
        end
    end

    src_sel = process_selection(src_selection, src_indices, src_root, src_shape, "src")
    vds_sel = process_selection(vds_selection, vds_indices, vds_root, vds_shape, "vds")

    return VirtualMapping(source_file, source_dataset, src_sel, vds_sel)
end

"""
    create_virtual_dataset(parent, name, dims, element_type, mappings; max_dims=dims)

Create virtual dataset combining data from multiple source files.

# Arguments
- `parent::Union{JLDFile, Group}`: Container file or group
- `name::String`: Virtual dataset name
- `dims::Tuple`: Virtual dataset dimensions
- `element_type::Type`: Element type (e.g., Float64, Int32)
- `mappings::Vector{VirtualMapping}`: Source file mappings
- `max_dims::Tuple`: Maximum dimensions (use `H5S_UNLIMITED` (-1) for unlimited, defaults to dims)

Returns `RelOffset` of written dataset. Supports pattern-based filenames like `"./sub-%b.jld2"`.
"""
function create_virtual_dataset(parent::Union{JLDFile, Group}, name::String,
                               dims::Tuple, element_type::Type, mappings::Vector{VirtualMapping};
                               max_dims::Tuple=dims)
    f = parent isa JLDFile ? parent : parent.f

    # Create a dummy array with the right dimensions and type for dataspace/datatype inference
    type = Array{element_type, length(dims)}
    writtenas = writeas(type)
    ODR = _odr(writtenas, type, odr(writtenas))

    rdims = reverse(dims)
    rmax_dims = reverse(max_dims) .% UInt64

    # Determine if we need max_dimensions (any dimension is unlimited)
    has_unlimited = any(is_unlimited, max_dims)

    # Create dataspace with optional max_dimensions
    dataspace = if isnothing(ODR)
        WriteDataspace(DS_NULL, (), (WrittenAttribute(f, :dimensions, collect(Int64, rdims)),))
    else
        ds_type = DS_SIMPLE
        ds_size = convert(Tuple{Vararg{Length}}, rdims)
        ds_attrs = ODR == RelOffset ?
            (WrittenAttribute(f, :julia_type, write_ref(f, T, f.datatype_wsession)),) : ()

        WriteDataspace(ds_type, ds_size, ds_attrs, rmax_dims)
    end

    datatype = if writtenas == type
        el_writtenas = writeas(element_type)
        if !hasfielddata(writtenas)
            h5type(f, el_writtenas, rconvert(element_type, newstruct(el_writtenas)))
        else
            h5fieldtype(f, el_writtenas, element_type, Val{false})
        end
    else
        h5fieldtype(f, writtenas, type, Val{true})
    end

    offset = write_virtual_dataset(f, dataspace, datatype, mappings)
    parent[name] = offset
    return offset
end

"""
    create_virtual_dataset(parent, name, source_files, dataset_name)

Create virtual dataset with automatic dimension/type inference from all source files.

# Arguments
- `parent::Union{JLDFile, Group}`: Container file or group
- `name::String`: Virtual dataset name
- `source_files::Vector{String}`: Source file paths (relative to VDS file)
- `dataset_name::String`: Dataset name in each source file

Inspects all files to determine element type and dimensions, then creates VDS
concatenating all sources. Sources can have variable sizes along the concatenation axis.
"""
function create_virtual_dataset(parent::Union{JLDFile, Group}, name::String,
                               source_files::Vector{String}, dataset_name::String)
    isempty(source_files) && throw(ArgumentError("Source files list cannot be empty"))

    f = parent isa JLDFile ? parent : parent.f
    base_dir = dirname(f.path)

    # Extract metadata from all source files
    source_metadata = map(source_files) do source_file
        file_path = joinpath(base_dir, source_file)
        isfile(file_path) || throw(ArgumentError("Source file not found: $file_path"))

        jldopen(file_path, "r") do src_f
            haskey(src_f, dataset_name) || throw(ArgumentError("Dataset '$dataset_name' not found in $source_file"))
            dset = get_dataset(src_f, dataset_name)
            (julia_repr(jltype(src_f, dset.datatype)), reverse(dset.dataspace.dimensions))
        end
    end

    # Validate consistency
    element_type, first_dims = source_metadata[1]
    ndims = length(first_dims)

    for idx in 2:length(source_metadata)
        elem_type, dims = source_metadata[idx]
        elem_type == element_type || throw(ArgumentError("Element type mismatch in source $idx"))
        length(dims) == ndims || throw(ArgumentError("Dimension count mismatch in source $idx"))
        dims[1:end-1] == first_dims[1:end-1] || throw(ArgumentError("Non-concat dimensions mismatch in source $idx"))
    end

    # Calculate VDS dimensions
    vds_dims = if ndims == 1
        # 1D sources: each becomes a column, no concatenation along original dimension
        (first_dims[1], length(source_files))
    else
        # N-D sources: concatenate along last dimension
        concat_size = sum(dims[end] for (_, dims) in source_metadata)
        (first_dims[1:end-1]..., concat_size)
    end

    # Create mappings with per-source positioning
    current_offset = 0
    mappings = map(enumerate(source_metadata), source_files) do (idx, (_, dims)), source_file
        # VDS indices depend on whether we're expanding 1D→2D or concatenating N-D
        vds_indices = if ndims == 1
            # 1D source → single column in 2D VDS
            (1:dims[1], idx:idx)
        else
            # N-D source → concatenate along last dimension
            col_range = current_offset + 1 : current_offset + dims[end]
            current_offset += dims[end]
            (ntuple(i -> 1:dims[i], ndims-1)..., col_range)
        end

        VirtualMapping(source_file, dataset_name; vds_indices)
    end

    return create_virtual_dataset(parent, name, vds_dims, element_type, mappings)
end

#
# Virtual Dataset Writing Implementation
#

"""
    write_virtual_dataset(f, dataspace, datatype, mappings)

Write virtual dataset with specified mappings in HDF5 VDS format.
"""
function write_virtual_dataset(f::JLDFile, dataspace, datatype, mappings::Vector{VirtualMapping})
    # Write virtual dataset mappings to global heap
    hid = write_virtual_mappings_to_heap(f, mappings)
    data_address = hid.heap_offset
    index = hid.index
    layout_class=LcVirtual

    # Build dataspace kwargs with optional max_dimensions
    # Only write max_dimensions if different from size (indicates unlimited or explicitly set max)
    has_max_dims = dataspace.max_dimensions != dataspace.size
    dataspace_kwargs = (
        flags = has_max_dims ? 0x01 : 0x00,
        dataspace_type = dataspace.dataspace_type,
        dimensions = dataspace.size,
        max_dimension_size = dataspace.max_dimensions,
    )

    psz = jlsizeof(Val(HmDataspace); dataspace_kwargs...)
    psz += jlsizeof(Val(HmDatatype), 1 | (2*isa(datatype, CommittedDatatype)); dt=datatype)
    psz += jlsizeof(Val(HmDataLayout); layout_class, data_address, index)
    psz += CONTINUATION_MSG_SIZE

    fullsz = jlsizeof(ObjectStart) + size_size(psz) + psz + 4

    # Allocate space at end of file
    header_offset = f.end_of_data
    io = f.io
    seek(io, header_offset)
    f.end_of_data = header_offset + fullsz

    # Write object header with checksum
    cio = begin_checksum_write(io, fullsz - 4)
    jlwrite(cio, ObjectStart(size_flag(psz)))
    write_size(cio, psz)

    # Delegate message writing to callback
    write_header_message(cio, Val(HmDataspace); dataspace_kwargs...)
    write_header_message(cio, Val(HmDatatype), 1 | (2*isa(datatype, CommittedDatatype)); dt=datatype)
    write_header_message(cio, Val(HmDataLayout); layout_class, data_address, index)
    # Finalize with continuation and checksum
    write_continuation_placeholder(cio)
    jlwrite(io, end_checksum(cio))

    return h5offset(f, header_offset)
end

"""
    write_virtual_mappings_to_heap(f, mappings)

Write virtual dataset mappings to global heap, return heap address and index.
"""
function write_virtual_mappings_to_heap(f::JLDFile, mappings::Vector{VirtualMapping})
    # Build VDS heap block data in buffer
    io_buf = IOBuffer()

    jlwrite(io_buf, UInt8(0))              # Version
    jlwrite(io_buf, UInt64(length(mappings)))  # Number of entries

    # Write each mapping
    for mapping in mappings
        # Null-terminated strings
        write(io_buf, mapping.source_filename, UInt8(0))
        write(io_buf, mapping.source_dataset_name, UInt8(0))

        # Selections
        jlwrite(io_buf, mapping.src_selection)
        jlwrite(io_buf, mapping.vds_selection)
    end

    # Append lookup3 checksum
    data = take!(io_buf)
    full_data = vcat(data, reinterpret(UInt8, [Lookup3.hash(data, 1, length(data), UInt32(0))]))

    return write_vds_to_global_heap(f, full_data) # returns GlobalHeapID
end

# Write VDS data to global heap (raw bytes variant of write_heap_object)
function write_vds_to_global_heap(f::JLDFile, data::Vector{UInt8})
    psz = length(data)
    objsz = 8 + jlsizeof(Length) + psz
    objsz += 8 - mod1(objsz, 8)
    io = f.io

    # Allocate space in global heap
    gh = allocate_in_global_heap(f, objsz)

    # Write object header
    index = length(gh.objects) + 1
    objoffset = gh.offset + 8 + jlsizeof(Length) + gh.length - gh.free
    seek(io, objoffset)
    jlwrite(io, UInt16(index))           # Heap object index
    jlwrite(io, UInt16(0))               # Reference count (0 for VDS)
    jlwrite(io, UInt32(0))               # Reserved
    jlwrite(io, Length(psz))             # Object size

    # Update global heap
    gh.free -= objsz
    push!(gh.objects, objoffset)

    # Write free space object if needed
    if gh.free >= 8 + jlsizeof(Length)
        seek(io, objoffset + objsz)
        jlwrite(io, UInt64(0))           # Object index, reference count, reserved
        jlwrite(io, Length(gh.free - 8 - jlsizeof(Length))) # Object size
    end

    # Write actual data
    seek(io, objoffset + 8 + jlsizeof(Length))
    write(io, data)

    return GlobalHeapID(h5offset(f, gh.offset), UInt32(index))
end
