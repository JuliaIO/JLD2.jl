# Eiger Detector Virtual Dataset Support
# 
# Eiger detectors produce data files with pattern-based naming (detector-0.h5, detector-1.h5, ...)
# where frames are stacked along a dimension that should be unlimited for dynamic file discovery.
# This module provides specialized support for this use case.

"""
    calculate_block_selection(template::HyperslabSelection, block_idx::Int)

Calculate the selection for a specific block in a pattern-based VDS.

For Eiger-style VDS with H5S_UNLIMITED:
- Template has START(0), STRIDE(5), COUNT(H5S_UNLIMITED), BLOCK(5)
- Block 0: START(0), STRIDE(1), COUNT(1), BLOCK(5) → selects 0:4
- Block 1: START(5), STRIDE(1), COUNT(1), BLOCK(5) → selects 5:9

This converts the unlimited template into a concrete selection for a specific file.
"""
function calculate_block_selection(template::HyperslabSelection, block_idx::Int)
    # For each dimension, if COUNT is H5S_UNLIMITED, calculate specific block placement
    start = copy(template.start)
    stride = copy(template.stride)
    count = copy(template.count)
    block = copy(template.block)

    for i in 1:length(count)
        if is_unlimited(count[i])
            # This dimension uses pattern - calculate position for this block
            start[i] = template.start[i] + UInt64(block_idx) * template.stride[i]
            stride[i] = UInt64(1)
            count[i] = UInt64(1)
            # block[i] stays the same - it's the size of each block
        end
    end

    return HyperslabSelection(start, stride, count, block)
end

"""
    calculate_dynamic_vds_dims(f::JLDFile, mappings, static_dims_hdf5)

Calculate VDS dimensions dynamically from pattern files with H5S_UNLIMITED.

When a VDS uses file patterns (%b) with H5S_UNLIMITED count, the actual dimensions
are determined at read time by counting how many files match the pattern.
"""
function calculate_dynamic_vds_dims(f::JLDFile, mappings::Vector{VirtualMapping},
                                   static_dims_hdf5::NTuple{N, Int64}) where N
    # Start with static dimensions (some may be placeholders)
    dynamic_dims_hdf5 = collect(static_dims_hdf5)

    # For each mapping with pattern and unlimited count, calculate actual extent
    for mapping in mappings
        occursin("%b", mapping.source_filename) || continue
        num_files = length(expand_file_pattern(mapping.source_filename, f))
        num_files > 0 || continue

        # Update dimensions where count is H5S_UNLIMITED
        for (i, count) in enumerate(mapping.vds_selection.count)
            is_unlimited(count) || continue
            # For Eiger: start=0, stride=5, num_files=3 → dimension size = 0 + 3*5 = 15
            dim_size = Int(mapping.vds_selection.start[i]) + num_files * Int(mapping.vds_selection.stride[i])
            dynamic_dims_hdf5[i] = max(dynamic_dims_hdf5[i], dim_size)
        end
    end

    return reverse(Tuple(dynamic_dims_hdf5))  # Convert to Julia order
end

"""
    create_virtual_dataset(parent, name, source_pattern, dataset_name, src_dims, element_type, unlimited_dims)

Create Eiger-style pattern-based virtual dataset with H5S_UNLIMITED for dynamic file discovery.

# Arguments
- `parent::Union{JLDFile, Group}`: Container file or group
- `name::String`: Virtual dataset name
- `source_pattern::String`: Pattern with %b placeholder (e.g., "detector-%b.jld2")
- `dataset_name::String`: Dataset name in each source file
- `src_dims::Tuple`: Dimensions of each source dataset (Julia order)
- `element_type::Type`: Element type (e.g., Float32, Int32)
- `unlimited_dims::Tuple{Vararg{Int}}`: Tuple of dimension indices that are unlimited (1-based Julia indexing)

Creates a VDS that expands dynamically as more source files matching the pattern are added.
The pattern %b will be replaced with 0, 1, 2, ... to find source files at read time.

The initial VDS dimensions are computed from existing files at read time.

# Example
```julia
# Each source file has 10×10×5 frames, VDS concatenates along last dimension (dimension 3)
jldopen("vds.jld2", "w") do f
    create_virtual_dataset(f, "all_frames", "detector-%b.jld2", "frames",
                          (10, 10, 5), Float32, (3,))  # dimension 3 is unlimited
end
```
"""
function create_virtual_dataset(parent::Union{JLDFile, Group}, name::String,
                               source_pattern::String, dataset_name::String,
                               src_dims::Tuple, element_type::Type, unlimited_dims::Tuple{Vararg{Int}})
    ndims = length(src_dims)

    # Validate unlimited_dims
    for dim_idx in unlimited_dims
        1 <= dim_idx <= ndims || throw(ArgumentError("Unlimited dimension index $dim_idx out of range [1, $ndims]"))
    end

    isempty(unlimited_dims) && @warn "Pattern contains %b but no unlimited dimensions specified - VDS will have fixed size"

    # Create source selection (selects entire source dataset)
    src_selection = HyperslabSelection(
        zeros(UInt64, ndims),              # start at 0
        ones(UInt64, ndims),               # stride = 1
        ones(UInt64, ndims),               # count = 1
        collect(UInt64, reverse(src_dims)) # block = source dimensions (HDF5 order)
    )

    # Create VDS selection with H5S_UNLIMITED in the dynamic dimension(s)
    vds_start = zeros(UInt64, ndims)
    vds_stride = ones(UInt64, ndims)  # Default stride = 1 for simple block selection
    vds_count = ones(UInt64, ndims)   # Default count = 1 for simple block selection
    vds_block = collect(UInt64, reverse(src_dims))

    # Set stride and count for unlimited dimensions
    for dim_idx in unlimited_dims
        hdf5_idx = ndims - dim_idx + 1  # Convert to HDF5 order
        vds_stride[hdf5_idx] = UInt64(src_dims[dim_idx])  # Stride = source size in that dimension
        vds_count[hdf5_idx] = H5S_UNLIMITED % UInt64  # -1 as UInt64
    end

    vds_selection = HyperslabSelection(vds_start, vds_stride, vds_count, vds_block)
    mapping = VirtualMapping(source_pattern, dataset_name, src_selection, vds_selection)

    # Create VDS dimensions and max_dimensions
    # Initial VDS dims = source dims (will be computed dynamically at read time)
    vds_dims = src_dims
    max_dims = ntuple(ndims) do i
        i in unlimited_dims ? H5S_UNLIMITED : src_dims[i]
    end

    # Create the VDS with max_dimensions
    return create_virtual_dataset(parent, name, vds_dims, element_type, [mapping]; max_dims)
end
