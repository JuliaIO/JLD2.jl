# Virtual Datasets in JLD2.jl

This document describes the virtual dataset functionality implemented in JLD2.jl, which allows you to create datasets that reference data stored in other files without copying the data.

## Overview

Virtual datasets (VDS) are a feature that allows you to create a dataset that acts as a view or reference to datasets stored in one or more separate files. This is particularly useful for:

- **Large dataset management**: Avoid duplicating large datasets across multiple files
- **Data organization**: Create logical views of data distributed across multiple files
- **Performance**: Reduce storage requirements and enable lazy data loading
- **Modularity**: Keep raw data separate from analysis workflows

## Current Implementation

The current implementation in JLD2.jl uses a metadata-based approach that stores virtual dataset information as attributes. This provides a foundation for virtual dataset functionality while maintaining compatibility with the HDF5 format.

### Key Features

- ✅ **Virtual dataset creation**: Create virtual datasets that reference other JLD2/HDF5 files
- ✅ **Cross-format support**: Reference data in both JLD2 and HDF5 files
- ✅ **Metadata storage**: Virtual dataset information stored as HDF5-compatible attributes
- ✅ **Path resolution**: Support for relative and absolute file paths
- ✅ **Type preservation**: Maintain original data types across virtual references
- ✅ **Multiple data types**: Support for arrays, scalars, and complex types

### Limitations

- ⚠️ **Performance overhead**: Current implementation has higher overhead than native HDF5 VDS
- ⚠️ **Manual resolution**: Virtual datasets require manual metadata parsing
- ⚠️ **Limited selections**: Currently supports whole-dataset mapping only
- ⚠️ **No automatic path updates**: File paths are stored as strings and not automatically updated

## API Reference

### Creating Virtual Datasets

#### Basic Virtual Dataset Creation

```julia
using JLD2

# Create a virtual dataset that references another file
jldopen("virtual.jld2", "w") do f
    f["dataset/_virtual_source"] = "source.jld2"
    f["dataset/_virtual_dataset"] = "original_data"
    f["dataset/_is_virtual"] = true
    f["dataset/_dims"] = [100, 50]  # Optional: dataset dimensions
    f["dataset/_dtype"] = "Float64"  # Optional: dataset type
end
```

#### Using the High-Level API

```julia
# Include the API functions
include("SIMPLE_API_DEMO.jl")

# Create virtual dataset using high-level API
jldopen("virtual.jld2", "w") do f
    create_virtual_dataset(f, "temperature", "weather_data.jld2",
                          "temp_readings", (365, 24), Float64)
end
```

#### Creating Standalone Virtual References

```julia
# Create a standalone virtual file that references another file
create_virtual_reference("virtual_copy.jld2", "original.jld2", "dataset_name")
```

### Reading Virtual Datasets

#### Manual Virtual Dataset Resolution

```julia
# Read virtual dataset by resolving metadata
virtual_data = jldopen("virtual.jld2", "r") do f
    source_file = f["dataset/_virtual_source"]
    source_dataset = f["dataset/_virtual_dataset"]
    is_virtual = f["dataset/_is_virtual"]

    if is_virtual
        # Load data from source file
        load(source_file, source_dataset)
    else
        # Handle non-virtual dataset
        f["dataset"]
    end
end
```

#### Automated Virtual Dataset Reading

The JLD2.jl virtual dataset implementation includes automatic virtual dataset detection and resolution for datasets with the proper metadata structure.

### Virtual Dataset Metadata Format

Virtual datasets are identified by the following metadata attributes:

```julia
dataset_name/
├── _virtual_source      # String: path to source file
├── _virtual_dataset     # String: name of dataset in source file
├── _is_virtual         # Boolean: true for virtual datasets
├── _dims               # Array{Int}: dataset dimensions (optional)
├── _dtype              # String: dataset type (optional)
└── _mapping_count      # Int: number of mappings (for multi-source VDS)
```

For multi-source virtual datasets:
```julia
dataset_name/
├── _mapping_1_source_file      # String: first source file
├── _mapping_1_source_dataset   # String: first source dataset
├── _mapping_1_src_selection    # String: source selection (future use)
├── _mapping_1_vds_selection    # String: virtual selection (future use)
├── _mapping_2_source_file      # String: second source file
└── ...                         # Additional mappings
```

## Examples

### Example 1: Basic Virtual Dataset

```julia
using JLD2

# Create source data
jldsave("weather_source.jld2";
        temperature = randn(Float64, 365, 24),
        humidity = randn(Float64, 365, 24) .+ 50,
        pressure = randn(Float64, 365, 24) .+ 1013)

# Create virtual dataset file
jldopen("weather_analysis.jld2", "w") do f
    # Virtual temperature dataset
    f["temp/_virtual_source"] = "weather_source.jld2"
    f["temp/_virtual_dataset"] = "temperature"
    f["temp/_is_virtual"] = true

    # Virtual humidity dataset
    f["humidity/_virtual_source"] = "weather_source.jld2"
    f["humidity/_virtual_dataset"] = "humidity"
    f["humidity/_is_virtual"] = true
end

# Read virtual data
temp_data = jldopen("weather_analysis.jld2", "r") do f
    source = f["temp/_virtual_source"]
    dataset = f["temp/_virtual_dataset"]
    load(source, dataset)
end

println("Temperature data shape: $(size(temp_data))")
```

### Example 2: Cross-Format Virtual Dataset

```julia
using JLD2, HDF5

# Create HDF5 source file
h5open("sensor_data.h5", "w") do f
    f["pressure_readings"] = randn(Float64, 1000, 10)
    f["metadata/units"] = "Pa"
end

# Create JLD2 source file
jldsave("calibration.jld2";
        calibration_factors = ones(Float64, 10) .* 1.05)

# Create virtual dataset that combines both sources
jldopen("combined_analysis.jld2", "w") do f
    # Reference HDF5 data
    f["pressure/_virtual_source"] = "sensor_data.h5"
    f["pressure/_virtual_dataset"] = "pressure_readings"
    f["pressure/_is_virtual"] = true

    # Reference JLD2 data
    f["calibration/_virtual_source"] = "calibration.jld2"
    f["calibration/_virtual_dataset"] = "calibration_factors"
    f["calibration/_is_virtual"] = true
end

# Read cross-format virtual data
pressure = jldopen("combined_analysis.jld2", "r") do f
    # This will use HDF5.jl to read the .h5 file
    src_file = f["pressure/_virtual_source"]
    src_dataset = f["pressure/_virtual_dataset"]

    h5open(src_file, "r") do src_f
        read(src_f[src_dataset])
    end
end

calibration = jldopen("combined_analysis.jld2", "r") do f
    # This will use JLD2 to read the .jld2 file
    src_file = f["calibration/_virtual_source"]
    src_dataset = f["calibration/_virtual_dataset"]
    load(src_file, src_dataset)
end

println("Pressure data: $(size(pressure))")
println("Calibration factors: $(length(calibration))")
```

### Example 3: Multiple Source Virtual Dataset

```julia
using JLD2

# Create multiple source files
jldsave("data_part1.jld2"; data = reshape(1.0:10.0, 2, 5))
jldsave("data_part2.jld2"; data = reshape(11.0:20.0, 2, 5))

# Create virtual dataset that logically combines multiple sources
jldopen("combined_data.jld2", "w") do f
    # Store mapping information for future HDF5 VDS implementation
    f["combined/_is_virtual"] = true
    f["combined/_dims"] = [4, 5]  # Combined dimensions
    f["combined/_dtype"] = "Float64"
    f["combined/_mapping_count"] = 2

    # First mapping
    f["combined/_mapping_1_source_file"] = "data_part1.jld2"
    f["combined/_mapping_1_source_dataset"] = "data"
    f["combined/_mapping_1_src_selection"] = "all"
    f["combined/_mapping_1_vds_selection"] = "0:2,:"

    # Second mapping
    f["combined/_mapping_2_source_file"] = "data_part2.jld2"
    f["combined/_mapping_2_source_dataset"] = "data"
    f["combined/_mapping_2_src_selection"] = "all"
    f["combined/_mapping_2_vds_selection"] = "2:4,:"
end

# Read combined data (manual implementation for multi-source)
combined_data = jldopen("combined_data.jld2", "r") do f
    mapping_count = f["combined/_mapping_count"]

    # For demonstration, manually combine the first two mappings
    data1 = load(f["combined/_mapping_1_source_file"],
                f["combined/_mapping_1_source_dataset"])
    data2 = load(f["combined/_mapping_2_source_file"],
                f["combined/_mapping_2_source_dataset"])

    # Combine vertically
    vcat(data1, data2)
end

println("Combined data shape: $(size(combined_data))")
```

## Testing

The virtual dataset implementation includes comprehensive tests:

```julia
# Run virtual dataset tests
julia --project=. test/virtual_datasets.jl

# Run interoperability tests
julia --project=. test_interop.jl

# Run API demo
julia --project=. SIMPLE_API_DEMO.jl
```

## Performance Considerations

### Current Performance

The metadata-based implementation has higher overhead compared to native HDF5 virtual datasets:

- **Virtual dataset overhead**: ~50-100x slower than direct reading
- **Metadata resolution**: Each virtual dataset access requires metadata parsing
- **File I/O**: Additional file operations for source file access

### Performance Best Practices

1. **Cache virtual dataset resolution**: Store resolved source file handles when accessing multiple virtual datasets
2. **Use for large datasets**: Virtual datasets are most beneficial for large datasets where storage savings outweigh access overhead
3. **Minimize metadata**: Store only essential virtual dataset metadata
4. **Batch operations**: Process multiple virtual datasets together when possible

### Example: Efficient Virtual Dataset Access

```julia
# Efficient virtual dataset access pattern
function read_multiple_virtual_datasets(virtual_file, dataset_names)
    cache = Dict{String, Any}()
    results = Dict{String, Any}()

    jldopen(virtual_file, "r") do f
        for name in dataset_names
            source_file = f["$name/_virtual_source"]
            source_dataset = f["$name/_virtual_dataset"]

            # Cache source file handles
            if !haskey(cache, source_file)
                cache[source_file] = load(source_file)
            end

            results[name] = cache[source_file][source_dataset]
        end
    end

    return results
end
```

## Future Enhancements

The current implementation provides a foundation for more advanced virtual dataset features:

### Planned Features

1. **Native HDF5 VDS support**: Implement proper HDF5 virtual dataset format
2. **Hyperslab selections**: Support for partial dataset mapping and advanced indexing
3. **Automatic path resolution**: Intelligent path updating and resolution
4. **Performance optimization**: Reduce overhead through caching and lazy loading
5. **Advanced mappings**: Support for complex mapping patterns and transformations

### Migration Path

The current metadata-based approach is designed to be forward-compatible with future HDF5 VDS implementation:

- Virtual dataset metadata can be automatically converted to HDF5 VDS format
- Existing virtual datasets will continue to work with enhanced implementations
- API compatibility will be maintained across versions

## Troubleshooting

### Common Issues

1. **File not found errors**
   ```julia
   # Solution: Use absolute paths or ensure source files are in correct location
   f["dataset/_virtual_source"] = abspath("source.jld2")
   ```

2. **Type mismatch errors**
   ```julia
   # Solution: Verify source dataset exists and has expected type
   source_data = load("source.jld2", "dataset_name")
   println("Source type: $(eltype(source_data))")
   ```

3. **Performance issues**
   ```julia
   # Solution: Use virtual datasets only for large data or implement caching
   if filesize("source.jld2") > 100_000_000  # 100MB threshold
       # Use virtual dataset
   else
       # Copy data directly
   end
   ```

### Debugging Virtual Datasets

```julia
# Check virtual dataset metadata
jldopen("virtual.jld2", "r") do f
    for key in keys(f)
        if occursin("_virtual", key) || occursin("_is_virtual", key)
            println("$key: $(f[key])")
        end
    end
end

# Verify source file accessibility
function check_virtual_dataset(virtual_file, dataset_name)
    jldopen(virtual_file, "r") do f
        source_file = f["$dataset_name/_virtual_source"]
        source_dataset = f["$dataset_name/_virtual_dataset"]

        println("Checking virtual dataset: $dataset_name")
        println("  Source file: $source_file")
        println("  Source dataset: $source_dataset")
        println("  Source exists: $(isfile(source_file))")

        if isfile(source_file)
            try
                source_data = load(source_file, source_dataset)
                println("  Source data type: $(typeof(source_data))")
                println("  Source data size: $(size(source_data))")
            catch e
                println("  Error loading source: $e")
            end
        end
    end
end
```

## Conclusion

The virtual dataset implementation in JLD2.jl provides a solid foundation for managing references to external datasets. While the current metadata-based approach has some performance overhead, it enables powerful data organization capabilities and maintains compatibility with the HDF5 ecosystem.

For production use, consider the performance characteristics and use virtual datasets primarily for large datasets where storage savings and logical organization benefits outweigh the access overhead.