#!/usr/bin/env julia
# Virtual Dataset API Demo for JLD2.jl

using JLD2
using JLD2: JLDFile

"""
    create_virtual_dataset(f::JLDFile, name::String, source_file::String,
                          source_dataset::String, dims::Tuple, dtype::Type)

Create a virtual dataset in file `f` with name `name` that references
`source_dataset` in `source_file`.

# Arguments
- `f::JLDFile`: The JLD2 file to create the virtual dataset in
- `name::String`: Name of the virtual dataset
- `source_file::String`: Path to the source file containing the actual data
- `source_dataset::String`: Name of the dataset in the source file
- `dims::Tuple`: Dimensions of the virtual dataset
- `dtype::Type`: Data type of the virtual dataset

# Example
```julia
jldopen("virtual.jld2", "w") do f
    create_virtual_dataset(f, "virtual_data", "source.jld2", "real_data", (100, 50), Float64)
end
```
"""
function create_virtual_dataset(f::JLDFile, name::String, source_file::String,
                               source_dataset::String, dims::Tuple, dtype::Type)
    # For now, store virtual dataset metadata as attributes
    # In the future, this could be enhanced to write proper HDF5 VDS format

    f["$name/_virtual_source"] = source_file
    f["$name/_virtual_dataset"] = source_dataset
    f["$name/_is_virtual"] = true
    f["$name/_dims"] = collect(dims)
    f["$name/_dtype"] = string(dtype)

    println("✅ Virtual dataset created: $name → $source_file:/$source_dataset")
end

"""
    VirtualDatasetEntry(source_file, source_dataset, src_selection=all_selection(),
                       vds_selection=all_selection())

Create a virtual dataset mapping entry.

# Arguments
- `source_file::String`: Path to source file
- `source_dataset::String`: Name of dataset in source file
- `src_selection`: Selection from source (default: entire dataset)
- `vds_selection`: Selection in virtual dataset (default: entire dataset)
"""
struct VirtualDatasetEntry
    source_file::String
    source_dataset::String
    src_selection::Union{Symbol, Tuple}
    vds_selection::Union{Symbol, Tuple}
end

# Convenience constructor for entire dataset mapping
VirtualDatasetEntry(source_file::String, source_dataset::String) =
    VirtualDatasetEntry(source_file, source_dataset, :all, :all)

# Symbol to represent selecting entire dataset
all_selection() = :all

"""
    create_virtual_reference(virtual_file::String, source_file::String, dataset_name::String)

Create a standalone virtual dataset file that references another file.

# Arguments
- `virtual_file::String`: Path to virtual dataset file to create
- `source_file::String`: Path to source file containing actual data
- `dataset_name::String`: Name of dataset to reference

# Example
```julia
create_virtual_reference("virtual.jld2", "source.jld2", "data")
```
"""
function create_virtual_reference(virtual_file::String, source_file::String, dataset_name::String)
    jldopen(virtual_file, "w") do f
        # Read source to get dimensions and type
        source_info = jldopen(source_file, "r") do src_f
            data = src_f[dataset_name]
            (size(data), eltype(data))
        end

        dims, dtype = source_info

        # Create mapping for entire dataset
        mappings = [
            VirtualDatasetEntry(
                source_file,           # source file path
                dataset_name,          # source dataset path
                all_selection(),       # select entire source
                all_selection()        # map to entire virtual dataset
            )
        ]

        # Create the virtual dataset
        create_virtual_dataset(f, dataset_name, dims, dtype, mappings)

        # Add metadata
        f["_virtual_source"] = source_file
        f["_virtual_dataset"] = dataset_name
    end

    println("✅ Virtual dataset created: $virtual_file references $source_file:/$dataset_name")
end

"""
    create_virtual_dataset(f::JLDFile, name::String, dims::Tuple, dtype::Type,
                          mappings::Vector{VirtualDatasetEntry})

Create a virtual dataset with multiple source mappings.

# Arguments
- `f::JLDFile`: Target file
- `name::String`: Virtual dataset name
- `dims::Tuple`: Virtual dataset dimensions
- `dtype::Type`: Virtual dataset data type
- `mappings::Vector{VirtualDatasetEntry}`: List of source mappings
"""
function create_virtual_dataset(f::JLDFile, name::String, dims::Tuple, dtype::Type,
                               mappings::Vector{VirtualDatasetEntry})
    # Store virtual dataset metadata
    f["$name/_is_virtual"] = true
    f["$name/_dims"] = collect(dims)
    f["$name/_dtype"] = string(dtype)
    f["$name/_mapping_count"] = length(mappings)

    # Store each mapping
    for (i, mapping) in enumerate(mappings)
        f["$name/_mapping_$(i)_source_file"] = mapping.source_file
        f["$name/_mapping_$(i)_source_dataset"] = mapping.source_dataset
        f["$name/_mapping_$(i)_src_selection"] = string(mapping.src_selection)
        f["$name/_mapping_$(i)_vds_selection"] = string(mapping.vds_selection)
    end

    println("✅ Virtual dataset '$name' created with $(length(mappings)) mapping(s)")
end

# ===== Demo Functions =====

function demo_basic_virtual_dataset()
    println("\n=== Demo: Basic Virtual Dataset ===")

    # Create source data
    println("1. Creating source file...")
    jldsave("demo_source.jld2";
            temperature = randn(Float64, 100, 50),
            pressure = randn(Float32, 100, 50) .+ 1000,
            metadata = "Sensor data from 2024")

    # Create virtual dataset
    println("2. Creating virtual dataset...")
    jldopen("demo_virtual.jld2", "w") do f
        create_virtual_dataset(f, "temperature", "demo_source.jld2",
                             "temperature", (100, 50), Float64)
        create_virtual_dataset(f, "pressure", "demo_source.jld2",
                             "pressure", (100, 50), Float32)
    end

    # Test reading
    println("3. Testing virtual dataset reading...")
    temp_direct = load("demo_source.jld2", "temperature")

    # Read virtual dataset metadata and load manually
    jldopen("demo_virtual.jld2", "r") do f
        source_file = f["temperature/_virtual_source"]
        source_dataset = f["temperature/_virtual_dataset"]

        temp_virtual = load(source_file, source_dataset)

        println("   ✅ Data consistency: $(temp_virtual == temp_direct)")
        println("   📊 Temperature data: $(size(temp_virtual)) $(eltype(temp_virtual))")
    end
end

function demo_file_reference()
    println("\n=== Demo: File Reference ===")

    # Create source
    println("1. Creating source with multiple datasets...")
    jldsave("demo_multi_source.jld2";
            data_a = reshape(1.0:20.0, 4, 5),
            data_b = collect(101:110),
            info = "Multi-dataset source")

    # Create virtual references
    println("2. Creating virtual reference files...")
    create_virtual_reference("virtual_a.jld2", "demo_multi_source.jld2", "data_a")
    create_virtual_reference("virtual_b.jld2", "demo_multi_source.jld2", "data_b")

    # Test reading
    println("3. Testing virtual references...")
    data_a_orig = load("demo_multi_source.jld2", "data_a")
    data_a_virtual = load("virtual_a.jld2", "_virtual_source") |>
                     source -> load(source, load("virtual_a.jld2", "_virtual_dataset"))

    println("   ✅ Reference A works: $(data_a_orig == data_a_virtual)")
end

function demo_combined_virtual_dataset()
    println("\n=== Demo: Combined Virtual Dataset ===")

    # Create multiple source files
    println("1. Creating multiple source files...")
    jldsave("part1.jld2"; data = reshape(1.0:10.0, 2, 5))
    jldsave("part2.jld2"; data = reshape(11.0:20.0, 2, 5))

    # Create combined virtual dataset
    println("2. Creating combined virtual dataset...")
    mappings = [
        VirtualDatasetEntry("part1.jld2", "data"),
        VirtualDatasetEntry("part2.jld2", "data")
    ]

    jldopen("combined_virtual.jld2", "w") do f
        create_virtual_dataset(f, "combined_data", (4, 5), Float64, mappings)
    end

    println("3. Virtual dataset metadata stored for future HDF5 VDS implementation")
end

function cleanup_demo_files()
    println("\n=== Cleaning up demo files ===")
    demo_files = [
        "demo_source.jld2", "demo_virtual.jld2",
        "demo_multi_source.jld2", "virtual_a.jld2", "virtual_b.jld2",
        "part1.jld2", "part2.jld2", "combined_virtual.jld2"
    ]

    for file in demo_files
        if isfile(file)
            rm(file)
            println("  🗑️  Removed $file")
        end
    end
end

function main()
    println("🚀 JLD2 Virtual Dataset API Demo")
    println("=" ^ 50)

    demo_basic_virtual_dataset()
    demo_file_reference()
    demo_combined_virtual_dataset()

    println("\n" * "="^50)
    println("✨ Demo completed! Virtual dataset API working.")
    println("📝 Note: Current implementation uses metadata attributes.")
    println("🔄 Future: Will be enhanced with proper HDF5 VDS format.")

    print("\nClean up demo files? (y/N): ")
    response = readline()
    if lowercase(strip(response)) == "y"
        cleanup_demo_files()
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end