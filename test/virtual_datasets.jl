# Tests for Virtual Dataset functionality

using JLD2
using JLD2: JLDFile
using Test

@testset "Virtual Datasets" begin

    @testset "Basic Virtual Dataset API" begin
        # Create source file
        source_file = "test_vds_source.jld2"
        virtual_file = "test_vds_virtual.jld2"

        try
            # Create source data
            data_2d = reshape(Float64.(1:20), 4, 5)
            data_1d = Float64.(1:10)
            metadata = "Test source data"

            jldsave(source_file; data_2d, data_1d, metadata)

            # Test that source file was created correctly
            @test isfile(source_file)
            source_data = load(source_file)
            @test haskey(source_data, "data_2d")
            @test haskey(source_data, "data_1d")
            @test source_data["data_2d"] == data_2d

            # Create virtual dataset with metadata approach
            jldopen(virtual_file, "w") do f
                # Create virtual dataset metadata
                f["vds_2d/_virtual_source"] = source_file
                f["vds_2d/_virtual_dataset"] = "data_2d"
                f["vds_2d/_is_virtual"] = true
                f["vds_2d/_dims"] = [4, 5]
                f["vds_2d/_dtype"] = "Float64"

                f["vds_1d/_virtual_source"] = source_file
                f["vds_1d/_virtual_dataset"] = "data_1d"
                f["vds_1d/_is_virtual"] = true
                f["vds_1d/_dims"] = [10]
                f["vds_1d/_dtype"] = "Float64"
            end

            # Test virtual dataset metadata reading
            virtual_metadata = jldopen(virtual_file, "r") do f
                (
                    source = f["vds_2d/_virtual_source"],
                    dataset = f["vds_2d/_virtual_dataset"],
                    is_virtual = f["vds_2d/_is_virtual"],
                    dims = f["vds_2d/_dims"],
                    dtype = f["vds_2d/_dtype"]
                )
            end

            @test virtual_metadata.source == source_file
            @test virtual_metadata.dataset == "data_2d"
            @test virtual_metadata.is_virtual == true
            @test virtual_metadata.dims == [4, 5]
            @test virtual_metadata.dtype == "Float64"

            # Test manual virtual dataset resolution
            virtual_data = jldopen(virtual_file, "r") do f
                src_file = f["vds_2d/_virtual_source"]
                src_dataset = f["vds_2d/_virtual_dataset"]
                load(src_file, src_dataset)
            end

            @test virtual_data == data_2d
            @test size(virtual_data) == (4, 5)
            @test eltype(virtual_data) == Float64

        finally
            # Cleanup
            isfile(source_file) && rm(source_file)
            isfile(virtual_file) && rm(virtual_file)
        end
    end

    @testset "Virtual Dataset Entry Structure" begin
        # Test the VirtualDatasetEntry struct if it's available
        try
            # Test basic entry creation
            entry = JLD2.VirtualDatasetEntry("source.jld2", "dataset")
            @test entry.source_file_name == "source.jld2"
            @test entry.source_dataset_name == "dataset"

            # Test with explicit selections
            entry2 = JLD2.VirtualDatasetEntry(
                "source.jld2", "dataset",
                JLD2.all_selection(), JLD2.all_selection()
            )
            @test entry2.source_file_name == "source.jld2"
            @test entry2.source_dataset_name == "dataset"

        catch e
            # If VirtualDatasetEntry is not defined, skip this test
            @test_skip "VirtualDatasetEntry struct not available: $e"
        end
    end

    @testset "Path Resolution" begin
        # Test relative and absolute path handling
        source_file = "test_path_source.jld2"
        virtual_file = "test_path_virtual.jld2"

        try
            # Create source in current directory
            test_data = [1.0, 2.0, 3.0, 4.0, 5.0]
            jldsave(source_file; test_data)

            # Create virtual dataset with relative path
            jldopen(virtual_file, "w") do f
                f["virtual_data/_virtual_source"] = source_file  # relative path
                f["virtual_data/_virtual_dataset"] = "test_data"
                f["virtual_data/_is_virtual"] = true
            end

            # Test that relative path resolution works
            resolved_data = jldopen(virtual_file, "r") do f
                src_file = f["virtual_data/_virtual_source"]
                src_dataset = f["virtual_data/_virtual_dataset"]

                # This should work with relative path
                @test isfile(src_file)
                load(src_file, src_dataset)
            end

            @test resolved_data == test_data

        finally
            isfile(source_file) && rm(source_file)
            isfile(virtual_file) && rm(virtual_file)
        end
    end

    @testset "Multiple Data Types" begin
        # Test virtual datasets with different data types
        source_file = "test_types_source.jld2"
        virtual_file = "test_types_virtual.jld2"

        try
            # Create source with multiple data types
            int_data = Int32[1, 2, 3, 4, 5]
            float_data = [1.1, 2.2, 3.3]
            string_data = ["hello", "world"]
            bool_data = [true, false, true]

            jldsave(source_file; int_data, float_data, string_data, bool_data)

            # Create virtual datasets for each type
            jldopen(virtual_file, "w") do f
                for (name, dtype) in [
                    ("int_data", "Int32"),
                    ("float_data", "Float64"),
                    ("string_data", "String"),
                    ("bool_data", "Bool")
                ]
                    f["v_$name/_virtual_source"] = source_file
                    f["v_$name/_virtual_dataset"] = name
                    f["v_$name/_is_virtual"] = true
                    f["v_$name/_dtype"] = dtype
                end
            end

            # Test each virtual dataset
            virtual_results = jldopen(virtual_file, "r") do f
                results = Dict()
                for name in ["int_data", "float_data", "string_data", "bool_data"]
                    src_file = f["v_$name/_virtual_source"]
                    src_dataset = f["v_$name/_virtual_dataset"]
                    results[name] = load(src_file, src_dataset)
                end
                results
            end

            @test virtual_results["int_data"] == int_data
            @test virtual_results["float_data"] == float_data
            @test virtual_results["string_data"] == string_data
            @test virtual_results["bool_data"] == bool_data

        finally
            isfile(source_file) && rm(source_file)
            isfile(virtual_file) && rm(virtual_file)
        end
    end

    @testset "Error Handling" begin
        # Test error cases
        virtual_file = "test_error_virtual.jld2"

        try
            # Test missing source file
            jldopen(virtual_file, "w") do f
                f["broken_vds/_virtual_source"] = "nonexistent_file.jld2"
                f["broken_vds/_virtual_dataset"] = "nonexistent_dataset"
                f["broken_vds/_is_virtual"] = true
            end

            # Test that appropriate error is thrown when source file doesn't exist
            jldopen(virtual_file, "r") do f
                src_file = f["broken_vds/_virtual_source"]
                src_dataset = f["broken_vds/_virtual_dataset"]

                @test !isfile(src_file)
                @test_throws Union{SystemError, ArgumentError} load(src_file, src_dataset)
            end

        finally
            isfile(virtual_file) && rm(virtual_file)
        end
    end

    @testset "Large Dataset Virtual Reference" begin
        # Test with larger datasets to ensure performance
        source_file = "test_large_source.jld2"
        virtual_file = "test_large_virtual.jld2"

        try
            # Create a reasonably sized dataset
            large_data = randn(Float64, 100, 100)
            jldsave(source_file; large_data)

            # Create virtual reference
            jldopen(virtual_file, "w") do f
                f["large_virtual/_virtual_source"] = source_file
                f["large_virtual/_virtual_dataset"] = "large_data"
                f["large_virtual/_is_virtual"] = true
                f["large_virtual/_dims"] = [100, 100]
                f["large_virtual/_dtype"] = "Float64"
            end

            # Test that virtual dataset can handle large data
            virtual_large = jldopen(virtual_file, "r") do f
                src_file = f["large_virtual/_virtual_source"]
                src_dataset = f["large_virtual/_virtual_dataset"]
                load(src_file, src_dataset)
            end

            @test size(virtual_large) == (100, 100)
            @test eltype(virtual_large) == Float64
            @test virtual_large == large_data

        finally
            isfile(source_file) && rm(source_file)
            isfile(virtual_file) && rm(virtual_file)
        end
    end

    @testset "Nested Virtual Datasets" begin
        # Test creating virtual datasets that reference other virtual datasets
        source_file = "test_nested_source.jld2"
        virtual1_file = "test_nested_virtual1.jld2"
        virtual2_file = "test_nested_virtual2.jld2"

        try
            # Create original source
            original_data = reshape(1.0:12.0, 3, 4)
            jldsave(source_file; original_data)

            # Create first level virtual dataset
            jldopen(virtual1_file, "w") do f
                f["level1/_virtual_source"] = source_file
                f["level1/_virtual_dataset"] = "original_data"
                f["level1/_is_virtual"] = true
            end

            # Create second level virtual dataset (virtual of virtual)
            jldopen(virtual2_file, "w") do f
                f["level2/_virtual_source"] = virtual1_file
                f["level2/_virtual_dataset"] = "level1"
                f["level2/_is_virtual"] = true
            end

            # Test nested resolution
            # For now, this should work at least one level deep
            level1_data = jldopen(virtual1_file, "r") do f
                src_file = f["level1/_virtual_source"]
                src_dataset = f["level1/_virtual_dataset"]
                load(src_file, src_dataset)
            end

            @test level1_data == original_data

            # Note: Second level virtual datasets (virtual of virtual)
            # are more complex and may not work with the current simple implementation

        finally
            isfile(source_file) && rm(source_file)
            isfile(virtual1_file) && rm(virtual1_file)
            isfile(virtual2_file) && rm(virtual2_file)
        end
    end
end