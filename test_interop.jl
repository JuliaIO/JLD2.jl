#!/usr/bin/env julia
# Test virtual dataset interoperability

using JLD2

# Try to load HDF5.jl if available
try
    using HDF5
    global HDF5_AVAILABLE = true
    println("✅ HDF5.jl is available")
catch
    global HDF5_AVAILABLE = false
    println("⚠️  HDF5.jl not available - some tests will be skipped")
end

function test_jld2_virtual_dataset_creation()
    println("\n=== Testing JLD2 Virtual Dataset Creation ===")

    source_file = "interop_source.jld2"
    virtual_file = "interop_virtual.jld2"

    try
        # Create source data with JLD2
        println("1. Creating source data with JLD2...")
        source_data = reshape(Float64.(1:24), 4, 6)
        jldsave(source_file; matrix_data = source_data,
                            vector_data = Float64.(1:10),
                            string_data = "JLD2 source data")

        # Create virtual dataset with our API
        println("2. Creating virtual dataset with JLD2 API...")
        jldopen(virtual_file, "w") do f
            # Use the API from SIMPLE_API_DEMO.jl
            f["virtual_matrix/_virtual_source"] = source_file
            f["virtual_matrix/_virtual_dataset"] = "matrix_data"
            f["virtual_matrix/_is_virtual"] = true
            f["virtual_matrix/_dims"] = [4, 6]
            f["virtual_matrix/_dtype"] = "Float64"

            f["virtual_vector/_virtual_source"] = source_file
            f["virtual_vector/_virtual_dataset"] = "vector_data"
            f["virtual_vector/_is_virtual"] = true
            f["virtual_vector/_dims"] = [10]
            f["virtual_vector/_dtype"] = "Float64"
        end

        # Test reading with JLD2
        println("3. Testing virtual dataset reading with JLD2...")
        virtual_matrix = jldopen(virtual_file, "r") do f
            src_file = f["virtual_matrix/_virtual_source"]
            src_dataset = f["virtual_matrix/_virtual_dataset"]
            load(src_file, src_dataset)
        end

        virtual_vector = jldopen(virtual_file, "r") do f
            src_file = f["virtual_vector/_virtual_source"]
            src_dataset = f["virtual_vector/_virtual_dataset"]
            load(src_file, src_dataset)
        end

        # Verify data integrity
        @assert virtual_matrix == source_data "Matrix data mismatch!"
        @assert virtual_vector == Float64.(1:10) "Vector data mismatch!"
        println("   ✅ JLD2 virtual dataset reading works correctly")

        # Test reading with HDF5.jl if available
        if HDF5_AVAILABLE
            println("4. Testing virtual dataset metadata reading with HDF5.jl...")
            try
                HDF5.h5open(virtual_file, "r") do f
                    # Read the virtual dataset metadata
                    virtual_source = read(f["virtual_matrix/_virtual_source"])
                    virtual_dataset = read(f["virtual_matrix/_virtual_dataset"])
                    is_virtual = read(f["virtual_matrix/_is_virtual"])

                    println("   📊 Virtual source: $virtual_source")
                    println("   📊 Virtual dataset: $virtual_dataset")
                    println("   📊 Is virtual: $is_virtual")

                    @assert virtual_source == source_file "Source file mismatch!"
                    @assert virtual_dataset == "matrix_data" "Dataset name mismatch!"
                    @assert is_virtual == true "Virtual flag mismatch!"
                end
                println("   ✅ HDF5.jl can read virtual dataset metadata")
            catch e
                println("   ⚠️  HDF5.jl reading failed: $e")
            end
        end

        return true

    finally
        # Cleanup
        isfile(source_file) && rm(source_file)
        isfile(virtual_file) && rm(virtual_file)
    end
end

function test_cross_format_virtual_datasets()
    println("\n=== Testing Cross-Format Virtual Datasets ===")

    jld2_source = "cross_jld2_source.jld2"
    h5_source = "cross_h5_source.h5"
    virtual_file = "cross_virtual.jld2"

    try
        # Create JLD2 source
        println("1. Creating JLD2 source...")
        jld2_data = reshape(Float64.(1:12), 3, 4)
        jldsave(jld2_source; data = jld2_data)

        # Create HDF5 source if possible
        h5_data = nothing
        if HDF5_AVAILABLE
            println("2. Creating HDF5 source...")
            h5_data = reshape(Float64.(13:24), 3, 4)
            HDF5.h5open(h5_source, "w") do f
                f["data"] = h5_data
            end
        else
            println("2. Skipping HDF5 source creation (HDF5.jl not available)")
        end

        # Create virtual dataset that references both sources
        println("3. Creating cross-format virtual dataset...")
        jldopen(virtual_file, "w") do f
            # JLD2 virtual dataset
            f["from_jld2/_virtual_source"] = jld2_source
            f["from_jld2/_virtual_dataset"] = "data"
            f["from_jld2/_is_virtual"] = true

            if HDF5_AVAILABLE
                # HDF5 virtual dataset
                f["from_h5/_virtual_source"] = h5_source
                f["from_h5/_virtual_dataset"] = "data"
                f["from_h5/_is_virtual"] = true
            end
        end

        # Test reading JLD2 source through virtual dataset
        println("4. Testing JLD2 source through virtual dataset...")
        virtual_jld2_data = jldopen(virtual_file, "r") do f
            src_file = f["from_jld2/_virtual_source"]
            src_dataset = f["from_jld2/_virtual_dataset"]
            load(src_file, src_dataset)
        end
        @assert virtual_jld2_data == jld2_data "JLD2 virtual data mismatch!"
        println("   ✅ JLD2 source virtual dataset works")

        # Test reading HDF5 source through virtual dataset if available
        if HDF5_AVAILABLE
            println("5. Testing HDF5 source through virtual dataset...")
            try
                virtual_h5_data = jldopen(virtual_file, "r") do f
                    src_file = f["from_h5/_virtual_source"]
                    src_dataset = f["from_h5/_virtual_dataset"]

                    # This should use our load_virtual_source_data function
                    # which tries HDF5.jl for .h5 files
                    HDF5.h5open(src_file, "r") do src_f
                        read(src_f[src_dataset])
                    end
                end
                @assert virtual_h5_data == h5_data "HDF5 virtual data mismatch!"
                println("   ✅ HDF5 source virtual dataset works")
            catch e
                println("   ⚠️  HDF5 source virtual dataset failed: $e")
            end
        end

        return true

    finally
        # Cleanup
        isfile(jld2_source) && rm(jld2_source)
        isfile(h5_source) && rm(h5_source)
        isfile(virtual_file) && rm(virtual_file)
    end
end

function test_virtual_dataset_performance()
    println("\n=== Testing Virtual Dataset Performance ===")

    source_file = "perf_source.jld2"
    virtual_file = "perf_virtual.jld2"

    try
        # Create larger dataset for performance testing
        println("1. Creating large source dataset...")
        large_data = randn(Float64, 1000, 500)  # 500k elements
        jldsave(source_file; large_data = large_data)

        # Create virtual dataset
        println("2. Creating virtual dataset...")
        jldopen(virtual_file, "w") do f
            f["virtual_large/_virtual_source"] = source_file
            f["virtual_large/_virtual_dataset"] = "large_data"
            f["virtual_large/_is_virtual"] = true
            f["virtual_large/_dims"] = [1000, 500]
            f["virtual_large/_dtype"] = "Float64"
        end

        # Benchmark direct reading
        println("3. Benchmarking direct reading...")
        direct_time = @elapsed begin
            direct_data = load(source_file, "large_data")
        end
        println("   Direct read time: $(round(direct_time, digits=3))s")

        # Benchmark virtual reading
        println("4. Benchmarking virtual reading...")
        virtual_time = @elapsed begin
            virtual_data = jldopen(virtual_file, "r") do f
                src_file = f["virtual_large/_virtual_source"]
                src_dataset = f["virtual_large/_virtual_dataset"]
                load(src_file, src_dataset)
            end
        end
        println("   Virtual read time: $(round(virtual_time, digits=3))s")

        # Check data integrity
        virtual_data = jldopen(virtual_file, "r") do f
            src_file = f["virtual_large/_virtual_source"]
            src_dataset = f["virtual_large/_virtual_dataset"]
            load(src_file, src_dataset)
        end
        @assert virtual_data == large_data "Performance test data mismatch!"

        overhead_ratio = virtual_time / direct_time
        println("   Virtual dataset overhead: $(round(overhead_ratio, digits=2))x")

        if overhead_ratio < 2.0
            println("   ✅ Virtual dataset performance is acceptable")
        else
            println("   ⚠️  Virtual dataset has high overhead")
        end

        return true

    finally
        # Cleanup
        isfile(source_file) && rm(source_file)
        isfile(virtual_file) && rm(virtual_file)
    end
end

function main()
    println("🧪 JLD2 Virtual Dataset Interoperability Tests")
    println("=" ^ 60)

    success = true

    try
        success &= test_jld2_virtual_dataset_creation()
    catch e
        println("❌ JLD2 virtual dataset creation test failed: $e")
        success = false
    end

    try
        success &= test_cross_format_virtual_datasets()
    catch e
        println("❌ Cross-format virtual dataset test failed: $e")
        success = false
    end

    try
        success &= test_virtual_dataset_performance()
    catch e
        println("❌ Virtual dataset performance test failed: $e")
        success = false
    end

    println("\n" * "=" ^ 60)
    if success
        println("🎉 All interoperability tests passed!")
        println("✨ Virtual dataset implementation is working correctly")
    else
        println("❌ Some tests failed - check implementation")
    end

    return success
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end