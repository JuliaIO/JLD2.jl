# Comprehensive tests for chunked array functionality
# Tests chunked array writing, reading, and validation against h5dump

using JLD2, Test, FileIO
using Pkg: Pkg
using JLD2Bzip2, JLD2Lz4

@testset "Chunked Arrays" begin
    @testset "Basic Chunked Array Writing and Reading" begin
        fn = joinpath(mktempdir(), "chunked_basic.jld2")

        # Test 2D array with simple chunking
        data_2d = collect(reshape(1.0:120.0, 30, 4))
        chunk_dims_2d = [10, 2]

        jldopen(fn, "w") do f
            JLD2.write_chunked_array(f, "data_2d", data_2d, chunk_dims_2d)
        end

        # Read back and verify
        result = jldopen(fn, "r") do f
            f["data_2d"]
        end
        @test result == data_2d
        @test size(result) == size(data_2d)
        @test eltype(result) == eltype(data_2d)

        rm(fn, force=true)
    end

    @testset "Various Chunk Sizes" begin
        fn = joinpath(mktempdir(), "chunked_sizes.jld2")

        test_cases = [
            ("small_chunks", collect(reshape(1:100, 10, 10)), [3, 3]),
            ("large_chunks", collect(reshape(1.0:1000.0, 50, 20)), [25, 10]),
            ("single_chunk", collect(reshape(1:24, 4, 6)), [4, 6]),
            ("1d_array", collect(1:100), [10]),
        ]

        jldopen(fn, "w") do f
            for (name, data, chunks) in test_cases
                JLD2.write_chunked_array(f, name, data, chunks)
            end
        end

        jldopen(fn, "r") do f
            for (name, expected_data, _) in test_cases
                result = f[name]
                @test result == expected_data
            end
        end

        rm(fn, force=true)
    end

    @testset "Edge Cases" begin
        fn = joinpath(mktempdir(), "chunked_edge.jld2")

        jldopen(fn, "w") do f
            # Non-evenly divisible dimensions
            data = collect(reshape(1:35, 7, 5))
            JLD2.write_chunked_array(f, "uneven", data, [3, 2])

            # Single element chunks
            data_single = collect(reshape(1:12, 3, 4))
            JLD2.write_chunked_array(f, "single_elem", data_single, [1, 1])

            # Chunk size equals array size
            data_full = collect(reshape(1.0:20.0, 4, 5))
            JLD2.write_chunked_array(f, "full_chunk", data_full, [4, 5])
        end

        jldopen(fn, "r") do f
            @test f["uneven"] == collect(reshape(1:35, 7, 5))
            @test f["single_elem"] == collect(reshape(1:12, 3, 4))
            @test f["full_chunk"] == collect(reshape(1.0:20.0, 4, 5))
        end

        rm(fn, force=true)
    end

    @testset "Different Data Types" begin
        fn = joinpath(mktempdir(), "chunked_types.jld2")

        jldopen(fn, "w") do f
            # Integers
            int_data = collect(reshape(Int32(1):Int32(60), 6, 10))
            JLD2.write_chunked_array(f, "int32", int_data, [2, 5])

            # Floats
            float_data = collect(reshape(1.0:40.0, 8, 5))
            JLD2.write_chunked_array(f, "float64", float_data, [4, 3])

            # Complex numbers
            complex_real = collect(1.0:30.0)
            complex_imag = collect(31.0:60.0)
            complex_data = collect(reshape(complex.(complex_real, complex_imag), 5, 6))
            JLD2.write_chunked_array(f, "complex", complex_data, [3, 3])
        end

        jldopen(fn, "r") do f
            @test f["int32"] == collect(reshape(Int32(1):Int32(60), 6, 10))
            @test f["float64"] == collect(reshape(1.0:40.0, 8, 5))
            complex_real = collect(1.0:30.0)
            complex_imag = collect(31.0:60.0)
            expected_complex = collect(reshape(complex.(complex_real, complex_imag), 5, 6))
            @test f["complex"] == expected_complex
        end

        rm(fn, force=true)
    end

    @testset "Chunked Arrays with Compression" begin
        fn = joinpath(mktempdir(), "chunked_compressed.jld2")

        # Create large, compressible data
        data = repeat(reshape(1.0:100.0, 10, 10), 3, 3)
        chunk_dims = [10, 10]

        # Test with Deflate compression
        jldopen(fn, "w"; compress=Deflate()) do f
            JLD2.write_chunked_array(f, "deflate", data, chunk_dims)
        end

        result = jldopen(fn, "r") do f
            f["deflate"]
        end
        @test result == data

        # Test with Zstd compression
        jldopen(fn, "w"; compress=ZstdFilter()) do f
            JLD2.write_chunked_array(f, "zstd", data, chunk_dims)
        end

        result = jldopen(fn, "r") do f
            f["zstd"]
        end
        @test result == data

        # Test with Shuffle + Deflate
        jldopen(fn, "w"; compress=[Shuffle(), Deflate()]) do f
            JLD2.write_chunked_array(f, "shuffle_deflate", data, chunk_dims)
        end

        result = jldopen(fn, "r") do f
            f["shuffle_deflate"]
        end
        @test result == data

        rm(fn, force=true)
    end

    @testset "Error Handling" begin
        fn = joinpath(mktempdir(), "chunked_errors.jld2")

        jldopen(fn, "w") do f
            data = collect(reshape(1:24, 4, 6))

            # Wrong number of chunk dimensions
            @test_throws ArgumentError JLD2.write_chunked_array(f, "wrong_dims", data, [3])
            @test_throws ArgumentError JLD2.write_chunked_array(f, "wrong_dims", data, [3, 2, 1])

            # Invalid chunk dimensions
            @test_throws ArgumentError JLD2.write_chunked_array(f, "zero_chunk", data, [0, 2])
            @test_throws ArgumentError JLD2.write_chunked_array(f, "neg_chunk", data, [3, -1])
        end

        rm(fn, force=true)

        # Read-only file
        fn_ro = joinpath(mktempdir(), "readonly.jld2")
        jldopen(fn_ro, "w") do f
            f["dummy"] = [1, 2, 3]
        end

        jldopen(fn_ro, "r") do f
            @test_throws ArgumentError JLD2.write_chunked_array(f, "data", reshape(1:12, 3, 4), [2, 2])
        end

        rm(fn_ro, force=true)
    end

    @testset "Large Arrays Triggering Multi-Node B-tree" begin
        fn = joinpath(mktempdir(), "chunked_large.jld2")

        # Create array with many chunks to trigger multi-node B-tree
        # 100x200 array with 3x2 chunks = ~3333 chunks (should exceed single node limit)
        data = collect(reshape(1.0:20000.0, 100, 200))
        chunk_dims = [3, 2]

        jldopen(fn, "w") do f
            JLD2.write_chunked_array(f, "large_data", data, chunk_dims)
        end

        result = jldopen(fn, "r") do f
            f["large_data"]
        end
        @test result == data
        @test size(result) == (100, 200)

        rm(fn, force=true)
    end
end

@testset "H5dump Validation" begin
    # Only run if h5dump is available
    h5dump_available = try
        run(pipeline(`h5dump --version`, stdout=devnull, stderr=devnull))
        true
    catch
        false
    end

    if h5dump_available
        fn = joinpath(mktempdir(), "h5dump_test.jld2")

        # Create a simple chunked array
        data = collect(reshape(1.0:24.0, 4, 6))
        chunk_dims = [2, 3]

        jldopen(fn, "w") do f
            JLD2.write_chunked_array(f, "test_data", data, chunk_dims)
        end

        # Run h5dump to verify structure
        h5dump_output = read(`h5dump -H $fn`, String)

        # Verify key features in output
        @test occursin("DATASPACE", h5dump_output)
        @test occursin("test_data", h5dump_output)

        # Verify data is readable and correct
        h5dump_data = read(`h5dump -d /test_data $fn`, String)
        @test !occursin("unable", lowercase(h5dump_data))
        @test occursin("DATA", h5dump_data)  # Check that data section exists

        rm(fn, force=true)
    else
        @warn "h5dump not available, skipping validation tests"
    end
end

@testset "Regression Tests" begin
    # Test for specific bugs that were fixed

    @testset "Boundary Key Calculation" begin
        fn = joinpath(mktempdir(), "boundary_test.jld2")

        # Specific case that previously failed
        data = collect(reshape(1.0:240.0, 30, 8))
        chunk_dims = [10, 4]

        jldopen(fn, "w") do f
            JLD2.write_chunked_array(f, "boundary", data, chunk_dims)
        end

        result = jldopen(fn, "r") do f
            f["boundary"]
        end
        @test result == data

        rm(fn, force=true)
    end

    @testset "First Element Correctness" begin
        fn = joinpath(mktempdir(), "first_elem.jld2")

        # Test that first element is not corrupted
        data = collect(reshape(1.0:100.0, 10, 10))
        chunk_dims = [3, 3]

        jldopen(fn, "w") do f
            JLD2.write_chunked_array(f, "data", data, chunk_dims)
        end

        result = jldopen(fn, "r") do f
            f["data"]
        end

        @test result[1, 1] == 1.0
        @test result[1, 2] == 11.0
        @test result[2, 1] == 2.0

        rm(fn, force=true)
    end

    @testset "Chunk Index Ordering" begin
        fn = joinpath(mktempdir(), "ordering.jld2")

        # Test proper HDF5 dimension ordering
        data = collect(reshape(1:60, 6, 10))
        chunk_dims = [2, 5]

        jldopen(fn, "w") do f
            JLD2.write_chunked_array(f, "ordered", data, chunk_dims)
        end

        result = jldopen(fn, "r") do f
            f["ordered"]
        end
        @test result == data

        rm(fn, force=true)
    end
end