# Comprehensive tests for chunked array I/O functionality
# Tests all indexing types and all relevant APIs

using JLD2, Test, FileIO
using Pkg: Pkg
using JLD2Bzip2, JLD2Lz4

@testset "Chunked Array I/O" begin

    @testset "Single Chunk Indexing (Type 1)" begin
        @testset "Basic single chunk" begin
            fn = joinpath(mktempdir(), "single_chunk.jld2")

            data = collect(reshape(1.0:24.0, 4, 6))

            jldopen(fn, "w") do f
                JLD2.Chunking.write_chunked(f, "data", data; chunk=(4, 6), indexing=:single_chunk)
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result == data
            @test size(result) == size(data)
            rm(fn, force=true)
        end

        @testset "Single chunk with compression" begin
            fn = joinpath(mktempdir(), "single_chunk_compressed.jld2")

            data = collect(reshape(1.0:100.0, 10, 10))

            jldopen(fn, "w") do f
                JLD2.Chunking.write_chunked(f, "data", data; chunk=(10, 10),
                    indexing=:single_chunk, filters=Deflate())
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result == data
            rm(fn, force=true)
        end

        @testset "Single chunk validation" begin
            fn = joinpath(mktempdir(), "single_chunk_error.jld2")

            data = collect(reshape(1:24, 4, 6))

            jldopen(fn, "w") do f
                # Single chunk requires chunks == size(data)
                @test_throws AssertionError JLD2.Chunking.write_chunked(f, "data", data;
                    chunk=(2, 3), indexing=:single_chunk)
            end

            rm(fn, force=true)
        end
    end

    @testset "Implicit Index (Type 2)" begin
        @testset "Basic implicit index" begin
            fn = joinpath(mktempdir(), "implicit_index.jld2")

            data = collect(reshape(Float32.(1:60), 6, 10))

            jldopen(fn, "w") do f
                JLD2.Chunking.write_chunked(f, "data", data; chunk=(3, 5),
                    indexing=:implicit_index, fill_value=Float32(0.0))
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result == data
            @test eltype(result) == Float32
            rm(fn, force=true)
        end

        @testset "Implicit index with partial chunks" begin
            fn = joinpath(mktempdir(), "implicit_partial.jld2")

            data = collect(reshape(1.0:35.0, 7, 5))

            jldopen(fn, "w") do f
                JLD2.Chunking.write_chunked(f, "data", data; chunk=(3, 2),
                    indexing=:implicit_index, fill_value=0.0)
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result == data
            rm(fn, force=true)
        end

        @testset "Implicit index validation" begin
            fn = joinpath(mktempdir(), "implicit_errors.jld2")

            data = collect(reshape(1.0:24.0, 4, 6))

            jldopen(fn, "w") do f
                # Must have fill_value
                @test_throws ArgumentError JLD2.Chunking.write_chunked(f, "data", data;
                    chunk=(2, 3), indexing=:implicit_index)

                # Cannot use compression
                @test_throws JLD2.UnsupportedFeatureException JLD2.Chunking.write_chunked(
                    f, "data", data; chunk=(2, 3), indexing=:implicit_index,
                    fill_value=0.0, filters=Deflate())
            end

            rm(fn, force=true)
        end
    end

    @testset "Fixed Array Indexing (Type 3)" begin
        @testset "Basic fixed array" begin
            fn = joinpath(mktempdir(), "fixed_array.jld2")

            data = collect(reshape(1.0:60.0, 6, 10))

            jldopen(fn, "w") do f
                JLD2.Chunking.write_chunked(f, "data", data; chunk=(3, 5), indexing=:fixed_array)
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result == data
            rm(fn, force=true)
        end

        @testset "Fixed array with compression" begin
            fn = joinpath(mktempdir(), "fixed_compressed.jld2")

            data = collect(reshape(1.0:100.0, 10, 10))

            jldopen(fn, "w") do f
                JLD2.Chunking.write_chunked(f, "data", data; chunk=(5, 5),
                    indexing=:fixed_array, filters=Deflate())
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result == data
            rm(fn, force=true)
        end

        @testset "Fixed array with multiple filters" begin
            fn = joinpath(mktempdir(), "fixed_multi_filter.jld2")

            data = collect(reshape(Float32.(1:200), 10, 20))

            jldopen(fn, "w") do f
                JLD2.Chunking.write_chunked(f, "data", data; chunk=(5, 10),
                    indexing=:fixed_array, filters=[Shuffle(), Deflate()])
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result == data
            rm(fn, force=true)
        end

        @testset "Fixed array edge cases" begin
            fn = joinpath(mktempdir(), "fixed_edge.jld2")

            jldopen(fn, "w") do f
                # Non-evenly divisible dimensions
                data1 = collect(reshape(1:35, 7, 5))
                JLD2.Chunking.write_chunked(f, "uneven", data1; chunk=(3, 2), indexing=:fixed_array)

                # Single element chunks
                data2 = collect(reshape(1:12, 3, 4))
                JLD2.Chunking.write_chunked(f, "single_elem", data2; chunk=(1, 1), indexing=:fixed_array)
            end

            jldopen(fn, "r") do f
                @test f["uneven"] == collect(reshape(1:35, 7, 5))
                @test f["single_elem"] == collect(reshape(1:12, 3, 4))
            end

            rm(fn, force=true)
        end
    end

    @testset "Extensible Array Indexing (Type 4)" begin
        @testset "Basic extensible array" begin
            fn = joinpath(mktempdir(), "extensible_array.jld2")

            data = collect(reshape(1.0:60.0, 6, 10))

            jldopen(fn, "w") do f
                JLD2.Chunking.write_chunked(f, "data", data; chunk=(3, 5),
                    indexing=:extensible_array, maxshape=(nothing, 10))
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result == data
            rm(fn, force=true)
        end

        @testset "Extensible array with compression" begin
            fn = joinpath(mktempdir(), "extensible_compressed.jld2")

            data = collect(reshape(Float32.(1:80), 8, 10))

            jldopen(fn, "w") do f
                JLD2.Chunking.write_chunked(f, "data", data; chunk=(4, 5),
                    indexing=:extensible_array, maxshape=(nothing, 10), filters=Deflate())
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result == data
            rm(fn, force=true)
        end

        @testset "Extensible array validation" begin
            fn = joinpath(mktempdir(), "extensible_errors.jld2")

            data = collect(reshape(1.0:24.0, 4, 6))

            jldopen(fn, "w") do f
                # Must have maxshape
                @test_throws ArgumentError JLD2.Chunking.write_chunked(f, "data", data;
                    chunk=(2, 3), indexing=:extensible_array)

                # Must have at least one unlimited dimension
                @test_throws ArgumentError JLD2.Chunking.write_chunked(f, "data", data;
                    chunk=(2, 3), indexing=:extensible_array, maxshape=(4, 6))
            end

            rm(fn, force=true)
        end
    end

    @testset "V2 B-tree Indexing (Type 5)" begin
        @testset "Basic V2 B-tree" begin
            fn = joinpath(mktempdir(), "v2btree.jld2")

            data = collect(reshape(1.0:60.0, 6, 10))

            jldopen(fn, "w") do f
                JLD2.Chunking.write_chunked(f, "data", data; chunk=(3, 5),
                    indexing=:v2btree, maxshape=(nothing, nothing))
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result == data
            rm(fn, force=true)
        end

        @testset "V2 B-tree with compression" begin
            fn = joinpath(mktempdir(), "v2btree_compressed.jld2")

            data = collect(reshape(1.0:100.0, 10, 10))

            jldopen(fn, "w") do f
                JLD2.Chunking.write_chunked(f, "data", data; chunk=(5, 5),
                    indexing=:v2btree, maxshape=(nothing, nothing), filters=Deflate())
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result == data
            rm(fn, force=true)
        end

        @testset "V2 B-tree validation" begin
            fn = joinpath(mktempdir(), "v2btree_errors.jld2")

            data = collect(reshape(1.0:24.0, 4, 6))

            jldopen(fn, "w") do f
                # Must have maxshape
                @test_throws ArgumentError JLD2.Chunking.write_chunked(f, "data", data;
                    chunk=(2, 3), indexing=:v2btree)

                # Must have at least one unlimited dimension
                @test_throws ArgumentError JLD2.Chunking.write_chunked(f, "data", data;
                    chunk=(2, 3), indexing=:v2btree, maxshape=(4, 6))
            end

            rm(fn, force=true)
        end
    end

    @testset "V1 B-tree Indexing (Legacy)" begin
        @testset "Basic V1 B-tree" begin
            fn = joinpath(mktempdir(), "v1btree.jld2")

            data = collect(reshape(1.0:120.0, 30, 4))

            jldopen(fn, "w") do f
                JLD2.Chunking.write_chunked(f, "data", data; chunk=(10, 2), indexing=:v1btree)
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result == data
            rm(fn, force=true)
        end

        @testset "V1 B-tree with partial chunks" begin
            fn = joinpath(mktempdir(), "v1btree_partial.jld2")

            data = collect(reshape(1.0:35.0, 7, 5))

            jldopen(fn, "w") do f
                JLD2.Chunking.write_chunked(f, "data", data; chunk=(3, 2), indexing=:v1btree)
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result == data
            rm(fn, force=true)
        end

        @testset "V1 B-tree with multi-node tree" begin
            fn = joinpath(mktempdir(), "v1btree_large.jld2")

            # Create array with many chunks to trigger multi-node B-tree
            data = collect(reshape(1.0:20000.0, 100, 200))

            jldopen(fn, "w") do f
                JLD2.Chunking.write_chunked(f, "data", data; chunk=(3, 2), indexing=:v1btree)
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result == data
            @test size(result) == (100, 200)
            rm(fn, force=true)
        end
    end

    @testset "High-Level Write API" begin
        @testset "write() with chunk keyword" begin
            fn = joinpath(mktempdir(), "write_chunk.jld2")

            data = collect(reshape(1.0:60.0, 6, 10))

            jldopen(fn, "w") do f
                write(f, "data", data; chunk=(3, 5))
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result == data
            rm(fn, force=true)
        end

        @testset "write() with chunk and compress" begin
            fn = joinpath(mktempdir(), "write_chunk_compress.jld2")

            data = collect(reshape(1.0:100.0, 10, 10))

            jldopen(fn, "w") do f
                write(f, "data", data; chunk=(5, 5), compress=Deflate())
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result == data
            rm(fn, force=true)
        end

        @testset "setindex! with chunk keyword" begin
            fn = joinpath(mktempdir(), "setindex_chunk.jld2")

            data = collect(reshape(1.0:60.0, 6, 10))

            jldopen(fn, "w") do f
                f["data", chunk=(3, 5)] = data
                f["compressed", chunk=(3, 5), compress=Deflate()] = data
            end

            jldopen(fn, "r") do f
                @test f["data"] == data
                @test f["compressed"] == data
            end

            rm(fn, force=true)
        end

        @testset "write() validation" begin
            fn = joinpath(mktempdir(), "write_validation.jld2")

            data = collect(reshape(1:24, 4, 6))

            jldopen(fn, "w") do f
                # Wrong number of dimensions
                @test_throws ArgumentError write(f, "data", data; chunk=(3,))
                @test_throws ArgumentError write(f, "data", data; chunk=(3, 2, 1))

                # Invalid chunk values
                @test_throws ArgumentError write(f, "data", data; chunk=(0, 2))
                @test_throws ArgumentError write(f, "data", data; chunk=(-1, 2))

                # Chunk on non-array
                @test_throws ArgumentError write(f, "scalar", 42; chunk=(1,))
            end

            rm(fn, force=true)
        end
    end

    @testset "WriteChunkedArray Wrapper" begin
        @testset "Basic WriteChunkedArray" begin
            fn = joinpath(mktempdir(), "wca_basic.jld2")

            data = collect(reshape(1.0:60.0, 6, 10))
            wca = JLD2.Chunking.WriteChunkedArray(data; chunk=(3, 5))

            jldopen(fn, "w") do f
                f["data"] = wca
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result == data
            rm(fn, force=true)
        end

        @testset "WriteChunkedArray with maxshape" begin
            fn = joinpath(mktempdir(), "wca_maxshape.jld2")

            data = collect(reshape(1.0:60.0, 6, 10))
            wca = JLD2.Chunking.WriteChunkedArray(data; chunk=(3, 5), maxshape=(nothing, 10))

            jldopen(fn, "w") do f
                f["data"] = wca
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result == data
            rm(fn, force=true)
        end

        @testset "WriteChunkedArray with filters" begin
            fn = joinpath(mktempdir(), "wca_filters.jld2")

            data = collect(reshape(1.0:100.0, 10, 10))
            wca = JLD2.Chunking.WriteChunkedArray(data; chunk=(5, 5), filters=Deflate())

            jldopen(fn, "w") do f
                f["data"] = wca
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result == data
            rm(fn, force=true)
        end
    end

    @testset "Different Data Types" begin
        fn = joinpath(mktempdir(), "types.jld2")

        jldopen(fn, "w") do f
            # Int32
            int_data = collect(reshape(Int32(1):Int32(60), 6, 10))
            JLD2.Chunking.write_chunked(f, "int32", int_data; chunk=(3, 5), indexing=:fixed_array)

            # Float32
            float_data = collect(reshape(Float32(1):Float32(40), 8, 5))
            JLD2.Chunking.write_chunked(f, "float32", float_data; chunk=(4, 3), indexing=:fixed_array)

            # Complex numbers
            complex_real = collect(1.0:30.0)
            complex_imag = collect(31.0:60.0)
            complex_data = collect(reshape(complex.(complex_real, complex_imag), 5, 6))
            JLD2.Chunking.write_chunked(f, "complex", complex_data; chunk=(3, 3), indexing=:fixed_array)

            # 1D array
            vec_data = collect(1:100)
            JLD2.Chunking.write_chunked(f, "vector", vec_data; chunk=(25,), indexing=:fixed_array)

            # 3D array
            array_3d = collect(reshape(1.0:120.0, 3, 4, 10))
            JLD2.Chunking.write_chunked(f, "array3d", array_3d; chunk=(2, 2, 5), indexing=:fixed_array)
        end

        jldopen(fn, "r") do f
            @test f["int32"] == collect(reshape(Int32(1):Int32(60), 6, 10))
            @test f["float32"] == collect(reshape(Float32(1):Float32(40), 8, 5))

            complex_real = collect(1.0:30.0)
            complex_imag = collect(31.0:60.0)
            expected_complex = collect(reshape(complex.(complex_real, complex_imag), 5, 6))
            @test f["complex"] == expected_complex

            @test f["vector"] == collect(1:100)
            @test f["array3d"] == collect(reshape(1.0:120.0, 3, 4, 10))
        end

        rm(fn, force=true)
    end

    @testset "Compression Formats" begin
        fn = joinpath(mktempdir(), "compression.jld2")

        # Create large, compressible data
        data = repeat(reshape(1.0:100.0, 10, 10), 3, 3)

        jldopen(fn, "w") do f
            # Deflate
            JLD2.Chunking.write_chunked(f, "deflate", data; chunk=(10, 10),
                indexing=:fixed_array, filters=Deflate())

            # Zstd
            JLD2.Chunking.write_chunked(f, "zstd", data; chunk=(10, 10),
                indexing=:fixed_array, filters=ZstdFilter())

            # Shuffle + Deflate
            JLD2.Chunking.write_chunked(f, "shuffle_deflate", data; chunk=(10, 10),
                indexing=:fixed_array, filters=[Shuffle(), Deflate()])
        end

        jldopen(fn, "r") do f
            @test f["deflate"] == data
            @test f["zstd"] == data
            @test f["shuffle_deflate"] == data
        end

        rm(fn, force=true)
    end

    @testset "Chunked Array Reading API" begin
        @testset "get_chunked_array basic functionality" begin
            fn = joinpath(mktempdir(), "read_chunks.jld2")

            data = collect(reshape(1.0:60.0, 6, 10))
            chunk_dims = (3, 5)

            jldopen(fn, "w") do f
                write(f, "data", data; chunk=chunk_dims)
            end

            jldopen(fn, "r") do f
                chunked = JLD2.get_chunked_array(f, "data")

                # Test metadata accessors
                @test JLD2.chunk_dimensions(chunked) == chunk_dims
                @test JLD2.num_chunks(chunked) == 4  # 2x2 grid
                @test JLD2.chunk_grid_size(chunked) == (2, 2)
            end

            rm(fn, force=true)
        end

        @testset "ChunkedArray iteration" begin
            fn = joinpath(mktempdir(), "iterate_chunks.jld2")

            data = collect(reshape(1.0:24.0, 4, 6))
            chunk_dims = (2, 3)

            jldopen(fn, "w") do f
                write(f, "data", data; chunk=chunk_dims)
            end

            jldopen(fn, "r") do f
                chunked = JLD2.get_chunked_array(f, "data")

                # Test iteration
                chunks_collected = collect(chunked)
                @test length(chunks_collected) == 4  # 2x2 grid

                # Verify chunk data
                for chunk in chunks_collected
                    @test chunk.data isa Matrix{Float64}
                    @test size(chunk.data) == (2, 3)
                end
            end

            rm(fn, force=true)
        end

        @testset "ChunkedArray indexing" begin
            fn = joinpath(mktempdir(), "index_chunks.jld2")

            data = collect(reshape(1.0:60.0, 6, 10))
            chunk_dims = (3, 5)

            jldopen(fn, "w") do f
                write(f, "data", data; chunk=chunk_dims)
            end

            jldopen(fn, "r") do f
                chunked = JLD2.get_chunked_array(f, "data")

                # Access specific chunks
                chunk_1_1 = chunked[1, 1]
                @test chunk_1_1.data == data[1:3, 1:5]

                chunk_2_1 = chunked[2, 1]
                @test chunk_2_1.data == data[4:6, 1:5]

                chunk_1_2 = chunked[1, 2]
                @test chunk_1_2.data == data[1:3, 6:10]

                # Test bounds checking
                @test_throws BoundsError chunked[0, 1]
                @test_throws BoundsError chunked[3, 1]
                @test_throws BoundsError chunked[1, 3]
            end

            rm(fn, force=true)
        end

        @testset "get_chunked_array error handling" begin
            fn = joinpath(mktempdir(), "errors.jld2")

            # Create a non-chunked dataset
            jldopen(fn, "w") do f
                f["normal"] = collect(1:10)
            end

            jldopen(fn, "r") do f
                # Should error on non-chunked dataset
                @test_throws ArgumentError JLD2.get_chunked_array(f, "normal")
            end

            rm(fn, force=true)
        end
    end

    @testset "Edge Cases and Regression Tests" begin
        @testset "Boundary key calculation" begin
            fn = joinpath(mktempdir(), "boundary.jld2")

            # Specific case that previously failed
            data = collect(reshape(1.0:240.0, 30, 8))
            chunk_dims = [10, 4]

            jldopen(fn, "w") do f
                JLD2.Chunking.write_chunked(f, "data", data; chunk=Tuple(chunk_dims), indexing=:v1btree)
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result == data
            rm(fn, force=true)
        end

        @testset "First element correctness" begin
            fn = joinpath(mktempdir(), "first_elem.jld2")

            # Test that first element is not corrupted
            data = collect(reshape(1.0:100.0, 10, 10))

            jldopen(fn, "w") do f
                JLD2.Chunking.write_chunked(f, "data", data; chunk=(3, 3), indexing=:fixed_array)
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result[1, 1] == 1.0
            @test result[1, 2] == 11.0
            @test result[2, 1] == 2.0
            rm(fn, force=true)
        end

        @testset "Partial edge chunks" begin
            fn = joinpath(mktempdir(), "partial.jld2")

            # Array size not evenly divisible by chunk size
            data = collect(reshape(1:35, 7, 5))

            jldopen(fn, "w") do f
                write(f, "data", data; chunk=(3, 2))
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result == data

            # Verify we can read chunks
            jldopen(fn, "r") do f
                chunked = JLD2.get_chunked_array(f, "data")
                @test JLD2.chunk_grid_size(chunked) == (3, 3)
                chunks = collect(chunked)
                @test length(chunks) == 9
            end

            rm(fn, force=true)
        end

        @testset "Single element values preservation" begin
            fn = joinpath(mktempdir(), "values.jld2")

            # Use predictable sequential data to verify element positions
            data = reshape(Float32.(1:300), 10, 30)

            jldopen(fn, "w") do f
                JLD2.Chunking.write_chunked(f, "data", data; chunk=(3, 2), indexing=:fixed_array)
            end

            result = jldopen(fn, "r") do f
                f["data"]
            end

            @test result[1] == 1.0f0
            @test result[end] == 300.0f0
            @test result[5, 10] == data[5, 10]
            @test all(result .== data)
            rm(fn, force=true)
        end
    end

    @testset "Read-only File Validation" begin
        fn = joinpath(mktempdir(), "readonly.jld2")

        # Create file with existing data
        jldopen(fn, "w") do f
            f["dummy"] = [1, 2, 3]
        end

        # Try to write chunked data to read-only file
        jldopen(fn, "r") do f
            data = reshape(1:12, 3, 4)
            @test_throws ArgumentError JLD2.Chunking.write_chunked(f, "data", data; chunk=(2, 2), indexing=:v1btree)
        end

        rm(fn, force=true)
    end
end
