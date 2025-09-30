# Tests for the new Chunked Array API

using JLD2, Test, FileIO

@testset "Chunked Array Writing API" begin
    @testset "setindex! with chunk keyword" begin
        fn = joinpath(mktempdir(), "setindex_chunk.jld2")

        data = collect(reshape(1.0:60.0, 6, 10))

        jldopen(fn, "w") do f
            # This syntax works in Julia!
            f["data", chunk=(3, 5)] = data
            f["compressed", chunk=(3, 5), compress=Deflate()] = data
        end

        # Verify data can be read back
        jldopen(fn, "r") do f
            @test f["data"] == data
            @test f["compressed"] == data
        end

        rm(fn, force=true)
    end

    @testset "write() with chunk keyword" begin
        fn = joinpath(mktempdir(), "write_chunk.jld2")

        data_2d = collect(reshape(1.0:120.0, 10, 12))

        jldopen(fn, "w") do f
            write(f, "data", data_2d; chunk=(5, 6))
        end

        # Verify data can be read back
        result = jldopen(fn, "r") do f
            f["data"]
        end

        @test result == data_2d
        rm(fn, force=true)
    end

    @testset "write() with chunk and compress" begin
        fn = joinpath(mktempdir(), "write_chunk_compress.jld2")

        data = collect(reshape(1.0:100.0, 10, 10))

        jldopen(fn, "w") do f
            write(f, "compressed", data; chunk=(5, 5), compress=Deflate())
        end

        result = jldopen(fn, "r") do f
            f["compressed"]
        end

        @test result == data

        # Verify file size is smaller due to compression (for repetitive data)
        data_rep = repeat([1.0, 2.0, 3.0], 100)
        data_rep_array = collect(reshape(data_rep, 10, 30))

        fn2 = joinpath(mktempdir(), "compressed.jld2")
        fn3 = joinpath(mktempdir(), "uncompressed.jld2")

        jldopen(fn2, "w") do f
            write(f, "data", data_rep_array; chunk=(5, 15), compress=Deflate())
        end

        jldopen(fn3, "w") do f
            write(f, "data", data_rep_array; chunk=(5, 15))
        end

        @test filesize(fn2) < filesize(fn3)

        rm(fn, force=true)
        rm(fn2, force=true)
        rm(fn3, force=true)
    end

    @testset "write() chunk parameter validation" begin
        fn = joinpath(mktempdir(), "validation.jld2")

        data = collect(reshape(1:24, 4, 6))

        jldopen(fn, "w") do f
            # Wrong number of dimensions
            @test_throws ArgumentError write(f, "data", data; chunk=(3,))
            @test_throws ArgumentError write(f, "data", data; chunk=(3, 2, 1))

            # Chunk on non-array
            @test_throws ArgumentError write(f, "scalar", 42; chunk=(1,))

            # Invalid chunk values
            @test_throws ArgumentError write(f, "data", data; chunk=(0, 2))
            @test_throws ArgumentError write(f, "data", data; chunk=(-1, 2))
        end

        rm(fn, force=true)
    end

    @testset "write() with different data types" begin
        fn = joinpath(mktempdir(), "types.jld2")

        jldopen(fn, "w") do f
            # Int32
            int_data = collect(reshape(Int32(1):Int32(60), 6, 10))
            write(f, "int32", int_data; chunk=(3, 5))

            # Float32
            float_data = collect(reshape(Float32(1):Float32(40), 8, 5))
            write(f, "float32", float_data; chunk=(4, 3))

            # 1D array
            vec_data = collect(1:100)
            write(f, "vector", vec_data; chunk=(25,))

            # 3D array
            array_3d = collect(reshape(1.0:120.0, 3, 4, 10))
            write(f, "array3d", array_3d; chunk=(2, 2, 5))
        end

        jldopen(fn, "r") do f
            @test f["int32"] == collect(reshape(Int32(1):Int32(60), 6, 10))
            @test f["float32"] == collect(reshape(Float32(1):Float32(40), 8, 5))
            @test f["vector"] == collect(1:100)
            @test f["array3d"] == collect(reshape(1.0:120.0, 3, 4, 10))
        end

        rm(fn, force=true)
    end
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

@testset "Integration Tests" begin
    @testset "Write and read chunks with compression" begin
        fn = joinpath(mktempdir(), "integration.jld2")

        data = collect(reshape(1.0:100.0, 10, 10))

        jldopen(fn, "w") do f
            write(f, "data", data; chunk=(5, 5), compress=Deflate())
        end

        # Read entire array
        result_full = jldopen(fn, "r") do f
            f["data"]
        end
        @test result_full == data

        # Read as chunked array
        jldopen(fn, "r") do f
            chunked = JLD2.get_chunked_array(f, "data")

            # Verify we can iterate
            @test length(chunked) == 4

            # Verify each chunk loads correctly
            for chunk in chunked
                @test chunk.data isa Matrix{Float64}
            end
        end

        rm(fn, force=true)
    end

    @testset "Edge cases - partial chunks" begin
        fn = joinpath(mktempdir(), "partial.jld2")

        # Array size not evenly divisible by chunk size
        data = collect(reshape(1:35, 7, 5))

        jldopen(fn, "w") do f
            write(f, "data", data; chunk=(3, 2))
        end

        jldopen(fn, "r") do f
            chunked = JLD2.get_chunked_array(f, "data")

            # Should have ceiling division chunks
            @test JLD2.chunk_grid_size(chunked) == (3, 3)

            # Read all chunks - edge chunks may be smaller
            chunks = collect(chunked)
            @test length(chunks) == 9
        end

        rm(fn, force=true)
    end
end