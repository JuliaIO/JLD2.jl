@testitem "Comprehensive Compression Tests" begin
    using JLD2

    # Test matrix: all chunk indexing types × all filter types
    # Implicit Index (Type 2) is excluded as it doesn't support filters per HDF5 spec

    @testset "Compression Tests: $type_name with $filter_name" for
        (type_name, shape, chunks, maxshape) in [
            ("Single Chunk", (10, 10), (10, 10), nothing),
            ("Fixed Array", (10, 10), (3, 3), nothing),
            ("Extensible Array", (10, 10), (3, 3), (nothing, 10)),
            ("V2 B-tree", (10, 10), (3, 3), (nothing, nothing))
        ],
        (filter_spec, filter_name) in [
            (:gzip, "gzip"),
            (:shuffle, "shuffle"),
            ([JLD2.Filters.Shuffle(), JLD2.Filters.Deflate(5)], "shuffle+gzip")
        ]

        data = reshape(Float32.(1:prod(shape)), shape)
        filename = "test_$(type_name)_$(filter_name).jld2"

        try
            # Write with compression
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=chunks, maxshape=maxshape, filters=filter_spec)
                write_chunked(f, "data", wca)
            end

            # Read and verify
            jldopen(filename, "r") do f
                data_read = f["data"]
                @test data_read == data
                @test sum(data_read) == sum(data)
                @test eltype(data_read) == Float32
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Different Data Types with Compression" begin
        types_to_test = [
            (Float32, "Float32"),
            (Float64, "Float64"),
            (Int32, "Int32"),
            (UInt8, "UInt8")
        ]

        for (dtype, dtype_name) in types_to_test
            data = reshape(dtype.(1:100), 10, 10)
            filename = "test_types_$(dtype_name).jld2"

            try
                jldopen(filename, "w") do f
                    wca = WriteChunkedArray(data, chunks=(3, 3), maxshape=(nothing, 10), filters=:gzip)
                    write_chunked(f, "data", wca)
                end

                jldopen(filename, "r") do f
                    data_read = f["data"]
                    @test data_read == data
                    @test eltype(data_read) == dtype
                end
            finally
                rm(filename, force=true)
            end
        end
    end

    @testset "Different Array Dimensions with Compression" begin
        # 1D array
        data_1d = Float32.(1:100)
        filename = "test_1d_compressed.jld2"
        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data_1d, chunks=(10,), maxshape=(nothing,), filters=:gzip)
                write_chunked(f, "data", wca)
            end
            jldopen(filename, "r") do f
                @test f["data"] == data_1d
            end
        finally
            rm(filename, force=true)
        end

        # 3D array
        data_3d = reshape(Float32.(1:1000), 10, 10, 10)
        filename = "test_3d_compressed.jld2"
        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data_3d, chunks=(3, 3, 3), maxshape=(nothing, 10, 10), filters=:gzip)
                write_chunked(f, "data", wca)
            end
            jldopen(filename, "r") do f
                @test f["data"] == data_3d
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Partial Chunks with Compression" begin
        # Array size not evenly divisible by chunk size
        data = reshape(Float32.(1:300), 30, 10)
        filename = "test_partial_chunks.jld2"

        try
            jldopen(filename, "w") do f
                # Chunk size 7x7 doesn't evenly divide 30x10
                wca = WriteChunkedArray(data, chunks=(7, 7), maxshape=(nothing, 10), filters=:gzip)
                write_chunked(f, "data", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["data"]
                @test data_read == data
                # Check edge elements specifically
                @test data_read[1, 1] == data[1, 1]
                @test data_read[end, end] == data[end, end]
                @test data_read[15, 5] == data[15, 5]
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Large Arrays with Compression" begin
        # Test with array requiring many chunks (for Extensible Array edge cases)
        data = reshape(Float32.(1:10000), 100, 100)
        filename = "test_large_array.jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(8, 8), maxshape=(nothing, 100), filters=:gzip)
                write_chunked(f, "data", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["data"]
                @test data_read == data
                @test sum(data_read) == sum(data)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Compression Ratios" begin
        # Test that data actually gets compressed
        # Use highly compressible data
        data = fill(Float32(42.0), 100, 100)

        filename_uncompressed = "test_uncompressed.jld2"
        filename_compressed = "test_compressed.jld2"

        try
            # Write without compression
            jldopen(filename_uncompressed, "w") do f
                wca = WriteChunkedArray(data, chunks=(10, 10), maxshape=(nothing, 100))
                write_chunked(f, "data", wca)
            end

            # Write with compression
            jldopen(filename_compressed, "w") do f
                wca = WriteChunkedArray(data, chunks=(10, 10), maxshape=(nothing, 100), filters=:gzip)
                write_chunked(f, "data", wca)
            end

            # Compressed file should be smaller
            size_uncompressed = filesize(filename_uncompressed)
            size_compressed = filesize(filename_compressed)

            @test size_compressed < size_uncompressed
            compression_ratio = size_compressed / size_uncompressed
            @test compression_ratio < 0.8  # At least 20% reduction for highly repetitive data
        finally
            rm(filename_uncompressed, force=true)
            rm(filename_compressed, force=true)
        end
    end
end
