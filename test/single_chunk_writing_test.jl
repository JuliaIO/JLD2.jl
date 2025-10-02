using JLD2, Test

@testset "Single Chunk Writing (Type 1)" begin
    @testset "Basic single chunk writing" begin
        # Test 1: Small 1D array
        data = Float32[1.0, 2.0, 3.0, 4.0, 5.0]
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=size(data))
                write_chunked(f, "data", wca)
            end

            # Read back and verify
            jldopen(filename, "r") do f
                data_read = f["data"]
                @test data_read == data
                @test eltype(data_read) == Float32
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "2D array single chunk" begin
        # Test 2: 2D array
        data = reshape(Float32.(1:20), 4, 5)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=size(data))
                write_chunked(f, "matrix", wca)
            end

            # Read back and verify
            jldopen(filename, "r") do f
                data_read = f["matrix"]
                @test data_read == data
                @test size(data_read) == (4, 5)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Different element types" begin
        # Test 3: Int64 array
        data_int = Int64[10, 20, 30, 40]
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data_int, chunks=size(data_int))
                write_chunked(f, "integers", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["integers"]
                @test data_read == data_int
                @test eltype(data_read) == Int64
            end
        finally
            rm(filename, force=true)
        end

        # Test 4: Float64 array
        data_float = [1.5, 2.5, 3.5, 4.5, 5.5]
        filename2 = tempname() * ".jld2"

        try
            jldopen(filename2, "w") do f
                wca = WriteChunkedArray(data_float, chunks=size(data_float))
                write_chunked(f, "doubles", wca)
            end

            jldopen(filename2, "r") do f
                data_read = f["doubles"]
                @test data_read == data_float
                @test eltype(data_read) == Float64
            end
        finally
            rm(filename2, force=true)
        end
    end

    @testset "3D arrays" begin
        # Test 5: 3D array
        data = reshape(Float32.(1:24), 2, 3, 4)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=size(data))
                write_chunked(f, "tensor", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["tensor"]
                @test data_read == data
                @test size(data_read) == (2, 3, 4)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Single chunk with compression (gzip)" begin
        # Test 6: Compressed single chunk
        data = Float32.(1:100)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                # Use true for compression (default Deflate) or specific filter
                wca = WriteChunkedArray(data, chunks=size(data), filters=true)
                write_chunked(f, "compressed", wca)
            end

            # Read back and verify
            jldopen(filename, "r") do f
                data_read = f["compressed"]
                @test data_read == data
                @test eltype(data_read) == Float32
            end

            # Verify file size is smaller (compression worked)
            filesize_compressed = filesize(filename)

            # Compare with uncompressed
            filename_uncompressed = tempname() * ".jld2"
            try
                jldopen(filename_uncompressed, "w") do f
                    wca = WriteChunkedArray(data, chunks=size(data))
                    write_chunked(f, "uncompressed", wca)
                end

                filesize_uncompressed = filesize(filename_uncompressed)
                @test filesize_compressed < filesize_uncompressed
            finally
                rm(filename_uncompressed, force=true)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Large array single chunk" begin
        # Test 7: Larger array to test chunking threshold
        data = reshape(Float32.(1:10000), 100, 100)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=size(data))
                write_chunked(f, "large", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["large"]
                @test data_read == data
                @test size(data_read) == (100, 100)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Multiple datasets in one file" begin
        # Test 8: Multiple single-chunk datasets
        data1 = Float32[1, 2, 3]
        data2 = Int32[10, 20, 30, 40]
        data3 = reshape(Float64.(1:12), 3, 4)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca1 = WriteChunkedArray(data1, chunks=size(data1))
                wca2 = WriteChunkedArray(data2, chunks=size(data2))
                wca3 = WriteChunkedArray(data3, chunks=size(data3))

                write_chunked(f, "dataset1", wca1)
                write_chunked(f, "dataset2", wca2)
                write_chunked(f, "dataset3", wca3)
            end

            jldopen(filename, "r") do f
                @test f["dataset1"] == data1
                @test f["dataset2"] == data2
                @test f["dataset3"] == data3
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Automatic single chunk selection" begin
        # Test 9: Auto-selection should choose single chunk when chunks == size
        data = Float32[1.0, 2.0, 3.0]
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                # Explicitly set chunks equal to size - should auto-select single chunk
                wca = WriteChunkedArray(data, chunks=size(data))
                write_chunked(f, "auto_single", wca)
            end

            jldopen(filename, "r") do f
                @test f["auto_single"] == data
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Validation errors" begin
        # Test 10: Error handling
        data = Float32[1, 2, 3, 4]
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                # This should work - chunks match data size
                wca = WriteChunkedArray(data, chunks=(4,))
                write_chunked(f, "valid", wca)
            end
        finally
            rm(filename, force=true)
        end
    end
end

println("All single chunk writing tests completed!")
