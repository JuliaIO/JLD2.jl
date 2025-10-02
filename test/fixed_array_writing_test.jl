using JLD2, Test

@testset "Fixed Array Writing (Type 3)" begin
    @testset "Basic 2D chunking" begin
        # Test 1: Simple 2D array with even chunking
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(3, 3))
                write_chunked(f, "chunked_2d", wca)
            end

            # Read back and verify
            jldopen(filename, "r") do f
                data_read = f["chunked_2d"]
                @test data_read == data
                @test eltype(data_read) == Float32
                @test size(data_read) == (10, 10)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "1D chunking" begin
        # Test 2: 1D array with chunks
        data = Float32.(1:50)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(10,))
                write_chunked(f, "chunked_1d", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["chunked_1d"]
                @test data_read == data
                @test length(data_read) == 50
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "3D chunking" begin
        # Test 3: 3D array
        data = reshape(Float32.(1:120), 4, 5, 6)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(2, 2, 3))
                write_chunked(f, "chunked_3d", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["chunked_3d"]
                @test data_read == data
                @test size(data_read) == (4, 5, 6)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Partial chunks at edges" begin
        # Test 4: Chunks that don't evenly divide the array
        # 10x10 array with 3x3 chunks means last row/col of chunks are partial
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(3, 3))
                write_chunked(f, "partial_chunks", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["partial_chunks"]
                @test data_read == data
                # Verify edge values specifically
                @test data_read[10, 10] == Float32(100)
                @test data_read[10, 1] == Float32(10)
                @test data_read[1, 10] == Float32(91)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "With compression" begin
        # Test 5: Compression is not yet supported for Fixed Array
        data = reshape(Float32.(1:200), 10, 20)
        filename = tempname() * ".jld2"

        try
            # Should throw UnsupportedFeatureException
            @test_throws JLD2.UnsupportedFeatureException jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(5, 5), filters=true)
                write_chunked(f, "compressed_chunked", wca)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Different element types" begin
        # Test 6: Int64
        data_int = reshape(Int64.(1:60), 6, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data_int, chunks=(3, 4))
                write_chunked(f, "int_chunked", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["int_chunked"]
                @test data_read == data_int
                @test eltype(data_read) == Int64
            end
        finally
            rm(filename, force=true)
        end

        # Test 7: Float64
        data_float = reshape(Float64.(1:48), 8, 6)
        filename2 = tempname() * ".jld2"

        try
            jldopen(filename2, "w") do f
                wca = WriteChunkedArray(data_float, chunks=(4, 3))
                write_chunked(f, "float64_chunked", wca)
            end

            jldopen(filename2, "r") do f
                data_read = f["float64_chunked"]
                @test data_read == data_float
                @test eltype(data_read) == Float64
            end
        finally
            rm(filename2, force=true)
        end
    end

    @testset "Various chunk sizes" begin
        # Test 8: Small chunks
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(2, 2))
                write_chunked(f, "small_chunks", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["small_chunks"]
                @test data_read == data
            end
        finally
            rm(filename, force=true)
        end

        # Test 9: Large chunks
        filename2 = tempname() * ".jld2"
        try
            jldopen(filename2, "w") do f
                wca = WriteChunkedArray(data, chunks=(8, 8))
                write_chunked(f, "large_chunks", wca)
            end

            jldopen(filename2, "r") do f
                data_read = f["large_chunks"]
                @test data_read == data
            end
        finally
            rm(filename2, force=true)
        end
    end

    @testset "Asymmetric chunk dimensions" begin
        # Test 10: Different chunk sizes in each dimension
        data = reshape(Float32.(1:120), 10, 12)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(3, 5))
                write_chunked(f, "asymmetric", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["asymmetric"]
                @test data_read == data
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Multiple datasets in one file" begin
        # Test 11: Multiple chunked datasets
        data1 = reshape(Float32.(1:24), 4, 6)
        data2 = reshape(Int32.(1:30), 5, 6)
        data3 = Float64.(1:20)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca1 = WriteChunkedArray(data1, chunks=(2, 3))
                wca2 = WriteChunkedArray(data2, chunks=(2, 2))
                wca3 = WriteChunkedArray(data3, chunks=(5,))

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

    @testset "Very small arrays" begin
        # Test 12: Minimal arrays
        data = reshape(Float32.(1:4), 2, 2)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                # 2x2 array with 1x1 chunks (worst case - 4 chunks)
                wca = WriteChunkedArray(data, chunks=(1, 1))
                write_chunked(f, "tiny", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["tiny"]
                @test data_read == data
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Large array with many chunks" begin
        # Test 13: Larger array to ensure scalability
        data = reshape(Float32.(1:1000), 25, 40)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(5, 8))
                write_chunked(f, "many_chunks", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["many_chunks"]
                @test data_read == data
                # Spot check some values
                @test data_read[1, 1] == Float32(1)
                @test data_read[25, 40] == Float32(1000)
                @test data_read[13, 20] == data[13, 20]  # Verify specific element
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Single row/column arrays" begin
        # Test 14: Row vector (1xN)
        data_row = reshape(Float32.(1:10), 1, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data_row, chunks=(1, 3))
                write_chunked(f, "row_vector", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["row_vector"]
                @test data_read == data_row
            end
        finally
            rm(filename, force=true)
        end

        # Test 15: Column vector (Nx1)
        data_col = reshape(Float32.(1:10), 10, 1)
        filename2 = tempname() * ".jld2"

        try
            jldopen(filename2, "w") do f
                wca = WriteChunkedArray(data_col, chunks=(3, 1))
                write_chunked(f, "col_vector", wca)
            end

            jldopen(filename2, "r") do f
                data_read = f["col_vector"]
                @test data_read == data_col
            end
        finally
            rm(filename2, force=true)
        end
    end

    @testset "Edge case: chunk equals array in one dimension" begin
        # Test 16: Chunks cover full dimension in one axis
        data = reshape(Float32.(1:60), 6, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                # Full rows, chunked columns
                wca = WriteChunkedArray(data, chunks=(6, 3))
                write_chunked(f, "full_rows", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["full_rows"]
                @test data_read == data
            end
        finally
            rm(filename, force=true)
        end
    end
end

println("All Fixed Array writing tests completed!")
