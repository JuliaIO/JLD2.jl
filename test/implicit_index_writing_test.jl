# Test suite for Implicit Index (Type 2) chunk writing
# Phase 3 of chunked array writing implementation

using Test
using JLD2

@testset "Implicit Index Writing (Type 2)" begin

    @testset "Basic 2D with fill value" begin
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(3, 3), fill_value=Float32(0))
                write_chunked(f, "implicit", wca)
            end

            # Verify we can read it back
            jldopen(filename, "r") do f
                data_read = f["implicit"]
                @test data_read == data
                @test eltype(data_read) == Float32
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "1D array with fill value" begin
        data = Float64.(1:50)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(7,), fill_value=0.0)
                write_chunked(f, "data1d", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["data1d"]
                @test data_read == data
                @test size(data_read) == (50,)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "3D array with fill value" begin
        data = reshape(Int32.(1:120), 4, 5, 6)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(2, 2, 2), fill_value=Int32(0))
                write_chunked(f, "data3d", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["data3d"]
                @test data_read == data
                @test size(data_read) == (4, 5, 6)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Partial chunks at edges" begin
        # Test various array sizes that don't divide evenly
        test_cases = [
            (size=(10, 10), chunks=(3, 3)),
            (size=(15, 8), chunks=(4, 3)),
            (size=(7, 13), chunks=(3, 5)),
            (size=(20, 15), chunks=(6, 4))
        ]

        for tc in test_cases
            data = reshape(Float32.(1:prod(tc.size)), tc.size)
            filename = tempname() * ".jld2"

            try
                jldopen(filename, "w") do f
                    wca = WriteChunkedArray(data, chunks=tc.chunks, fill_value=Float32(-1))
                    write_chunked(f, "partial", wca)
                end

                jldopen(filename, "r") do f
                    data_read = f["partial"]
                    @test data_read == data
                end
            finally
                rm(filename, force=true)
            end
        end
    end

    @testset "Error: missing fill value" begin
        data = Float32.(reshape(1:100, 10, 10))
        filename = tempname() * ".jld2"

        try
            @test_throws ArgumentError jldopen(filename, "w") do f
                # No fill_value specified - should error
                wca = WriteChunkedArray(data, chunks=(3, 3), indexing=:implicit_index)
                write_chunked(f, "test", wca)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Error: filters not supported" begin
        data = Float32.(reshape(1:100, 10, 10))
        filename = tempname() * ".jld2"

        try
            @test_throws JLD2.UnsupportedFeatureException jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(3, 3),
                                       fill_value=Float32(0),
                                       indexing=:implicit_index,
                                       filters=:gzip)
                write_chunked(f, "test", wca)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Different fill values" begin
        # Test various fill value types
        test_cases = [
            (data=Float32.(reshape(1:100, 10, 10)), fill=Float32(0)),
            (data=Float32.(reshape(1:100, 10, 10)), fill=Float32(-1)),
            (data=Float32.(reshape(1:100, 10, 10)), fill=Float32(999)),
            (data=Float64.(reshape(1:100, 10, 10)), fill=0.0),
            (data=Int32.(reshape(1:100, 10, 10)), fill=Int32(0)),
            (data=Int64.(reshape(1:100, 10, 10)), fill=Int64(-999))
        ]

        for (i, tc) in enumerate(test_cases)
            filename = tempname() * ".jld2"

            try
                jldopen(filename, "w") do f
                    wca = WriteChunkedArray(tc.data, chunks=(3, 3), fill_value=tc.fill)
                    write_chunked(f, "filltest", wca)
                end

                jldopen(filename, "r") do f
                    data_read = f["filltest"]
                    @test data_read == tc.data
                end
            finally
                rm(filename, force=true)
            end
        end
    end

    @testset "Various chunk sizes" begin
        data = reshape(Float32.(1:1000), 25, 40)

        chunk_sizes = [(5, 8), (3, 10), (10, 5), (7, 7)]

        for chunks in chunk_sizes
            filename = tempname() * ".jld2"

            try
                jldopen(filename, "w") do f
                    wca = WriteChunkedArray(data, chunks=chunks, fill_value=Float32(0))
                    write_chunked(f, "varsize", wca)
                end

                jldopen(filename, "r") do f
                    data_read = f["varsize"]
                    @test data_read == data
                end
            finally
                rm(filename, force=true)
            end
        end
    end

    @testset "Asymmetric chunk dimensions" begin
        data = reshape(Int32.(1:200), 10, 20)
        filename = tempname() * ".jld2"

        try
            # Very different chunk dimensions
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(2, 15), fill_value=Int32(0))
                write_chunked(f, "asym", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["asym"]
                @test data_read == data
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Multiple datasets in one file" begin
        data1 = reshape(Float32.(1:100), 10, 10)
        data2 = Float64.(1:50)
        data3 = reshape(Int32.(1:60), 3, 4, 5)

        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                write_chunked(f, "d1", WriteChunkedArray(data1, chunks=(3, 3), fill_value=Float32(0)))
                write_chunked(f, "d2", WriteChunkedArray(data2, chunks=(7,), fill_value=0.0))
                write_chunked(f, "d3", WriteChunkedArray(data3, chunks=(2, 2, 2), fill_value=Int32(0)))
            end

            jldopen(filename, "r") do f
                @test f["d1"] == data1
                @test f["d2"] == data2
                @test f["d3"] == data3
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Very small arrays" begin
        # Test edge case of very small array
        data = Float32.([1.0, 2.0, 3.0])
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(2,), fill_value=Float32(0))
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
        # Test with larger array to ensure linear indexing works correctly
        data = reshape(Float32.(1:10000), 100, 100)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                # This will create a 10×10 grid of chunks (100 chunks total)
                wca = WriteChunkedArray(data, chunks=(10, 10), fill_value=Float32(0))
                write_chunked(f, "large", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["large"]
                @test data_read == data

                # Verify specific values
                # For column-major reshape: data[i,j] = i + (j-1)*100
                @test data_read[1, 1] == Float32(1)
                @test data_read[50, 50] == Float32(4950)  # 50 + (50-1)*100 = 4950
                @test data_read[100, 100] == Float32(10000)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Single row/column arrays" begin
        # Edge case: single row
        row_data = reshape(Float32.(1:20), 1, 20)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(row_data, chunks=(1, 5), fill_value=Float32(0))
                write_chunked(f, "row", wca)
            end

            jldopen(filename, "r") do f
                @test f["row"] == row_data
            end
        finally
            rm(filename, force=true)
        end

        # Edge case: single column
        col_data = reshape(Float32.(1:20), 20, 1)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(col_data, chunks=(5, 1), fill_value=Float32(0))
                write_chunked(f, "col", wca)
            end

            jldopen(filename, "r") do f
                @test f["col"] == col_data
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "NaN fill value" begin
        data = reshape(Float64.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(3, 3), fill_value=NaN)
                write_chunked(f, "nanfill", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["nanfill"]
                @test data_read == data
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Automatic selection with fill_value" begin
        # When fill_value is provided and chunks != size(data),
        # system may select implicit index (if enabled in selection logic)
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                # Manual override to force implicit index
                wca = WriteChunkedArray(data, chunks=(3, 3),
                                       fill_value=Float32(0),
                                       indexing=:implicit_index)
                write_chunked(f, "auto", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["auto"]
                @test data_read == data
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Edge case: chunk equals array size" begin
        # When chunk size equals array size, implicit index still valid
        # (though single chunk would be more efficient)
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(10, 10),
                                       fill_value=Float32(0),
                                       indexing=:implicit_index)
                write_chunked(f, "equal", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["equal"]
                @test data_read == data
            end
        finally
            rm(filename, force=true)
        end
    end

end  # Main testset
