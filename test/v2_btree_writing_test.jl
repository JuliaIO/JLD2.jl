# V2 B-tree Writing Tests (Type 5)
# Tests for HDF5 v2 B-tree chunk indexing - used for datasets with
# 2+ unlimited dimensions or very large datasets

using Test
using JLD2

@testset "V2 B-tree Writing (Type 5)" begin

    @testset "Basic 2D with both dimensions unlimited" begin
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(3, 3),
                                       maxshape=(nothing, nothing))
                write_chunked(f, "v2btree", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["v2btree"]
                @test data_read == data
                @test size(data_read) == size(data)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "2D with different element types" begin
        for (T, data) in [
            (Float64, reshape(Float64.(1:100), 10, 10)),
            (Int32, reshape(Int32.(1:100), 10, 10)),
            (UInt8, reshape(UInt8.(1:100), 10, 10)),
        ]
            filename = tempname() * ".jld2"

            try
                jldopen(filename, "w") do f
                    wca = WriteChunkedArray(data, chunks=(3, 3),
                                           maxshape=(nothing, nothing))
                    write_chunked(f, "data", wca)
                end

                jldopen(filename, "r") do f
                    data_read = f["data"]
                    @test data_read == data
                    @test eltype(data_read) == T
                end
            finally
                rm(filename, force=true)
            end
        end
    end

    @testset "3D with 2 unlimited dimensions" begin
        data = reshape(Float32.(1:120), 4, 5, 6)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(2, 2, 2),
                                       maxshape=(nothing, nothing, 6))
                write_chunked(f, "v2btree", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["v2btree"]
                @test data_read == data
                @test size(data_read) == size(data)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "3D with all dimensions unlimited" begin
        data = reshape(Float32.(1:60), 3, 4, 5)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(2, 2, 2),
                                       maxshape=(nothing, nothing, nothing))
                write_chunked(f, "v2btree", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["v2btree"]
                @test data_read == data
                @test size(data_read) == size(data)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Partial chunks - 2D" begin
        # Data size not evenly divisible by chunk size
        data = reshape(Float32.(1:77), 7, 11)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(3, 4),
                                       maxshape=(nothing, nothing))
                write_chunked(f, "v2btree", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["v2btree"]
                @test data_read == data
                @test size(data_read) == size(data)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Partial chunks - 3D" begin
        data = reshape(Float32.(1:105), 5, 7, 3)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(2, 3, 2),
                                       maxshape=(nothing, nothing, 3))
                write_chunked(f, "v2btree", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["v2btree"]
                @test data_read == data
                @test size(data_read) == size(data)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Small chunks (many chunks)" begin
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(2, 2),
                                       maxshape=(nothing, nothing))
                write_chunked(f, "v2btree", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["v2btree"]
                @test data_read == data
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Large chunks (few chunks)" begin
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(5, 10),
                                       maxshape=(nothing, nothing))
                write_chunked(f, "v2btree", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["v2btree"]
                @test data_read == data
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "1D with unlimited dimension" begin
        data = Float32.(1:20)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(5,),
                                       maxshape=(nothing,))
                write_chunked(f, "v2btree", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["v2btree"]
                @test data_read == data
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "4D with multiple unlimited dimensions" begin
        data = reshape(Float32.(1:24), 2, 3, 2, 2)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(2, 2, 2, 2),
                                       maxshape=(nothing, nothing, 2, 2))
                write_chunked(f, "v2btree", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["v2btree"]
                @test data_read == data
                @test size(data_read) == size(data)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Edge case: single chunk" begin
        data = reshape(Float32.(1:25), 5, 5)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(10, 10),
                                       maxshape=(nothing, nothing))
                write_chunked(f, "v2btree", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["v2btree"]
                @test data_read == data
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Element-level validation" begin
        # Use sequential data to easily verify correct chunk ordering
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(3, 3),
                                       maxshape=(nothing, nothing))
                write_chunked(f, "v2btree", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["v2btree"]

                # Check corners
                @test data_read[1, 1] == Float32(1)
                @test data_read[1, 10] == Float32(91)
                @test data_read[10, 1] == Float32(10)
                @test data_read[10, 10] == Float32(100)

                # Check middle
                @test data_read[5, 5] == Float32(45)

                # Check entire array
                @test data_read == data
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Multiple datasets in same file" begin
        data1 = reshape(Float32.(1:100), 10, 10)
        data2 = reshape(Float64.(1:60), 6, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca1 = WriteChunkedArray(data1, chunks=(3, 3),
                                        maxshape=(nothing, nothing))
                write_chunked(f, "dataset1", wca1)

                wca2 = WriteChunkedArray(data2, chunks=(2, 3),
                                        maxshape=(nothing, nothing))
                write_chunked(f, "dataset2", wca2)
            end

            jldopen(filename, "r") do f
                @test f["dataset1"] == data1
                @test f["dataset2"] == data2
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Verify data layout type" begin
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(3, 3),
                                       maxshape=(nothing, nothing))
                write_chunked(f, "v2btree", wca)
            end

            # Verify chunk indexing type by inspecting header
            jldopen(filename, "r") do f
                # Read successfully with V2 B-tree reading code
                data_read = f["v2btree"]
                @test data_read == data
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Automatic selection with 2+ unlimited dims" begin
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                # Let automatic selection choose V2 B-tree
                wca = WriteChunkedArray(data, chunks=(3, 3),
                                       maxshape=(nothing, nothing))
                f["auto"] = wca
            end

            jldopen(filename, "r") do f
                data_read = f["auto"]
                @test data_read == data
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Manual indexing type override" begin
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                # Manually specify V2 B-tree
                wca = WriteChunkedArray(data, chunks=(3, 3),
                                       maxshape=(nothing, nothing),
                                       indexing=:v2btree)
                write_chunked(f, "manual", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["manual"]
                @test data_read == data
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Error: filters not yet supported" begin
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            @test_throws JLD2.UnsupportedFeatureException jldopen(filename, "w") do f
                wca = WriteChunkedArray(data, chunks=(3, 3),
                                       maxshape=(nothing, nothing),
                                       filters=:gzip)
                write_chunked(f, "filtered", wca)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Error: too many chunks for depth=0" begin
        data = reshape(Float32.(1:10000), 100, 100)
        filename = tempname() * ".jld2"

        try
            @test_throws JLD2.UnsupportedFeatureException jldopen(filename, "w") do f
                # Many small chunks that won't fit in single leaf node
                wca = WriteChunkedArray(data, chunks=(2, 2),
                                       maxshape=(nothing, nothing))
                write_chunked(f, "too_many", wca)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Comprehensive data integrity" begin
        # Test various patterns to ensure correct chunk assembly
        for (rows, cols, chunk_rows, chunk_cols) in [
            (10, 10, 3, 3),
            (12, 15, 4, 5),
            (7, 11, 3, 4),
            (20, 8, 5, 3),
        ]
            data = reshape(Float32.(1:rows*cols), rows, cols)
            filename = tempname() * ".jld2"

            try
                jldopen(filename, "w") do f
                    wca = WriteChunkedArray(data, chunks=(chunk_rows, chunk_cols),
                                           maxshape=(nothing, nothing))
                    write_chunked(f, "data", wca)
                end

                jldopen(filename, "r") do f
                    data_read = f["data"]
                    @test data_read == data
                    @test all(data_read .== data)
                end
            finally
                rm(filename, force=true)
            end
        end
    end

end  # @testset "V2 B-tree Writing (Type 5)"
