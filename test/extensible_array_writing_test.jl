using Test
using JLD2

@testset "Extensible Array Writing (Type 4)" begin

    @testset "Basic 2D with first dimension unlimited" begin
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = JLD2.WriteChunkedArray(data, chunks=(3, 3), maxshape=(nothing, 10))
                JLD2.write_chunked(f, "extensible", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["extensible"]
                @test data_read == data
                @test size(data_read) == size(data)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Basic 2D with second dimension unlimited" begin
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = JLD2.WriteChunkedArray(data, chunks=(3, 3), maxshape=(10, nothing))
                JLD2.write_chunked(f, "extensible", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["extensible"]
                @test data_read == data
                @test size(data_read) == size(data)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Both dimensions unlimited (requires V2 B-tree)" begin
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            # 2+ unlimited dimensions requires V2 B-tree (Phase 5)
            @test_throws JLD2.UnsupportedFeatureException jldopen(filename, "w") do f
                wca = JLD2.WriteChunkedArray(data, chunks=(3, 3), maxshape=(nothing, nothing))
                JLD2.write_chunked(f, "extensible", wca)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "1D array with unlimited dimension" begin
        data = Float32.(1:50)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = JLD2.WriteChunkedArray(data, chunks=(10,), maxshape=(nothing,))
                JLD2.write_chunked(f, "extensible", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["extensible"]
                @test data_read == data
                @test size(data_read) == size(data)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "3D array with one unlimited dimension" begin
        data = reshape(Float64.(1:120), 3, 4, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = JLD2.WriteChunkedArray(data, chunks=(2, 2, 3), maxshape=(nothing, 4, 10))
                JLD2.write_chunked(f, "extensible", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["extensible"]
                @test data_read == data
                @test size(data_read) == size(data)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Partial chunks at edges" begin
        # Non-even dimensions to test edge chunk handling
        data = reshape(Float32.(1:80), 8, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = JLD2.WriteChunkedArray(data, chunks=(3, 3), maxshape=(nothing, 10))
                JLD2.write_chunked(f, "extensible", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["extensible"]
                @test data_read == data
                # Test specific edge values
                @test data_read[1, 1] == 1.0f0
                @test data_read[8, 10] == 80.0f0
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Error: missing maxshape with manual indexing" begin
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            # Manually forcing extensible_array without maxshape should fail
            @test_throws ArgumentError jldopen(filename, "w") do f
                wca = JLD2.WriteChunkedArray(data, chunks=(3, 3), indexing=:extensible_array)
                JLD2.write_chunked(f, "extensible", wca)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Error: no unlimited dimensions" begin
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            @test_throws ArgumentError jldopen(filename, "w") do f
                # This should use fixed_array instead
                wca = JLD2.WriteChunkedArray(data, chunks=(3, 3), maxshape=(10, 10))
                # Force extensible_array
                wca_forced = JLD2.WriteChunkedArray(data, chunks=(3, 3), maxshape=(10, 10),
                                                    indexing=:extensible_array)
                JLD2.write_chunked(f, "extensible", wca_forced)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Different element types" begin
        for (T, test_data) in [
            (Float32, reshape(Float32.(1:60), 6, 10)),
            (Float64, reshape(Float64.(1:60), 6, 10)),
            (Int32, reshape(Int32.(1:60), 6, 10)),
            (Int64, reshape(Int64.(1:60), 6, 10))
        ]
            filename = tempname() * ".jld2"
            try
                jldopen(filename, "w") do f
                    wca = JLD2.WriteChunkedArray(test_data, chunks=(2, 3), maxshape=(nothing, 10))
                    JLD2.write_chunked(f, "data", wca)
                end

                jldopen(filename, "r") do f
                    data_read = f["data"]
                    @test data_read == test_data
                    @test eltype(data_read) == T
                end
            finally
                rm(filename, force=true)
            end
        end
    end

    @testset "Various chunk sizes" begin
        data = reshape(Float32.(1:200), 10, 20)

        for chunks in [(2, 2), (5, 5), (3, 7), (10, 10)]
            filename = tempname() * ".jld2"
            try
                jldopen(filename, "w") do f
                    wca = JLD2.WriteChunkedArray(data, chunks=chunks, maxshape=(nothing, 20))
                    JLD2.write_chunked(f, "data", wca)
                end

                jldopen(filename, "r") do f
                    data_read = f["data"]
                    @test data_read == data
                end
            finally
                rm(filename, force=true)
            end
        end
    end

    @testset "Asymmetric unlimited dimensions" begin
        data = reshape(Float32.(1:60), 6, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                # Mix of limited and unlimited
                wca = JLD2.WriteChunkedArray(data, chunks=(2, 3), maxshape=(nothing, 10))
                JLD2.write_chunked(f, "data", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["data"]
                @test data_read == data
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Multiple datasets in one file" begin
        data1 = reshape(Float32.(1:100), 10, 10)
        data2 = reshape(Float64.(1:60), 6, 10)
        data3 = Float32.(1:50)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca1 = JLD2.WriteChunkedArray(data1, chunks=(3, 3), maxshape=(nothing, 10))
                JLD2.write_chunked(f, "data1", wca1)

                wca2 = JLD2.WriteChunkedArray(data2, chunks=(2, 3), maxshape=(6, nothing))
                JLD2.write_chunked(f, "data2", wca2)

                wca3 = JLD2.WriteChunkedArray(data3, chunks=(10,), maxshape=(nothing,))
                JLD2.write_chunked(f, "data3", wca3)
            end

            jldopen(filename, "r") do f
                @test f["data1"] == data1
                @test f["data2"] == data2
                @test f["data3"] == data3
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Very small arrays" begin
        data = reshape(Float32.(1:4), 2, 2)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = JLD2.WriteChunkedArray(data, chunks=(1, 1), maxshape=(nothing, 2))
                JLD2.write_chunked(f, "small", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["small"]
                @test data_read == data
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Large array with many chunks" begin
        # Test with moderate size (not too large for test suite)
        data = reshape(Float32.(1:500), 20, 25)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                wca = JLD2.WriteChunkedArray(data, chunks=(4, 5), maxshape=(nothing, 25))
                JLD2.write_chunked(f, "large", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["large"]
                @test data_read == data
                @test data_read[1, 1] == 1.0f0
                @test data_read[20, 25] == 500.0f0
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Single row/column arrays" begin
        # Single row
        data_row = reshape(Float32.(1:20), 1, 20)
        filename_row = tempname() * ".jld2"

        try
            jldopen(filename_row, "w") do f
                wca = JLD2.WriteChunkedArray(data_row, chunks=(1, 5), maxshape=(nothing, 20))
                JLD2.write_chunked(f, "row", wca)
            end

            jldopen(filename_row, "r") do f
                @test f["row"] == data_row
            end
        finally
            rm(filename_row, force=true)
        end

        # Single column
        data_col = reshape(Float32.(1:20), 20, 1)
        filename_col = tempname() * ".jld2"

        try
            jldopen(filename_col, "w") do f
                wca = JLD2.WriteChunkedArray(data_col, chunks=(5, 1), maxshape=(nothing, 1))
                JLD2.write_chunked(f, "col", wca)
            end

            jldopen(filename_col, "r") do f
                @test f["col"] == data_col
            end
        finally
            rm(filename_col, force=true)
        end
    end

    @testset "Automatic selection with maxshape" begin
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                # Should automatically select extensible_array
                wca = JLD2.WriteChunkedArray(data, chunks=(3, 3), maxshape=(nothing, 10))
                JLD2.write_chunked(f, "auto", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["auto"]
                @test data_read == data
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Edge case: chunk equals array size with unlimited" begin
        # Even though chunk == size, maxshape has unlimited so should use extensible array
        data = reshape(Float32.(1:100), 10, 10)
        filename = tempname() * ".jld2"

        try
            jldopen(filename, "w") do f
                # Single chunk but with unlimited dimension
                wca = JLD2.WriteChunkedArray(data, chunks=(10, 10), maxshape=(nothing, 10))
                JLD2.write_chunked(f, "single_chunk", wca)
            end

            jldopen(filename, "r") do f
                data_read = f["single_chunk"]
                @test data_read == data
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "3D with two unlimited dimensions (requires V2 B-tree)" begin
        data = reshape(Float32.(1:120), 3, 4, 10)
        filename = tempname() * ".jld2"

        try
            # 2+ unlimited dimensions requires V2 B-tree (Phase 5)
            @test_throws JLD2.UnsupportedFeatureException jldopen(filename, "w") do f
                wca = JLD2.WriteChunkedArray(data, chunks=(2, 2, 3), maxshape=(nothing, nothing, 10))
                JLD2.write_chunked(f, "data", wca)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "3D with all unlimited dimensions (requires V2 B-tree)" begin
        data = reshape(Float32.(1:60), 3, 4, 5)
        filename = tempname() * ".jld2"

        try
            # 2+ unlimited dimensions requires V2 B-tree (Phase 5)
            @test_throws JLD2.UnsupportedFeatureException jldopen(filename, "w") do f
                wca = JLD2.WriteChunkedArray(data, chunks=(2, 2, 2), maxshape=(nothing, nothing, nothing))
                JLD2.write_chunked(f, "data", wca)
            end
        finally
            rm(filename, force=true)
        end
    end

    @testset "Verify chunk indexing type selection" begin
        data = reshape(Float32.(1:100), 10, 10)

        # Test automatic selection logic
        @test JLD2.select_chunk_index_type((10, 10), (3, 3), (nothing, 10), nothing) == :extensible_array
        @test JLD2.select_chunk_index_type((10, 10), (3, 3), (nothing, nothing), nothing) == :v2btree  # 2+ unlimited
        @test JLD2.select_chunk_index_type((10, 10), (3, 3), nothing, nothing) == :fixed_array  # no unlimited
    end

end
