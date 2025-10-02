# Tests for Chunked Array Writing API
# This tests the API logic, validation, and automatic selection BEFORE actual writing is implemented

using JLD2, Test

@testset "WriteChunkedArray Construction" begin
    @testset "Basic construction" begin
        data = rand(Float32, 30, 10)

        # Valid construction
        ca = WriteChunkedArray(data, chunks=(5, 5))
        @test size(ca) == (30, 10)
        @test ca.chunks == (5, 5)
        @test isnothing(ca.maxshape)
        @test isnothing(ca.fill_value)
        @test isnothing(ca.indexing)
        @test isnothing(ca.filters)
    end

    @testset "With maxshape" begin
        data = rand(Float32, 30, 10)

        # One unlimited dimension
        ca = WriteChunkedArray(data, chunks=(5, 5), maxshape=(nothing, 10))
        @test ca.maxshape == (nothing, 10)

        # Two unlimited dimensions
        ca = WriteChunkedArray(data, chunks=(5, 5), maxshape=(nothing, nothing))
        @test ca.maxshape == (nothing, nothing)

        # Mixed
        ca = WriteChunkedArray(data, chunks=(5, 5), maxshape=(100, nothing))
        @test ca.maxshape == (100, nothing)
    end

    @testset "With fill_value" begin
        data = rand(Float32, 30, 10)
        ca = WriteChunkedArray(data, chunks=(5, 5), fill_value=0.0f0)
        @test ca.fill_value == 0.0f0
    end

    @testset "With indexing override" begin
        data = rand(Float32, 30, 10)
        ca = WriteChunkedArray(data, chunks=(5, 5), indexing=:v2btree)
        @test ca.indexing == :v2btree
    end

    @testset "Auto chunking" begin
        data = rand(Float32, 100, 50)
        ca = WriteChunkedArray(data, chunks=:auto)
        @test ca.chunks isa NTuple{2,Int}
        @test all(ca.chunks .> 0)
        println("  Auto chunks for (100,50): $(ca.chunks)")
    end
end

@testset "Validation" begin
    @testset "Chunk dimension validation" begin
        data = rand(Float32, 30, 10)

        # Wrong number of dimensions (caught by Julia's type system)
        @test_throws TypeError WriteChunkedArray(data, chunks=(5,))
        @test_throws TypeError WriteChunkedArray(data, chunks=(5, 5, 5))

        # Non-positive chunk sizes (caught by our validation)
        @test_throws ArgumentError WriteChunkedArray(data, chunks=(0, 5))
        @test_throws ArgumentError WriteChunkedArray(data, chunks=(-1, 5))

        # Chunk larger than data (should warn but not error)
        @test_logs (:warn,) WriteChunkedArray(data, chunks=(100, 5))
    end

    @testset "maxshape validation" begin
        data = rand(Float32, 30, 10)

        # Wrong number of dimensions (caught by Julia's type system)
        @test_throws TypeError WriteChunkedArray(data, chunks=(5, 5), maxshape=(100,))

        # maxshape smaller than current data (caught by our validation)
        @test_throws ArgumentError WriteChunkedArray(data, chunks=(5, 5), maxshape=(20, 10))
        @test_throws ArgumentError WriteChunkedArray(data, chunks=(5, 5), maxshape=(30, 5))

        # Valid: maxshape >= data size or nothing
        @test WriteChunkedArray(data, chunks=(5, 5), maxshape=(30, 10)) isa WriteChunkedArray
        @test WriteChunkedArray(data, chunks=(5, 5), maxshape=(100, 20)) isa WriteChunkedArray
        @test WriteChunkedArray(data, chunks=(5, 5), maxshape=(nothing, 10)) isa WriteChunkedArray
    end

    @testset "fill_value type validation" begin
        data = rand(Float32, 30, 10)

        # Wrong type (caught by Julia's type system)
        @test_throws TypeError WriteChunkedArray(data, chunks=(5, 5), fill_value=0.0)  # Float64 != Float32

        # Correct type
        @test WriteChunkedArray(data, chunks=(5, 5), fill_value=0.0f0) isa WriteChunkedArray
    end

    @testset "indexing type validation" begin
        data = rand(Float32, 30, 10)

        # Invalid index type
        @test_throws ArgumentError WriteChunkedArray(data, chunks=(5, 5), indexing=:invalid_type)

        # Valid index types
        for index_type in [:single_chunk, :implicit_index, :fixed_array, :extensible_array, :v2btree]
            @test WriteChunkedArray(data, chunks=(5, 5), indexing=index_type) isa WriteChunkedArray
        end
    end
end

@testset "Automatic Index Type Selection" begin
    @testset "Single chunk" begin
        data_size = (30, 10)
        chunks = (30, 10)  # Same as data size
        @test JLD2.select_chunk_index_type(data_size, chunks, nothing, nothing) == :single_chunk
    end

    @testset "Fixed array" begin
        data_size = (30, 10)
        chunks = (5, 5)

        # No maxshape
        @test JLD2.select_chunk_index_type(data_size, chunks, nothing, nothing) == :fixed_array

        # maxshape with all dimensions fixed
        @test JLD2.select_chunk_index_type(data_size, chunks, (30, 10), nothing) == :fixed_array
        @test JLD2.select_chunk_index_type(data_size, chunks, (100, 20), nothing) == :fixed_array

        # Even with fill_value (matches h5py behavior)
        @test JLD2.select_chunk_index_type(data_size, chunks, nothing, 0.0f0) == :fixed_array
    end

    @testset "Extensible array" begin
        data_size = (30, 10)
        chunks = (5, 5)

        # One unlimited dimension (different positions)
        @test JLD2.select_chunk_index_type(data_size, chunks, (nothing, 10), nothing) == :extensible_array
        @test JLD2.select_chunk_index_type(data_size, chunks, (30, nothing), nothing) == :extensible_array
    end

    @testset "V2 B-tree" begin
        data_size = (30, 10)
        chunks = (5, 5)

        # Two unlimited dimensions
        @test JLD2.select_chunk_index_type(data_size, chunks, (nothing, nothing), nothing) == :v2btree
    end

    @testset "3D arrays" begin
        data_size = (100, 50, 20)
        chunks = (10, 10, 5)

        @test JLD2.select_chunk_index_type(data_size, chunks, nothing, nothing) == :fixed_array
        @test JLD2.select_chunk_index_type(data_size, chunks, (nothing, 50, 20), nothing) == :extensible_array
        @test JLD2.select_chunk_index_type(data_size, chunks, (nothing, nothing, 20), nothing) == :v2btree
        @test JLD2.select_chunk_index_type(data_size, chunks, (nothing, nothing, nothing), nothing) == :v2btree
    end
end

@testset "Auto Chunk Size" begin
    @testset "Small arrays - single chunk" begin
        # For small arrays that fit in 32KB, use single chunk
        @test JLD2.auto_chunk_size((100, 50), 4) == (100, 50)  # 20KB total
        @test JLD2.auto_chunk_size((50, 50, 2), 4) == (50, 50, 2)  # 20KB total
    end

    @testset "Large arrays - divide evenly" begin
        # For large arrays, divide to get ~32KB chunks
        result = JLD2.auto_chunk_size((10000, 10000), 4)
        @test result isa NTuple{2,Int}
        @test all(result .> 0)
        @test all(result .<= (10000, 10000))
        println("  Auto chunks for (10000,10000): $result")

        # Target size check: should be roughly 32KB
        chunk_bytes = prod(result) * 4
        @test 16_000 <= chunk_bytes <= 64_000  # Allow some flexibility
    end

    @testset "1D arrays" begin
        result = JLD2.auto_chunk_size((1_000_000,), 4)
        @test result isa NTuple{1,Int}
        @test result[1] > 0
        println("  Auto chunks for (1000000,): $result")
    end
end

@testset "AbstractArray Interface" begin
    data = reshape(1.0f0:300.0f0, 30, 10)
    ca = WriteChunkedArray(data, chunks=(5, 5))

    @testset "Basic properties" begin
        @test size(ca) == (30, 10)
        @test length(ca) == 300
        @test eltype(ca) == Float32
    end

    @testset "Indexing" begin
        @test ca[1, 1] == 1.0f0
        @test ca[2, 1] == 2.0f0
        @test ca[30, 10] == 300.0f0
    end

    @testset "Iteration" begin
        @test collect(ca) == reshape(1.0f0:300.0f0, 30, 10)
        @test sum(ca) == sum(1.0f0:300.0f0)
    end
end

@testset "write_chunked Function" begin
    @testset "Parameter validation" begin
        # Write to read-only file
        mktempdir() do dir
            filename = joinpath(dir, "readonly.jld2")
            jldsave(filename; dummy=1)

            jldopen(filename, "r") do f
                data = rand(Float32, 10, 10)
                @test_throws ArgumentError write_chunked(f, "test", data; chunks=(5, 5))
            end
        end
    end

    @testset "Stub implementations throw appropriate errors" begin
        mktempdir() do dir
            filename = joinpath(dir, "test_stubs.jld2")

            # Test each chunk index type stub
            data = rand(Float32, 30, 10)

            # Single chunk
            jldopen(filename, "w") do f
                @test_throws JLD2.UnsupportedFeatureException write_chunked(
                    f, "single", data; chunks=(30, 10)
                )
            end

            # Fixed array
            jldopen(filename, "w") do f
                @test_throws JLD2.UnsupportedFeatureException write_chunked(
                    f, "fixed", data; chunks=(5, 5)
                )
            end

            # Extensible array
            jldopen(filename, "w") do f
                @test_throws JLD2.UnsupportedFeatureException write_chunked(
                    f, "ext", data; chunks=(5, 5), maxshape=(nothing, 10)
                )
            end

            # V2 B-tree
            jldopen(filename, "w") do f
                @test_throws JLD2.UnsupportedFeatureException write_chunked(
                    f, "btree", data; chunks=(5, 5), maxshape=(nothing, nothing)
                )
            end

            # Implicit index (manual override)
            jldopen(filename, "w") do f
                @test_throws JLD2.UnsupportedFeatureException write_chunked(
                    f, "implicit", data; chunks=(5, 5), indexing=:implicit_index
                )
            end
        end
    end

    @testset "Auto chunking with write_chunked" begin
        mktempdir() do dir
            filename = joinpath(dir, "test_auto.jld2")

            jldopen(filename, "w") do f
                data = rand(Float32, 100, 100)
                # Should not error during parameter validation
                # (will error in stub implementation)
                @test_throws JLD2.UnsupportedFeatureException write_chunked(
                    f, "auto", data; chunks=:auto
                )
            end
        end
    end
end

@testset "Integration with WriteChunkedArray" begin
    @testset "Index type inference through wrapper" begin
        data = rand(Float32, 30, 10)

        # Single chunk
        ca = WriteChunkedArray(data, chunks=(30, 10))
        inferred = JLD2.select_chunk_index_type(
            size(ca.data), ca.chunks, ca.maxshape, ca.fill_value
        )
        @test inferred == :single_chunk

        # Fixed array
        ca = WriteChunkedArray(data, chunks=(5, 5))
        inferred = JLD2.select_chunk_index_type(
            size(ca.data), ca.chunks, ca.maxshape, ca.fill_value
        )
        @test inferred == :fixed_array

        # Extensible array
        ca = WriteChunkedArray(data, chunks=(5, 5), maxshape=(nothing, 10))
        inferred = JLD2.select_chunk_index_type(
            size(ca.data), ca.chunks, ca.maxshape, ca.fill_value
        )
        @test inferred == :extensible_array

        # V2 B-tree
        ca = WriteChunkedArray(data, chunks=(5, 5), maxshape=(nothing, nothing))
        inferred = JLD2.select_chunk_index_type(
            size(ca.data), ca.chunks, ca.maxshape, ca.fill_value
        )
        @test inferred == :v2btree
    end

    @testset "Manual override" begin
        data = rand(Float32, 30, 10)

        # Override automatic selection
        ca = WriteChunkedArray(data, chunks=(5, 5), indexing=:v2btree)
        @test ca.indexing == :v2btree

        # Automatic selection would give :fixed_array
        auto = JLD2.select_chunk_index_type(
            size(ca.data), ca.chunks, ca.maxshape, ca.fill_value
        )
        @test auto == :fixed_array  # Different from override
    end
end

println("\n" * "="^70)
println("Chunked Writing API Tests Complete!")
println("="^70)
println("\nSummary:")
println("  ✓ WriteChunkedArray construction and validation")
println("  ✓ Automatic chunk index type selection")
println("  ✓ Auto-chunking heuristics")
println("  ✓ AbstractArray interface")
println("  ✓ write_chunked function")
println("  ✓ Stub implementations throw appropriate errors")
println("\nAll API logic validated. Ready for Phase 1-5 implementations!")
println("="^70)
