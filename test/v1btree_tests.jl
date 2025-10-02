# V1 B-tree Implementation Tests
# Tests for V1 B-tree writing functionality for chunked dataset indexing

using JLD2
using Test

@testset "V1 B-tree Core Functionality" begin
    @testset "V1 B-tree Data Structures" begin
        # Test key creation and validation
        # HDF5 stores dimensions in reverse order: Julia [1,2,3] -> HDF5 [2,1,0,0] (0-based, reversed, + datatype offset)
        key1 = JLD2.create_chunk_key([1, 2, 3], UInt32(1024))
        @test key1.indices == UInt64[2, 1, 0, 0]  # Reversed and 0-based for HDF5 compatibility
        @test key1.chunk_size == 1024
        @test key1.filter_mask == 0

        # Test key validation
        @test_throws ArgumentError JLD2.V1ChunkKey(UInt32(1024), UInt32(0), UInt64[])  # Empty indices
        @test_throws ArgumentError JLD2.V1ChunkKey(UInt32(1024), UInt32(0), UInt64[1])  # Only one index
        @test_throws ArgumentError JLD2.V1ChunkKey(UInt32(1024), UInt32(0), UInt64[1, 2, 3])  # Last not zero

        # Test key comparison (using actual HDF5-ordered indices)
        key2 = JLD2.create_chunk_key([1, 2, 4], UInt32(1024))  # -> [3, 1, 0, 0]
        key3 = JLD2.create_chunk_key([1, 1, 4], UInt32(1024))  # -> [3, 0, 0, 0]
        @test JLD2.compare_chunk_keys(key1, key2) < 0  # [2,1,0,0] < [3,1,0,0]
        @test JLD2.compare_chunk_keys(key3, key2) < 0  # [3,0,0,0] < [3,1,0,0] (0 < 1 in second dimension)
        @test JLD2.compare_chunk_keys(key1, key1) == 0  # Equal

        # Test max entries calculation
        file = tempname()
        jldopen(file, "w") do f
            max_2d = JLD2.calculate_max_entries(f, UInt8(2))
            max_3d = JLD2.calculate_max_entries(f, UInt8(3))
            @test max_2d > 0
            @test max_3d > 0
            # 3D keys should fit fewer entries per node (larger keys)
            # But with cap at 64, they might be equal
            @test max_3d <= max_2d
        end
        rm(file, force=true)
    end

    @testset "V1 B-tree Basic Operations" begin
        file = tempname()
        try
            jldopen(file, "w") do f
                # Create a simple B-tree
                btree = JLD2.V1BTree(JLD2.UNDEFINED_ADDRESS, UInt8(2), UInt16(10), f)

                # Insert some chunks
                JLD2.insert_chunk!(btree, [1, 1], JLD2.h5offset(f, 1000), UInt32(1024))
                JLD2.insert_chunk!(btree, [1, 2], JLD2.h5offset(f, 2000), UInt32(1024))
                JLD2.insert_chunk!(btree, [2, 1], JLD2.h5offset(f, 3000), UInt32(1024))

                # Check pending chunks
                @test length(btree.pending_chunks) == 3

                # Finalize B-tree (max_indices for 2x2 array: (1, 1, 0) in HDF5 order)
                JLD2.finalize_btree!(btree, (1, 1, 0))
                @test btree.root != JLD2.UNDEFINED_ADDRESS

                # Pending chunks should be cleared
                @test isempty(btree.pending_chunks)
            end
        finally
            rm(file, force=true)
        end
    end
end
