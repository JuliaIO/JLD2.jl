using JLD2, Test

@testset "Link Types" begin
    @testset "Link Type" begin
        # Test hard link (internal constructor)
        offset = JLD2.RelOffset(UInt64(1000))
        hard_link = JLD2.Link(offset)
        @test hard_link.offset == offset
        @test JLD2.is_hard_link(hard_link)

        # Test soft link
        soft_link = JLD2.Link("/path/to/dataset")
        @test soft_link.path == "/path/to/dataset"
        @test JLD2.is_soft_link(soft_link)

        # Test soft link validation
        @test_throws ArgumentError JLD2.Link("")
        relative_link = JLD2.Link("../relative/path")
        @test relative_link.path == "../relative/path"

        # Test external link
        external_link = JLD2.Link("/dataset"; file="external_file.h5")
        @test external_link.external_file == "external_file.h5"
        @test external_link.path == "/dataset"
        @test JLD2.is_external_link(external_link)

        # Test external link validation
        @test_throws ArgumentError JLD2.Link("/dataset"; file="")
        # Path traversal is allowed
        external_with_dotdot = JLD2.Link("/dataset"; file="../path/file.h5")
        @test external_with_dotdot.external_file == "../path/file.h5"
    end

    @testset "Link Equality and Hashing" begin
        offset1 = JLD2.RelOffset(UInt64(1000))
        offset2 = JLD2.RelOffset(UInt64(2000))

        # Hard link equality
        hard1 = JLD2.Link(offset1)
        hard2 = JLD2.Link(offset1)
        hard3 = JLD2.Link(offset2)
        @test hard1 == hard2
        @test hard1 != hard3
        @test hash(hard1) == hash(hard2)
        @test hash(hard1) != hash(hard3)

        # Soft link equality
        soft1 = JLD2.Link("/path1")
        soft2 = JLD2.Link("/path1")
        soft3 = JLD2.Link("/path2")
        @test soft1 == soft2
        @test soft1 != soft3
        @test hash(soft1) == hash(soft2)
        @test hash(soft1) != hash(soft3)

        # External link equality
        ext1 = JLD2.Link("/dataset1"; file="file1.h5")
        ext2 = JLD2.Link("/dataset1"; file="file1.h5")
        ext3 = JLD2.Link("/dataset1"; file="file2.h5")
        ext4 = JLD2.Link("/dataset2"; file="file1.h5")
        @test ext1 == ext2
        @test ext1 != ext3
        @test ext1 != ext4
        @test hash(ext1) == hash(ext2)
        @test hash(ext1) != hash(ext3)
        @test hash(ext1) != hash(ext4)

        # Cross-type inequality
        @test hard1 != soft1
        @test hard1 != ext1
        @test soft1 != ext1
    end

    @testset "Display Methods" begin
        offset = JLD2.RelOffset(UInt64(1000))
        hard_link = JLD2.Link(offset)
        soft_link = JLD2.Link("/path/to/dataset")
        external_link = JLD2.Link("/dataset"; file="external.h5")

        # Test that show methods don't throw
        @test sprint(show, hard_link) isa String
        @test sprint(show, soft_link) isa String
        @test sprint(show, external_link) isa String

        # Test basic formatting
        @test occursin("hard", sprint(show, hard_link))
        @test occursin("soft", sprint(show, soft_link))
        @test occursin("external", sprint(show, external_link))
        @test occursin("/path/to/dataset", sprint(show, soft_link))
        @test occursin("external.h5", sprint(show, external_link))
    end
end

@testset "Phase 2: External Link Creation API" begin

    jldopen("test_external.jld2", "w") do f
        f["dataset1"] = [1, 2, 3, 4, 5]
        f["dataset2"] = "Hello from external file!"
        f["nested/data"] = Dict("key1" => 100, "key2" => 200)
    end

    jldopen("test_main.jld2", "w") do f
        # Test external link creation
        f["external_data"] = JLD2.Link("/dataset1"; file="test_external.jld2")
        f["external_string"] = JLD2.Link("/dataset2"; file="test_external.jld2")
        f["external_nested"] = JLD2.Link("/nested/data"; file="test_external.jld2")

        # Test direct Link assignment
        external_link = JLD2.Link("/dataset1"; file="test_external.jld2")
        f["direct_external"] = external_link

        # Test soft link creation
        f["original_data"] = [10, 20, 30]
        f["soft_link_to_data"] = JLD2.Link("/original_data")
    end

    jldopen("test_main.jld2", "r+") do f
        @test f["direct_external"] == collect(1:5)
        @test_throws ArgumentError f["direct_external/dummy"] = 42
        @test_throws ArgumentError f["original_data/dummy"] = 42

        jldopen("test_external.jld2", "r") do _
            # Hit different code path if external file is already open
            @test f["direct_external"] == collect(1:5)
        end
    end



    # Clean up test files
    for file in ["test_main.jld2", "test_external.jld2"]
        isfile(file) && rm(file)
    end
end

using JLD2: UnsupportedFeatureException

@testset "Soft Link Support" begin

    @testset "Enhanced Soft Link Path Resolution" begin
        # Test soft link resolution with current Phase 5 capabilities
        mktempdir() do dir
            test_file = joinpath(dir, "soft_link_resolution_test.jld2")

            jldopen(test_file, "w") do f
                # Create hierarchical structure
                data_group = JLD2.Group(f, "data")
                measurements_group = JLD2.Group(data_group, "measurements")
                f["data/measurements/temperature"] = [20.5, 21.0, 19.8, 22.1]
                data_group["abs_link_to_temp"] = JLD2.Link("/data/measurements/temperature")
                data_group["rel_link_to_meas_temp"] = JLD2.Link("measurements/temperature")
            end

            jldopen(test_file, "r") do f
                @test f["data/abs_link_to_temp"] == [20.5, 21.0, 19.8, 22.1]
                @test f["data/rel_link_to_meas_temp"] == [20.5, 21.0, 19.8, 22.1]
            end
        end
    end

    @testset "Soft Link Error Handling" begin
        # Test error conditions for broken soft links
        mktempdir() do dir
            test_file = joinpath(dir, "soft_link_errors_test.jld2")

            jldopen(test_file, "w") do f
                data_group = JLD2.Group(f, "data")
                f["data/existing"] = 42

                # Create soft links to non-existent targets
                data_group["broken_absolute"] = JLD2.Link("/nonexistent/path")
                data_group["broken_relative"] = JLD2.Link("../missing/data")
                f.root_group["broken_root"] = JLD2.Link("/does/not/exist")
            end

            jldopen(test_file, "r") do f
                # Test that broken soft links throw appropriate errors
                @test_throws KeyError f["data/broken_absolute"]
                @test_throws KeyError f["data/broken_relative"]
                @test_throws KeyError f["broken_root"]
            end
        end
    end
end
