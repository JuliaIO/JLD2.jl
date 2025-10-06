using JLD2, Test

@testset "Link Types" begin
    @testset "AbstractLink Type Hierarchy" begin
        # Test HardLink
        offset = JLD2.RelOffset(UInt64(1000))
        hard_link = JLD2.HardLink(offset)
        @test hard_link.target == offset

        # Test SoftLink
        soft_link = JLD2.SoftLink("/path/to/dataset")
        @test soft_link.path == "/path/to/dataset"

        # Test SoftLink validation
        @test_throws ArgumentError JLD2.SoftLink("")
        relative_link = JLD2.SoftLink("../relative/path")
        @test relative_link.path == "../relative/path"

        # Test ExternalLink
        external_link = JLD2.ExternalLink("external_file.h5", "/dataset")
        @test external_link.file_path == "external_file.h5"
        @test external_link.object_path == "/dataset"

        # Test ExternalLink validation
        @test_throws ArgumentError JLD2.ExternalLink("", "/dataset")
        @test_throws ArgumentError JLD2.ExternalLink("file.h5", "")
        # Path traversal is now allowed (security checks removed)
        external_with_dotdot = JLD2.ExternalLink("../path/file.h5", "/dataset")
        @test external_with_dotdot.file_path == "../path/file.h5"
    end

    @testset "Link Equality and Hashing" begin
        offset1 = JLD2.RelOffset(UInt64(1000))
        offset2 = JLD2.RelOffset(UInt64(2000))

        # HardLink equality
        hard1 = JLD2.HardLink(offset1)
        hard2 = JLD2.HardLink(offset1)
        hard3 = JLD2.HardLink(offset2)
        @test hard1 == hard2
        @test hard1 != hard3
        @test hash(hard1) == hash(hard2)
        @test hash(hard1) != hash(hard3)

        # SoftLink equality
        soft1 = JLD2.SoftLink("/path1")
        soft2 = JLD2.SoftLink("/path1")
        soft3 = JLD2.SoftLink("/path2")
        @test soft1 == soft2
        @test soft1 != soft3
        @test hash(soft1) == hash(soft2)
        @test hash(soft1) != hash(soft3)

        # ExternalLink equality
        ext1 = JLD2.ExternalLink("file1.h5", "/dataset1")
        ext2 = JLD2.ExternalLink("file1.h5", "/dataset1")
        ext3 = JLD2.ExternalLink("file2.h5", "/dataset1")
        ext4 = JLD2.ExternalLink("file1.h5", "/dataset2")
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
        hard_link = JLD2.HardLink(offset)
        soft_link = JLD2.SoftLink("/path/to/dataset")
        external_link = JLD2.ExternalLink("external.h5", "/dataset")

        # Test that show methods don't throw
        @test sprint(show, hard_link) isa String
        @test sprint(show, soft_link) isa String
        @test sprint(show, external_link) isa String

        # Test basic formatting
        @test occursin("HardLink", sprint(show, hard_link))
        @test occursin("SoftLink", sprint(show, soft_link))
        @test occursin("ExternalLink", sprint(show, external_link))
        @test occursin("/path/to/dataset", sprint(show, soft_link))
        @test occursin("external.h5", sprint(show, external_link))
    end
end

@testset "Group Link Storage" begin
    @testset "Basic Link Operations" begin
        # Create a temporary test file
        fn = joinpath(mktempdir(), "test_links.jld2")

        jldopen(fn, "w") do f
            # Test that groups now use AbstractLink storage
            # Just test that they are dictionaries mapping String to AbstractLink
            @test eltype(f.root_group.unwritten_links) == Pair{String, JLD2.AbstractLink}
            @test eltype(f.root_group.written_links) == Pair{String, JLD2.AbstractLink}

            # Test lookup_link function
            @test JLD2.lookup_link(f.root_group, "nonexistent") === nothing

            # Test setting a link directly
            offset = JLD2.RelOffset(UInt64(1000))
            hard_link = JLD2.HardLink(offset)
            f.root_group["test_hardlink"] = hard_link

            retrieved_link = JLD2.lookup_link(f.root_group, "test_hardlink")
            @test retrieved_link !== nothing
            @test retrieved_link isa JLD2.HardLink
            @test retrieved_link.target == offset

            # Test lookup_link functionality that replaces lookup_offset
            link = lookup_link(f.root_group, "test_hardlink")
            @test link !== nothing && isa(link, HardLink) && link.target == offset
            @test lookup_link(f.root_group, "nonexistent") === nothing
        end
    end
end

using JLD2, Test
using JLD2: ExternalLink, SoftLink, HardLink, lookup_link

@testset "Phase 2: External Link Creation API" begin

    jldopen("test_external.jld2", "w") do f
        f["dataset1"] = [1, 2, 3, 4, 5]
        f["dataset2"] = "Hello from external file!"
        f["nested/data"] = Dict("key1" => 100, "key2" => 200)
    end

    jldopen("test_main.jld2", "w") do f
        # Test create_external_link! function
        create_external_link!(f, "external_data", "test_external.jld2", "/dataset1")
        create_external_link!(f, "external_string", "test_external.jld2", "/dataset2")
        create_external_link!(f, "external_nested", "test_external.jld2", "/nested/data")

        # Test direct AbstractLink assignment
        external_link = ExternalLink("test_external.jld2", "/dataset1")
        f["direct_external"] = external_link

        # Test soft link creation
        f["original_data"] = [10, 20, 30]
        create_soft_link!(f, "soft_link_to_data", "/original_data")
    end

    # Step 3: Test group internal state
    @testset "Group State Management" begin
        jldopen("test_main.jld2", "r") do f
            # After file is written and closed, all links should be in written_links
            root = f.root_group
            @test isempty(root.unwritten_links)
            @test !isempty(root.written_links)

            # Check that external links are properly stored
            @test haskey(root.written_links, "external_data")
            @test haskey(root.written_links, "direct_external")
            @test haskey(root.written_links, "soft_link_to_data")

            # Check link types
            @test isa(root.written_links["external_data"], ExternalLink)
            @test isa(root.written_links["direct_external"], ExternalLink)
            @test isa(root.written_links["soft_link_to_data"], SoftLink)

            # Test that original data is a hard link
            @test isa(root.written_links["original_data"], HardLink)
        end
    end

    # Step 4: Test link properties
    @testset "Link Properties" begin
        jldopen("test_main.jld2", "r") do f
            root = f.root_group

            # Test external link properties
            ext_link = root.written_links["external_data"]::ExternalLink
            @test ext_link.file_path == "test_external.jld2"
            @test ext_link.object_path == "/dataset1"

            # Test soft link properties
            soft_link = root.written_links["soft_link_to_data"]::SoftLink
            @test soft_link.path == "/original_data"
        end
    end

    @testset "Link Lookup Functions" begin
        jldopen("test_main.jld2", "r") do f
            root = f.root_group

            # Test lookup_link function
            ext_link = lookup_link(root, "external_data")
            @test isa(ext_link, ExternalLink)
            @test ext_link.file_path == "test_external.jld2"

            # Test that lookup_link works for hard links and returns correct link types
            hard_link = lookup_link(root, "original_data")
            @test hard_link !== nothing && isa(hard_link, HardLink)

            # Test that lookup_link returns the correct link type for external links
            ext_link_direct = lookup_link(root, "external_data")
            @test ext_link_direct !== nothing && isa(ext_link_direct, ExternalLink)
        end
    end

    # Clean up test files
    for file in ["test_main.jld2", "test_external.jld2", "test_chain.jld2"]
        isfile(file) && rm(file)
    end
end


# Phase 4: Advanced Error Handling & Edge Cases Test Suite
# Tests for sophisticated circular reference detection, error recovery,
# permission-based access control, and enhanced error handling

using JLD2, Test
using JLD2: ExternalLink, SoftLink, HardLink
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
                calibration_group = JLD2.Group(data_group, "calibration")
                results_group = JLD2.Group(f, "results")
                analysis_group = JLD2.Group(results_group, "analysis")

                # Add some test data
                f["data/measurements/temperature"] = [20.5, 21.0, 19.8, 22.1]
                f["data/measurements/pressure"] = [101.3, 101.1, 101.5]
                f["data/calibration/offset"] = 0.5
                f["results/summary"] = "Test complete"

                # Test absolute soft links (fully supported)
                JLD2.create_soft_link!(measurements_group, "abs_link_to_offset", "/data/calibration/offset")
                JLD2.create_soft_link!(analysis_group, "abs_link_to_temp", "/data/measurements/temperature")
                JLD2.create_soft_link!(f.root_group, "abs_link_to_summary", "/results/summary")

                # Test simple relative soft links (fully supported)
                JLD2.create_soft_link!(measurements_group, "rel_link_to_temp", "temperature")
                JLD2.create_soft_link!(measurements_group, "rel_link_to_pressure", "pressure")

                # Test subdirectory relative navigation (fully supported)
                JLD2.create_soft_link!(data_group, "rel_link_to_meas_temp", "measurements/temperature")
            end

            # Test reading through soft links
            jldopen(test_file, "r") do f
                # Test absolute soft link resolution
                @test f["data/measurements/abs_link_to_offset"] == 0.5
                @test f["results/analysis/abs_link_to_temp"] == [20.5, 21.0, 19.8, 22.1]
                @test f["abs_link_to_summary"] == "Test complete"

                # Test simple relative soft link resolution
                @test f["data/measurements/rel_link_to_temp"] == [20.5, 21.0, 19.8, 22.1]
                @test f["data/measurements/rel_link_to_pressure"] == [101.3, 101.1, 101.5]

                # Test subdirectory relative navigation
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
                JLD2.create_soft_link!(data_group, "broken_absolute", "/nonexistent/path")
                JLD2.create_soft_link!(data_group, "broken_relative", "../missing/data")
                JLD2.create_soft_link!(f.root_group, "broken_root", "/does/not/exist")
            end

            jldopen(test_file, "r") do f
                # Test that broken soft links throw appropriate errors
                @test_throws ArgumentError f["data/broken_absolute"]
                @test_throws ArgumentError f["data/broken_relative"]
                @test_throws ArgumentError f["broken_root"]
            end
        end
    end

    @testset "Soft Link Display and Inspection" begin
        # Test that soft links display correctly in group listings
        mktempdir() do dir
            test_file = joinpath(dir, "soft_link_display_test.jld2")

            jldopen(test_file, "w") do f
                data_group = JLD2.Group(f, "data")
                f["data/original"] = [1, 2, 3, 4, 5]

                # Create various link types for display testing
                create_soft_link!(data_group, "soft_link", "/data/original")
                f["data/hard_link"] = f["data/original"]  # Hard link

                # The display function should work without errors
                # Note: The exact display format may vary, so we just test that it doesn't error
                display_output = sprint(show, data_group)
                @test !isempty(display_output)
                @test isa(display_output, String)
            end
        end
    end
end
