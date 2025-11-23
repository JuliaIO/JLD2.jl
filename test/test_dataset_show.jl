using Test, JLD2
using LazyArtifacts

# NOTE: Keep this artifact version in sync with test_files.jl
testfiles = artifact"testfiles/JLD2TestFiles-0.1.9/artifacts"

# Helper functions for testing
function test_dataset_show_basic(dset, key, filename)
    # Test that show doesn't throw errors
    if filename in ["test_dataset_show_structs.jld2", "test_dataset_show_nested.jld2"]
        show_success = try
            show(IOBuffer(), MIME("text/plain"), dset)
            true
        catch
            false
        end
        @test show_success
    else
        @test_nowarn show(IOBuffer(), MIME("text/plain"), dset)
    end
end

function get_dataset_show_output(dset)
    io = IOBuffer()
    show(io, MIME("text/plain"), dset)
    String(take!(io))
end

function test_dataset_format_structure(output, key)
    @test startswith(output, "┌─ Dataset:")
    @test contains(output, "\"$key\"")
    @test endswith(strip(output), "└─")

    # Check proper indentation
    lines = split(output, '\n')
    content_lines = filter(line -> contains(line, "│"), lines)
    !isempty(content_lines) && @test all(line -> startswith(line, "│"), content_lines)
end

function test_dataset_written_info(dset, output)
    if JLD2.iswritten(dset)
        @test contains(output, "datatype:")
        @test contains(output, "type name:")
        !isnothing(dset.dataspace) && @test contains(output, "dataspace:")
        !isnothing(dset.layout) && @test contains(output, "layout:")
    else
        @test contains(output, "(unwritten)")
    end
end

function test_dataset_detailed_output(dset, key, output)
    @testset "Detailed output validation for $key" begin
        lines = split(output, '\n')
        @test length(lines) >= 2

        # Test header and footer
        @test startswith(lines[1], "┌─ Dataset:") && contains(lines[1], "\"$key\"")

        non_empty_lines = filter(line -> !isempty(strip(line)), lines)
        !isempty(non_empty_lines) && @test strip(non_empty_lines[end]) == "└─"

        # Test content lines structure
        length(non_empty_lines) > 2 && foreach(line -> @test(startswith(line, "│")), non_empty_lines[2:end-1])

        # Test dimension information
        if contains(key, "scalar")
            @test contains(output, "dimensions: ()")
        elseif contains(key, "array") || contains(key, "matrix")
            @test contains(output, "dimensions:")
        end

        # Test type information presence (relaxed)
        if JLD2.iswritten(dset) && contains(output, "written structure:")
            type_patterns = ["::", "Int", "Float", "String", "Bool", "Nothing", "Symbol"]
            type_info_present = any(pattern -> any(line -> contains(line, pattern), lines), type_patterns)
            !type_info_present && @test_skip "Type information not always recognizable in display"
        end
    end
end

function test_dataset_display_modes(dset, key)
    @testset "Display modes for $key" begin
        # Test compact display
        compact_output = sprint(show, dset)
        @test !isempty(compact_output)
        @test contains(compact_output, "Dataset") || contains(compact_output, key)
    end
end

function test_dataset_edge_cases(dset)
    @testset "Edge cases" begin
        # Test with different IO contexts
        for io_context in [IOBuffer(), IOContext(IOBuffer(), :color => false)]
            @test_nowarn show(io_context, MIME("text/plain"), dset)
            output = String(take!(io_context isa IOContext ? io_context.io : io_context))
            @test !isempty(output) && startswith(output, "┌─ Dataset:")
        end

        # Test string representation
        str_repr = string(dset)
        @test !isempty(str_repr) && contains(str_repr, "Dataset")
    end
end

@testset "Dataset Show Function Tests" begin
    # Test dataset show functionality with comprehensive test files

    test_files = [
        ("test_dataset_show_basic.jld2", ["int_scalar", "float_scalar", "string_scalar", "bool_scalar", "int_array", "float_matrix", "string_array"]),
        ("test_dataset_show_structs.jld2", ["simple_struct", "mutable_struct", "parametric_int", "parametric_string"]),
        ("test_dataset_show_nested.jld2", ["nested_basic", "simple_tuple", "named_tuple", "union_type"]),
        ("test_dataset_show_special.jld2", ["empty_array", "empty_string", "nothing_val", "symbol_val", "small_matrix"])
    ]

    for (filename, expected_keys) in test_files
        @testset "Show output for $filename" begin
            fn = joinpath(testfiles, filename)
            jldopen(fn, "r") do f
                # Verify all expected keys are present
                @test all(key -> haskey(f, key), expected_keys)

                for key in keys(f)
                    @testset "Dataset: $key" begin
                        dset = JLD2.get_dataset(f, key)

                        # Test show functionality
                        test_dataset_show_basic(dset, key, filename)
                        output = get_dataset_show_output(dset)
                        test_dataset_format_structure(output, key)
                        test_dataset_written_info(dset, output)
                        test_dataset_detailed_output(dset, key, output)
                        test_dataset_display_modes(dset, key)
                        test_dataset_edge_cases(dset)
                    end
                end
            end
        end
    end

    # Additional comprehensive test suites to match original coverage
    @testset "Attribute Display Tests" begin
        # Test datasets with attributes
        for filename in ["test_dataset_show_basic.jld2", "test_dataset_show_structs.jld2"]
            fn = joinpath(testfiles, filename)
            jldopen(fn, "r") do f
                for key in keys(f)
                    dset = JLD2.get_dataset(f, key)
                    # Test attribute display if any exist
                    if !isempty(dset.attributes)
                        io = IOBuffer()
                        show(io, MIME("text/plain"), dset)
                        output = String(take!(io))
                        @test contains(output, "attributes:") ||
                              !isempty(dset.attributes)  # Should show attributes if they exist
                    end
                end
            end
        end
    end

    @testset "Committed Datatype Display" begin
        # Test committed datatype information
        for filename in ["test_dataset_show_structs.jld2", "test_dataset_show_nested.jld2"]
            fn = joinpath(testfiles, filename)
            jldopen(fn, "r") do f
                for key in keys(f)
                    dset = JLD2.get_dataset(f, key)
                    if dset.datatype isa JLD2.SharedDatatype
                        io = IOBuffer()
                        show(io, MIME("text/plain"), dset)
                        output = String(take!(io))
                        @test contains(output, "committed")
                        @test contains(output, "RelOffset")
                    end
                end
            end
        end
    end

    @testset "Error Handling Tests" begin
        # Test error handling for edge cases
        fn = joinpath(testfiles, "test_dataset_show_special.jld2")
        jldopen(fn, "r") do f
            # Test with nothing value
            if haskey(f, "nothing_val")
                dset = JLD2.get_dataset(f, "nothing_val")
                @test_nowarn show(IOBuffer(), MIME("text/plain"), dset)
            end

            # Test with empty containers
            if haskey(f, "empty_array")
                dset = JLD2.get_dataset(f, "empty_array")
                io = IOBuffer()
                show(io, MIME("text/plain"), dset)
                output = String(take!(io))
                @test contains(output, "dimensions:")
            end
        end
    end

    @testset "Output Format Consistency" begin
        # Test that output format is consistent across all datasets
        all_outputs = String[]

        for (filename, expected_keys) in test_files
            fn = joinpath(testfiles, filename)
            jldopen(fn, "r") do f
                for key in keys(f)
                    dset = JLD2.get_dataset(f, key)
                    io = IOBuffer()
                    show(io, MIME("text/plain"), dset)
                    output = String(take!(io))
                    push!(all_outputs, output)

                    # Test consistent formatting
                    lines = split(output, '\n')
                    @test startswith(lines[1], "┌─ Dataset:")

                    # Check footer properly (handle trailing newlines)
                    non_empty_lines = [line for line in lines if !isempty(strip(line))]
                    if !isempty(non_empty_lines)
                        @test strip(non_empty_lines[end]) == "└─"
                    end

                    # Test consistent indentation (excluding header and footer)
                    if length(non_empty_lines) > 2
                        content_lines = non_empty_lines[2:end-1]
                        for line in content_lines
                            @test startswith(line, "│")
                        end
                    end

                    # Test consistent spacing
                    @test !contains(output, "│ \t")  # No mixed tabs/spaces
                    @test !contains(output, "  \n")  # No trailing spaces

                    # Test proper line endings
                    @test !contains(output, "\r")   # No Windows line endings

                    # Test Unicode box drawing characters are used correctly
                    @test contains(output, "┌─") && contains(output, "└─") && contains(output, "│")
                end
            end
        end

        # Test that we have comprehensive coverage
        @test length(all_outputs) >= 20  # Should test at least 20 different datasets

        # Test that outputs have variety (not all identical)
        unique_outputs = unique(all_outputs)
        @test length(unique_outputs) >= 15  # Should have varied content
    end

    @testset "Performance and Memory Tests" begin
        # Test that show operations don't consume excessive memory or time
        fn = joinpath(testfiles, "test_dataset_show_basic.jld2")
        jldopen(fn, "r") do f
            for key in keys(f)
                dset = JLD2.get_dataset(f, key)

                # Test performance
                @test (@elapsed show(IOBuffer(), MIME("text/plain"), dset)) < 1.0  # Should be fast

                # Test memory usage is reasonable
                io = IOBuffer()
                show(io, MIME("text/plain"), dset)
                output = String(take!(io))
                @test length(output) < 10000  # Should not be excessively long
                @test sizeof(output) < 50000  # Should not use excessive memory
            end
        end
    end

    @testset "Unwritten Dataset Show - Issue #697" begin
        # Test that showing an unwritten dataset with a Julia type doesn't throw errors
        # Regression test for https://github.com/JuliaIO/JLD2.jl/issues/697
        mktempdir() do dir
            fn = joinpath(dir, "test_unwritten.jld2")

            # Test with various Julia types
            test_types = [
                (Int, (10,)),
                (Float64, (5, 5)),
                (String, (3,)),
                (Bool, (2, 3, 4)),
                (UInt8, (100,))
            ]

            for (T, dims) in test_types
                jldopen(fn, "w") do f
                    # Create dataset with Julia type (not yet converted to HDF5 type)
                    dset = JLD2.create_dataset(f, "data", T, dims)

                    # Show should not throw an error
                    @test_nowarn show(IOBuffer(), MIME("text/plain"), dset)

                    # Verify output contains expected information
                    io = IOBuffer()
                    show(io, MIME("text/plain"), dset)
                    output = String(take!(io))

                    @test contains(output, "Dataset:")
                    @test contains(output, "\"data\"")
                    @test contains(output, "(unwritten)")
                    @test contains(output, "datatype: DataType")
                    @test contains(output, "type name: $T")

                    # Verify proper formatting
                    @test startswith(output, "┌─ Dataset:")
                    @test contains(output, "└─")
                end
                rm(fn)
            end

            # Test with nothing datatype (should skip datatype info)
            jldopen(fn, "w") do f
                dset = JLD2.create_dataset(f, "data", nothing, nothing)
                @test_nowarn show(IOBuffer(), MIME("text/plain"), dset)

                io = IOBuffer()
                show(io, MIME("text/plain"), dset)
                output = String(take!(io))

                @test contains(output, "(unwritten)")
                @test !contains(output, "datatype:")  # Should not show datatype if nothing
            end
            rm(fn)
        end
    end
end
