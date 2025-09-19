#!/usr/bin/env julia
using JLD2

# Define various custom structs for testing
struct SimpleStruct
    x::Int64
    y::Float64
    name::String
end

mutable struct MutableStruct
    counter::Int
    data::Vector{Float64}
    flag::Bool
end

struct ParametricStruct{T}
    value::T
    metadata::String
end

struct NestedStruct
    simple::SimpleStruct
    numbers::Vector{Int}
    optional::Union{Nothing, String}
end

struct EmptyStruct
end

struct SingleFieldStruct{T}
    field::T
end

# Complex nested structure
struct ComplexNested
    id::UInt64
    nested::NestedStruct
    parametric::ParametricStruct{Float32}
    collection::Dict{String, Any}
end

# Generate test data files

function create_basic_types_file()
    println("Creating basic_types.jld2...")
    jldsave("test_basic_types.jld2";
        # Basic numeric types
        int8_val=Int8(42),
        int16_val=Int16(1000),
        int32_val=Int32(100000),
        int64_val=Int64(123456789),
        uint8_val=UInt8(255),
        uint16_val=UInt16(65535),
        uint32_val=UInt32(4294967295),
        uint64_val=UInt64(18446744073709551615),

        # Floating point
        float16_val=Float16(3.14),
        float32_val=Float32(2.71828),
        float64_val=Float64(1.41421356),

        # Complex numbers
        complex64_val=ComplexF32(1.0, 2.0),
        complex128_val=ComplexF64(3.0, 4.0),

        # Boolean
        bool_true=true,
        bool_false=false,

        # Strings
        empty_string="",
        simple_string="Hello World",
        unicode_string="Hello 🌍 Unicode! αβγ",
        long_string=repeat("A", 1000),

        # Arrays
        empty_array=Int[],
        small_array=[1, 2, 3, 4, 5],
        matrix=reshape(1:12, 3, 4),
        multidim_array=reshape(1:24, 2, 3, 4),
        string_array=["one", "two", "three"],
        mixed_array=Any[1, "two", 3.0, true],

        # Special values
        nothing_val=nothing,
        missing_val=missing
    )
end

function create_custom_structs_file()
    println("Creating custom_structs.jld2...")

    simple = SimpleStruct(42, 3.14159, "test")
    mutable_struct = MutableStruct(10, [1.0, 2.0, 3.0], true)
    parametric_int = ParametricStruct{Int}(100, "integer version")
    parametric_string = ParametricStruct{String}("hello", "string version")

    jldsave("test_custom_structs.jld2";
        simple_struct=simple,
        mutable_struct=mutable_struct,
        parametric_int=parametric_int,
        parametric_string=parametric_string,
        empty_struct=EmptyStruct(),
        single_field_int=SingleFieldStruct{Int}(42),
        single_field_array=SingleFieldStruct{Vector{Float64}}([1.0, 2.0]),
    )
end

function create_complex_nested_file()
    println("Creating complex_nested.jld2...")

    simple = SimpleStruct(1, 2.0, "nested")
    nested = NestedStruct(simple, [10, 20, 30], "optional_string")
    nested_with_nothing = NestedStruct(simple, [1, 2], nothing)

    parametric = ParametricStruct{Float32}(Float32(3.14), "pi")

    collection = Dict{String, Any}(
        "number" => 42,
        "text" => "value",
        "array" => [1, 2, 3],
        "nested_dict" => Dict("inner" => "value")
    )

    complex = ComplexNested(UInt64(123456), nested, parametric, collection)

    jldsave("test_complex_nested.jld2";
        nested_basic=nested,
        nested_with_nothing=nested_with_nothing,
        complex_nested=complex,

        # Unions (store actual values, not Union constructors)
        union_int=42,  # Int value that could be Union{Int, String}
        union_string="hello",  # String value that could be Union{Int, String}
        union_nothing=nothing,  # Nothing value that could be Union{Int, Nothing}

        # Tuples
        simple_tuple=(1, 2, 3),
        named_tuple=(x=1, y=2.0, name="test"),
        nested_tuple=(simple, [1, 2, 3]),

        # Symbol
        symbol_val=:test_symbol,
        symbol_array=[:a, :b, :c]
    )
end

function create_special_cases_file()
    println("Creating special_cases.jld2...")

    # Large arrays
    large_array = rand(1000, 100)

    jldsave("test_special_cases.jld2";
        # Empty collections
        empty_dict=Dict{String, Int}(),
        empty_vector=Float64[],
        empty_matrix=reshape(Float64[], 0, 0),

        # Large data
        large_array=large_array,

        # Special numeric values
        nan_val=NaN,
        inf_val=Inf,
        neg_inf_val=-Inf,

        # Extreme values
        max_int=typemax(Int64),
        min_int=typemin(Int64),
        tiny_float=nextfloat(0.0),
        huge_float=prevfloat(Inf),
    )
end

function create_attributes_heavy_file()
    println("Creating attributes_heavy.jld2...")

    # Create file with explicit dataset API to add attributes
    jldopen("test_attributes_heavy.jld2", "w") do f
        # Create datasets with various data types
        simple_data = [1, 2, 3, 4, 5]
        dset1 = JLD2.create_dataset(f, "simple_array")
        JLD2.write_dataset(dset1, simple_data)

        # Add various attribute types
        JLD2.add_attribute(dset1, "description", "This is a simple integer array")
        JLD2.add_attribute(dset1, "created_by", "dataset_show_test")
        JLD2.add_attribute(dset1, "version", 1.0)
        JLD2.add_attribute(dset1, "count", length(simple_data))
        JLD2.add_attribute(dset1, "is_sorted", true)
        JLD2.add_attribute(dset1, "tags", ["test", "array", "integer"])

        # Complex data with attributes
        struct_data = SimpleStruct(42, 3.14, "attributed")
        dset2 = JLD2.create_dataset(f, "struct_with_attrs")
        JLD2.write_dataset(dset2, struct_data)

        JLD2.add_attribute(dset2, "struct_type", "SimpleStruct")
        JLD2.add_attribute(dset2, "x_description", "Integer field")
        JLD2.add_attribute(dset2, "y_description", "Float field")
        JLD2.add_attribute(dset2, "metadata", Dict("key1" => "value1", "key2" => 42))

        # Empty dataset (unwritten)
        dset3 = JLD2.create_dataset(f, "unwritten_dataset")
        JLD2.add_attribute(dset3, "status", "unwritten")
        JLD2.add_attribute(dset3, "placeholder", true)
        # Note: don't write this dataset to test unwritten state

        # Compressed dataset
        large_data = rand(100, 100)
        dset4 = JLD2.create_dataset(f, "compressed_data")
        dset4.filters = JLD2.Deflate()
        JLD2.write_dataset(dset4, large_data)

        JLD2.add_attribute(dset4, "compression", "deflate")
        JLD2.add_attribute(dset4, "original_size", sizeof(large_data))
        JLD2.add_attribute(dset4, "dimensions", size(large_data))
    end
end

function main()
    println("Generating test files for Dataset show function testing...")

    # Change to test directory
    cd(dirname(@__FILE__))

    # Create all test files
    create_basic_types_file()
    create_custom_structs_file()
    create_complex_nested_file()
    create_special_cases_file()
    create_attributes_heavy_file()

    println("Generated test files:")
    for file in ["test_basic_types.jld2", "test_custom_structs.jld2",
                 "test_complex_nested.jld2", "test_special_cases.jld2",
                 "test_attributes_heavy.jld2"]
        if isfile(file)
            size_kb = round(filesize(file) / 1024, digits=1)
            println("  - $file ($(size_kb) KB)")
        end
    end

    println("Test file generation complete!")
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end