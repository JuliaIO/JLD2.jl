using Test, JLD2

#=
Generic IO Testing Framework for JLD2

This file provides a unified testing framework for all IO types supported by JLD2.
Instead of duplicating tests for each IO type (Vector{UInt8}, IOBuffer, etc.),
we define tests once and run them against all IO types.

## Adding a New IO Type

To test a new IO type, create a helper struct and implement the interface:

```julia
struct MyIOHelper <: IOTestHelper end

# Required methods:
create_io(::MyIOHelper) = MyCustomIO()              # Create fresh instance
get_bytes(::MyIOHelper, io) = extract_bytes(io)     # Get raw bytes
reset_for_reading!(::MyIOHelper, io) = prepare(io)  # Prepare for reading
supports_take!(::MyIOHelper) = false                # Does it support take!()?
io_name(::MyIOHelper) = "MyCustomIO"                # Display name

# Then add to the test suite:
run_generic_io_tests(MyIOHelper())
```

This automatically runs all generic tests for the new IO type!
=#

"""
    IOTestHelper

Abstract type for IO test helpers. Each concrete type provides methods to:
- Create a fresh IO instance
- Extract raw bytes (for disk compatibility tests)
- Reset for reading
"""
abstract type IOTestHelper end

"""
Helper for testing Vector{UInt8} IO
"""
struct ByteVectorHelper <: IOTestHelper end

create_io(::ByteVectorHelper) = UInt8[]
get_bytes(::ByteVectorHelper, io) = io
reset_for_reading!(::ByteVectorHelper, io) = nothing  # No-op, Vector is always ready
supports_take!(::ByteVectorHelper) = false
io_name(::ByteVectorHelper) = "Vector{UInt8}"

"""
Helper for testing IOBuffer
"""
struct IOBufferHelper <: IOTestHelper end

create_io(::IOBufferHelper) = IOBuffer()
get_bytes(::IOBufferHelper, io) = take!(io)
function reset_for_reading!(::IOBufferHelper, io)
    seekstart(io)
end
supports_take!(::IOBufferHelper) = true
io_name(::IOBufferHelper) = "IOBuffer"

"""
Helper for testing IOStream (temporary files)
"""
struct IOStreamHelper <: IOTestHelper end

function create_io(::IOStreamHelper)
    path = tempname()
    # We open in "w+" mode (read/write/create/truncate)
    # We need to track the file path to clean it up later, but IOTestHelper 
    # interface assumes just returning the IO. 
    # Since these are unit tests, we'll rely on OS/Julia temp cleanup or 
    # explicitly delete in the test runner if we changed the interface.
    # For now, let's just return the stream.
    return open(path, "w+")
end

function get_bytes(::IOStreamHelper, io)
    flush(io)
    path = io.name
    # Make sure we preserve position
    pos = position(io)
    seekstart(io)
    data = read(io)
    seek(io, pos)
    # If file was deleted, this might fail, but tests usually keep it open
    return data
end

function reset_for_reading!(::IOStreamHelper, io)
    seekstart(io)
end

supports_take!(::IOStreamHelper) = false
io_name(::IOStreamHelper) = "IOStream"

"""
    run_generic_io_tests(helper::IOTestHelper)

    Run a comprehensive suite of tests for an IO type.
"""
function run_generic_io_tests(helper::IOTestHelper)
    name = io_name(helper)

    # Helper to cleanup IO if it's an IOStream/file based
    function cleanup_io(io)
        if io isa IOStream
            path = io.name[7:end-1] # format is <file path>
            close(io)
            # Try to delete file if it exists
            try
                rm(path, force=true)
            catch
            end
        elseif io isa IO
            close(io)
        end
        # Arrays/Vectors don't need closing
    end

    @testset "$name - Basic read/write" begin
        io = create_io(helper)

        # Write data
        f = jldopen(io, "w")
        f["a"] = 42
        f["b"] = "hello"
        f["c"] = [1.0, 2.0, 3.0]
        close(f)

        # Read data back
        reset_for_reading!(helper, io)
        f = jldopen(io, "r")
        @test f["a"] == 42
        @test f["b"] == "hello"
        @test f["c"] == [1.0, 2.0, 3.0]
        close(f)
        
        cleanup_io(io)
    end

    @testset "$name - Append mode" begin
        io = create_io(helper)

        # Create initial file
        jldopen(io, "w") do f
            f["a"] = 1
        end

        # Append to existing file
        reset_for_reading!(helper, io)
        jldopen(io, "r+") do f
            @test f["a"] == 1
            f["b"] = 2
        end

        # Verify both values exist
        reset_for_reading!(helper, io)
        jldopen(io, "r") do f
            @test f["a"] == 1
            @test f["b"] == 2
        end
        
        cleanup_io(io)
    end

    @testset "$name - Complex types" begin
        io = create_io(helper)

        # Use a NamedTuple as a complex type
        original = (x=42, y="test", z=[1, 2, 3])

        jldopen(io, "w") do f
            f["custom"] = original
        end

        reset_for_reading!(helper, io)
        jldopen(io, "r") do f
            loaded = f["custom"]
            @test loaded == original
        end
        
        cleanup_io(io)
    end

    @testset "$name - Large data" begin
        io = create_io(helper)

        # Create large arrays
        large_array = rand(1000, 1000)

        jldopen(io, "w") do f
            f["large"] = large_array
        end

        reset_for_reading!(helper, io)
        jldopen(io, "r") do f
            loaded = f["large"]
            @test loaded == large_array
        end
        
        cleanup_io(io)
    end

    @testset "$name - Groups" begin
        io = create_io(helper)

        jldopen(io, "w") do f
            g = JLD2.Group(f, "mygroup")
            g["a"] = 1
            g["b"] = 2
        end

        reset_for_reading!(helper, io)
        jldopen(io, "r") do f
            @test f["mygroup/a"] == 1
            @test f["mygroup/b"] == 2
        end
        
        cleanup_io(io)
    end

    @testset "$name - Compression" begin
        io_plain = create_io(helper)
        io_compressed = create_io(helper)

        test_data = rand(100, 100)

        # Write without compression
        jldopen(io_plain, "w") do f
            f["data"] = test_data
        end

        # Write with compression
        jldopen(io_compressed, "w"; compress=true) do f
            f["data"] = test_data
        end

        # Both should read correctly
        reset_for_reading!(helper, io_plain)
        jldopen(io_plain, "r") do f
            @test f["data"] == test_data
        end

        reset_for_reading!(helper, io_compressed)
        jldopen(io_compressed, "r") do f
            @test f["data"] == test_data
        end
        
        cleanup_io(io_plain)
        cleanup_io(io_compressed)
    end

    @testset "$name - Truncate mode" begin
        io = create_io(helper)

        # Write initial data
        jldopen(io, "w") do f
            f["a"] = 1
        end

        # Overwrite with truncate
        reset_for_reading!(helper, io)
        jldopen(io, "w") do f
            f["b"] = 2
        end

        # Verify old data is gone and new data exists
        reset_for_reading!(helper, io)
        jldopen(io, "r") do f
            @test !haskey(f, "a")
            @test f["b"] == 2
        end
        
        cleanup_io(io)
    end

    @testset "$name - Compatibility with file-based IO" begin
        io = create_io(helper)

        test_value = [1, 2, 3, 4, 5]

        jldopen(io, "w") do f
            f["test"] = test_value
        end

        # Get bytes and write to disk
        bytes = get_bytes(helper, io)

        mktempdir() do dir
            filepath = joinpath(dir, "test.jld2")
            write(filepath, bytes)

            # Read from disk file
            jldopen(filepath, "r") do f
                @test f["test"] == test_value
            end

            # Read disk file into bytes and verify
            disk_data = read(filepath)
            @test disk_data == bytes
        end
        
        cleanup_io(io)
    end
end

# Run tests for all IO types
@testset "Generic IO Tests" begin
    @testset "All IO Types" begin
        run_generic_io_tests(ByteVectorHelper())
        run_generic_io_tests(IOBufferHelper())
        run_generic_io_tests(IOStreamHelper())

        # Adding a new IO type is easy! Just create a helper and add it here:
        # run_generic_io_tests(MyNewIOHelper())
    end

    @testset "IO Capability Validation" begin
        # Test that isreadable and iswritable are properly checked
        # These tests exercise the _isreadable and _iswritable checks in _wrap_io

        # Create a non-readable IO type by wrapping an IOBuffer
        struct NonReadableIO <: IO
            buf::IOBuffer
        end
        NonReadableIO() = NonReadableIO(IOBuffer())
        Base.isreadable(::NonReadableIO) = false
        Base.iswritable(::NonReadableIO) = true
        seekable(::NonReadableIO) = true
        for f in (:position, :seek, :seekend, :seekstart, :write, :read, :eof, :bytesavailable)
            @eval Base.$f(io::NonReadableIO, args...) = $f(io.buf, args...)
        end

        # Create a non-writable IO type by wrapping an IOBuffer
        struct NonWritableIO <: IO
            buf::IOBuffer
        end
        NonWritableIO(data::Vector{UInt8}) = NonWritableIO(IOBuffer(data))
        Base.isreadable(::NonWritableIO) = true
        Base.iswritable(::NonWritableIO) = false
        isseekable(::NonWritableIO) = true
        for f in (:position, :seek, :seekend, :seekstart, :write, :read, :eof, :bytesavailable)
            @eval Base.$f(io::NonWritableIO, args...) = $f(io.buf, args...)
        end

        # Test that non-readable IO throws an error
        @test_throws ArgumentError("IO object is not readable") begin
            io = NonReadableIO()
            jldopen(io, "r")
        end

        # Test that non-writable IO throws an error when opened in writable mode
        @test_throws ArgumentError("IO object is not writable") begin
            io = NonWritableIO(UInt8[])
            jldopen(io, "w")
        end
    end
end
