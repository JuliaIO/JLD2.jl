using JLD2, Test

fn = joinpath(mktempdir(), "test.jld")
f = jldopen(fn, "w")
write(f, "/test_group_1/x1", 1)
@test !isempty(read(f, "test_group_1"))
write(f, "test_group_1/x2", 2)
write(f, "/test_group_2/x1", 3)
write(f, "test_group_3/x1", 4)
read(f, "test_group_3")["contained_group/x1"] = 5
JLD2.Group(f, "test_group_4")["x1"] = 6
JLD2.Group(read(f, "test_group_4"), "contained_group")["x1"] = 7
@test isempty(JLD2.Group(f, "empty_group"))
JLD2.Group(f, "group_with_one_group_child/empty_group")
@test !isempty(read(f, "group_with_one_group_child"))
@test_throws ArgumentError JLD2.Group(f, "test_group_4")
@test_throws ArgumentError write(f, "test_group_4/contained_group", 0)
@test !haskey(f, "path_that_does_not_exist")
@test !haskey(f, "/test_group_1/x1/path/that/does/not/exist")
@test !haskey(f["test_group_1"], "/test_group_1/x1/path/that/does/not/exist")
@test !haskey(f["test_group_1"], "x1/path/that/does/not/exist")
@test haskey(f, "/test_group_1/x1")
close(f)
f = jldopen(fn, "a")
@test haskey(f, "test_group_1")
@test haskey(f, "test_group_1/x1")

@test get(f, "path_that_does_not_exist", 2.0) == 2.0
@test get!(f, "path_created_by_get!", "x") == "x"
@test f["path_created_by_get!"] == "x"
@test get(JLD2.Group(f, "test_group_5"), "path_that_does_not_exist", 3.0) == 3.0
@test get!(f["test_group_5"], "path_created_by_get!", 4.0) == 4.0
@test f["test_group_5"]["path_created_by_get!"] == 4.0

# Test that the default element constructor is called only when necessary
# First, we'll access a group which *does* exist, so the default constructor is not called:
@test get(() -> error("this should not be called because the key exists"), f["test_group_5"], "path_created_by_get!") == 4.0

# Then we'll access a few keys that do not exist, and verify that the default
# constructor was called:
default_called = false
get(f, "path_that_does_not_exist") do
    global default_called
    default_called = true
end
@test default_called

default_called = false
get!(JLD2.Group(f, "test_group_6"), "path_created_by_get!") do
    global default_called
    default_called = true
end
@test default_called
close(f)

f = jldopen(fn, "r")
@test read(f, "/test_group_1/x1") == 1
@test read(f, "/test_group_1/x2") == 2
@test read(f, "/test_group_2/x1") == 3
@test read(f, "/test_group_3/x1") == 4
@test read(f, "/test_group_3/contained_group/x1") == 5
@test_throws KeyError read(f, "/test_group_3/contained_group/x2")
@test_throws KeyError read(f, "/test_group_3/contained_group/x2/oops")

@test read(f, "test_group_1/x1") == 1
@test read(f, "test_group_1/x2") == 2
@test read(f, "test_group_2/x1") == 3
@test read(f, "test_group_3/x1") == 4
@test read(f, "test_group_3/contained_group/x1") == 5

@test read(f, "test_group_1")["x1"]  == 1
@test read(f, "test_group_1")["x2"]  == 2
@test read(f, "test_group_2")["x1"]  == 3
@test read(f, "test_group_3")["x1"]  == 4
@test read(f, "test_group_3")["contained_group"]["x1"]  == 5

@test read(f, "test_group_1")["/test_group_2/x1"]  == 3
@test read(f, "test_group_3/contained_group")["x1"]  == 5
@test read(f, "test_group_3")["contained_group/x1"]  == 5

@test !isempty(read(f, "group_with_one_group_child"))
@test !isempty(read(f, "test_group_1"))
@test isempty(read(f, "empty_group"))
@test isempty(read(f, "group_with_one_group_child/empty_group"))

@test get(f, "path_that_does_not_exist", 1.0) == 1.0
@test get(f["test_group_5"], "path_that_does_not_exist", 1.0) == 1.0
@test f["test_group_5"]["path_created_by_get!"] == 4.0

# Throws an ArgumentError because the element doesn't exist and cannot
# be created for a read-only file:
@test_throws ArgumentError get!(f, "path_that_does_not_exist", 1.0)
@test_throws ArgumentError get!(f["test_group_5"], "path_that_does_not_exist", 1.0)
# Does not throw an ArgumentError because the element does exist:
@test get!(f["test_group_1"], "x1", 2) == 1

# Make sure printing doesn't error
show(IOBuffer(), f)
close(f)


# Test for non-default group size
@testset "Custom estimated Group size" begin
    fn = joinpath(mktempdir(), "test.jld2")

    # It is difficult to test the behaviour 
    # of non-default group allocation size
    # as results should always be independent of this.
    # Test against regressions by comparing to current number of allocated bytes.
    # This may need to be updated if the relevant code sections are changed.
    # Also test for persistence of the settings when loading a file again.

    # An empty group leaves has 151 bytes by default
    jldopen(fn, "w") do f
        JLD2.Group(f, "g")
    end
    jldopen(fn, "r") do f
        g = f["g"]
        chunk_length = g.last_chunk_checksum_offset - g.last_chunk_start_offset
        @test chunk_length == 151
    end

    # An empty group with non-default size allocates more space
    jldopen(fn, "w") do f
        g = JLD2.Group(f, "g"; est_num_entries=42, est_link_name_len=21)
        @test g.est_link_name_len == 21
        @test g.est_num_entries == 42
    end
    jldopen(fn, "r") do f
        g = f["g"]
        @test g.est_link_name_len == 21
        @test g.est_num_entries == 42
        chunk_length = g.last_chunk_checksum_offset - g.last_chunk_start_offset
        @test chunk_length == 1614
    end
end


# Test for Issue #307
@testset "Repeatedly appending to file" begin
    fn = joinpath(mktempdir(), "test.jld2")   
    for n = 1:25
        jldopen(fn, "a") do fid
            group = JLD2.Group(fid, string(n))
            group["result"] = 1
            @test keys(fid) == string.(1:n)
        end
    end
end


# Test for Base.keytype
@testset "Base.keytype" begin
    fn = joinpath(mktempdir(), "test.jld2")
    jldopen(fn, "w") do f
        @test Base.keytype(f) == String
        @test Base.keytype(f.root_group) == String
    end
end

# Test show method
@testset "Show a group / file" begin
    fn = joinpath(mktempdir(), "test.jld2")   
    f = jldopen(fn, "w")
    f["a"] = 1
    f["b"] = 2

    filestr = let
        io = IOBuffer()
        show(io, f)
        String(take!(io))
    end
    @test !(match(r"^JLDFile.*\(read/write\)$", filestr) === nothing)

    groupstr = let
        io = IOBuffer()
        show(io, f.root_group)
        String(take!(io))
    end
    @test groupstr == "JLD2.Group"


    groupstr = let
        io = IOBuffer()
        show(io, MIME("text/plain"), f.root_group)
        String(take!(io))
    end
    @test groupstr == "JLD2.Group\n ├─🔢 a\n └─🔢 b"

    close(f)      
end
