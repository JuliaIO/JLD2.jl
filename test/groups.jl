using JLD2, Test

fn = joinpath(tempdir(),"test.jld")
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
