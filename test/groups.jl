using JLD2, Base.Test

fn = joinpath(tempdir(),"test.jld")
f = jldopen(fn, "w")
write(f, "/test_group_1/x1", 1)
write(f, "test_group_1/x2", 2)
write(f, "/test_group_2/x1", 3)
write(f, "test_group_3/x1", 4)
read(f, "test_group_3")["contained_group/x1"] = 5
close(f)

f = jldopen(fn, "r")
@test read(f, "/test_group_1/x1") == 1
@test read(f, "/test_group_1/x2") == 2
@test read(f, "/test_group_2/x1") == 3
@test read(f, "/test_group_3/x1") == 4
@test read(f, "/test_group_3/contained_group/x1") == 5
@test_throws KeyError read(f, "/test_group_3/contained_group/x2")

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
close(f)
