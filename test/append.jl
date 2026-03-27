using JLD2, Test

fn = joinpath(mktempdir(), "test.jld")
f = jldopen(fn, "w")
write(f, "x1", 1.0)
close(f)

f = jldopen(fn, "a")
write(f, "x2", 1.0+2.0im)
close(f)

struct AppendedStruct
    x::Int
end
f = jldopen(fn, "a")
write(f, "x3", AppendedStruct(9))
close(f)

f = jldopen(fn, "r")
@test read(f, "x1") === 1.0
@test read(f, "x2") === 1.0+2.0im
@test read(f, "x3") === AppendedStruct(9)
close(f)

# IOStream: continuation chunk on group overflow
# est_num_entries=1 leaves no spare space, so the first append forces a
# continuation chunk.  A bug caused link messages to be written to the raw
# IOStream instead of the BufferedWriter used for checksum tracking.
fn2 = joinpath(mktempdir(), "test_iostream_continuation.jld2")
jldopen(fn2, "w"; iotype=IOStream) do f
    g = JLD2.Group(f, "g"; est_num_entries=1)
    g["a"] = 1
end
jldopen(fn2, "a"; iotype=IOStream) do f
    f["g/b"] = 2
end
jldopen(fn2, "r"; iotype=IOStream) do f
    @test f["g/a"] == 1
    @test f["g/b"] == 2
end
