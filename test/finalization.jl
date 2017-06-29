using JLD2, Base.Test

# Make sure that the file gets written properly regardless of the order in which the
# MmapIO and JLDFile are finalized
fn = joinpath(tempdir(), "test.jld")
for order in [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]
    f = jldopen(fn, "w")
    write(f, "x", 1:10)
    objs = [f, f.io.f, f.io.arr]
    finalize(objs[order[1]])
    finalize(objs[order[2]])
    finalize(objs[order[3]])

    f = jldopen(fn, "r")
    @test read(f, "x") == 1:10
    close(f)
end

cmd = """
using JLD2
f = jldopen($(repr(fn)), "w")
write(f, "x", 1:10)
"""
run(`julia -e $cmd`)

f = jldopen(fn, "r")
@test read(f, "x") == 1:10
