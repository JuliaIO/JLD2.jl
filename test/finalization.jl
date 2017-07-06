using JLD2, Base.Test

# Make sure that the file gets written properly regardless of the order in which the
# IO and JLDFile are finalized
fn = joinpath(tempdir(), "test.jld")

f = jldopen(fn, "w")
close(f)

for order in [[1, 2], [2, 1]]
    f = jldopen(fn, "w")
    write(f, "x", 1:10)

    objs = isa(f, JLD2.JLDFile{IOStream}) ? [f, f.io] : [f, f.io.f]
    for i in order
        finalize(objs[i])
    end

    f = jldopen(fn, "r")
    @test read(f, "x") == 1:10
    close(f)
end

cmd = """
using JLD2
f = jldopen($(repr(fn)), "w")
write(f, "x", 1:10)
"""
run(`$(Base.julia_cmd()) -e $cmd`)

f = jldopen(fn, "r")
@test read(f, "x") == 1:10
close(f)
