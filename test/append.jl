using JLD2, Base.Test

fn = joinpath(tempdir(),"test.jld")
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
