using JLD2, Base.Test

fn = joinpath(tempdir(),"test.jld")
f = jldopen(fn, "w")
write(f, "x1", 1.0+2.0im)
close(f)

f = jldopen(fn, "a")
write(f, "x2", 3.0+4.0im)
close(f)

f = jldopen(fn, "r")
@test read(f, "x1") === 1.0+2.0im
@test read(f, "x2") === 3.0+4.0im
close(f)
f = open(fn)
data = read(f, UInt8, filesize(fn))
close(f)

f = jldopen(fn, "w")
write(f, "x1", 1.0+2.0im)
write(f, "x2", 3.0+4.0im)
close(f)

f = open(fn)
@test read(f, UInt8, filesize(fn)) == data
close(f)
