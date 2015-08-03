using JLD2

type TestType1
    x::Int
end
type TestType2
    x::Int
end
immutable TestType3
    x::TestType2
end

type TestType4
    x::Int
end
type TestType5
    x::TestType4
end
type TestType6 end
bitstype 8 TestType7
immutable TestType8
    a::TestType4
    b::TestType5
    c::TestType6
    d::TestType7
end

immutable TestTypeContainer{T}
    a::T
    b::Int
end

fn = joinpath(tempdir(),"test.jld")
file = jldopen(fn, "w")
write(file, "x1", TestType1(57))
write(file, "x2", TestType3(TestType2(1)))
write(file, "x3", TestType4(1))
write(file, "x4", TestType5(TestType4(2)))
write(file, "x5", [TestType5(TestType4(i)) for i = 1:5])
write(file, "x6", TestType6())
write(file, "x7", reinterpret(TestType7, 0x77))
write(file, "x8", TestType8(TestType4(2), TestType5(TestType4(3)),
                            TestType6(), reinterpret(TestType7, 0x12)))
write(file, "x9", (TestType4(1),
                   (TestType5(TestType4(2)),
                    [TestType5(TestType4(i)) for i = 1:5]),
                   TestType6()))
write(file, "x10", TestTypeContainer(TestType4(3), 4))
close(file)

workspace()
using LastMain.JLD2, Base.Test
type TestType1
    x::Float64
end
type TestType2
    x::Int
end
immutable TestType3
    x::TestType1
end
immutable TestTypeContainer{T}
    a::T
    b::Int
end

file = jldopen(LastMain.fn, "r")
x = read(file, "x1")
@test isa(x, TestType1)
@test x.x === 57.0
@test_throws MethodError read(file, "x2")
println("The following missing type warnings are a sign of normal operation.")
@test read(file, "x3").x == 1
@test read(file, "x4").x.x == 2

x = read(file, "x5")
for i = 1:5
    @test x[i].x.x == i
end
@test isempty(fieldnames(typeof(read(file, "x6"))))
@test reinterpret(UInt8, read(file, "x7")) == 0x77

x = read(file, "x8")
@test x.a.x == 2
@test x.b.x.x == 3
@test isempty(fieldnames(typeof(x.c)))
@test reinterpret(UInt8, x.d) == 0x12

x = read(file, "x9")
@test isa(x, Tuple)
@test length(x) == 3
@test x[1].x == 1
@test isa(x[2], Tuple)
@test length(x[2]) == 2
@test x[2][1].x.x == 2
for i = 1:5
    @test x[2][2][i].x.x == i
end
@test isempty(fieldnames(typeof(x[3])))

x = read(file, "x10")
@test isa(x, TestTypeContainer)
@test x.a.x === 3
@test x.b === 4
close(file)
