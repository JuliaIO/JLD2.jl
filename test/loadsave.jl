using JLD2, FileIO, Base.Test

fn = joinpath(tempdir(), "test.jld2")

# Test load macros
jldopen(fn, "w") do f
    write(f, "loadmacrotestvar1", ['a', 'b', 'c'])
    write(f, "loadmacrotestvar2", 1)
end

@eval begin # wrapped in eval since @load with no args needs file at compile time
function func1()
    @load $fn
    @test loadmacrotestvar1 == ['a', 'b', 'c']
    @test loadmacrotestvar2 == 1
end
end

func1()

function func2()
    @load fn loadmacrotestvar1 loadmacrotestvar2
    @test loadmacrotestvar1 == ['a', 'b', 'c']
    @test loadmacrotestvar2 == 1
end

func2()

@test !isdefined(:loadmacrotestvar1) # should not be in global scope
@test (@eval @load $fn) == [:loadmacrotestvar1, :loadmacrotestvar2]
@test loadmacrotestvar1 == ['a', 'b', 'c']
@test loadmacrotestvar2 == 1

# Test save macros
hello = "world"
@save fn hello
jldopen(fn, "r") do f
    @test read(f, "hello") == "world"
end

cmd = """
using JLD2
hello = "there"
@save $(repr(fn))
"""
run(`$(Base.julia_cmd()) -e $cmd`)
jldopen(fn, "r") do f
    @test read(f, "hello") == "there"
end

# Test Dict save/load
save(format"JLD2", fn, Dict("the"=>"quick", "brown"=>"fox", "stuff"=>reshape(1:4, (2, 2))))
@test load(fn) == Dict("the"=>"quick", "brown"=>"fox", "stuff"=>reshape(1:4, (2, 2)))

# Test load/save with pairs
save(format"JLD2", fn, "jumps", "over", "the", "lazy", "dog", reshape(1:4, (2, 2)))
@test load(fn, "jumps", "the", "dog") == ("over", "lazy", reshape(1:4, (2, 2)))
@test load(fn, "jumps") == "over"

jldopen(fn, "r+") do f
    @test !isempty(f)
    @test haskey(f, "dog")
    @test !haskey(f, "notdog")
    @test keys(f) == ["jumps", "the", "dog"]
    x1 = rand(UInt8, 1024^2+1024)
    x2 = rand(UInt8, 1024)
    f["x1"] = x1
    f["x2"] = x2
    @test f["x1"] == x1
    @test f["x2"] == x2
end

# Issue #19
save(fn, Dict("a"=>[1,2,3]))
io = open(fn)
info("The next error message (involving Nullable{String}) is a sign of normal operation")
@test_throws MethodError load(io)
close(io)
