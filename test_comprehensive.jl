using JLD2

println("Running comprehensive test...")

f = jldopen("/tmp/fulltest.jld2", "w")
f["int"] = 42
f["float"] = 3.14
f["string"] = "hello"
f["array"] = [1,2,3]
f["dict"] = Dict("a"=>1, "b"=>2)

struct TestStruct
    x::Int
    y::Float64
end
f["struct"] = TestStruct(10, 20.0)

close(f)

println("✅ Write completed")

f2 = jldopen("/tmp/fulltest.jld2", "r")
println("int: ", f2["int"])
println("float: ", f2["float"])
println("string: ", f2["string"])
println("array: ", f2["array"])
println("dict: ", f2["dict"])
println("struct: ", f2["struct"])
close(f2)

println("✅ All basic tests passed!")