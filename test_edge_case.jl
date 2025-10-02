using JLD2

# Try to reproduce the error from the test suite
println("Testing edge case with CommittedDatatype...")

try
    f = jldopen("/tmp/test_edge.jld2", "w")

    # Try writing something that might trigger CommittedDatatype
    # Based on the stack trace, it seems to involve write_dataset with CommittedDatatype

    # Create a custom type that might trigger this
    struct CustomType
        x::Int
        y::Float64
    end

    f["custom"] = CustomType(1, 2.0)
    close(f)

    println("✅ Write succeeded")

    f2 = jldopen("/tmp/test_edge.jld2", "r")
    result = f2["custom"]
    println("✅ Read succeeded: $result")
    close(f2)

catch e
    println("❌ Error: $e")
    for (exc, bt) in Base.catch_stack()
        showerror(stdout, exc, bt)
        println()
    end
end