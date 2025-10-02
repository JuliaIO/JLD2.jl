using MacroTools
include("src/macros_utils.jl")

# Helper functions
isset(flags, bit) = (flags & (1 << bit)) != 0

println("=" ^ 80)
println("ANALYSIS: compute_size requirements")
println("=" ^ 80)

# Example 1: Field with fixed size (not used in conditions)
ex1 = @macroexpand @pseudostruct SizeExample1 begin
    version::UInt8 = 1
    flags::UInt8 = 0
    name_length::UInt16
    name::@FixedLengthString(name_length)
end

println("\nExample 1: Fixed-size fields + variable-length field")
println("Generated compute_size:")
for arg in ex1.args
    if arg isa Expr && arg.head == :function
        if arg.args[1] isa Expr && arg.args[1].args[1] == :compute_size
            println(MacroTools.prettify(arg))
        end
    end
end

# Example 2: Conditional fields
ex2 = @macroexpand @pseudostruct SizeExample2 begin
    version::UInt8 = 1
    flags::UInt8 = 0
    isset(flags, 0) && max_value::Int64
    isset(flags, 1) && min_value::Int64
end

println("\n" * "=" ^ 80)
println("Example 2: Conditional fields based on flags")
println("Generated compute_size:")
for arg in ex2.args
    if arg isa Expr && arg.head == :function
        if arg.args[1] isa Expr && arg.args[1].args[1] == :compute_size
            println(MacroTools.prettify(arg))
        end
    end
end

println("\n" * "=" ^ 80)
println("ANALYSIS SUMMARY:")
println("=" ^ 80)
println("""
Fields needed for size computation:
1. Fields used in conditions (e.g., 'flags' to determine if optional fields are present)
2. Fields used to determine variable-length sizes (e.g., 'name_length' for @FixedLengthString)

Fields NOT needed for size computation:
1. Fixed-size fields that are not used in conditions
2. The content of variable-length fields (only their size is needed)

Current problem:
- compute_size requires ALL fields as keyword arguments
- Even fixed-size fields like 'version::UInt8 = 1' require haskey check
- Even variable-length content fields require the value (when only size is needed)
""")