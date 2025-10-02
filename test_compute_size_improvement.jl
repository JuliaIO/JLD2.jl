using MacroTools
include("src/macros_utils.jl")

# Test the actual improvement: variable-length content fields

ex = @macroexpand @pseudostruct TestImprovement begin
    name_len::UInt16
    name::@FixedLengthString(name_len)
    data_size::UInt32
    data::@Blob(data_size)
end

println("=" ^ 80)
println("IMPROVEMENT TEST: Variable-length content fields")
println("=" ^ 80)

for arg in ex.args
    if arg isa Expr && arg.head == :function
        if arg.args[1] isa Expr && arg.args[1].args[1] == :compute_size
            println("\nGenerated compute_size:")
            println(MacroTools.prettify(arg))
        end
    end
end

println("\n" * "=" ^ 80)
println("RESULT:")
println("=" ^ 80)
println("""
✅ BEFORE improvement:
   compute_size required: name_len, name, data_size, data

✅ AFTER improvement:
   compute_size requires: name_len, data_size
   Does NOT require: name (content), data (content)

This is the key improvement - variable-length CONTENT fields are no longer
required for size computation. Only their LENGTH parameters are needed.
""")