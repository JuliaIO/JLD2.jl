using JLD2
using MacroTools

# Test @pseudostruct with various patterns

# Simple struct with basic types
@pseudostruct TestSimple begin
    version::UInt8 = 1
    flags::UInt8 = 0
    count::UInt32
end

# Struct with conditional fields
@pseudostruct TestConditional begin
    version::UInt8 = 2
    flags::UInt8 = 0
    isset(flags, 0) && max_value::Int64
    isset(flags, 1) && min_value::Int64
end

# Struct with special field types
@pseudostruct TestSpecial begin
    version::UInt8 = 1
    name_len::UInt16
    name::@FixedLengthString(name_len)
    data_size::UInt32
    data::@Blob(data_size)
    offset::@Offset
    computed_value::@computed(version + 1)
end

# Struct with nested conditions
@pseudostruct TestNested begin
    version::UInt8 = 3
    if version == 3
        flags::UInt8 = 0
        isset(flags, 0) && extra::UInt32
    end
    if version == 2
        old_flags::UInt16
    end
end

println("Testing code generation...")

# Inspect generated methods
println("\n=== Methods for TestSimple ===")
println(methods(JLD2.jlwrite, (Val{:TestSimple}, Any, Any, Any)))
println(methods(JLD2.compute_size, (Val{:TestSimple}, Any, Any, Any)))

# Try to get code_lowered to see generated code
println("\n=== Lowered code for TestSimple jlwrite ===")
m = first(methods(JLD2.jlwrite, (Val{:TestSimple}, Any, Any, Any)))
println(code_lowered(m))

println("\n=== Lowered code for TestSimple compute_size ===")
m = first(methods(JLD2.compute_size, (Val{:TestSimple}, Any, Any, Any)))
println(code_lowered(m))