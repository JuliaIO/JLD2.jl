using MacroTools

# Load the macro definition
include("src/macros_utils.jl")

# Test simple case
macro_expr = quote
    @pseudostruct TestSimple begin
        version::UInt8 = 1
        flags::UInt8 = 0
        count::UInt32
    end
end

println("="^80)
println("SIMPLE STRUCT EXPANSION")
println("="^80)
expanded = macroexpand(Main, macro_expr)
println(MacroTools.prettify(expanded))

# Test conditional fields
macro_expr2 = quote
    @pseudostruct TestConditional begin
        version::UInt8 = 2
        flags::UInt8 = 0
        isset(flags, 0) && max_value::Int64
        isset(flags, 1) && min_value::Int64
    end
end

println("\n" * "="^80)
println("CONDITIONAL STRUCT EXPANSION")
println("="^80)
expanded2 = macroexpand(Main, macro_expr2)
println(MacroTools.prettify(expanded2))

# Test special types
macro_expr3 = quote
    @pseudostruct TestSpecial begin
        version::UInt8 = 1
        name_len::UInt16
        name::@FixedLengthString(name_len)
        data_size::UInt32
        data::@Blob(data_size)
    end
end

println("\n" * "="^80)
println("SPECIAL TYPES STRUCT EXPANSION")
println("="^80)
expanded3 = macroexpand(Main, macro_expr3)
println(MacroTools.prettify(expanded3))

# Test nested conditions
macro_expr4 = quote
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
end

println("\n" * "="^80)
println("NESTED CONDITIONS STRUCT EXPANSION")
println("="^80)
expanded4 = macroexpand(Main, macro_expr4)
println(MacroTools.prettify(expanded4))