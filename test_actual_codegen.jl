"""
Test actual code generation by defining a struct and examining it.
"""

using JLD2
using InteractiveUtils

println("\n" * "="^80)
println("EXAMINING ACTUAL GENERATED CODE")
println("="^80)

# Define the test struct
@eval JLD2 begin
    @pseudostruct TestContentAnalysis begin
        version::UInt8 = 1
        name_len::UInt16 = 100
        name::@FixedLengthString(name_len)
        data_size::UInt32 = 1000
        data::@Blob(data_size)
        after::UInt32 = 42
    end
end

println("\n✅ Defined TestContentAnalysis")

println("\n📋 Structure:")
println("""
version::UInt8
name_len::UInt16
name::@FixedLengthString(name_len)  # Content: 100 bytes
data_size::UInt32
data::@Blob(data_size)              # Content: 1000 bytes
after::UInt32                        # Target field
""")

# Get the generated method
methods_list = methods(Base.getproperty, (JLD2.HmWrap{:TestContentAnalysis},Symbol))

println("\n🔍 Found $(length(methods_list)) getproperty methods")

if length(methods_list) > 0
    m = first(methods_list)
    println("\n📝 Lowered code:")
    println("="^80)

    code = Base.uncompressed_ir(m.source)
    println(code)

    # Count how many jlread calls there are
    code_str = string(code)
    jlread_count = count("jlread", code_str)
    string_count = count("String", code_str)

    println("\n" * "="^80)
    println("ANALYSIS:")
    println("  - Total jlread calls in generated code: $jlread_count")
    println("  - Total String conversions: $string_count")

    if string_count >= 1
        println("\n❌ Code contains String conversions")
        println("   This suggests name content is being read")
    else
        println("\n✅ No String conversions found")
        println("   Content fields may be skipped correctly")
    end
else
    println("❌ No methods found")
end
