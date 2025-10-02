"""
Examine code generated for an existing pseudostruct.
"""

using JLD2
using InteractiveUtils

println("\n" * "="^80)
println("EXAMINING HmLinkMessage GENERATED CODE")
println("="^80)

println("\n📋 HmLinkMessage structure (simplified):")
println("""
version::UInt8 = 1
flags::UInt8 = 0x18
isset(flags, 3) && link_type::UInt8
isset(flags, 2) && creation_order::Int64
isset(flags, 4) && link_name_charset::UInt8
link_name_len::@Int(2^(flags%4))
link_name::@FixedLengthString(link_name_len)  # <-- Large content field
target::RelOffset
""")

# Get the generated getproperty method
methods_list = methods(Base.getproperty, (JLD2.HmWrap{JLD2.HmLinkMessage},Symbol))

println("\n🔍 Found $(length(methods_list)) getproperty method(s)")

if length(methods_list) > 0
    m = first(methods_list)
    println("\n📝 Code info:")

    # Get code_lowered
    ci = @code_lowered Base.getproperty(JLD2.HmWrap{JLD2.HmLinkMessage, JLD2.MmapIO}(
        JLD2.Message{JLD2.MmapIO}(0, JLD2.RelOffset(0), JLD2.MmapIO(UInt8[], "", false, false, 0)),
        0x00, 0x0000), :target)

    println(ci)

    # Search for string reads in the code
    code_str = string(ci)

    has_string = occursin("String", code_str)
    has_jlread_array = occursin(r"jlread\(.*, UInt8,", code_str)

    println("\n" * "="^80)
    println("ANALYSIS OF :target ACCESSOR:")
    println("  - Contains String conversions: ", has_string ? "❌ YES (reads link_name content)" : "✅ NO")
    println("  - Contains array jlread: ", has_jlread_array ? "❌ YES (reads content)" : "✅ NO")
    println("="^80)

    if has_string || has_jlread_array
        println("\n❌ PROBLEM CONFIRMED")
        println("When accessing :target field (after link_name), the code reads link_name content")
        println("This is wasteful - we only need link_name_len for offset calculation")
    else
        println("\n✅ OPTIMIZATION WORKING")
        println("Content fields are being skipped correctly")
    end
else
    println("❌ No methods found")
end
