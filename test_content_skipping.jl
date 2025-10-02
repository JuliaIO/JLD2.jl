"""
Test to verify that @pseudostruct skips reading content fields
when computing offsets for later fields.
"""

using JLD2
using MacroTools

println("\n" * "="^80)
println("TESTING CONTENT FIELD SKIPPING OPTIMIZATION")
println("="^80)

# Define a test pseudostruct with large content field
@eval JLD2 begin
    @pseudostruct TestContentSkip begin
        version::UInt8 = 1
        name_len::UInt16 = 1000
        name::@FixedLengthString(name_len)  # 1000 bytes of content
        after::UInt32 = 42
    end
end

println("\n📝 Defined test structure:")
println("""
@pseudostruct TestContentSkip begin
    version::UInt8 = 1
    name_len::UInt16 = 1000
    name::@FixedLengthString(name_len)  # 1000 bytes!
    after::UInt32 = 42
end
""")

# Examine the generated getproperty code
println("\n🔍 Analyzing generated code for accessing :after field...")
println("="^80)

# Get the generated method
methods_list = methods(Base.getproperty, (JLD2.HmWrap{:TestContentSkip},Symbol))
println("\nNumber of getproperty methods for TestContentSkip: ", length(methods_list))

# Try to expand the pseudostruct to see generated code
expr = quote
    @pseudostruct TestExample begin
        version::UInt8 = 1
        name_len::UInt16 = 100
        name::@FixedLengthString(name_len)
        after::UInt32 = 42
    end
end

println("\n📋 Macro expansion of similar structure:")
println("="^80)

expanded = macroexpand(JLD2, expr)

# Extract the getproperty function from the expanded code
getprop_func = nothing
for ex in expanded.args
    if ex isa Expr && ex.head == :function
        # Check if it's the getproperty function
        if ex.args[1] isa Expr && ex.args[1].head == :call
            fname = ex.args[1].args[1]
            if fname isa Expr && fname.args[end] == :getproperty
                getprop_func = ex
                break
            end
        end
    end
end

if !isnothing(getprop_func)
    println("\n✅ Found getproperty function!")
    println("\nLooking for :after field accessor:")
    println("-"^80)

    # Find the :after accessor in the function body
    function find_after_accessor(ex, depth=0)
        if ex isa Expr
            # Look for if s == :after
            if ex.head == :if && length(ex.args) >= 2
                cond = ex.args[1]
                if cond isa Expr && cond.head == :call && cond.args[1] == :(==)
                    if length(cond.args) >= 3 && cond.args[3] isa QuoteNode &&
                       cond.args[3].value == :after
                        return ex
                    end
                end
            end
            # Recurse
            for arg in ex.args
                result = find_after_accessor(arg, depth+1)
                if !isnothing(result)
                    return result
                end
            end
        end
        return nothing
    end

    after_accessor = find_after_accessor(getprop_func)

    if !isnothing(after_accessor)
        println("Found :after accessor:")
        println(after_accessor)

        # Check if it contains a String(...) call (reading name content)
        has_string_read = false
        function check_for_string_read(ex)
            if ex isa Expr
                if ex.head == :call && length(ex.args) >= 1
                    if ex.args[1] == :String
                        return true
                    end
                end
                for arg in ex.args
                    if check_for_string_read(arg)
                        return true
                    end
                end
            end
            return false
        end

        has_string_read = check_for_string_read(after_accessor)

        println("\n" * "="^80)
        if has_string_read
            println("❌ FAIL: The :after accessor contains String(...) - reading name content!")
            println("This means we're reading 100 bytes unnecessarily.")
        else
            println("✅ SUCCESS: The :after accessor does NOT contain String(...)")
            println("This means we're skipping the name content and only using name_len for offset!")
        end
        println("="^80)
    else
        println("⚠️  Could not find :after accessor in generated code")
    end
else
    println("\n⚠️  Could not extract getproperty function from macro expansion")
end

# Functional test: verify it actually works
println("\n\n🧪 FUNCTIONAL TEST")
println("="^80)

println("\nTesting compute_size function...")
try
    size1 = JLD2.compute_size(Val(:TestContentSkip), 0x0, 0x0000,
                               (version=1, name_len=100, name="x"^100, after=42))
    println("✅ compute_size with content: $size1 bytes")

    # Try without content (should work with optimization!)
    size2 = JLD2.compute_size(Val(:TestContentSkip), 0x0, 0x0000,
                               (version=1, name_len=100, after=42))
    println("✅ compute_size without content: $size2 bytes")

    if size1 == size2
        println("✅ SUCCESS: Both sizes match - content field is not required!")
    else
        println("❌ Different sizes: with=$size1, without=$size2")
    end
catch e
    if e isa ArgumentError && occursin("name", string(e))
        println("❌ FAIL: Still requires 'name' argument (content field not skipped)")
        println("Error: $e")
    else
        println("⚠️  Unexpected error: $e")
        rethrow()
    end
end

println("\n" * "="^80)
println("TEST COMPLETE")
println("="^80)
