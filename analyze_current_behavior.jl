"""
Analyze current behavior: are content fields being read unnecessarily?
"""

using JLD2
using MacroTools

println("\n" * "="^80)
println("ANALYZING CURRENT @pseudostruct BEHAVIOR")
println("="^80)

# Expand a simple example to see what code is generated
expr = quote
    @pseudostruct TestAnalyze begin
        version::UInt8 = 1
        name_len::UInt16 = 100
        name::@FixedLengthString(name_len)
        data_size::UInt32 = 1000
        data::@Blob(data_size)
        after::UInt32 = 42
    end
end

println("\n📋 Test structure:")
println("""
@pseudostruct TestAnalyze begin
    version::UInt8
    name_len::UInt16
    name::@FixedLengthString(name_len)  # 100 bytes
    data_size::UInt32
    data::@Blob(data_size)              # 1000 bytes
    after::UInt32                        # <-- What we want to access
end
""")

println("\n🔍 Expanding macro...")
expanded = macroexpand(JLD2, expr)

# Find the getproperty function
getprop_func = nothing
for ex in expanded.args
    if ex isa Expr && ex.head == :function
        if ex.args[1] isa Expr && ex.args[1].head == :call
            fname = ex.args[1].args[1]
            if fname isa Expr && length(fname.args) > 0 && fname.args[end] == :getproperty
                getprop_func = ex
                break
            end
        end
    end
end

if isnothing(getprop_func)
    println("❌ Could not find getproperty function in expansion")
    exit(1)
end

println("✅ Found getproperty function")

# Extract the :after accessor
function find_accessor(ex, target_symbol)
    if ex isa Expr
        if ex.head == :if && length(ex.args) >= 2
            cond = ex.args[1]
            if cond isa Expr && cond.head == :call && cond.args[1] == :(==)
                if length(cond.args) >= 3 && cond.args[3] isa QuoteNode &&
                   cond.args[3].value == target_symbol
                    return ex
                end
            end
        end
        for arg in ex.args
            result = find_accessor(arg, target_symbol)
            if !isnothing(result)
                return result
            end
        end
    end
    return nothing
end

after_accessor = find_accessor(getprop_func, :after)

if isnothing(after_accessor)
    println("❌ Could not find :after accessor")
    exit(1)
end

println("\n" * "="^80)
println("ACCESSOR FOR :after FIELD")
println("="^80)
println(after_accessor)

# Analyze what it reads
println("\n" * "="^80)
println("ANALYSIS")
println("="^80)

function contains_string_read(ex)
    if ex isa Expr
        if ex.head == :call && length(ex.args) >= 1
            if ex.args[1] == :String
                return true
            end
        end
        for arg in ex.args
            if contains_string_read(arg)
                return true
            end
        end
    end
    return false
end

function contains_vector_read(ex)
    if ex isa Expr
        if ex.head == :call && length(ex.args) >= 2
            if ex.args[1] == :jlread && length(ex.args) >= 4
                # jlread(io, UInt8, size) reads size bytes
                if ex.args[2] == :UInt8
                    return true
                end
            end
        end
        for arg in ex.args
            if contains_vector_read(arg)
                return true
            end
        end
    end
    return false
end

has_string = contains_string_read(after_accessor)
has_blob = contains_vector_read(after_accessor)

println("\n📊 What does :after accessor read?")
println("  - String(jlread(...)) for name content: ", has_string ? "❌ YES" : "✅ NO")
println("  - jlread(io, UInt8, size) for blob data: ", has_blob ? "❌ YES" : "✅ NO")

if has_string || has_blob
    println("\n❌ PROBLEM CONFIRMED: Content fields are being read unnecessarily!")
    println("   When accessing :after, we should skip:")
    println("   - name content (100 bytes)")
    println("   - data content (1000 bytes)")
    println("   Total waste: 1100 bytes per property access")
else
    println("\n✅ OPTIMIZATION ALREADY WORKING: Content fields are being skipped!")
end

println("\n" * "="^80)
