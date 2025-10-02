using MacroTools
include("src/macros_utils.jl")

# Helper functions needed by generated code
jlwrite(io, x) = write(io, x)
jlwrite(io, ::Type{T}, x) where T = write(io, T(x))
jlread(io, ::Type{T}) where T = read(io, T)
jlread(io, ::Type{T}, n::Int) where T = [read(io, T) for _ in 1:n]
jlsizeof(x) = sizeof(x)
write_zerobytes(io, n) = write(io, zeros(UInt8, n))
write_nb_int(io, x, n) = write(io, x)
read_nb_uint(io, n) = UInt64(0)
isset(flags, bit) = (flags & (1 << bit)) != 0
struct RelOffset; offset::Int64; end
struct Message{T}; io::T; address::Int64; offset::Int64; end
struct HmWrap{Name, T}; m::Message{T}; hflags::UInt8; size::UInt64; end

println("=" ^ 80)
println("ANALYSIS: Simple structure with two fields")
println("=" ^ 80)
ex1 = @macroexpand @pseudostruct TestSimple begin
    version::UInt8 = 1
    flags::UInt8 = 0
end
println(MacroTools.prettify(ex1))

println("\n" * "=" ^ 80)
println("ANALYSIS: Conditional fields (shows redundant flag reads in getproperty)")
println("=" ^ 80)
ex2 = @macroexpand @pseudostruct TestConditional begin
    version::UInt8 = 1
    flags::UInt8 = 0
    isset(flags, 0) && max_value::Int64
    isset(flags, 1) && min_value::Int64
end
println(MacroTools.prettify(ex2))

println("\n" * "=" ^ 80)
println("ANALYSIS: Multiple fields depending on same flag bit")
println("=" ^ 80)
ex3 = @macroexpand @pseudostruct TestMultiDep begin
    flags::UInt8 = 0
    isset(flags, 0) && field_a::Int64
    isset(flags, 0) && field_b::Int32
    isset(flags, 1) && field_c::Int64
end
println(MacroTools.prettify(ex3))

println("\n" * "=" ^ 80)
println("ANALYSIS: Sequential variable-length reads")
println("=" ^ 80)
ex4 = @macroexpand @pseudostruct TestSequential begin
    name_len::UInt16
    name::@FixedLengthString(name_len)
    data_size::UInt32
    data::@Blob(data_size)
end
println(MacroTools.prettify(ex4))