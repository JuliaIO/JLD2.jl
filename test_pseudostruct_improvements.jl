using MacroTools
include("src/macros_utils.jl")

# Test structures to analyze code generation patterns
# Note: We need these functions for the macro to work
jlwrite(io, ::Type{T}, x) where T = write(io, x)
jlread(io, ::Type{T}) where T = read(io, T)
jlread(io, ::Type{T}, n::Int) where T = [read(io, T) for _ in 1:n]
jlsizeof(x) = sizeof(x)
write_zerobytes(io, n) = write(io, zeros(UInt8, n))
write_nb_int(io, x, n) = write(io, x)
read_nb_uint(io, n) = UInt64(0)
isset(flags, bit) = (flags & (1 << bit)) != 0
struct RelOffset; offset::Int64; end
struct SharedDatatype; header_offset::Int64; end
struct H5Datatype end

@pseudostruct TestSimple begin
    version::UInt8 = 1
    flags::UInt8 = 0
end

@pseudostruct TestConditional begin
    version::UInt8 = 1
    flags::UInt8 = 0
    isset(flags, 0) && max_value::Int64
    isset(flags, 1) && min_value::Int64
end

@pseudostruct TestNestedConditional begin
    version::UInt8 = 2
    if version == 1
        old_field::UInt16
    end
    if version == 2
        new_field::UInt32
        flags::UInt8 = 0
        isset(flags, 0) && optional_data::Int64
    end
end

@pseudostruct TestSequentialReads begin
    version::UInt8 = 1
    name_size::UInt16
    name::@FixedLengthString(name_size)
    data_size::UInt32
    data::@Blob(data_size)
end

@pseudostruct TestMultipleFlagDeps begin
    version::UInt8 = 1
    flags::UInt8 = 0
    isset(flags, 0) && field_a::Int64
    isset(flags, 1) && field_b::Int64
    isset(flags, 2) && field_c::Int64
    isset(flags, 0) && field_d::Int32  # Reuses same flag condition
end

# Expansion analysis
println("=" ^ 80)
println("TestSimple expansion:")
println("=" ^ 80)
ex = @macroexpand @pseudostruct TestSimple begin
    version::UInt8 = 1
    flags::UInt8 = 0
end
println(ex)

println("\n" * "=" ^ 80)
println("TestConditional expansion:")
println("=" ^ 80)
ex = @macroexpand @pseudostruct TestConditional begin
    version::UInt8 = 1
    flags::UInt8 = 0
    isset(flags, 0) && max_value::Int64
    isset(flags, 1) && min_value::Int64
end
println(ex)

println("\n" * "=" ^ 80)
println("TestMultipleFlagDeps expansion (shows redundant flag reads):")
println("=" ^ 80)
ex = @macroexpand @pseudostruct TestMultipleFlagDeps begin
    version::UInt8 = 1
    flags::UInt8 = 0
    isset(flags, 0) && field_a::Int64
    isset(flags, 1) && field_b::Int64
    isset(flags, 2) && field_c::Int64
    isset(flags, 0) && field_d::Int32
end
println(ex)