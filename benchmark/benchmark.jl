using BenchmarkTools, JLD2

const BACKEND = JLD2.MmapIO
# const BACKEND = IOStream
const TEMPFILE = begin
    tmp, io = mktemp()
    close(io)
    tmp
end

function jld_benchmark_write(x)
    f = jldopen(TEMPFILE, true, true, true, BACKEND)
    write(f, "x", x)
    close(f)
end

function jld_benchmark_read()
    f = jldopen(TEMPFILE, false, false, false, BACKEND)
    read(f, "x")
    close(f)
end

function serialize_benchmark_write(x)
    f = open(TEMPFILE, "w")
    serialize(f, x)
    close(f)
end

function serialize_benchmark_read()
    f = open(TEMPFILE)
    deserialize(f)
    close(f)
end

function bench(title, data)
    gc()
    println(title, " JLD write")
    show(STDOUT, MIME("text/plain"), @benchmark jld_benchmark_write($data))
    println("\n")
    gc()
    println(title, " JLD read")
    show(STDOUT, MIME("text/plain"), @benchmark jld_benchmark_read())
    println("\n")
    gc()
    println(title, " Base.serialize")
    show(STDOUT, MIME("text/plain"), @benchmark serialize_benchmark_write($data))
    println("\n")
    gc()
    println(title, " Base.deserialize")
    show(STDOUT, MIME("text/plain"), @benchmark serialize_benchmark_read())
    println("\n\n")
    isa(data, Array) && empty!(data)
end

# Vector{Int}
bench("int_vector", rand(typemin(Int):typemax(Int), 100000000))

# Vector{Any} of Ints
bench("any_int_vector", convert(Vector{Any}, rand(typemin(Int):typemax(Int), 1000000)))

# Vector{Integer} of Ints
bench("integer_int_vector", convert(Vector{Integer}, rand(typemin(Int):typemax(Int), 1000000)))

# Vector of non-builtin immutable
bench("complex128_vector", [complex(rand(), rand()) for i = 1:10000000])

# Vector{Any} of non-builtin immutable
struct IntWrapper
    x::Int
end
bench("any_complex128_vector", Any[complex(rand(), rand()) for i = 1:1000000])

# Many empty arrays
bench("empty_arrays", Any[Int[] for i = 1:1000000])

# Many one-eleemnt arrays
bench("one_element_arrays", [[rand(typemin(Int):typemax(Int))] for i = 1:1000000])

# Equivalent benchmark from https://github.com/timholy/HDF5.jl/issues/170
# This is 1/10 of the amount of data, but we don't have a problem with
# scaling, so this should be fine
mutable struct Cell
    a::Array{Float64,1}
    b::Array{Float64,1}
    c::Array{Float64,2}
    d::Float64
    e::Array{Float64,1}
end
bench("cell", [Cell(rand(3), rand(3), rand(8, 3), rand(), rand(3)) for i = 1:118138])
