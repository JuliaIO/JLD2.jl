using Benchmarks, JLD2

const TEMPFILE = begin
    tmp, io = mktemp()
    close(io)
    tmp
end

function jld_benchmark_write(x)
    f = jldopen(TEMPFILE, "w")
    write(f, "x", x)
    close(f)
end

function jld_benchmark_read()
    f = jldopen(TEMPFILE)
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

macro benchmark(title, data)
    jldwrite = symbol(string(title, "_jld_write"))
    jldread = symbol(string(title, "_jld_read"))
    serializewrite = symbol(string(title, "_serialize_write"))
    serializeread = symbol(string(title, "_serialize_read"))
    quote
        let data = $(data)
            Benchmarks.@benchmarkable(
                $jldwrite,
                nothing,
                jld_benchmark_write(data),
                nothing
            )
            Benchmarks.@benchmarkable(
                $jldread,
                nothing,
                jld_benchmark_read(),
                nothing
            )
            Benchmarks.@benchmarkable(
                $serializewrite,
                nothing,
                serialize_benchmark_write(data),
                nothing
            )
            Benchmarks.@benchmarkable(
                $serializeread,
                nothing,
                serialize_benchmark_read(),
                nothing
            )
            gc()
            println($(string(title, " JLD write")))
            println(Benchmarks.execute($jldwrite))
            println()
            gc()
            println($(string(title, " JLD read")))
            println(Benchmarks.execute($jldread))
            println()
            gc()
            println($(string(title, " Base.serialize")))
            println(Benchmarks.execute($serializewrite))
            println()
            gc()
            println($(string(title, " Base.deserialize")))
            println(Benchmarks.execute($serializeread))
            println()
            println()
            isa(data, Array) && empty!(data)
        end
    end
end

# Vector{Int}
@benchmark int_vector rand(typemin(Int):typemax(Int), 100000000)

# Vector{Any} of Ints
@benchmark any_int_vector convert(Vector{Any}, rand(typemin(Int):typemax(Int), 1000000))

# Vector{Integer} of Ints
@benchmark integer_int_vector convert(Vector{Integer}, rand(typemin(Int):typemax(Int), 1000000))

# Vector of non-builtin immutable
@benchmark complex128_vector [complex(rand(), rand()) for i = 1:10000000]

# Vector{Any} of non-builtin immutable
immutable IntWrapper
    x::Int
end
@benchmark any_complex128_vector Any[complex(rand(), rand()) for i = 1:1000000]

# Many empty arrays
@benchmark empty_arrays Any[Int[] for i = 1:1000000]

# Many one-eleemnt arrays
@benchmark one_element_arrays [[rand(typemin(Int):typemax(Int))] for i = 1:1000000]

# Equivalent benchmark from https://github.com/timholy/HDF5.jl/issues/170
# This is 1/10 of the amount of data, but we don't have a problem with
# scaling, so this should be fine
type Cell
    a::Array{Float64,1}
    b::Array{Float64,1}
    c::Array{Float64,2}
    d::Float64
    e::Array{Float64,1}
end
@benchmark cell [Cell(rand(3), rand(3), rand(8, 3), rand(), rand(3)) for i = 1:118138]
