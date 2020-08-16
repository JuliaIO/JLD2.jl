using JLD2, FileIO
using Test

struct Anint; x::Int; end
struct Bparam{T}; x::T; end

@testset "Inline Union Representation" begin
    fn, _ = mktemp()*".jld2"
    u = Union{Int,Float64}[1, 2.5, 3, 4.5]
    save(fn, "u" u)
    @test u == load(fn, "u")

    u = Union{Float64, Missing}[1.0, missing, 2.0, 10.5, missing]
    save(fn, "u" u)
    @test u == load(fn, "u")

    u = Union{Anint, Missing}[Anint(10), missing, missing, Anint(20)]
    save(fn, "u" u)
    @test u == load(fn, "u")

    u = Union{Anint, Float64, Int}[Anint(1), 2.5, 3]
    save(fn, "u" u)
    @test u == load(fn, "u")

    u = Union{Anint,Bparam}[Anint(1), Bparam(3)]
    save(fn, "u" u)
    @test u == load(fn, "u")

    u = Bparam[Bparam(1), Bparam(2.0)]
    save(fn, "u" u)
    @test u == load(fn, "u")
end
