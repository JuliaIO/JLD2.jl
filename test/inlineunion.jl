using JLD2, FileIO
using Test

struct Anint; x::Int; end
struct Bparam{T}; x::T; end

@testset "Inline Union Representation" begin
    fn = mktemp()[1]*".jld2"
    u = Union{Int,Float64}[1, 2.5, 3, 4.5]
    save(fn, "u", u)
    @test all(skipmissing(u) .== skipmissing(load(fn, "u")))
    
    jldopen(fn, "r") do f
        @test u == f["u"]
        # Test again to verify that the correct object was cached
        # See issue #348
        @test u == f["u"]
    end

    u = Union{Float64, Missing}[1.0, missing, 2.0, 10.5, missing]
    save(fn, "u", u; compress=true)
    @test all(skipmissing(u) .== skipmissing(load(fn, "u")))

    u = Union{Anint, Missing}[Anint(10), missing, missing, Anint(20)]
    save(fn, "u", u)
    @test all(skipmissing(u) .== skipmissing(load(fn, "u")))

    u = Union{Anint, Float64, Int}[Anint(1), 2.5, 3]
    save(fn, "u", u)
    @test u == load(fn, "u")

    u = Union{Anint,Bparam}[Anint(1), Bparam(3)]
    save(fn, "u", u)
    @test u == load(fn, "u")

    u = Bparam[Bparam(1), Bparam(2.0)]
    save(fn, "u", u)
    @test u == load(fn, "u")

    u = Union{Float32, Missing}[rand(5,5);]
    save(fn, "u", u)
    @test u == load(fn, "u")
end
