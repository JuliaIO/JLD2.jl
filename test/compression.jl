using JLD2, Test, FileIO
using Pkg: Pkg
using JLD2Deflate, JLD2Bzip2, JLD2Lz4, JLD2Zstd, JLD2Blosc


# This is for testing the different syntax versions as well as the library
@testset "Compression with CodecZlib" begin
    fn = joinpath(mktempdir(), "test.jld2")

    randomdata = repeat(rand(2000), 10)
    @save fn {compress=true} randomdata
    r = jldopen(f -> f["randomdata"], fn, "r")
    @test r == randomdata

    jldopen(fn, "w"; compress=Deflate()) do f
        f["randomdata"] = randomdata
    end
    @test load(fn, "randomdata") == randomdata

    jldopen(fn, "w") do f
        write(f, "randomdata", randomdata; compress=Deflate())
    end
    @test load(fn, "randomdata") == randomdata
end


@testset "Compression with CodecBzip2" begin
    fn = joinpath(mktempdir(), "test.jld2")

    randomdata = repeat(rand(2000), 10)
    @save fn {compress=Bzip2Filter()} randomdata

    r = jldopen(f -> f["randomdata"], fn, "r")
    @test r == randomdata
end


@testset "Compression with CodecLz4" begin
    fn = joinpath(mktempdir(), "test.jld2")

    randomdata = repeat(rand(2000), 10)
    @save fn {compress=Lz4Filter()} randomdata

    r = jldopen(f -> f["randomdata"], fn, "r")
    @test r == randomdata
end

@testset "Compression with CodecZstd" begin
    fn = joinpath(mktempdir(), "test.jld2")

    randomdata = repeat(rand(2000), 10)
    @save fn {compress=ZstdFilter()} randomdata

    r = jldopen(f -> f["randomdata"], fn, "r")
    @test r == randomdata
end

@testset "Verify Correctness of Compressor" begin
    fn = joinpath(mktempdir(), "test.jld2")

    @test_throws ArgumentError jldopen(fn, "w"; compress = 42)

    jldopen(fn, "w") do f
        @test_throws ArgumentError write(f, "x", zeros(10); compress = π)
    end
end
