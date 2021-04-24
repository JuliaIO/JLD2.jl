using JLD2, Test, FileIO

using CodecZlib, CodecBzip2, CodecLz4


# This is for testing the different syntax versions as well as the library
@testset "Compression with CodecZlib" begin
    fn = joinpath(mktempdir(), "test.jld2")

    randomdata = repeat(rand(2000), 10)
    @save fn {compress=ZlibCompressor()} randomdata
    r = jldopen(f -> f["randomdata"], fn, "r")
    @test r == randomdata

    jldopen(fn, "w"; compress=ZlibCompressor()) do f
        f["randomdata"] = randomdata
    end
    @test load(fn, "randomdata") == randomdata

    jldopen(fn, "w") do f
        write(f, "randomdata", randomdata; compress=ZlibCompressor())
    end
    @test load(fn, "randomdata") == randomdata
end


@testset "Compression with CodecBzip2" begin
    fn = joinpath(mktempdir(), "test.jld2")

    randomdata = repeat(rand(2000), 10)
    @save fn {compress=Bzip2Compressor()} randomdata

    r = jldopen(f -> f["randomdata"], fn, "r")
    @test r == randomdata
end


@testset "Compression with CodecLz4" begin
    fn = joinpath(mktempdir(), "test.jld2")

    randomdata = repeat(rand(2000), 10)
    @save fn {compress=LZ4FrameCompressor()} randomdata
    
    r = jldopen(f -> f["randomdata"], fn, "r")
    @test r == randomdata
end