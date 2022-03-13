using JLD2, Test, FileIO
using Pkg: Pkg
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

@testset "Verify Correctness of Compressor" begin
    fn = joinpath(mktempdir(), "test.jld2")

    @test_throws ArgumentError jldopen(fn, "w"; compress = 42)

    jldopen(fn, "w") do f
        @test_throws ArgumentError write(f, "x", zeros(10); compress = π)
    end
end


@testset "issue #368 - dynamically loaded CodecZlib" begin
    testprojectpath = Pkg.project().path
    # Simply test if this fails
    code = """
    using JLD2

    N = 100
    a = (rand(N, N), rand(N, N))
    save("test.jld2", "a", a; compress = true)
    """

    cd(mktempdir()) do
        my_cmd = `$(Base.julia_cmd()) --project=$(testprojectpath) -e $(code)`
        @test better_success(my_cmd)
    end
end
