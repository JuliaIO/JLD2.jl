using JLD2, Test, FileIO
using Pkg: Pkg
using JLD2Bzip2, JLD2Lz4


# This is for testing the different syntax versions as well as the library
@testset "Compression with Deflate" begin
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


@testset "Compression with Filters" begin
    fn = joinpath(mktempdir(), "test.jld2")
    filters = [
        Deflate(),
        Bzip2Filter(),
        Lz4Filter(),
        Lz4Filter(10^5),
        ZstdFilter(),
        #= BloscFilter(),
        BitshuffleFilter(),
        BitshuffleFilter(compressor=:lz4),
        BitshuffleFilter(compressor=:zstd), =#
        [Shuffle(), Deflate()]
    ]

    randomdata = repeat(rand(200), 100)
    for filter in filters
        @save fn {compress=filter} randomdata
        r = jldopen(f -> f["randomdata"], fn, "r")
        @test r == randomdata
    end
end

@testset "Compress Arrays with non-trivial layout" begin
    fn = joinpath(mktempdir(), "compress_with_conversion.jld2")
    fn2 = joinpath(mktempdir(), "no_compress_with_conversion.jld2")

    data = [(nothing, 0x00) for n in 1:1000]
    @save fn {compress=true} data
    @save fn2 {compress=false} data
    r = jldopen(f -> f["data"], fn, "r")
    @test r == data
    @test filesize(fn) < filesize(fn2)

    data = [(1, 0x00) for n in 1:1000]
    @save fn {compress=true} data
    @save fn2 {compress=false} data
    r = jldopen(f -> f["data"], fn, "r")
    @test r == data
    @test filesize(fn) < filesize(fn2)
end

@testset "Verify Correctness of Compressor" begin
    fn = joinpath(mktempdir(), "test.jld2")

    @test_throws ArgumentError jldopen(fn, "w"; compress = 42)

    jldopen(fn, "w") do f
        @test_throws ArgumentError write(f, "x", zeros(10); compress = π)
    end
end
