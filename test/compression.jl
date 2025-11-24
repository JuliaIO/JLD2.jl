using JLD2, Test
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

@testset "Filter Constructor API" begin
    # Test that both positional and keyword constructors work for all filters

    # Test Deflate filter
    @test Deflate(3).level == 3
    @test Deflate(level=3).level == 3
    @test Deflate(level=7).level == 7

    # Test ZstdFilter
    @test ZstdFilter(10).level == 10
    @test ZstdFilter(level=10).level == 10
    @test (ZstdFilter(-10) |> JLD2.Filters.client_values) == (-10 % UInt32, )

    # Test Shuffle filter
    @test Shuffle(UInt32(8)).element_size == 8

    # Test Bzip2Filter
    @test Bzip2Filter(5).blocksize100k == 5
    @test Bzip2Filter(blocksize100k=5).blocksize100k == 5

    # Test Lz4Filter
    @test Lz4Filter(12345).blocksize == 12345
    @test Lz4Filter(blocksize=12345).blocksize == 12345
end

@testset "Compression Filters Coverage" begin
    using JLD2.Filters

    # Test data
    data = UInt8[i % 256 for i in 1:10000]

    pipelines = [
        # Double compression (unknown intermediate size)
        Filters.FilterPipeline(Deflate(), ZstdFilter()),
        # Shuffle + Compression
        Filters.FilterPipeline(Shuffle(4), Deflate()),
        Filters.FilterPipeline(Shuffle(4), ZstdFilter()),
        Filters.FilterPipeline(Shuffle(4), Bzip2Filter()),
        Filters.FilterPipeline(Shuffle(4), Lz4Filter()),
    ]

    for pipeline in pipelines
        # Manually compress
        buf = copy(data)
        ref = Ref(buf)

        for filter in pipeline.filters
             Filters.apply_filter!(filter, ref, true)
        end
        compressed_data = ref[]

        # Decompress using Filters.decompress with unknown size (nothing)
        # This ensures we hit the branch where output_size is nothing in apply_filter!
        io = IOBuffer(compressed_data)
        decompressed = Filters.decompress(pipeline, io, length(compressed_data), nothing)

        @test decompressed == data
    end
end
