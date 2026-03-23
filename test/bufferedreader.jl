using JLD2
using Test
using Random

tdir = tempdir()

@testset "BufferedReader" begin
    cd(tdir) do
        a = randn(100)
        open("testfile.1", "w") do ff
            write(ff, a)
        end
        b = open("testfile.1", "r") do ff
            br = JLD2.BufferedReader(ff)
            b = JLD2.jlread(br, Float64, 100)
        end
        @test a ≈ b
        rm("testfile.1")
    end
end

@testset "BufferedReader: ensureroom appends at buffer end" begin
    # Read a few bytes (fills the 16 384-byte lookahead), then seek past the
    # 16 384 boundary.  A bug caused the second fill to write at the cursor
    # position instead of the end of the buffer, corrupting already-buffered
    # bytes and mapping subsequent offsets incorrectly.
    data = rand(UInt8, 40_000)
    fn = joinpath(tdir, "testfile.2")
    write(fn, data)
    open(fn, "r") do f
        br = JLD2.BufferedReader(f)
        for _ in 1:10
            JLD2.jlread(br, UInt8)   # advance cursor; triggers initial 16 384-byte fill
        end
        seek(br, 20_000)             # crosses the 16 384-byte boundary
        @test JLD2.jlread(br, UInt8) == data[20_001]
    end
    rm(fn)
end

