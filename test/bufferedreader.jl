using JLD2
using Test
using Random

@testset "BufferedReader" begin
    mktemp() do path, io
        a = randn(100)
        write(io, a)
        close(io)
        b = open(path, "r") do ff
            br = JLD2.BufferedReader(ff)
            b = JLD2.jlread(br, Float64, 100)
        end
        @test a ≈ b
    end
end

@testset "BufferedReader: ensureroom appends at buffer end" begin
    # Read a few bytes (fills the 16 384-byte lookahead), then seek past the
    # 16 384 boundary.  A bug caused the second fill to write at the cursor
    # position instead of the end of the buffer, corrupting already-buffered
    # bytes and mapping subsequent offsets incorrectly.
    mktemp() do fn, io
        data = rand(UInt8, 40_000)
        write(io, data)
        close(io)
        open(fn, "r") do f
            br = JLD2.BufferedReader(f)
            for _ in 1:10
                JLD2.jlread(br, UInt8)   # advance cursor; triggers initial 16 384-byte fill
            end
            seek(br, 20_000)             # crosses the 16 384-byte boundary
            @test JLD2.jlread(br, UInt8) == data[20_001]
        end
    end
end

