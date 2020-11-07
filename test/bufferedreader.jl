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
            b = read(br, Float64, 100)
        end
        @test a ≈ b
        rm("testfile.1")
    end
end

