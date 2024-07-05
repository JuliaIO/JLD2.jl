using Test, JLD2

@testset "Write to IOBuffer" begin
    iobuf = IOBuffer()
    f = jldopen(iobuf, "w")
    f["a"] = 42
    close(f)
    
    seekstart(iobuf)
    f = jldopen(iobuf, "r")
    @test f["a"] == 42
    
    seekstart(iobuf)
    cd(mktempdir()) do 
        write("test.jld2", take!(iobuf))
        @test load("test.jld2", "a") == 42
    end
end

@testset "Append to IOBuffer file" begin
    iobuf = IOBuffer()
    jldopen(iobuf, "w") do f
        f["a"] = 42
    end

    seekstart(iobuf)
    f = jldopen(iobuf, "r+")
    @test f["a"] == 42
    f["b/c"] = "a string"
    close(f)

    seekstart(iobuf)
    cd(mktempdir()) do 
        write("test.jld2", take!(iobuf))
        @test load("test.jld2", "a") == 42
        @test load("test.jld2", "b/c") == "a string"

    end
end
