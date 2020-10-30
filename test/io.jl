using JLD2, FileIO, Test

mktempdir() do d
    fn = joinpath(d, "testio.jld2")
    data = Dict(
        "var1" => 1,
        "var2" => "a string",
        "var3" => (an=:array, seriously=rand(10))
    )

    save(fn, data)

    @testset "jlread" begin
        # We'll start by establishing that we can read from an IOBuffer created from the
        # bytes of a normal jld2 file
        io = IOBuffer(read(fn))
        seekstart(io)
        # Check that the read result is our data
        @test jlread(io) == data
    end

    @testset "jlwrite" begin
        # If jlread works then we can just check that jlwrite also works by reading it back
        # from our buffer.
        @show size(read(fn))
        io = IOBuffer()
        jlwrite(io, data)
        seekstart(io)
        @test jlread(io) == data
    end
end
