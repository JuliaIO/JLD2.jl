using JLD2
using UnPack
using Test

@testset "UnPack-Extension" begin

    x, y = rand(2)
    m, n = rand(Int64, 2)

    # Open file and group
    path = joinpath(mktempdir(), "temp_file.jld2")
    file = jldopen(path, "w")
    file_group = JLD2.Group(file, "mygroup")

    # write into file
    @pack! file = x, y
    @pack! file_group = n, m

    # Test that entries where created
    @test "x" ∈ keys(file)
    @test "y" ∈ keys(file)
    @test "n" ∈ keys(file_group)
    @test "m" ∈ keys(file_group)

    x_ref, y_ref = x, y
    n_ref, m_ref = n, m
    x, y = 0, 0
    n, m = 0, 0

    # Test extraction of entries
    @unpack x, y = file
    @unpack n, m = file_group

    @test (x, y) == (x_ref, y_ref)
    @test (n, m) == (n_ref, m_ref)

    # Cleanup
    close(file)
    rm(path)
end
