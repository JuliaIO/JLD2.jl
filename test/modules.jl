push!(LOAD_PATH, joinpath(pwd(),"testmodules/"))
module TestModule

using A
using B
using Test
using JLD2

x =  AType(1)

S = BType(x)

@testset "save and load" begin
	T = deepcopy(S)
	@test T == S
	fn = joinpath(mktempdir(), "test_out.jld")
	@save fn T
	T = nothing
	@load fn T
	@test T == S
end

@testset "name collisions" begin
    mods = collect(values(Base.loaded_modules))
    # use whichever module would not be found first in a linear search
    M = findfirst(==(A), mods) < findfirst(==(B), mods) ? B : A
    x = M.SameNameType(42)
    file = joinpath(mktempdir(), "collision.jld")
    @save file x
    x = nothing
    @load file x
    @test x isa M.SameNameType
end

end
