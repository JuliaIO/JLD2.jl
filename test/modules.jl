push!(LOAD_PATH, pwd())

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

end
