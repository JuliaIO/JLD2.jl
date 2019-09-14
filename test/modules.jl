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
	@save "test_out.jld" T
	T = nothing
	@load "test_out.jld" T
	@test T == S	
end

end