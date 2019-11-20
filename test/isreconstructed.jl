using JLD2, Test

struct Foo end

sym = gensym(:Bar)
Core.eval(JLD2.ReconstructedTypes, :(struct $(sym) end))
T = getfield(JLD2.ReconstructedTypes, sym)

@testset "isreconstructed" begin
    @test !JLD2.isreconstructed(Foo)
    a = Foo()
    @test !JLD2.isreconstructed(a)
    @test JLD2.isreconstructed(T)
    b = T()
    @test JLD2.isreconstructed(b)
end
