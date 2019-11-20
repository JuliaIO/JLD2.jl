using JLD2, Test

struct Foo2 end

sym = gensym(:Bar2)
Core.eval(JLD2.ReconstructedTypes, :(struct $(sym) end))
T = getfield(JLD2.ReconstructedTypes, sym)

@testset "isreconstructed" begin
    @test !JLD2.isreconstructed(Foo2)
    a = Foo2()
    @test !JLD2.isreconstructed(a)
    @test JLD2.isreconstructed(T)
    b = T()
    @test JLD2.isreconstructed(b)
end
