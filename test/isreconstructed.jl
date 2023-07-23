using JLD2, Test

struct Foo2 end

@testset "isreconstructed" begin
    @test !JLD2.isreconstructed(Foo2)
    a = Foo2()
    @test !JLD2.isreconstructed(a)
    @test JLD2.isreconstructed(JLD2.ReconstructedPrimitive{:Test, UInt8}(1))
    @test JLD2.isreconstructed(JLD2.ReconstructedStatic{:Test, (:a, :b), Tuple{Int, Float64}}((; a=1, b=2.0)))
    @test JLD2.isreconstructed(JLD2.ReconstructedMutable{:Test, (:a, :b), Tuple{Int, Float64}}(Any[1, 2.0]))
end
