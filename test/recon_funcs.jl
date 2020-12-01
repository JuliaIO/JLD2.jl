using FileIO, JLD2, Test

struct S1
    a
    f
end

struct S2{F}
    a
    f::F
end

struct S3{F}
    f::F
end

λ() = 42

function round_trip(x)
    mktempdir() do dir
        fn = joinpath(dir, "test.jld2")
        @save fn grp=x
        @load fn grp
        return grp
    end
end

@testset "round trip Function values" begin
    @test 42 == round_trip(λ)()
    @test 42 == first(round_trip((λ,)))()
    @test 42 == first(round_trip((λ, λ)))()
    @test 42 == first(round_trip((λ, 42)))()
    @test 42 == first(round_trip([λ]))()
    @test 42 == first(round_trip([λ, λ]))()
    @test 42 == first(round_trip([λ, 42]))()
    @test 42 == round_trip(S1(42, λ)).f()
    @test 42 == round_trip(S2(42, λ)).f()
    @test 42 == round_trip(S3(λ)).f()
end
