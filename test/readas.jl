using JLD2, Test
using LazyArtifacts

testfiles = artifact"testfiles/JLD2TestFiles-0.1.4/artifacts"

module Readas
    using JLD2
    struct UndefinedFunction <:Function
        fun
    end
    (f::UndefinedFunction)(args...; kwargs...) = error("The function \$(f.fun) is not defined")

    # Case when readas is defined
    # If `F` doesn't exist when reading, should be read as `Foo{UndefinedFunction}`
    struct Foo{F<:Function}
        fun::F
    end
    struct FooSerialization
        fun
    end
    JLD2.writeas(::Type{<:Foo}) = FooSerialization
    Base.convert(::Type{<:FooSerialization}, f::Foo) = FooSerialization(f.fun)

    JLD2.readas(::Type{<:FooSerialization}) = Foo
    function Base.convert(::Type{<:Foo}, f::FooSerialization)
        isa(f.fun, Function) && return Foo(f.fun)
        return Foo(UndefinedFunction(f.fun))
    end

    # Case when readas is not defined (N)
    # If `F` doesn't exist when reading, should be read as `FooNSerialization`
    struct FooN{F<:Function}
        fun::F
    end
    struct FooNSerialization
        fun
    end
    JLD2.writeas(::Type{<:FooN}) = FooNSerialization
    Base.convert(::Type{<:FooNSerialization}, f::FooN) = FooNSerialization(f.fun)
    Base.convert(::Type{<:FooN}, f::FooNSerialization) = FooN(f.fun)
end

# jldsave("readas_foo_sin.jld2"; foo=Readas.Foo(sin))
# fun(x) = x^2
# jldsave("readas_foo_a.jld2"; foo=Readas.Foo(fun))
# jldsave("readas_foo_n_sin.jld2"; foo=Readas.FooN(sin))
# jldsave("readas_foo_n_a.jld2"; foo=Readas.FooN(fun))

@testset "readas api for struct upgrades" begin
    getfoo(file) = jldopen(joinpath(testfiles,file)) do io; io["foo"]; end
    @test getfoo("readas_foo_sin.jld2") isa Readas.Foo{typeof(sin)}
    @test getfoo("readas_foo_n_sin.jld2") isa Readas.FooN{typeof(sin)}
    @test getfoo("readas_foo_a.jld2") isa Readas.Foo{Readas.UndefinedFunction}
    @test getfoo("readas_foo_n_a.jld2") isa Readas.FooNSerialization
end