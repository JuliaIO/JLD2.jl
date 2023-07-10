@testset "readas" begin
    definitions = """
        using JLD2, Test

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
    """
    save_files = definitions*"""
        jldsave("readas_foo_sin.jld2"; foo=Foo(sin))
        fun(x) = x^2
        jldsave("readas_foo_a.jld2"; foo=Foo(fun))
        jldsave("readas_foo_n_sin.jld2"; foo=FooN(sin))
        jldsave("readas_foo_n_a.jld2"; foo=FooN(fun))
    """

    read_files = definitions*"""
        getfoo(file) = jldopen(file) do io; io["foo"]; end
        @test getfoo("readas_foo_sin.jld2") isa Foo{typeof(sin)}
        @test getfoo("readas_foo_n_sin.jld2") isa FooN{typeof(sin)}
        @test getfoo("readas_foo_a.jld2") isa Foo{UndefinedFunction}
        @test getfoo("readas_foo_n_a.jld2") isa FooNSerialization
        rm("readas_foo_sin.jld2")
        rm("readas_foo_n_sin.jld2")
        rm("readas_foo_a.jld2")
        rm("readas_foo_n_a.jld2")
    """
    save_cmd = `$(Base.julia_cmd()) --project=$(pwd()) -e $(save_files)`
    read_cmd = `$(Base.julia_cmd()) --project=$(pwd()) -e $(read_files)`
    cd(mktempdir(pwd())) do
        @test better_success(save_cmd)
        @test better_success(read_cmd)
    end
end