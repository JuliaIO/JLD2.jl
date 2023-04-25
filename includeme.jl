using JLD2
struct Foo{F<:Function}
    fun::F
end
struct FooSerialization
    fun
end
struct UndefinedFunction <:Function
    fun
end
(f::UndefinedFunction)(args...; kwargs...) = throw(ErrorException("The function $(f.fun) is not defined"))

JLD2.writeas(::Type{<:Foo}) = FooSerialization
Base.convert(::Type{<:FooSerialization}, f::Foo) = FooSerialization(f.fun)
function Base.convert(::Type{<:Foo}, f::FooSerialization)
    isa(f.fun, Function) && return Foo(f.fun)
    return Foo(UndefinedFunction(f.fun))
end

# Break it
struct Bar{F<:Function}
    fun::F
end

struct BarSerialization
    fun
end

JLD2.writeas(::Type{<:Bar}) = BarSerialization
Base.convert(::Type{BarSerialization}, f::Bar) = BarSerialization(f.fun)
Base.convert(::Type{Bar{F}}, f::BarSerialization) where {F<:Function} = Bar{F}(f.fun)