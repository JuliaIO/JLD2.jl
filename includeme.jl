using JLD2

# Some way to define an unknown function
struct UndefinedFunction <:Function
    fun
end
(f::UndefinedFunction)(args...; kwargs...) = throw(ErrorException("The function $(f.fun) is not defined"))

# Foo has all required definitions to be reloaded back to an unknown function
struct Foo{F<:Function}
    fun::F
end
struct FooSerialization
    fun
end

JLD2.readas(::Type{<:FooSerialization}) = Foo
JLD2.writeas(::Type{<:Foo}) = FooSerialization
Base.convert(::Type{<:FooSerialization}, f::Foo) = FooSerialization(f.fun)
function Base.convert(::Type{<:Foo}, f::FooSerialization)
    isa(f.fun, Function) && return Foo(f.fun)
    return Foo(UndefinedFunction(f.fun))
end

# We do not define JLD2.readas for Bar: It should remain unconverted
struct Bar{F<:Function}
    fun::F
end
struct BarSerialization
    fun
end

JLD2.writeas(::Type{<:Bar}) = BarSerialization
Base.convert(::Type{BarSerialization}, f::Bar) = BarSerialization(f.fun)
Base.convert(::Type{Bar}, f::BarSerialization) = Bar(f.fun)
function Base.convert(::Type{<:Bar}, f::BarSerialization)
    isa(f.fun, Function) && return Bar(f.fun)
    return Bar(UndefinedFunction(f.fun))
end