# Custom Serialization

JLD2 makes it easy to define custom serialization for your own types. To do this, define a new type (e.g. `ASerialization`) that contains the fields you want to store, and then tell JLD2 how to convert between your type and the serialization type.

For example:

```julia
struct A
    x::Int
end

struct ASerialization
    x::Vector{Int}
end

JLD2.writeas(::Type{A}) = ASerialization
Base.convert(::Type{ASerialization}, a::A) = ASerialization([a.x])
Base.convert(::Type{A}, a::ASerialization) = A(only(a.x))
```

JLD2 will automatically use these conversions when saving and loading objects of type `A`.

!!! warning "Already defined custom serialization"
    Some Julia built-in types already use custom serialization, and JLD2 cannot nest these. To avoid unexpected behavior, always define a wrapper type for your serialization (as in the example above, where `ASerialization` is used instead of a plain `Vector{Int}`). In particular, avoid using built-in types like `<: AbstractDict` or `<: Array` directly as your serialization type.

If you prefer not to overload `Base.convert`, you can instead define the following methods:

```julia
JLD2.wconvert(::Type{ASerialization}, a::A) = ASerialization([a.x])
JLD2.rconvert(::Type{A}, a::ASerialization) = A(only(a.x))
```

This approach is especially useful if you do not own the type you want to serialize, or want to avoid extending `Base`.

Here's another example:

```julia
struct B
    x::Float64
end

JLD2.writeas(::Type{B}) = Float64
JLD2.wconvert(::Type{Float64}, b::B) = b.x
JLD2.rconvert(::Type{B}, x::Float64) = B(x)

arr = [B(rand()) for i in 1:10]

@save "test.jld2" arr
```

In this example, JLD2 converts the array of `B` structs to a plain `Vector{Float64}` before storing it to disk.
