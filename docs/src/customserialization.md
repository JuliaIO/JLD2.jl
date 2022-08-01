# Custom Serialization (Experimental)

Version `v0.3.0` of introduces support for custom serialization.
For now this feature is considered experimental as it passes tests but 
has little testing in the wild. → Please test and report if you encounter problems.

The API is simple enough, to enable custom serialization for your type `A` you define
a new type e.g. `ASerialization` that contains the fields you want to store and define
`JLD2.writeas(::Type{A}) = ASerialization`.
Internally JLD2 will call `Base.convert` when writing and loading, so you need to make sure to extend that for your type.

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

!!! warning "Already defined custom serialization"
    Take care, some Julia internal types already use a `CustomSerialization` and JLD2.jl cannot nest them.
    In order to avoid unexpected behavior you should define a wrapper type,
    such as in the example above `ASerialization` even if you could use a simple Julia built in type (as in this case `Vector{Int}`).
    
If you do not want to overload `Base.convert` then you can also define

```julia
JLD2.wconvert(::Type{ASerialization}, a::A) = ASerialization([a.x])
JLD2.rconvert(::Type{A}, a::ASerialization) = A(only(a.x))
```

instead. This may be particularly relevant when types are involved that are not your own.

```julia
struct B
    x::Float64
end

JLD2.writeas(::Type{B}) = Float64
JLD2.wconvert(::Type{Float64}, b::B) = b.x
JLD2.rconvert(::Type{B}, x::Float64) = B(x)

arr = [B(rand()) for i=1:10]

@save "test.jld2" arr
```

In this example JLD2 converts the array of `B` structs to a plain `Vector{Float64}` prior to 
storing to disk.
