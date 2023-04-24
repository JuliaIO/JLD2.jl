include("includeme.jl")

jldsave("sin.jld2"; foo=Foo(sin))
jldsave("tmp.jld2"; foo=Foo(x->x^2))
jldsave("bar.jld2"; bar=Bar(x->x^2))