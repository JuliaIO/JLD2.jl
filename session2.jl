include("includeme.jl")

s = jldopen("sin.jld2", "r"); ss=s["foo"]; close(s); ss
a = jldopen("tmp.jld2", "r"); aa=a["foo"]; close(a); aa
b = jldopen("bar.jld2", "r"); bb=b["bar"]; close(b); bb