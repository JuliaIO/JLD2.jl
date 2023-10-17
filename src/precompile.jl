

@setup_workload begin
    a=1
    b=2.0
    c=[1.0,2.0]
    d=rand(ComplexF64,100,100)
    e=Dict(s => 2 for s in ["a", "b", "c"])
    f="A String"
    g=ntuple(i->i^2, 5)
    h=rand(2000)

    @compile_workload begin
        mktemp() do path, _

            # jldsave
            jldsave(path; a, b, c, d, e, f, g, h)

            jldopen(path) do f
                for k in keys(f)
                    f[k]
                end
            end

            nothing
        end
    end
end