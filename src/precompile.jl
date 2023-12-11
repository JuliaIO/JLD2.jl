

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
            
            # Win7 and below systems use IOStream method to ensure pre-compilation correct
            if Sys.iswindows() && Sys.windows_version().major <= 6 && Sys.windows_version().minor <= 1
                jldsave(path, IOStream; a, b, c, d, e, f, g, h)
                jldopen(path; iotype = IOStream) do f
                    for k in keys(f)
                        f[k]
                    end
                end
            else
                jldsave(path; a, b, c, d, e, f, g, h)
                jldopen(path) do f
                    for k in keys(f)
                        f[k]
                    end
                end
            end
            nothing
        end
    end
end