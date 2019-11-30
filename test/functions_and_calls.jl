@testset "Functions and Calls" begin
    tmpdir = mktempdir()
    atexit(() -> rm(tmpdir; force = true, recursive = true))

    model_filename = joinpath(tmpdir, "model.jld2")
    saving_filename = joinpath(tmpdir, "saving.jl")
    loading_filename = joinpath(tmpdir, "loading.jl")

    saving_contents = """
    module TestModule
        struct MyHardStruct{T,T2,T3}
            f::T
            u::T2
            p::T3
        end
        function (m::MyHardStruct)(t)
            m.u[2]*t + (1-t)*m.u[1]
        end
        export MyHardStruct
    end
    using .TestModule
    function f(u,p,t)
        [u[1],u[2]] .* p
    end
    u = [[1.0,3.0],[5.0,5.0]]
    p = (1.0,2.0)
    m = MyHardStruct(f,u,p)

    using JLD2
    @save "out.jld2" m
    """

    loading_contents = """
    module TestModule
        struct MyHardStruct{T,T2,T3}
            f::T
            u::T2
            p::T3
        end
        function (m::MyHardStruct)(t)
            m.u[2]*t + (1-t)*m.u[1]
        end
        export MyHardStruct
    end
    using .TestModule, JLD2, Test
    function f(u,p,t)
        [u[1],u[2]] .* p
    end
    JLD2.@load "out.jld2" m
    @test m(0.5) == [3.0,4.0]
    @test m.f(u[1],m.p,0.0) == [1.0,6.0]
    """

    rm(mycustomload_filename; force = true, recursive = true)
    rm(model_filename; force = true, recursive = true)
    rm(saving_filename; force = true, recursive = true)
    rm(loading_filename; force = true, recursive = true)

    open(saving_filename, "w") do io
        println(io, saving_contents)
    end
    open(loading_filename, "w") do io
        println(io, loading_contents)
    end

    saving_cmd = `$(Base.julia_cmd()) $(saving_filename)`
    loading_cmd = `$(Base.julia_cmd()) $(loading_filename)`

    rm(model_filename; force = true, recursive = true)

    @test success(saving_cmd)
    @test success(loading_cmd)

    rm(tmpdir; force = true, recursive = true)
end
