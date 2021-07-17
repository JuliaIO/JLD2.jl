using FileIO, JLD2, Test

struct S1
    a
    f
end

struct S2{F}
    a
    f::F
end

struct S3{F}
    f::F
end

λ() = 42

function round_trip(x)
    mktempdir() do dir
        fn = joinpath(dir, "test.jld2")
        @save fn grp=x
        @load fn grp
        return grp
    end
end

@testset "round trip Function values" begin
    @test 42 == round_trip(λ)()
    @test 42 == first(round_trip((λ,)))()
    @test 42 == first(round_trip((λ, λ)))()
    @test 42 == first(round_trip((λ, 42)))()
    @test 42 == first(round_trip([λ]))()
    @test 42 == first(round_trip([λ, λ]))()
    @test 42 == first(round_trip([λ, 42]))()
    @test 42 == round_trip(S1(42, λ)).f()
    @test 42 == round_trip(S2(42, λ)).f()
    @test 42 == round_trip(S3(λ)).f()
end

# Same as in "modules-nested.jl". Only here to make script run as standalone.
function better_success(cmd)
    fn1, _ = mktemp()
    fn2, _ = mktemp()
    try
       run(pipeline(cmd, stdout=fn1, stderr=fn2))
    catch
        println(String(read(fn1)))
        println(String(read(fn2)))
        return false
    end
    return true
end


@testset "Anonymous Functions" begin
    fn = joinpath(mktempdir(), "test.jld2")

    # Regular function first
    jldsave(fn; cos)
    loaded_cos = load(fn, "cos")
    @test cos === loaded_cos

    # "Pure" function
    f1 = (x) -> x^2

    # Closure
    f2 = let x=0; () -> (x+=1); end
    f2(); f2(); # Call twice for good measure

    # Global Ref
    f3 = () -> global_variable

    jldsave(fn; f1, f2, f3)

    g1 = load(fn, "f1")
    g2 = load(fn, "f2")
    g3 = load(fn, "f3")

    @test f1 !== g1
    @test f1(42) == g1(42)

    @test f2 !== g2
    @test f2() == g2()
    # Test again to test mutation of internal state
    @test f2() == g2()

    @test f3 !== g3
    @test_throws UndefVarError g3()
    global global_variable = 2π
    @test f3() == g3()

    # Now test again in a new julia session
    @testset "Anonymous Function from different session" begin
        tmpdir = mktempdir()
        atexit(() -> rm(tmpdir; force = true, recursive = true))

        saving_filename = joinpath(tmpdir, "saving.jld2")
        fn = joinpath(tmpdir, "funs.jld2")
        saving_contents = """
            append!(Base.LOAD_PATH, $(Base.LOAD_PATH))
            unique!(Base.LOAD_PATH)
            using JLD2
            
            # "Pure" function
            f1 = (x) -> sqrt(x)
    
            # Closure
            f2 = let x=""; () -> (x*="a"); end
            f2(); f2(); # Call twice for good measure
        
            # Global Ref
            f3 = () -> global_variable

            # With struct
            struct SomeStructWithFunctionInside
                x::Int
                func::Function

                function SomeStructWithFunctionInside(x)
                    return new(x, (y)-> y * exp(x))
                end
            end


            module AnonFunctionModule
                # "Pure" function
                f4 = (x) -> sqrt(x)
        
                # Closure
                f5 = let x=""
                     () -> (x*="a")
                    end
                f5(); f5(); # Call twice for good measure
            
                # Global Ref
                f6 = () -> module_variable
            end
            using .AnonFunctionModule: f4, f5, f6
            jldsave("$(fn)"; f1, f2, f3, f4, f5, f6, a=SomeStructWithFunctionInside(1))
        """
        if Sys.iswindows()
            saving_contents = replace(saving_contents, '\\' => "\\\\")
        end

        open(saving_filename, "w") do io
            println(io, saving_contents)
        end
        saving_cmd = `$(Base.julia_cmd()) $(saving_filename)`
        @test better_success(saving_cmd)

        f1 = load(fn, "f1")
        @test f1(4) == 2
        f2 = load(fn, "f2")
        @test f2() == "aaa"
        f3 = load(fn, "f3")
        @test f3() == global_variable
        f4 = load(fn, "f4")
        @test f4(4) ==2 
        f5 = load(fn, "f5")
        @test f5() == "aaa"
        f6 = load(fn, "f6")
        @test_throws UndefVarError f6()
        global module_variable = 3
        @test f6() == module_variable
        @eval module AnonFunctionModule
            module_variable = 4
        end
        f6 = load(fn, "f6")
        f6()

        # With struct
        load(fn, "a")
        struct SomeStructWithFunctionInside
            x::Int
            func::Function
            SomeStructWithFunctionInside(x) = new(x, (y)-> y * exp(x))
        end
        @test_nowarn load(fn, "a")
    end

end