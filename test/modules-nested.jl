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



@testset "Nested modules" begin
    if VERSION >= v"1.3" # LIBSVM no longer supports julia < v1.3
        @testset "issue #149 - LIBSVM Kernel inside nested modules" begin
            tmpdir = mktempdir()
            atexit(() -> rm(tmpdir; force = true, recursive = true))

            mycustomload_filename = joinpath(tmpdir, "mycustomload.jl")
            model_filename = joinpath(tmpdir, "model.jld2")
            saving_filename = joinpath(tmpdir, "saving.jl")
            loading_filename = joinpath(tmpdir, "loading.jl")

            mycustomload_contents = """
            module MyCustomLoad
            using FileIO, JLD2

            function load_my_model(filename::AbstractString)
                d = FileIO.load(filename)
                model = d["model"]
                return model
            end

            end # module
            """

            saving_contents = """
                append!(Base.LOAD_PATH, $(Base.LOAD_PATH))
                unique!(Base.LOAD_PATH)
                using FileIO, JLD2, LIBSVM, RDatasets
                iris = dataset("datasets", "iris")
                labels = convert(Vector{String}, iris[!, :Species])
                instances = convert(Matrix{Float64}, iris[!, 1:4])'
                model = svmtrain(instances[:, 1:2:end], labels[1:2:end])
                model_filename = "$(model_filename)"
                rm(model_filename; force = true, recursive = true)
                FileIO.save(model_filename, Dict("model" => model))
            """

            loading_contents = """
                append!(Base.LOAD_PATH, $(Base.LOAD_PATH))
                unique!(Base.LOAD_PATH)
                tmpdir = "$(tmpdir)"
                include(joinpath(tmpdir, "mycustomload.jl"))
                using LIBSVM, Test
                model_filename = "$(model_filename)"
                model = @test_nowarn MyCustomLoad.load_my_model(model_filename)
                @test model.SVMtype isa DataType
                @test model.SVMtype <: LIBSVM.SVC
                @test model.kernel isa LIBSVM.Kernel.KERNEL
                @test model.SVs isa LIBSVM.SupportVectors
                using JLD2
                @test !JLD2.isreconstructed(model)
                @test !JLD2.isreconstructed(model.SVMtype)
                @test !JLD2.isreconstructed(model.kernel)
                @test !JLD2.isreconstructed(model.SVs)
                @test !JLD2.isreconstructed(LIBSVM.SVC)
                @test !JLD2.isreconstructed(LIBSVM.Kernel.KERNEL)
                @test !JLD2.isreconstructed(LIBSVM.SupportVectors)
            """

            rm(mycustomload_filename; force = true, recursive = true)
            rm(model_filename; force = true, recursive = true)
            rm(saving_filename; force = true, recursive = true)
            rm(loading_filename; force = true, recursive = true)

            if Sys.iswindows()
                # This is needed for the backslash path separator to survive the
                # roundtrip from string to file to included code
                saving_contents = replace(saving_contents, '\\' => "\\\\")
                loading_contents = replace(loading_contents, '\\' => "\\\\")
            end

            open(mycustomload_filename, "w") do io
                println(io, mycustomload_contents)
            end
            open(saving_filename, "w") do io
                println(io, saving_contents)
            end
            open(loading_filename, "w") do io
                println(io, loading_contents)
            end

            saving_cmd = `$(Base.julia_cmd()) $(saving_filename)`
            loading_cmd = `$(Base.julia_cmd()) $(loading_filename)`

            rm(model_filename; force = true, recursive = true)

            @test better_success(saving_cmd)
            @test better_success(loading_cmd)

            rm(tmpdir; force = true, recursive = true)
        end
    end
    @testset "issue #112 - Random.DSFMT" begin
        tmpdir = mktempdir()
        atexit(() -> rm(tmpdir; force = true, recursive = true))

        mycustomload_filename = joinpath(tmpdir, "mycustomload.jl")
        my_rng_filename = joinpath(tmpdir, "my_rng.jld2")
        saving_filename = joinpath(tmpdir, "saving.jl")
        loading_filename = joinpath(tmpdir, "loading.jl")

        mycustomload_contents = """
        module MyCustomLoad
        using FileIO, JLD2

        function load_my_rng(filename::AbstractString)
            d = FileIO.load(filename)
            my_rng = d["my_rng"]
            return my_rng
        end

        end # module
        """

        saving_contents = """
            append!(Base.LOAD_PATH, $(Base.LOAD_PATH))
            unique!(Base.LOAD_PATH)
            using FileIO, JLD2, Random
            my_rng = Random.MersenneTwister(1)
            my_rng_filename = "$(my_rng_filename)"
            rm(my_rng_filename; force = true, recursive = true)
            FileIO.save(my_rng_filename, Dict("my_rng" => my_rng))
        """

        loading_contents = """
            append!(Base.LOAD_PATH, $(Base.LOAD_PATH))
            unique!(Base.LOAD_PATH)
            tmpdir = "$(tmpdir)"
            include(joinpath(tmpdir, "mycustomload.jl"))
            using Random, Test
            my_rng_filename = "$(my_rng_filename)"
            my_rng = @test_nowarn MyCustomLoad.load_my_rng(my_rng_filename)
            @test my_rng isa Random.MersenneTwister
            using JLD2
            @test !JLD2.isreconstructed(my_rng)
            @test !JLD2.isreconstructed(Random.MersenneTwister)
        """

        rm(mycustomload_filename; force = true, recursive = true)
        rm(my_rng_filename; force = true, recursive = true)
        rm(saving_filename; force = true, recursive = true)
        rm(loading_filename; force = true, recursive = true)

        if Sys.iswindows()
            saving_contents = replace(saving_contents, '\\' => "\\\\")
            loading_contents = replace(loading_contents, '\\' => "\\\\")
        end

        open(mycustomload_filename, "w") do io
            println(io, mycustomload_contents)
        end
        open(saving_filename, "w") do io
            println(io, saving_contents)
        end
        open(loading_filename, "w") do io
            println(io, loading_contents)
        end

        saving_cmd = `$(Base.julia_cmd()) $(saving_filename)`
        loading_cmd = `$(Base.julia_cmd()) $(loading_filename)`

        rm(my_rng_filename; force = true, recursive = true)

        @test better_success(saving_cmd)
        @test better_success(loading_cmd)

        rm(tmpdir; force = true, recursive = true)
    end
    @testset "Struct that exists in Main" begin
        tmpdir = mktempdir()
        atexit(() -> rm(tmpdir; force = true, recursive = true))

        mycustomload_filename = joinpath(tmpdir, "mycustomload.jl")
        my_object_filename = joinpath(tmpdir, "my_object.jld2")
        saving_filename = joinpath(tmpdir, "saving.jl")
        loading_filename = joinpath(tmpdir, "loading.jl")

        mycustomload_contents = """
        module MyCustomLoad
        using FileIO, JLD2

        function load_my_object(filename::AbstractString)
            d = FileIO.load(filename)
            my_object = d["my_object"]
            return my_object
        end

        end # module
        """

        saving_contents = """
            append!(Base.LOAD_PATH, $(Base.LOAD_PATH))
            unique!(Base.LOAD_PATH)
            using FileIO, JLD2
            abstract type AbstractFoo end
            abstract type AbstractBar <: AbstractFoo end
            struct Baz <: AbstractBar
                x::Int
                y::Int
                z::Int
            end
            my_object = Baz(1, 2, 3)
            my_object_filename = "$(my_object_filename)"
            rm(my_object_filename; force = true, recursive = true)
            FileIO.save(my_object_filename, Dict("my_object" => my_object))
        """

        loading_contents = """
            append!(Base.LOAD_PATH, $(Base.LOAD_PATH))
            unique!(Base.LOAD_PATH)
            tmpdir = "$(tmpdir)"
            include(joinpath(tmpdir, "mycustomload.jl"))
            using Test
            my_object_filename = "$(my_object_filename)"
            abstract type AbstractFoo end
            abstract type AbstractBar <: AbstractFoo end
            struct Baz <: AbstractBar
                x::Int
                y::Int
                z::Int
            end
            my_object = @test_nowarn MyCustomLoad.load_my_object(my_object_filename)
            @test my_object isa Baz
            @test my_object.x == 1
            @test my_object.y == 2
            @test my_object.z == 3
            using JLD2
            @test !JLD2.isreconstructed(my_object)
            @test !JLD2.isreconstructed(Baz)
        """

        rm(mycustomload_filename; force = true, recursive = true)
        rm(my_object_filename; force = true, recursive = true)
        rm(saving_filename; force = true, recursive = true)
        rm(loading_filename; force = true, recursive = true)

        if Sys.iswindows()
            saving_contents = replace(saving_contents, '\\' => "\\\\")
            loading_contents = replace(loading_contents, '\\' => "\\\\")
        end

        open(mycustomload_filename, "w") do io
            println(io, mycustomload_contents)
        end
        open(saving_filename, "w") do io
            println(io, saving_contents)
        end
        open(loading_filename, "w") do io
            println(io, loading_contents)
        end

        saving_cmd = `$(Base.julia_cmd()) $(saving_filename)`
        loading_cmd = `$(Base.julia_cmd()) $(loading_filename)`

        rm(my_object_filename; force = true, recursive = true)

        @test better_success(saving_cmd)
        @test better_success(loading_cmd)

        rm(tmpdir; force = true, recursive = true)
    end
    @testset "Struct that cannot be found and needs to be reconstruct" begin
        tmpdir = mktempdir()
        atexit(() -> rm(tmpdir; force = true, recursive = true))

        mycustomload_filename = joinpath(tmpdir, "mycustomload.jl")
        my_object_filename = joinpath(tmpdir, "my_object.jld2")
        saving_filename = joinpath(tmpdir, "saving.jl")
        loading_filename = joinpath(tmpdir, "loading.jl")

        mycustomload_contents = """
        module MyCustomLoad
        using FileIO, JLD2

        function load_my_object(filename::AbstractString)
            d = FileIO.load(filename)
            my_object = d["my_object"]
            return my_object
        end

        end # module
        """

        saving_contents = """
            append!(Base.LOAD_PATH, $(Base.LOAD_PATH))
            unique!(Base.LOAD_PATH)
            using FileIO, JLD2
            abstract type AbstractFoo end
            abstract type AbstractBar <: AbstractFoo end
            struct Baz <: AbstractBar
                x::Int
                y::Int
                z::Int
            end
            my_object = Baz(1, 2, 3)
            my_object_filename = "$(my_object_filename)"
            rm(my_object_filename; force = true, recursive = true)
            FileIO.save(my_object_filename, Dict("my_object" => my_object))
        """

        loading_contents = """
            append!(Base.LOAD_PATH, $(Base.LOAD_PATH))
            unique!(Base.LOAD_PATH)
            tmpdir = "$(tmpdir)"
            include(joinpath(tmpdir, "mycustomload.jl"))
            using Test
            my_object_filename = "$(my_object_filename)"
            my_object = @test_logs (:warn, "type Main.Baz does not exist in workspace; reconstructing") MyCustomLoad.load_my_object(my_object_filename)
            @test my_object.x == 1
            @test my_object.y == 2
            @test my_object.z == 3
            using JLD2
            @test JLD2.isreconstructed(my_object)
        """

        rm(mycustomload_filename; force = true, recursive = true)
        rm(my_object_filename; force = true, recursive = true)
        rm(saving_filename; force = true, recursive = true)
        rm(loading_filename; force = true, recursive = true)

        if Sys.iswindows()
            saving_contents = replace(saving_contents, '\\' => "\\\\")
            loading_contents = replace(loading_contents, '\\' => "\\\\")
        end

        open(mycustomload_filename, "w") do io
            println(io, mycustomload_contents)
        end
        open(saving_filename, "w") do io
            println(io, saving_contents)
        end
        open(loading_filename, "w") do io
            println(io, loading_contents)
        end

        saving_cmd = `$(Base.julia_cmd()) $(saving_filename)`
        loading_cmd = `$(Base.julia_cmd()) $(loading_filename)`

        rm(my_object_filename; force = true, recursive = true)

        @test better_success(saving_cmd)
        @test better_success(loading_cmd)

        rm(tmpdir; force = true, recursive = true)
    end
    @testset "issue #154 - type exists in child module but not in parent module" begin
        original_directory = pwd()
        tmpdir = mktempdir()
        atexit(() -> rm(tmpdir; force = true, recursive = true))

        cd(tmpdir)
        rm("test.jld"; force = true, recursive = true)

        code = """
        using JLD2

        # Always start fresh
        rm("test.jld"; force=true)

        # Write out jld file in child module
        module Child
        using JLD2, LinearAlgebra, Pkg
        jldopen("test.jld", "w") do io
            # Symmetric is correctly loadable
            io["linalg_obj"] = Symmetric(randn(2,2,))

            # PackageSpec is actually Pkg.Types.PackageSpec
            io["pkg_types_obj"] = PackageSpec(;name = "Foo")
        end
        @info("Wrote out to test.jld")

        # Parent module will call do_read()
        function do_read()
            jldopen("test.jld", "r") do io
                io["linalg_obj"], io["pkg_types_obj"]
            end
        end
        end # module Child

        # Read it out in parent module (where Pkg and LinearAlgebra don't exist)
        import .Child

        result = Child.do_read()

        import Test

        Test.@test result isa Tuple
        Test.@test length(result) == 2

        Test.@test size(result[1]) == (2, 2)
        Test.@test length(result[1]) == 4
        Test.@test result[1][1] isa Float64
        Test.@test result[1][2] isa Float64
        Test.@test result[1][3] isa Float64
        Test.@test result[1][4] isa Float64

        Test.@test result[2].name isa String
        Test.@test result[2].name == "Foo"

        Test.@test !JLD2.isreconstructed(result)

        Test.@test !JLD2.isreconstructed(result[1])
        Test.@test !JLD2.isreconstructed(result[1][1])
        Test.@test !JLD2.isreconstructed(result[1][2])
        Test.@test !JLD2.isreconstructed(result[1][3])
        Test.@test !JLD2.isreconstructed(result[1][4])

        Test.@test !JLD2.isreconstructed(result[2])
        Test.@test !JLD2.isreconstructed(result[2].name)

        # NOTE: we do not import Pkg and LinearAlgebra into the parent
        # module until AFTER we have run `Child.do_read()`
        import Pkg
        import LinearAlgebra

        Test.@test result[1] isa LinearAlgebra.Symmetric{Float64,Array{Float64,2}}
        Test.@test result[2] isa Pkg.Types.PackageSpec
        """

        my_cmd = `$(Base.julia_cmd()) -e $(code)`
        @test better_success(my_cmd)

        cd(original_directory)
        rm(tmpdir; force = true, recursive = true)
    end
end
