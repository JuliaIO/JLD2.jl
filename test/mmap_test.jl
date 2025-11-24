using JLD2, Test

@testset "Mmapped Arrays" begin
    cd(mktempdir()) do

        a = rand(100,100);
        b = rand(ComplexF64, 5,5)
        c = 42
        d = [ntuple(x->Bool(x%2), Val(24))  for i=1:100]

        fn = "test.jld2"
        jldsave(fn; a, b, c, d)

        jldopen(fn, "r") do f
            dset = JLD2.get_dataset(f, "a")
            @test JLD2.ismmappable(dset)
            @test JLD2.readmmap(dset) == a
            dset = JLD2.get_dataset(f, "b")
            @test JLD2.ismmappable(dset)
            @test JLD2.readmmap(dset) == b
            dset = JLD2.get_dataset(f, "c")
            @test JLD2.ismmappable(dset) == false
            dset = JLD2.get_dataset(f, "d")
            @test JLD2.ismmappable(dset) == true
        end

        if Sys.iswindows()
            jldopen(fn, "a") do f
                dset = JLD2.get_dataset(f, "a")
                @test JLD2.ismmappable(dset) == false
                @test_logs (:warn, "On Windows memory-mapping is only possible for files in read-only mode.") JLD2.ismmappable(dset)
                dset = JLD2.get_dataset(f, "c")
                @test JLD2.ismmappable(dset) == false
                @test_nowarn JLD2.ismmappable(dset)
            end
        else
            jldopen(fn, "a") do f
                dset = JLD2.get_dataset(f, "a")
                @test JLD2.ismmappable(dset)
                @test JLD2.readmmap(dset) == a
                JLD2.readmmap(dset)[1,1] = 42.0

                dset = JLD2.get_dataset(f, "b")
                @test JLD2.ismmappable(dset)
                @test JLD2.readmmap(dset) == b
                JLD2.readmmap(dset)[1,1] = 4.0 + 2.0im

                dset = JLD2.get_dataset(f, "c")
                @test JLD2.ismmappable(dset) == false

                dset = JLD2.get_dataset(f, "d")
                @test JLD2.ismmappable(dset) == true
            end

            jldopen(fn, "r") do f
                @test f["a"][1,1] == 42.0
                @test f["b"][1,1] == 4.0 + 2.0im
                @test f["d"] == d
            end
        end
    end
end


@testset "Early Allocation" begin
    fn = tempname() * ".jld2"
    # Update this for proper API eventually
    if !Sys.iswindows()
        jldopen(fn, "w") do f
            dset = JLD2.create_dataset(f, "data")

            dset.datatype = JLD2.h5fieldtype(f, Float64, Float64, Val{false})

            dims = (100,100)
            dset.dataspace = JLD2.WriteDataspace(JLD2.DS_SIMPLE, UInt64.(reverse(dims)), ())

            JLD2.allocate_early(dset, Float64)

            @test JLD2.ismmappable(dset)

            emptyarr = JLD2.readmmap(dset)

            emptyarr[1:2:100] .= 1:50
        end

        data = JLD2.load(fn, "data")
        @test all(data[2:2:100] .== 0.0)
        @test all(data[1:2:100] .== 1:50)
    end

    jldopen(fn, "w") do f
        dset = JLD2.create_dataset(f, "data", Float64, (20,20); allocate=true)
        dset[1:20, 1] = 10.0
        dset[20,20] = 20.0
    end
    data = load(fn, "data")
    @test all(data[1:20] .== 10.0)
    @test data[20,20] = 20.0
end
