using Test, JLD2
@testset "HDF5 compat test files" begin
    # These are test files copied from the HDF5.jl test suite
    cd(joinpath(@__DIR__,"test_files")) do

        fn = "compound.h5"
        jldopen(fn) do f 
            data = f["data"]
            @test data[1] == data[2]
            nt = data[1]
            @test nt.wgt == 1.0
            @test nt.xyz == [-2.4559041161056125, 0.43236207188504794, -0.5088338908493437]
            @test nt.uvw == [-0.44966656055677057, 0.6453930541533174, 0.6174688574881305]
            @test nt.E == 1.1915731810042547
        end

        # Should return some enum type and load names correctly
        fn = "h5ex_t_enum.h5"
        jldopen(fn) do f
            @test size(f["DS1"]) == (7,4)
        end 

        fn = "h5ex_t_array.h5"
        jldopen(fn) do f
            @test f["DS1"][1] == (0:-1:-4) .* [0,1,2]'
            @test f["DS1"][2] == hcat(collect(0:4), ones(Int,5), collect(2:-1:-2))
        end

        fn = "h5ex_t_float.h5"
        jldopen(fn) do f
            @test size(f["DS1"]) == (7,4)
            @test f["DS1"][9] ≈ 5/3
        end

        # Big Endian Integers are not implemented
        fn = "h5ex_t_int.h5"
        jldopen(fn) do f
            @test_broken f["DS1"]
        end

        fn = "h5ex_t_objref.h5"
        jldopen(fn) do f
            @test f["DS1"][1] === f["G1"]
            @test f["DS1"][2] === f["DS2"]
        end

        fn = "h5ex_t_opaque.h5"
        jldopen(fn) do f
            @test_broken f["DS1"]
        end

        fn = "h5ex_t_string.h5"
        jldopen(fn) do f
            @test_broken f["DS1"]
        end

        fn = "h5ex_t_vlen.h5"
        jldopen(fn) do f
            @test f["DS1"][1] == [3, 2, 1]
            @test f["DS1"][2] == [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]
        end

        fn = "h5ex_t_vlstring.h5"
        jldopen(fn) do f
            @test f["DS1"] == ["Parting", "is such", "sweet", "sorrow."]
        end

        fn = "nullterm_ascii.h5"
        jldopen(fn) do f
            @test f["test"] == "Hello World"
        end

        fn = "large_fractal_heap.h5"
        jldopen(fn) do f
            @test length(keys(f)) == 200000
        end

        fn = "netcdf.nc"
        jldopen(fn) do f
            @test f["hello"] == ones(5)
            @test_broken f["x"]
            @test_broken f["z"]
            @test f["grouped/data"] == 0:9
            @test_broken f["grouped/y"]
        end

        fn = "simple.nc"
        jldopen(fn) do f
            @test f["dim1"] == [2, 4, 6]
            @test f["dim2"] == ["a", "b", "c", "d"]
            @test f["mydata"] == Matrix(reshape(1:12, 4, 3))
            JLD2.load_attributes(f, "dim1") # not sure what to test for. just not erroring so far
            JLD2.load_attributes(f, "dim2")
            JLD2.load_attributes(f, "mydata")
        end

        # julia> using JLD
        # julia> struct A; x::Int; y::Float64; z::String; end
        # julia> save("jldstruct.jld", "a", A(1,2.0,"3"))
        fn = "jldstruct.jld"
        jldopen(fn) do f
            a = f["a"]
            @test a.x == 1
            @test a.y == 2.0
            @test a.z == "3"
        end

        fn = "chunking1.h5"
        jldopen(fn) do f
            @test f["uncompressed_chunks"] == reshape(1:1000., 25, 40)
            @test f["compressed_chunks"] == reshape(1:1000., 25, 40)
            @test_broken f["shuffle_compressed_chunks"] = reshape(1:1000, 25, 40)
            @test size(f["incomplete_allocation"]) == (50,50,10)
            @test f["incomplete_allocation"][1:50,1:50, 2] == reshape(1:2500, 50,50)
            #f["incomplete_allocation"][1,1,1] == 0 
        end
    end
end