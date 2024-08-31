using JLD2, Test

@testset "Dataset API" begin
    cd(mktempdir()) do 
        fn = "test.jld2"
        jldopen(fn, "w") do f
            dset = JLD2.create_dataset(f, "d")
            JLD2.add_attribute(dset, "description", "A description of what this is for.")

            # Check that double attributes are not allowed
            @test_throws ArgumentError JLD2.add_attribute(dset, "description", "I changed my mind.")

            JLD2.write_dataset(dset, 42)
        end
        jldopen(fn, "a") do f
            @test f["d"] == 42
            dset = JLD2.get_dataset(f, "d")
            attrs = JLD2.attributes(dset)
            only(attrs) == ("description" => "A description of what this is for.")
             # Add another attribute that has to be attached to the existing (written dataset)
            JLD2.add_attribute(dset, "addition", "A different description.")

            # Check that double attributes are not allowed
            @test_throws ArgumentError JLD2.add_attribute(dset, "addition", "A very different description.")
        end

        jldopen(fn, "w") do f
            dset = JLD2.create_dataset(f, "d")
            dset.filters = JLD2.FilterPipeline([JLD2.Filter(1, 0, "", [])])
            JLD2.write_dataset(dset, zeros(1000,1000))
        end
        @test load(fn)["d"] == zeros(1000,1000)
    end
end

@testset "Slicing & Updating" begin
    cd(mktempdir()) do 
        fn = "test.jld2"
        jldsave(fn; a=42, b = [42 43 44; 45 46 47], c = [(0x00, 1f0), (0x42, 2f0)])
        jldopen(fn) do f
            dset = JLD2.get_dataset(f, "a")
            @test dset[] == 42
            
            dset = JLD2.get_dataset(f, "b")
            @test dset[] == [42 43 44; 45 46 47]
            @test dset[1] == 42
            @test dset[1,1] == 42
            @test dset[1:2, 1:2] == [42 43; 45 46]
            @test dset[1,1:2:3] == [42, 44]
            @test_throws BoundsError dset[7]
            @test_throws BoundsError dset[2,4]
            @test_throws ArgumentError dset[1] = 1
        end
        jldopen(fn, "a") do f
            dset = JLD2.get_dataset(f, "b")
            dset[2] = -1
            @test dset[] == [42 43 44; -1 46 47]
            dset[1,1:2:3] = [1,5]
            @test dset[] == [1 43 5; -1 46 47]

            dset = JLD2.get_dataset(f, "c")
            dset[2] = (0xff, 0f0)
            @test f["c"] == [(0x00, 1f0), (0xff, 0f0)]
        end
    end
end