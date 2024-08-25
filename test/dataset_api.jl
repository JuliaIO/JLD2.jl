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