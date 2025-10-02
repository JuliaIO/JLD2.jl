using JLD2

# Create a simple test file with a group
fname = tempname() * ".jld2"
try
    f = jldopen(fname, "w")
    g = JLD2.Group(f)
    f["testgroup"] = g
    close(f)

    # Try to read it back
    f = jldopen(fname, "r")
    println("File opened successfully")
    g = f["testgroup"]
    println("Group loaded successfully")
    close(f)
finally
    isfile(fname) && rm(fname)
end

println("Test passed!")