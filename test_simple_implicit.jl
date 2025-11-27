using JLD2

fn = "/tmp/test_simple.jld2"

# Write
data = collect(reshape(Float32.(1:12), 3, 4))
println("Writing: ", data)

jldopen(fn, "w") do f
    JLD2.Chunking.write_chunked(f, "data", data; chunk=(3, 2), indexing=:implicit_index, fill_value=Float32(0.0))
end

println("File size: ", filesize(fn))

# Read
result = jldopen(fn, "r") do f
    f["data"]
end

println("Read: ", result)
println("Match: ", result == data)
