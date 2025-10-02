using JLD2

# Create test file with single chunk datasets
filename = "phase1_single_chunk_test.jld2"

# Test 1: Simple 1D array
data1d = Float32[1.0, 2.0, 3.0, 4.0, 5.0]

# Test 2: 2D array
data2d = reshape(Float32.(1:20), 4, 5)

# Test 3: With compression
data_compressed = Float32.(1:100)

# Write datasets
jldopen(filename, "w") do f
    # 1D uncompressed
    wca1 = WriteChunkedArray(data1d, chunks=size(data1d))
    write_chunked(f, "data1d", wca1)

    # 2D uncompressed
    wca2 = WriteChunkedArray(data2d, chunks=size(data2d))
    write_chunked(f, "data2d", wca2)

    # Compressed
    wca3 = WriteChunkedArray(data_compressed, chunks=size(data_compressed), filters=true)
    write_chunked(f, "compressed", wca3)
end

println("Created $filename")

# Verify with JLD2
jldopen(filename, "r") do f
    println("\nReading with JLD2:")
    data1d_read = f["data1d"]
    println("  data1d: ", data1d_read == data1d ? "✓" : "✗")

    data2d_read = f["data2d"]
    println("  data2d: ", data2d_read == data2d ? "✓" : "✗")

    data_comp_read = f["compressed"]
    println("  compressed: ", data_comp_read == data_compressed ? "✓" : "✗")
end

println("\nNow validate with:")
println("  python3 -c \"import h5py; f = h5py.File('$filename'); print(list(f.keys())); print(f['data1d'][:]); print(f['data2d'][:]); print(f['compressed'][:])\"")
println("  h5ls -rv $filename")
