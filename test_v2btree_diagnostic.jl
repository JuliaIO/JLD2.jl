#!/usr/bin/env julia

using JLD2

# Compare with extensible array (which works)
println("Testing dimension handling...")
println()

# Test extensible array file
ext_data = jldopen("test_latest_extensible.h5") do f
    f["latest_ext"]
end

println("Extensible Array:")
println("  Size: $(size(ext_data))")
println()

# Test v2 btree file
v2_data = jldopen("test_v4_v2btree.h5") do f
    f["v4_v2btree"]
end

println("V2 B-tree:")
println("  Size: $(size(v2_data))")
println()

# Check with h5py
println("Expected from h5py:")
run(`python3 -c "import h5py; print('Extensible:', h5py.File('test_latest_extensible.h5')['latest_ext'].shape); print('V2Btree:', h5py.File('test_v4_v2btree.h5')['v4_v2btree'].shape)"`)
