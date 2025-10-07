# V1 B-tree Core Algorithms
# Key comparison, node insertion, and tree management
"""
    calculate_max_entries(f::JLDFile, dimensionality::UInt8, target_node_size::Int = 4096)::UInt16

Calculate the maximum number of entries that can fit in a V1 B-tree node
for the given dimensionality and target node size.

Algorithm:
1. Calculate fixed overhead (signature, header, sibling pointers)
2. Calculate size per entry (key + child pointer)
3. Account for extra key at end
4. Determine max entries that fit in target size
"""
function calculate_max_entries(f::JLDFile, dimensionality::UInt8, target_node_size::Int = 4096)::UInt16
    # Fixed overhead per node
    header_size = 4 + 1 + 1 + 2 + 2*jlsizeof(RelOffset)  # signature + type + level + entries + siblings

    # Size per entry
    key_size = 4 + 4 + 8 * (dimensionality + 1)  # chunk_size + filter_mask + indices
    child_size = jlsizeof(RelOffset)
    entry_size = key_size + child_size

    # Extra key at end
    extra_key_size = key_size

    # Calculate max entries that fit in target size
    available_space = target_node_size - header_size - extra_key_size
    max_entries = available_space ÷ entry_size

    return UInt16(max(1, min(max_entries, 64)))  # At least 1, at most 64 (HDF5 V1 B-tree limit)
end
