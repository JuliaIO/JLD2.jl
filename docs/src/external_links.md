# Links in JLD2

JLD2 supports three types of links compatible with the HDF5 specification:

- **Hard Links** (default): Direct pointers to objects within the same file
- **Soft Links**: Path-based symbolic links resolved at access time
- **External Links**: References to objects in different files

## Usage

```@example
using JLD2
jldopen("file.jld2", "w") do f
    f["data"] = [1, 2, 3, 4, 5]

    # Soft link (within same file)
    f["data_alias"] = JLD2.Link("/data")

    # External link (to different file)
    f["remote_data"] = JLD2.Link("/dataset"; file="other.jld2")
end

# Create external file (before or after)
jldsave("other.jld2"; dataset="external data")

# Access links transparently
jldopen("file.jld2", "r") do f
    f["data_alias"]   # Returns [1, 2, 3, 4, 5]
    f["remote_data"]  # Loads from other.jld2
end
```
