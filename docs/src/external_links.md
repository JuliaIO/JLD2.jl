# External Links and Soft Links in JLD2

JLD2 supports external links and soft links, enabling cross-file references and flexible file organization patterns. This feature is fully compatible with the HDF5 specification and standard HDF5 tools.

## Overview

JLD2 supports three types of links:

- **Hard Links** (default): Direct pointers to objects within the same file
- **Soft Links**: Path-based references resolved at access time within the same file
- **External Links**: References to objects in different HDF5/JLD2 files

### Creating Soft Links

```julia
jldopen("example.jld2", "w") do f
    # Create data and groups
    f["dataset1"] = [1, 2, 3, 4, 5]
    group = JLD2.Group(f, "analysis")
    group["results"] = [10, 20, 30]

    # Create soft links
    create_soft_link!(f, "data_alias", "/dataset1")
    create_soft_link!(f, "results_link", "/analysis/results")
end

# Access via soft links
jldopen("example.jld2", "r") do f
    original = f["dataset1"]    # [1, 2, 3, 4, 5]
    via_link = f["data_alias"]  # [1, 2, 3, 4, 5] (same data)
end
```

### Creating External Links

```julia
using JLD2

# Create an external data file
jldsave("data.jld2";
        temperature=[23.5, 24.1, 22.8, 25.3],
        pressure=[1013.2, 1012.8, 1014.1, 1013.5])

# Create main file with external link
jldopen("main.jld2", "w") do f
    f["local_data"] = [1, 2, 3, 4, 5]

    # Create external links
    create_external_link!(f, "external_temp", "data.jld2", "/temperature")
    create_external_link!(f, "external_pressure", "data.jld2", "/pressure")
end

# Access external data transparently
jldopen("main.jld2", "r") do f
    local_data = f["local_data"]           # [1, 2, 3, 4, 5]
    external_temp = f["external_temp"]     # [23.5, 24.1, 22.8, 25.3]
    external_press = f["external_pressure"] # [1013.2, 1012.8, 1014.1, 1013.5]
end
```
