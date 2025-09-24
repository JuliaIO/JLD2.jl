# Path Resolution for JLD2 External Links
# This module provides secure path resolution functionality for external file links

"""
    resolve_external_file_path(current_file_path::String, external_file_path::String) -> String

Resolve an external file path relative to the current file's directory.

# Arguments
- `current_file_path`: Path to the current JLD2 file (used as reference for relative paths)
- `external_file_path`: The external file path from the external link (may be relative or absolute)

# Returns
The resolved absolute path to the external file.

# Path Processing
- Normalizes path separators for cross-platform compatibility
- Resolves relative paths relative to the current file's directory

# Examples
```julia
# Relative external file path
current = "/home/user/data/main.jld2"
external = "./external_data.jld2"
resolved = resolve_external_file_path(current, external)
# Result: "/home/user/data/external_data.jld2"

# Absolute external file path
current = "/home/user/data/main.jld2"
external = "/shared/datasets/public.jld2"
resolved = resolve_external_file_path(current, external)
# Result: "/shared/datasets/public.jld2"
```
"""
function resolve_external_file_path(current_file_path::String, external_file_path::String)
    # Handle absolute paths directly
    if isabspath(external_file_path)
        resolved_path = abspath(external_file_path)
    else
        # Resolve relative to current file's directory
        current_dir = dirname(current_file_path)
        resolved_path = abspath(joinpath(current_dir, external_file_path))
    end

    return resolved_path
end


"""
    resolve_soft_link_path(group_path::String, soft_link_path::String) -> String

Resolve a soft link path within the same HDF5 file.

# Arguments
- `group_path`: Current group's absolute path within the HDF5 file (e.g., "/data/measurements")
- `soft_link_path`: The soft link target path (may be absolute or relative)

# Returns
The resolved absolute path within the HDF5 file.

# Path Resolution Rules
- Absolute paths (starting with '/') are used as-is
- Relative paths are resolved relative to the containing group
- Path separators are normalized to '/' for HDF5 compatibility
- ".." components navigate up the group hierarchy

# Examples
```julia
# Absolute soft link
group_path = "/data/measurements"
soft_path = "/results/analysis"
resolved = resolve_soft_link_path(group_path, soft_path)
# Result: "/results/analysis"

# Relative soft link
group_path = "/data/measurements"
soft_path = "../calibration/offset"
resolved = resolve_soft_link_path(group_path, soft_path)
# Result: "/data/calibration/offset"
```
"""
function resolve_soft_link_path(group_path::String, soft_link_path::String)
    # Normalize path separators to HDF5 standard
    normalized_soft_path = replace(soft_link_path, '\\' => '/')
    normalized_group_path = replace(group_path, '\\' => '/')

    # Handle absolute paths
    if startswith(normalized_soft_path, '/')
        return normalize_hdf5_path(normalized_soft_path)
    end

    # Handle relative paths
    # Start from the group itself (not the parent directory)
    base_path = normalized_group_path

    # Handle empty base path (root group case)
    if isempty(base_path) || base_path == "."
        base_path = "/"
    end

    # Resolve relative path components
    path_components = split(normalized_soft_path, '/')
    current_components = split(base_path, '/', keepempty=false)

    for component in path_components
        if component == ".." && !isempty(current_components)
            pop!(current_components)  # Go up one level
        elseif component != "." && !isempty(component)
            push!(current_components, component)  # Add component
        end
        # Ignore "." and empty components
    end

    # Reconstruct path
    if isempty(current_components)
        return "/"
    else
        return "/" * join(current_components, "/")
    end
end

"""
    normalize_hdf5_path(path::String) -> String

Normalize an HDF5 path to standard format.

# Rules
- Ensures path starts with '/' (absolute)
- Normalizes path separators to '/'
- Removes redundant components like "//" or "/./"
- Handles ".." components properly

# Examples
```julia
normalize_hdf5_path("/data//measurements/./temp") # => "/data/measurements/temp"
normalize_hdf5_path("data/measurements") # => "/data/measurements"
```
"""
function normalize_hdf5_path(path::String)
    # Normalize separators
    normalized = replace(path, '\\' => '/')

    # Ensure absolute path
    if !startswith(normalized, '/')
        normalized = "/" * normalized
    end

    # Split, process, and rejoin components
    components = split(normalized, '/', keepempty=false)
    result_components = String[]

    for component in components
        if component == ".."
            # Go up one level (remove last component if any)
            if !isempty(result_components)
                pop!(result_components)
            end
        elseif component != "." && !isempty(component)
            # Add non-empty, non-current-directory components
            push!(result_components, component)
        end
        # Ignore "." and empty components
    end

    # Reconstruct path
    if isempty(result_components)
        return "/"
    else
        return "/" * join(result_components, "/")
    end
end

