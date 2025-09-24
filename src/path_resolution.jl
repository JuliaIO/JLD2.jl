# Path Resolution for JLD2 External Links
# This module provides secure path resolution functionality for external file links

"""
    resolve_external_file_path(current_file_path::String, external_file_path::String) -> String

Resolve an external file path relative to the current file's directory.
Performs security validation to prevent directory traversal attacks.

# Arguments
- `current_file_path`: Path to the current JLD2 file (used as reference for relative paths)
- `external_file_path`: The external file path from the external link (may be relative or absolute)

# Returns
The resolved absolute path to the external file.

# Security
- Validates that the resolved path doesn't escape intended directories via ".." components
- Normalizes path separators for cross-platform compatibility
- Rejects paths that would allow directory traversal attacks

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
    # Handle absolute paths directly (but still validate)
    if isabspath(external_file_path)
        resolved_path = abspath(external_file_path)
    else
        # Resolve relative to current file's directory
        current_dir = dirname(current_file_path)
        resolved_path = abspath(joinpath(current_dir, external_file_path))
    end

    # Security validation: prevent directory traversal attacks
    validate_resolved_path_security(resolved_path, dirname(current_file_path))

    return resolved_path
end

"""
    validate_resolved_path_security(resolved_path::String, base_dir::String)

Validate that a resolved path doesn't represent a security risk via directory traversal.

# Arguments
- `resolved_path`: The fully resolved absolute path
- `base_dir`: The base directory for relative path resolution

# Security Checks
- Validates that ".." components don't escape reasonable directory boundaries
- Currently uses a conservative approach to prevent obvious attacks
- Note: This is basic validation; production systems may need additional restrictions

# Throws
`ArgumentError` if the path is deemed unsafe.
"""
function validate_resolved_path_security(resolved_path::String, base_dir::String)
    # Normalize paths for comparison
    resolved_norm = normpath(resolved_path)
    base_norm = normpath(base_dir)

    # Basic check: reject paths with ".." that could be dangerous
    # This is a conservative approach - we could be more sophisticated about
    # allowing legitimate use cases while blocking attacks

    # Check for excessive upward traversal (more than 3 levels up from base directory)
    # This allows reasonable relative navigation while blocking obvious attacks like ../../../etc/passwd
    relative_to_base = try
        relpath(resolved_norm, base_norm)
    catch
        # If relpath fails, paths might be on different drives/roots - allow absolute paths
        return
    end

    # Count upward traversals
    path_components = split(relative_to_base, ['/', '\\'])
    upward_count = count(x -> x == "..", path_components)
    if upward_count > 3
        throw(ArgumentError("External file path represents excessive directory traversal ($(upward_count) levels up): $resolved_path"))
    end

    # Additional check: reject paths that contain suspicious patterns
    if contains(resolved_norm, "..") && (contains(resolved_norm, "etc") || contains(resolved_norm, "sys") || contains(resolved_norm, "proc"))
        throw(ArgumentError("External file path contains potentially dangerous system directory access: $resolved_path"))
    end
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

"""
    ExternalFileAccessPolicy

Configuration for external file access control.

# Fields
- `allowed_directories`: Set of directories where external files may be accessed
- `blocked_directories`: Set of directories that are explicitly blocked
- `require_same_directory`: If true, external files must be in same directory as current file
- `max_traversal_depth`: Maximum number of ".." components allowed in paths
- `allowed_extensions`: Set of allowed file extensions (empty = allow all)
- `audit_access`: If true, log all external file access attempts
"""
mutable struct ExternalFileAccessPolicy
    allowed_directories::Set{String}
    blocked_directories::Set{String}
    require_same_directory::Bool
    max_traversal_depth::Int
    allowed_extensions::Set{String}
    audit_access::Bool

    function ExternalFileAccessPolicy()
        new(
            Set{String}(),                                    # allowed_directories
            Set(["/etc", "/sys", "/proc", "/root", "/boot"]), # blocked_directories
            false,                                            # require_same_directory
            3,                                                # max_traversal_depth
            Set([".jld2", ".jld", ".h5", ".hdf5", ".hdf"]),  # allowed_extensions
            false                                             # audit_access
        )
    end
end

# Global access policy (can be configured by users)
const EXTERNAL_FILE_ACCESS_POLICY = ExternalFileAccessPolicy()

"""
    configure_external_file_access!(;
        allowed_directories=nothing,
        blocked_directories=nothing,
        require_same_directory=nothing,
        max_traversal_depth=nothing,
        allowed_extensions=nothing,
        audit_access=nothing
    )

Configure the external file access policy.

# Keyword Arguments
- `allowed_directories`: Vector of directory paths to allow (empty = allow all)
- `blocked_directories`: Vector of directory paths to block
- `require_same_directory`: Require external files to be in same directory as current file
- `max_traversal_depth`: Maximum number of ".." components allowed
- `allowed_extensions`: Vector of allowed file extensions (empty = allow all)
- `audit_access`: Enable audit logging of external file access

# Examples
```julia
# Restrict to data directory only
configure_external_file_access!(allowed_directories=["/data"], require_same_directory=false)

# Very restrictive: same directory only
configure_external_file_access!(require_same_directory=true, max_traversal_depth=0)

# Enable auditing
configure_external_file_access!(audit_access=true)
```
"""
function configure_external_file_access!(;
    allowed_directories=nothing,
    blocked_directories=nothing,
    require_same_directory=nothing,
    max_traversal_depth=nothing,
    allowed_extensions=nothing,
    audit_access=nothing
)
    policy = EXTERNAL_FILE_ACCESS_POLICY

    if allowed_directories !== nothing
        empty!(policy.allowed_directories)
        for dir in allowed_directories
            push!(policy.allowed_directories, normpath(abspath(dir)))
        end
    end

    if blocked_directories !== nothing
        empty!(policy.blocked_directories)
        for dir in blocked_directories
            push!(policy.blocked_directories, normpath(abspath(dir)))
        end
    end

    if require_same_directory !== nothing
        policy.require_same_directory = require_same_directory
    end

    if max_traversal_depth !== nothing
        policy.max_traversal_depth = max_traversal_depth
    end

    if allowed_extensions !== nothing
        empty!(policy.allowed_extensions)
        for ext in allowed_extensions
            push!(policy.allowed_extensions, lowercase(startswith(ext, ".") ? ext : ".$ext"))
        end
    end

    if audit_access !== nothing
        policy.audit_access = audit_access
    end

    return policy
end

"""
    get_external_file_access_policy() -> ExternalFileAccessPolicy

Get the current external file access policy.
"""
function get_external_file_access_policy()
    return EXTERNAL_FILE_ACCESS_POLICY
end

"""
    is_safe_external_path(file_path::String, current_file_path::String="") -> Bool

Check if an external file path is considered safe for access according to the current access policy.

# Arguments
- `file_path`: The external file path to check
- `current_file_path`: The current file path (for same-directory checks)

# Returns
`true` if the path is allowed by the current access policy, `false` otherwise.

# Access Control
Uses the global EXTERNAL_FILE_ACCESS_POLICY to determine access permissions.
"""
function is_safe_external_path(file_path::String, current_file_path::String="")
    policy = EXTERNAL_FILE_ACCESS_POLICY
    normalized = normpath(abspath(file_path))

    # Audit logging if enabled
    if policy.audit_access
        @info "External file access attempt" file_path=normalized current_file=current_file_path
    end

    # Check allowed directories (if any specified)
    if !isempty(policy.allowed_directories)
        allowed = false
        for allowed_dir in policy.allowed_directories
            if startswith(normalized, allowed_dir)
                allowed = true
                break
            end
        end
        if !allowed
            if policy.audit_access
                @warn "External file access denied: not in allowed directories" file_path=normalized
            end
            return false
        end
    end

    # Check blocked directories
    for blocked_dir in policy.blocked_directories
        if startswith(normalized, blocked_dir)
            if policy.audit_access
                @warn "External file access denied: in blocked directory" file_path=normalized blocked_dir=blocked_dir
            end
            return false
        end
    end

    # Check same directory requirement
    if policy.require_same_directory && !isempty(current_file_path)
        current_dir = dirname(normpath(abspath(current_file_path)))
        external_dir = dirname(normalized)
        if current_dir != external_dir
            if policy.audit_access
                @warn "External file access denied: not in same directory" file_path=normalized current_dir=current_dir external_dir=external_dir
            end
            return false
        end
    end

    # Check directory traversal depth
    if contains(file_path, "..")
        path_components = split(file_path, ['/', '\\'])
        upward_count = count(x -> x == "..", path_components)
        if upward_count > policy.max_traversal_depth
            if policy.audit_access
                @warn "External file access denied: excessive directory traversal" file_path=normalized upward_count=upward_count max_allowed=policy.max_traversal_depth
            end
            return false
        end
    end

    # Check file extension
    if !isempty(policy.allowed_extensions)
        ext = lowercase(splitext(normalized)[2])
        if !isempty(ext) && !(ext in policy.allowed_extensions)
            if policy.audit_access
                @warn "External file access denied: disallowed extension" file_path=normalized extension=ext allowed_extensions=policy.allowed_extensions
            end
            return false
        end
    else
        # If no restrictions, still warn about unexpected extensions
        ext = lowercase(splitext(normalized)[2])
        if !isempty(ext) && !(ext in [".jld2", ".jld", ".h5", ".hdf5", ".hdf"])
            @warn "External file has unexpected extension '$ext': $file_path"
        end
    end

    if policy.audit_access
        @info "External file access granted" file_path=normalized
    end

    return true
end