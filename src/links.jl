# Link Types for JLD2
# This module defines the abstract link type hierarchy to support
# hard links, soft links, and external links in JLD2 files

"""
    AbstractLink

Abstract base type for all link types in JLD2. Links represent connections
between names in groups and objects or paths in the HDF5 file structure.

The concrete subtypes are:
- `HardLink`: Direct pointer to an object header (traditional JLD2 behavior)
- `SoftLink`: Path string resolved at access time within the same file
- `ExternalLink`: Reference to an object in a different HDF5 file

See the HDF5 specification section 3.2.J for complete link message format details.
"""
abstract type AbstractLink end

"""
    HardLink <: AbstractLink

Represents a hard link that points directly to an object header within the same file.
This is the traditional link type used throughout JLD2 and maintains backward compatibility.

# Fields
- `target::RelOffset`: The file offset where the target object header is located

# HDF5 Details
Hard links correspond to HDF5 link type 0. They contain a direct address pointer
to the object header, making access very efficient but preventing cross-file references.
"""
struct HardLink <: AbstractLink
    target::RelOffset
end

"""
    SoftLink <: AbstractLink

Represents a soft link that contains a path string resolved at access time.
The path can be absolute (starting with '/') or relative to the containing group.

# Fields
- `path::String`: The path to resolve when the link is accessed

# HDF5 Details
Soft links correspond to HDF5 link type 1. They contain a path string that is
resolved when the link is dereferenced, allowing for more flexible file organization
but requiring path resolution overhead.

# Path Format
- Absolute paths: `/group/subgroup/dataset`
- Relative paths: `../other_group/dataset`
- Path separator is always '/' regardless of platform
"""
struct SoftLink <: AbstractLink
    path::String

    # Inner constructor with validation
    function SoftLink(path::String)
        isempty(path) && throw(ArgumentError("Soft link path cannot be empty"))
        new(path)
    end
end

"""
    ExternalLink <: AbstractLink

Represents an external link that references an object in a different HDF5 file.
This enables cross-file data dependencies and modular file organization.

# Fields
- `file_path::String`: Path to the external HDF5 file (relative or absolute)
- `object_path::String`: Path within the external file to the target object

# HDF5 Details
External links correspond to HDF5 link type 64. They contain two null-terminated
strings: the external file path and the object path within that file.

# Example
```julia
# Link to /data/temperature in external_file.h5
link = ExternalLink("./external_file.h5", "/data/temperature")
```
"""
struct ExternalLink <: AbstractLink
    file_path::String
    object_path::String

    # Inner constructor with validation
    function ExternalLink(file_path::String, object_path::String)
        isempty(file_path) && throw(ArgumentError("External file path cannot be empty"))
        isempty(object_path) && throw(ArgumentError("External object path cannot be empty"))

        # Normalize path separators and validate object path format
        normalized_object_path = replace(object_path, '\\' => '/')
        if !startswith(normalized_object_path, '/') && !isempty(normalized_object_path)
            # For HDF5 compatibility, object paths should typically be absolute
            @warn "External object path '$object_path' is relative - this may cause resolution issues"
        end

        new(file_path, normalized_object_path)
    end
end

# Convenience validation functions (struct constructors are used directly)

"""
    validate_soft_link_path(path::String)

Validate a soft link path. Throws ArgumentError if invalid.
"""
function validate_soft_link_path(path::String)
    isempty(path) && throw(ArgumentError("Soft link path cannot be empty"))
    return path
end

"""
    validate_external_link_paths(file_path::String, object_path::String)

Validate external link paths. Returns normalized paths or throws ArgumentError if invalid.
"""
function validate_external_link_paths(file_path::String, object_path::String)
    isempty(file_path) && throw(ArgumentError("External file path cannot be empty"))
    isempty(object_path) && throw(ArgumentError("External object path cannot be empty"))

    # Normalize path separators and validate object path format
    normalized_object_path = replace(object_path, '\\' => '/')
    if !startswith(normalized_object_path, '/') && !isempty(normalized_object_path)
        # For HDF5 compatibility, object paths should typically be absolute
        @warn "External object path '$object_path' is relative - this may cause resolution issues"
    end

    return (file_path, normalized_object_path)
end

# Constructors are now handled by inner constructors in the struct definitions above

# Display methods for better debugging

Base.show(io::IO, link::HardLink) = print(io, "HardLink($(link.target))")
Base.show(io::IO, link::SoftLink) = print(io, "SoftLink(\"$(link.path)\")")
Base.show(io::IO, link::ExternalLink) = print(io, "ExternalLink(\"$(link.file_path)\", \"$(link.object_path)\")")

# Equality and hashing for proper dictionary behavior

Base.:(==)(a::HardLink, b::HardLink) = a.target == b.target
Base.:(==)(a::SoftLink, b::SoftLink) = a.path == b.path
Base.:(==)(a::ExternalLink, b::ExternalLink) = a.file_path == b.file_path && a.object_path == b.object_path

Base.hash(link::HardLink, h::UInt) = hash(link.target, h)
Base.hash(link::SoftLink, h::UInt) = hash(link.path, h)
Base.hash(link::ExternalLink, h::UInt) = hash((link.file_path, link.object_path), h)

# Type utilities for compatibility

"""
    is_hard_link(link::AbstractLink)

Returns `true` if the link is a hard link, `false` otherwise.
This can be used for performance optimizations where hard links can be handled more efficiently.
"""
is_hard_link(link::AbstractLink) = isa(link, HardLink)

"""
    is_soft_link(link::AbstractLink)

Returns `true` if the link is a soft link, `false` otherwise.
"""
is_soft_link(link::AbstractLink) = isa(link, SoftLink)

"""
    is_external_link(link::AbstractLink)

Returns `true` if the link is an external link, `false` otherwise.
"""
is_external_link(link::AbstractLink) = isa(link, ExternalLink)

"""
    requires_resolution(link::AbstractLink)

Returns `true` if the link requires resolution (soft or external links),
`false` for hard links that can be accessed directly.
"""
requires_resolution(link::AbstractLink) = !is_hard_link(link)

# Note: HardLink(offset::RelOffset) constructor is automatically provided by the struct definition

"""
    get_target(link::HardLink)

Extract the target RelOffset from a hard link.
This provides compatibility with existing code that expects RelOffset values.
"""
get_target(link::HardLink) = link.target