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
"""
struct SoftLink <: AbstractLink
    path::String

    function SoftLink(path::String)
        isempty(path) && throw(ArgumentError("Soft link path cannot be empty"))
        new(path)
    end
end

"""
    ExternalLink <: AbstractLink

Represents an external link that references an object in a different HDF5 file.
This enables cross-file data dependencies and modular file organization.

## Fields
- `file_path::String`: Path to the external HDF5 file (relative or absolute)
- `object_path::String`: Path within the external file to the target object

## Example
```julia
# Link to /data/temperature in external_file.h5
link = ExternalLink("./external_file.h5", "/data/temperature")
```
"""
struct ExternalLink <: AbstractLink
    file_path::String
    object_path::String

    # Inner constructor with validation
    function ExternalLink(file_path::AbstractString, object_path::AbstractString)
        isempty(file_path) && throw(ArgumentError("External file path cannot be empty"))
        isempty(object_path) && throw(ArgumentError("External object path cannot be empty"))

        # Normalize path separators and validate object path format
        normalized_object_path = replace(object_path, '\\' => '/')
        new(file_path, normalized_object_path)
    end
end

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
