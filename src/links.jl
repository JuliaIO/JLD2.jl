"""
    Link

Unified link type representing hard links, soft links, and external links.
Uses a discriminated union approach for type stability.

# Link types (determined by field values):
- Hard link: `path` and `external_file` empty (offset may be UNDEFINED_ADDRESS for uninitialized)
- Soft link: `path` non-empty, `external_file` empty
- External link: `external_file` non-empty
"""
struct Link
    offset::RelOffset    # Hard link target (may be UNDEFINED_ADDRESS for uninitialized hard links)
    path::String         # Object path within file (for soft/external links)
    external_file::String # External file path (empty for hard/soft links)

    # Inner constructor - allows all combinations including uninitialized hard links
    function Link(offset::RelOffset, path::String, external_file::String)
        new(offset, path, external_file)
    end
end

# User-facing constructor with keyword argument for external links
"""
    Link(path::String; file::Union{String,Nothing}=nothing) -> Link

Create a soft link or external link to an object.

# Arguments
- `path`: Path to the target object
- `file`: (Optional) External file path. If provided, creates an external link to `path` in `file`.
          If omitted, creates a soft link to `path` in the current file.

# Examples
```julia
# Soft link (within same file)
g["alias"] = JLD2.Link("/path/to/data")

# External link (to another file)
g["external"] = JLD2.Link("/path/to/data"; file="/path/to/external/file.jld2")
```
"""
function Link(path::String; file::Union{AbstractString,Nothing}=nothing)
    isempty(path) && throw(ArgumentError("Link path cannot be empty"))
    if isnothing(file)
        Link(UNDEFINED_ADDRESS, path, "")  # Soft link
    else
        isempty(file) && throw(ArgumentError("External file path cannot be empty"))
        Link(UNDEFINED_ADDRESS, replace(path, '\\' => '/'), String(file))  # External link
    end
end

# Internal constructor for hard links (used by JLD2 internals, not exported)
Link(offset::RelOffset) = Link(offset, "", "")
Link() = Link(UNDEFINED_ADDRESS, "", "")

# Type predicates (zero-cost field checks)
# Note: Hard links are identified by having empty path and external_file fields
# The offset may be UNDEFINED_ADDRESS for uninitialized hard links
is_hard_link(link::Link) = isempty(link.path) && isempty(link.external_file)
is_external_link(link::Link) = !isempty(link.external_file)
is_soft_link(link::Link) = !isempty(link.path) && isempty(link.external_file)
is_set(link::Link) = (link.offset != UNDEFINED_ADDRESS) || !isempty(link.path)

# Display methods
function Base.show(io::IO, link::Link)
    if is_hard_link(link)
        if link.offset == UNDEFINED_ADDRESS
            print(io, "Link(hard: uninitialized)")
        else
            print(io, "Link(hard: ", link.offset, ")")
        end
    elseif is_soft_link(link)
        print(io, "Link(soft: \"", link.path, "\")")
    else
        print(io, "Link(external: \"", link.external_file, "\", \"", link.path, "\")")
    end
end

# Equality and hashing
Base.:(==)(a::Link, b::Link) =
    a.offset == b.offset && a.path == b.path && a.external_file == b.external_file

Base.hash(link::Link, h::UInt) = hash((link.offset, link.path, link.external_file), h)

"""
    load_external_dataset(current_file::JLDFile, link::Link)

Load a dataset from an external file referenced by an external link.
"""
function load_external_dataset(current_file, link::Link)
    external_file = normpath(joinpath(dirname(current_file.path), link.external_file))
    isfile(external_file) || throw(ArgumentError("External file not found: $external_file"))

    # Check if file is already open
    lock(OPEN_FILES_LOCK) do
        if external_file in keys(OPEN_FILES)
            existing_file = OPEN_FILES[external_file].value
            !isnothing(existing_file) && return existing_file[link.path]
        end
    end

    jldopen(external_file, "r") do f
        f[link.path]
    end
end
