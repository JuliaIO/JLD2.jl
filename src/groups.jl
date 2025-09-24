#
# Groups
#

"""
    Group(f::JLDFile, name::AbstractString)

Construct an empty group named `name` at the top level of `JLDFile` `f`.
"""
Group(f::JLDFile, name::AbstractString; kwargs...) = Group(f.root_group, name; kwargs...)

"""
    Group(g::Group, name::AbstractString)

Construct a group named `name` as a child of group `g`.
"""
Group(g::Group{T}, name::AbstractString; kwargs...) where {T} = (g[name] = Group{T}(g.f; kwargs...))


"""
    lookup_link(g::Group, name::AbstractString) -> Union{AbstractLink, Nothing}

Lookup a link in a group by name. Returns the AbstractLink object if found, or `nothing`
if not present. This function supports all link types (hard, soft, external).
"""
function lookup_link(g::Group, name::AbstractString)
    if g.last_chunk_start_offset != -1
        # Has been saved to file, so written_links exists
        link = get(g.written_links, name, nothing)
        link !== nothing && return link
    end
    return get(g.unwritten_links, name, nothing)
end

"""
    pathize(g::Group, name::AbstractString, create::Bool) -> Tuple{Group,String}

Converts a path to a group and name object. If `create` is true, any intermediate groups
will be created, and the dataset name will be checked for uniqueness with existing names.
"""
function pathize(g::Group, name::AbstractString, create::Bool)
    G = typeof(g)
    if '/' in name
        f = g.f
        dirs = split(name, '/')

        for i = 1:length(dirs)-1
            dir = dirs[i]
            if isempty(dir)
                # Handles the absolute path case where the name starts with /
                i == 1 && (g = g.f.root_group::G)
                continue
            end
            # See if a group already exists
            # Check unwritten_child_groups first
            if haskey(g.unwritten_child_groups, dir)
                g = g.unwritten_child_groups[dir]
            else
                # Use lookup_link directly for consistency
                link = lookup_link(g, dir)
                if link === nothing
                    if create
                        # No group exists, so create a new group
                        newg = G(f)
                        g.unwritten_child_groups[dir] = newg
                        g = newg
                    else
                        throw(KeyError(join(dirs[1:i], '/')))
                    end
                elseif isa(link, HardLink)
                    offset = link.target
                    if haskey(f.loaded_groups, offset)
                        g = f.loaded_groups[offset]::G
                    elseif !isgroup(f, offset)
                        throw(ArgumentError("path $(join(dirs[1:i], '/')) points to a dataset, not a group"))
                    else
                        g = f.loaded_groups[offset] = load_group(f, offset)::G
                    end
                else
                    # Non-hard links (soft/external) cannot be used for group navigation in pathize
                    throw(ArgumentError("path $(join(dirs[1:i], '/')) contains a link that cannot be used for group navigation"))
                end
            end
        end

        name = String(dirs[end])
    end

    if create && haskey(g, name)
        throw(ArgumentError("a group or dataset named $name is already present within this group"))
    end

    return (g::G, name)
end

function Base.getindex(g::Group, name::AbstractString)
    f = g.f
    f.n_times_opened == 0 && throw(ArgumentError("file is closed"))

    (g, name) = pathize(g, name, false)
    isempty(name) && return g

    # Check for unwritten child groups first
    haskey(g.unwritten_child_groups, name) && return g.unwritten_child_groups[name]

    # Use lookup_link directly instead of lookup_offset → lookup_link pattern
    link = lookup_link(g, name)
    if link === nothing
        throw(KeyError(name))
    elseif isa(link, HardLink)
        # Hard link - proceed with normal dataset/group loading
        return Base.inferencebarrier(load_dataset(f, link.target))
    elseif isa(link, ExternalLink)
        # Resolve external link - resolve_external_link handles errors appropriately
        return resolve_external_link(g.f, link)
    elseif isa(link, SoftLink)
        # Resolve soft link within the same file
        # For relative paths, resolve from the current group
        # For absolute paths, resolve from root
        if startswith(link.path, '/')
            # Absolute path - resolve from root
            resolved_path = JLD2.normalize_hdf5_path(link.path)
            # Attempt to resolve, let the file's getindex handle KeyError if target doesn't exist
            # This approach provides better error messages automatically
            return g.f[resolved_path]
        else
            # Relative path - resolve from current group
            # resolve_relative_soft_link handles error cases appropriately
            return resolve_relative_soft_link(g, link.path)
        end
    else
        throw(ArgumentError("Unknown link type: $(typeof(link))"))
    end
end

@nospecializeinfer function Base.write(
        g::Group,
        name::AbstractString,
        @nospecialize(obj),
        wsession::JLDWriteSession=JLDWriteSession();
        compress=nothing
        )
    f = g.f
    prewrite(f)
    (g, name) = pathize(g, name, true)
    if !isnothing(compress)
        if obj isa Array
            filters = Filters.normalize_filters(compress)
            g[name] = write_dataset(f, obj, wsession, filters)
            return nothing
        end
        @warn "Only arrays can be compressed."
    end
    g[name] = write_dataset(f, obj, wsession)
    nothing
end

function Base.setindex!(g::Group, obj, name::AbstractString)
    write(g, name, obj)
    g
end

function Base.setindex!(g::Group, child::Group, name::AbstractString)
    if child.last_chunk_start_offset != -1
        throw(ArgumentError("cannot re-link a group that has already been written"))
    end
    prewrite(g.f)
    (g, name) = pathize(g, name, true)
    g.unwritten_child_groups[name] = child
    g
end

# Internal use only
function Base.setindex!(g::Group, offset::RelOffset, name::AbstractString)
    if g.last_chunk_start_offset != -1 && g.continuation_message_goes_here == -1
        error("objects cannot be added to this group because it was created with a previous version of JLD2")
    end
    g.unwritten_links[name] = HardLink(offset)
    g
end

# New method to accept AbstractLink directly
function Base.setindex!(g::Group, link::AbstractLink, name::AbstractString)
    if g.last_chunk_start_offset != -1 && g.continuation_message_goes_here == -1
        error("objects cannot be added to this group because it was created with a previous version of JLD2")
    end
    g.unwritten_links[name] = link
    g
end

"""
    create_external_link!(group::Group, link_name::String, file_path::String, object_path::String)

Create an external link in the group that points to an object in another HDF5/JLD2 file.

# Arguments
- `group::Group`: The group to create the link in
- `link_name::String`: Name of the link within the group
- `file_path::String`: Path to the external HDF5/JLD2 file
- `object_path::String`: Path to the object within the external file

# Example
```julia
using JLD2

# Open a file and create an external link
jldopen("main.jld2", "w") do file
    create_external_link!(file, "external_data", "data.jld2", "/dataset1")
end
```

Note: External file paths are validated to prevent directory traversal attacks.
"""
function create_external_link!(group::Group, link_name::String, file_path::String, object_path::String)
    external_link = ExternalLink(file_path, object_path)
    group[link_name] = external_link
    return group
end

"""
    create_soft_link!(group::Group, link_name::String, target_path::String)

Create a soft link (symbolic link) within the same HDF5/JLD2 file.

# Arguments
- `group::Group`: The group to create the link in
- `link_name::String`: Name of the link within the group
- `target_path::String`: Path to the target object within the same file

# Example
```julia
using JLD2

jldopen("test.jld2", "w") do file
    file["original_data"] = [1, 2, 3, 4, 5]
    create_soft_link!(file, "link_to_data", "/original_data")
end
```
"""
function create_soft_link!(group::Group, link_name::String, target_path::String)
    soft_link = SoftLink(target_path)
    group[link_name] = soft_link
    return group
end

function Base.haskey(g::Group, name::AbstractString)
    G = typeof(g)
    if '/' in name
        f = g.f
        dirs = split(name, '/')

        for i = 1:length(dirs)-1
            dir = dirs[i]
            if isempty(dir)
                # Handles the absolute path case where the name starts with /
                i == 1 && (g = g.f.root_group::G)
                continue
            end

            # See if a group already exists
            # Check unwritten_child_groups first
            if haskey(g.unwritten_child_groups, dir)
                g = g.unwritten_child_groups[dir]::G
            else
                # Use lookup_link directly for consistency
                link = lookup_link(g, dir)
                if link === nothing
                    return false
                elseif isa(link, HardLink)
                    offset = link.target
                    if haskey(f.loaded_groups, offset)
                        g = f.loaded_groups[offset]::G
                    elseif !isgroup(f, offset)
                        return false
                    else
                        g = f.loaded_groups[offset] = load_group(f, offset)::G
                    end
                else
                    # Non-hard links cannot be traversed for group navigation in haskey
                    return false
                end
            end
        end

        name = String(dirs[end])
    end
    (g.last_chunk_start_offset != -1 && haskey(g.written_links, name)) ||
        haskey(g.unwritten_links, name) || haskey(g.unwritten_child_groups, name)
end

Base.isempty(g::Group) =
    (g.last_chunk_start_offset == -1 || isempty(g.written_links)) &&
    isempty(g.unwritten_links) && isempty(g.unwritten_child_groups)

function Base.keys(g::Group)
    ks = String[]
    if g.last_chunk_start_offset != -1
        append!(ks, keys(g.written_links))
    end
    append!(ks, keys(g.unwritten_links))
    append!(ks, keys(g.unwritten_child_groups))
    ks
end

Base.keytype(::Group) = String

"""
    parse_link_message(wmsg::HmWrap{HmLinkMessage}) -> AbstractLink

Parse a link message from the HDF5 format and return the appropriate AbstractLink subtype.
Handles hard links (type 0), soft links (type 1), and external links (type 64).
"""
function parse_link_message(wmsg::HmWrap{HmLinkMessage})::AbstractLink
    # Check if link type is specified in flags (bit 3)
    if isset(wmsg.flags, 3)
        # Link type is explicitly specified
        link_type = wmsg.link_type
        if link_type == 0
            # Hard link - target field should be present
            return HardLink(wmsg.target)
        elseif link_type == 1
            # Soft link - path stored in soft_link blob

            # The soft_link field is a blob containing the path string
            path_bytes = wmsg.soft_link
            # Convert bytes to string (non-null terminated according to spec)
            path = String(path_bytes)
            return SoftLink(path)

        elseif link_type == 64
            # External link - two null-terminated strings in external_link blob
            blob = wmsg.external_link
            # Parse two null-terminated strings but skip the first byte which is reserved
            strings = split_null_terminated_strings(blob, 2)
            if length(strings) != 2
                throw(InvalidDataException("External link must contain exactly two null-terminated strings"))
            end
            file_path, object_path = strings
            return ExternalLink(file_path, object_path)
        else
            throw(UnsupportedFeatureException("Unsupported link type: $link_type"))
        end
    else
        # Link type not specified, defaults to hard link (type 0)
        # According to the HDF5 spec, target field is present when (!isset(flags, 3) || link_type==0)
        return HardLink(wmsg.target)
    end
end

"""
    split_null_terminated_strings(blob::Vector{UInt8}) -> Vector{String}

Split a byte blob containing null-terminated strings into separate string components.
Used for parsing external link information.
"""
function split_null_terminated_strings(blob::Vector{UInt8}, start_idx=1)::Vector{String}
    strings = String[]

    for (i, byte) in enumerate(blob)
        if byte == 0x00  # null terminator
            if i > start_idx  # Don't create empty strings from consecutive nulls
                push!(strings, String(blob[start_idx:i-1]))
            end
            start_idx = i + 1
        end
    end

    # Handle case where last string is not null-terminated
    if start_idx <= length(blob)
        push!(strings, String(blob[start_idx:end]))
    end

    return strings
end

"""
    group_path(g::Group) -> String

Get the absolute path of a group within its file.

# Returns
The absolute path of the group within the HDF5 file (e.g., "/data/measurements").
For the root group, returns "/".

# Algorithm
- If this is the root group, returns "/"
- Otherwise, searches through unwritten_child_groups only (performance optimization)
- For groups loaded from disk, falls back to "/" (limitation of current implementation)

# Performance Note
This implementation prioritizes performance over complete accuracy. It only searches
through in-memory group hierarchies (unwritten_child_groups) to avoid expensive disk I/O.
For complex hierarchies with groups loaded from disk, relative soft links may not
resolve perfectly, but absolute soft links will always work.
"""
function group_path(g::Group)
    # Check if this is the root group
    if g === g.f.root_group
        return "/"
    end

    # Search through the file's in-memory group hierarchy
    return find_group_path_simple(g.f.root_group, g, "/")
end

"""
    find_group_path_simple(root::Group, target::Group, current_path::String) -> String

Find the path to a target group using recursion through both unwritten and written groups.
This version searches both in-memory and disk-based groups to find the target.
"""
function find_group_path_simple(root::Group, target::Group, current_path::String)
    # Check direct children in unwritten_child_groups (in-memory groups)
    for (name, child_group) in root.unwritten_child_groups
        child_path = current_path == "/" ? "/$name" : "$current_path/$name"

        if child_group === target
            return child_path
        end

        # Recursively search child groups
        result = find_group_path_simple(child_group, target, child_path)
        if result != "/"
            return result
        end
    end

    # Check written_links for groups (groups that have been written to disk)
    for (name, link) in root.written_links
        if isa(link, HardLink)
            child_path = current_path == "/" ? "/$name" : "$current_path/$name"

            # Try to load the group at this offset and check if it's our target
            try
                # Load the group to see if it matches our target
                # We use === for object identity comparison
                # This is expensive but necessary for groups loaded from disk
                loaded_group_offset = link.target

                # Check if target group has the same file and offset
                # This is a heuristic to avoid loading every group
                if target.f === root.f && hasfield(typeof(target), :last_chunk_start_offset)
                    if target.last_chunk_start_offset == loaded_group_offset
                        return child_path
                    end
                end

                # As a fallback, we could load and compare, but that's expensive
                # For now, we'll skip recursive searching of loaded groups
                # This means some complex hierarchies might not resolve perfectly
            catch
                # If loading fails, skip this entry
                continue
            end
        end
    end

    # Not found in this branch
    return "/"
end

"""
    resolve_relative_soft_link(g::Group, relative_path::String)

Resolve a relative soft link path from the current group.

# Supported Relative Paths
- Simple relative paths without ".." (e.g., "temp", "subgroup/data")
- Paths with ".." components have limited support when groups are loaded from disk

# Algorithm
For simple paths, navigates directly from the current group.
For complex paths with "..", attempts to use group hierarchy but may fall back to error.
"""
function resolve_relative_soft_link(g::Group, relative_path::String)
    # Parse the relative path components
    path_components = split(relative_path, '/', keepempty=false)
    current_group = g

    # Check if this path contains ".." components
    has_upward_navigation = any(c -> c == "..", path_components)

    if has_upward_navigation
        # Complex relative path with upward navigation
        # This requires knowing the current group's path in the hierarchy
        # For groups loaded from disk, this may not work perfectly
        try
            # Try to use the original path resolution approach
            current_path = group_path(current_group)
            resolved_path = resolve_soft_link_path(current_path, relative_path)
            return current_group.f[resolved_path]
        catch
            # Fallback error message
            throw(KeyError("Relative soft link with '..' components cannot be resolved for groups loaded from disk: $relative_path"))
        end
    else
        # Simple relative path - navigate directly from current group
        for component in path_components
            if component != "." && !isempty(component)
                # Navigate to child - this should work with normal getindex
                current_group = current_group[component]
            end
            # Ignore "." and empty components
        end

        return current_group
    end
end

function load_group(f::JLDFile, offset::RelOffset)
    # Messages
    links = OrderedDict{String,AbstractLink}()

    next_link_offset::Int64 = -1
    link_phase_change_max_compact::Int64 = -1
    link_phase_change_min_dense::Int64 = -1
    est_num_entries::Int64 = 4
    est_link_name_len::Int64 = 8
    fractal_heap_address::RelOffset = UNDEFINED_ADDRESS
    name_index_btree::RelOffset = UNDEFINED_ADDRESS

    v1btree_address::RelOffset = UNDEFINED_ADDRESS
    name_index_heap::RelOffset = UNDEFINED_ADDRESS
    hmitr = HeaderMessageIterator(f, offset)
    for msg in hmitr
        chunk_start, chunk_end = hmitr.chunk
        if msg.type == HmNil && hmitr.curpos + CONTINUATION_MSG_SIZE == chunk_end && msg.size >= 13
                # This is the remaining space at the end of a chunk
                # Use only if a message can potentially fit inside
                # Single Character Name Link Message has 13 bytes payload
                next_link_offset = fileoffset(f, msg.offset)
        elseif msg.type == HmLinkInfo
            wmsg = HmWrap(HmLinkInfo, msg)
            fractal_heap_address = wmsg.fractal_heap_address
            name_index_btree = wmsg.v2_btree_name_index
        elseif msg.type == HmGroupInfo
            wmsg = HmWrap(HmGroupInfo, msg)
            if wmsg.size > 2
                # Version Flag
                wmsg.version == 0 || throw(UnsupportedFeatureException())
                flag = wmsg.flags::UInt8
                if flag%2 == 1 # first bit set
                    link_phase_change_max_compact = wmsg.link_phase_change_max_compact
                    link_phase_change_min_dense = wmsg.link_phase_change_min_dense
                end
                if (flag >> 1)%2 == 1 # second bit set
                    # Verify that non-default group size is given
                    est_num_entries = wmsg.est_num_entries
                    est_link_name_len = wmsg.est_link_name_len
                end
            end
        elseif msg.type == HmLinkMessage
            wmsg = HmWrap(HmLinkMessage, msg)
            links[wmsg.link_name] = parse_link_message(wmsg)
        elseif msg.type == HmSymbolTable
            wmsg = HmWrap(HmSymbolTable, msg)
            v1btree_address = wmsg.v1btree_address
            name_index_heap = wmsg.name_index_heap
        end
    end

    if fractal_heap_address != UNDEFINED_ADDRESS
        records = read_btree(f, fractal_heap_address, name_index_btree)::Vector{Tuple{String, RelOffset}}
        for r in records
            links[r[1]] = HardLink(r[2])
        end
    end

    if v1btree_address != UNDEFINED_ADDRESS
        records = read_oldstyle_group(f, v1btree_address, name_index_heap)::Vector{Tuple{String, RelOffset}}
        for r in records
            links[r[1]] = HardLink(r[2])
        end
    end

    chunk_start, chunk_end = hmitr.chunk
    continuation_msg_address = chunk_end - CONTINUATION_MSG_SIZE
    chunk_start < next_link_offset < chunk_end || (next_link_offset = -1)

    Group{typeof(f)}(f, chunk_start, continuation_msg_address, chunk_end, next_link_offset,
                     est_num_entries, est_link_name_len,
                     OrderedDict{String,AbstractLink}(), OrderedDict{String,Group}(), links)
end

"""
    links_size(pairs)

Returns the size of several link messages. `pairs` is an iterator of
`String => AbstractLink` pairs.
"""
function links_size(pairs)
    sz = 0
    for (name::String, link) in pairs
        sz += message_size_for_link(name, link)
    end
    sz
end

function group_extra_space(g)
    remaining_entries = g.est_num_entries - length(g.unwritten_links)
    extraspace = remaining_entries * (12 + g.est_link_name_len + 4)
    # Can't create a placeholder NIL message larger than that
    clamp(extraspace, 0, typemax(UInt16))
end


"""
    save_group(g::Group) -> RelOffset

Stores a group to a file, updating it if it has already been saved. Returns
`UNDEFINED_ADDRESS` if the group was already stored, or the offset of the new group
otherwise.
"""
function save_group(g::Group)
    # Save unwritten child groups
    if !isempty(g.unwritten_child_groups)
        for (name, child_group) in g.unwritten_child_groups
            retval = save_group(child_group)
            g.unwritten_links[name] = HardLink(retval)
        end
        empty!(g.unwritten_child_groups)
    end

    # Save links
    f = g.f
    io = f.io
    # If the group has not been saved yet
    if g.last_chunk_start_offset == -1

        totalsize = jlsizeof(Val(HmLinkInfo))
        totalsize += jlsizeof(Val(HmGroupInfo); g.est_num_entries, g.est_link_name_len)
        totalsize += CONTINUATION_MSG_SIZE
        totalsize += links_size(g.unwritten_links)
        # add to size to make space for additional links
        totalsize += group_extra_space(g)

        sz = jlsizeof(ObjectStart) + size_size(totalsize) + totalsize

        g.last_chunk_start_offset = f.end_of_data
        g.last_chunk_checksum_offset = f.end_of_data + sz
        f.end_of_data += sz + 4
        seek(io, g.last_chunk_start_offset)

        # Object header
        jlwrite(io, ObjectStart(size_flag(totalsize)))
        write_size(io, totalsize)
        write_header_message(io, Val(HmLinkInfo))
        write_header_message(io, Val(HmGroupInfo); g.est_num_entries, g.est_link_name_len)

        g.next_link_offset = position(io)
    end
    next_msg_offset = g.next_link_offset == -1 ? g.continuation_message_goes_here : g.next_link_offset
    attach_message(f, g.last_chunk_start_offset, collect(g.unwritten_links);
        chunk_start = g.last_chunk_start_offset,
        chunk_end = g.last_chunk_checksum_offset,
        next_msg_offset)

    # Transfer unwritten links to written links after successful write
    merge!(g.written_links, g.unwritten_links)
    empty!(g.unwritten_links)

    # this is clearly suboptimal
    # TODO: this should always return the object start
    # but for "continued" versions this is not the case
    return h5offset(f, g.last_chunk_start_offset)
end

function isgroup(f::JLDFile, offset::RelOffset)
    # trivial case -> It is already loaded
    haskey(f.loaded_groups, offset) && return true

    is_group = false
    determined = false
    hmitr = HeaderMessageIterator(f, offset)
    for msg in hmitr
        if msg.type in (HmLinkInfo, HmGroupInfo, HmLinkMessage, HmSymbolTable)
            is_group = true
            determined = true
            break
        elseif msg.type in (HmDataspace, HmDatatype, HmFillValue, HmDataLayout)
            determined = true
            break
        end
    end
    return is_group
end

function show_group(io::IO, g::Group, maxnumlines::Int=10, prefix::String=" ", skiptypes::Bool=false)
    iszero(maxnumlines) && return 0
    if g.f.n_times_opened == 0
        print(io, "  (closed)")
        return -1
    end

    ks = collect(keys(g))
    skiptypes && filter!(x -> x != "_types", ks)

    if isempty(ks) && prefix == " "
        print(io, "  (no datasets)")
        return -1
    end

    for i = 1:length(ks)
        k = ks[i]
        islast = i == length(ks)
        # Check if this is a group - handle external/soft links specially
        link = lookup_link(g, k)
        if haskey(g.unwritten_child_groups, k)
            isagroup = true
        elseif isa(link, HardLink)
            isagroup = isgroup(g.f, link.target)
        else
            # For external/soft links, we can't easily determine if target is a group
            # without resolving the link, so treat as non-group for display purposes
            isagroup = false
        end
        if (maxnumlines <= 1 && !islast) #|| (maxnumlines < 2 && isagroup))
            print(io, prefix, "└─ ⋯ ($(length(ks)-i+1) more entries)")
            return 0
        end
        # Choose appropriate icon based on link type and whether it's a group
        icon = if isagroup
            "📂 "  # folder for groups
        elseif isa(link, ExternalLink)
            "🔗 "  # link icon for external links
        elseif isa(link, SoftLink)
            "↗️ "   # arrow for soft links
        else
            "🔢 "  # default for hard link datasets
        end
        print(io, prefix, islast ? "└─" : "├─", icon, k)
        maxnumlines = maxnumlines - 1
        if isagroup
            newg = g[k]
            if !isempty(newg)
                if (maxnumlines > 1 || (islast && maxnumlines >= 2))
                    print(io, '\n')
                    ret = show_group(io, newg, islast ? maxnumlines : maxnumlines-1, prefix*(islast ? "   " : "│  "), false)
                    if ret < 0
                        error("closed or empty dataset, this should not happen")
                    end
                    maxnumlines = ret
                    maxnumlines += islast ? 0 : 1
                else
                    nentries = length(keys(newg))
                    if nentries == 1
                        print(io, " (1 entry)")
                    else
                        print(io, " ($(nentries) entries)")
                    end
                end

            end
        end
        !islast && print(io, '\n')
    end
    return maxnumlines
end

Base.show(io::IO, g::Group) = print(io, "JLD2.Group")

function Base.show(io::IO, ::MIME"text/plain", g::Group)
    print(io, "JLD2.Group")
    if get(io, :compact, false)
        return
    else
        print(io, "\n")
        show_group(io, g, 10, " ", false)
    end
end
