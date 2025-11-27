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

# Lookup a link in a group by name. Returns a Link object.
# Use is_set(link) to check if it is valid.
function lookup_link(g::Group, name::AbstractString)
    link = get(g.written_links, name, nothing)
    link !== nothing && return link
    return get(g.unwritten_links, name, Link())
end


"""
    getoffset(group::Group, link::Link; erroroninvalid::Bool=true) -> RelOffset

Get the offset for a link by resolving it if necessary.

- **Hard links**: Returns `link.offset` directly (may be UNDEFINED_ADDRESS)
- **Soft links**: Recursively resolves the soft link path and returns the target offset
- **External links**: Errors if `erroroninvalid=true`, otherwise returns UNDEFINED_ADDRESS

This centralizes link resolution logic and simplifies calling code.
"""
function getoffset(g::Group, link::Link; erroroninvalid::Bool=true)
    if is_hard_link(link)
        erroroninvalid && !is_set(link) && throw(ArgumentError("Invalid hard link"))
        return link.offset
    elseif is_soft_link(link)
        # Recursively resolve soft link
        (g, name) = pathize(g, link.path, false)
        target_link = lookup_link(g, name)
        if !is_set(target_link)
            erroroninvalid && throw(ArgumentError("Soft link target not found: $(link.path)"))
            return UNDEFINED_ADDRESS
        end
        # Recursively resolve in case target is also a soft link
        return getoffset(g, target_link; erroroninvalid)
    else  # external link
        erroroninvalid && throw(ArgumentError("Cannot get offset for external link to $(link.external_file):$(link.path)"))
        return UNDEFINED_ADDRESS
    end
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
                link = lookup_link(g, dir)
                if !is_set(link)
                    if create
                        # No group exists, so create a new group
                        newg = G(f)
                        g.unwritten_child_groups[dir] = newg
                        g = newg
                    else
                        throw(KeyError(join(dirs[1:i], '/')))
                    end
                elseif is_hard_link(link)
                    offset = link.offset
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
    g.f.n_times_opened == 0 && throw(ArgumentError("file is closed"))

    (g, name) = pathize(g, name, false)
    isempty(name) && return g

    # Check for unwritten child groups first
    haskey(g.unwritten_child_groups, name) && return g.unwritten_child_groups[name]

    link = lookup_link(g, name)
    !is_set(link) && throw(KeyError(name))
    is_external_link(link) && return load_external_dataset(g.f, link)
    offset = getoffset(g, link)
    return load_dataset(g.f, offset)
end

@nospecializeinfer function Base.write(
        g::Group,
        name::AbstractString,
        @nospecialize(obj),
        wsession::JLDWriteSession=JLDWriteSession();
        compress=nothing,
        chunk=nothing
        )
    f = g.f
    prewrite(f)
    (g, name) = pathize(g, name, true)

    # Handle chunked arrays
    if !isnothing(chunk)
        if !(obj isa Array)
            throw(ArgumentError("chunk parameter can only be used with arrays"))
        end

        # Convert chunk to Vector{Int}
        chunk_dims = if chunk isa Tuple
            collect(Int, chunk)
        elseif chunk isa AbstractVector
            collect(Int, chunk)
        else
            throw(ArgumentError("chunk must be a tuple or vector of integers"))
        end

        # Validate chunk dimensions
        if length(chunk_dims) != ndims(obj)
            throw(ArgumentError("chunk dimensions ($(length(chunk_dims))) must match array dimensions ($(ndims(obj)))"))
        end

        # Use write_chunked helper function
        filters = isnothing(compress) ? nothing : compress
        Chunking.write_chunked(g, name, obj; chunk=Tuple(chunk_dims), indexing=:v1btree, filters=filters)
        return nothing
    end

    # Original behavior for non-chunked arrays
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

function Base.setindex!(g::Group, obj, name::AbstractString; chunk=nothing, compress=nothing)
    write(g, name, obj; chunk, compress)
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
    g.unwritten_links[name] = Link(offset)
    g
end

# New method to accept Link directly
function Base.setindex!(g::Group, link::Link, name::AbstractString)
    if g.last_chunk_start_offset != -1 && g.continuation_message_goes_here == -1
        error("objects cannot be added to this group because it was created with a previous version of JLD2")
    end
    g.unwritten_links[name] = link
    g
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
                link = lookup_link(g, dir)
                offset = getoffset(g, link; erroroninvalid=false)
                offset == UNDEFINED_ADDRESS && return false
                if haskey(f.loaded_groups, offset)
                    g = f.loaded_groups[offset]::G
                elseif !isgroup(f, offset)
                    return false
                else
                    g = f.loaded_groups[offset] = load_group(f, offset)::G
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
    parse_link_message(wmsg::HmWrap{HmLinkMessage}) -> Link

Parse a link message from the HDF5 format and return the appropriate Link.
Handles hard links (type 0), soft links (type 1), and external links (type 64).
"""
function parse_link_message(wmsg::HmWrap{HmLinkMessage})::Link
    if isset(wmsg.flags, 3)
        link_type = wmsg.link_type
        link_type == 0 && return Link(wmsg.target)
        link_type == 1 && return Link(UNDEFINED_ADDRESS, String(wmsg.soft_link), "")
        if link_type == 64
            strings = split(String(wmsg.external_link), '\0', keepempty=false)
            length(strings) == 2 ||
                throw(InvalidDataException("External link must have two null-terminated strings"))
            return Link(UNDEFINED_ADDRESS, String(strings[2]), String(strings[1]))
        else
            throw(UnsupportedFeatureException("Unsupported link type: $link_type"))
        end
    end
    # Default to hard link when type flag not set
    return Link(wmsg.target)
end

function load_group(f::JLDFile, offset::RelOffset)
    # Messages
    links = OrderedDict{String,Link}()

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
        records = read_fractal_heap_group(f, fractal_heap_address, name_index_btree)::Vector{Tuple{String, RelOffset}}
        for r in records
            links[r[1]] = Link(r[2])
        end
    end

    if v1btree_address != UNDEFINED_ADDRESS
        records = read_oldstyle_group(f, v1btree_address, name_index_heap)::Vector{Tuple{String, RelOffset}}
        for r in records
            links[r[1]] = Link(r[2])
        end
    end

    chunk_start, chunk_end = hmitr.chunk
    continuation_msg_address = chunk_end - CONTINUATION_MSG_SIZE
    chunk_start < next_link_offset < chunk_end || (next_link_offset = -1)

    Group{typeof(f)}(f, chunk_start, continuation_msg_address, chunk_end, next_link_offset,
                     est_num_entries, est_link_name_len,
                     OrderedDict{String,Link}(), OrderedDict{String,Group}(), links)
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
            g.unwritten_links[name] = Link(retval)
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
        totalsize += sum(message_size, g.unwritten_links, init=0)
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
        # Check if this is a group
        link = lookup_link(g, k)
        if haskey(g.unwritten_child_groups, k)
            isagroup = true
        else
            # Try to get the offset; for external links or unresolvable soft links, treat as non-group
            offset = getoffset(g, link; erroroninvalid=false)
            isagroup = (offset != UNDEFINED_ADDRESS) && isgroup(g.f, offset)
        end
        if (maxnumlines <= 1 && !islast) #|| (maxnumlines < 2 && isagroup))
            print(io, prefix, "└─ ⋯ ($(length(ks)-i+1) more entries)")
            return 0
        end
        # Choose appropriate icon based on link type and whether it's a group
        icon = if isagroup
            "📂 "  # folder for groups
        elseif is_external_link(link)
            "🔗 "  # link icon for external links
        elseif is_soft_link(link)
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
