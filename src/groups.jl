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
    lookup_offset(g::Group, name::AbstractString) -> RelOffset

Lookup the offset of a dataset in a group. Returns `UNDEFINED_ADDRESS` if the dataset is
not present. Does not inspect `unwritten_child_groups`.
"""
function lookup_offset(g::Group, name::AbstractString)
    if g.last_chunk_start_offset != -1
        # Has been saved to file, so written_links exists
        roffset = get(g.written_links, name, UNDEFINED_ADDRESS)
        roffset != UNDEFINED_ADDRESS && return roffset
    end
    roffset = get(g.unwritten_links, name, UNDEFINED_ADDRESS)
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
            offset = lookup_offset(g, dir)
            if offset == UNDEFINED_ADDRESS
                if haskey(g.unwritten_child_groups, dir)
                    # It's possible that lookup_offset fails because the group has not yet
                    # been written to the file
                    g = g.unwritten_child_groups[dir]
                elseif create
                    # No group exists, so create a new group
                    newg = G(f)
                    g.unwritten_child_groups[dir] = newg
                    g = newg
                else
                    throw(KeyError(join(dirs[1:i], '/')))
                end
            elseif haskey(f.loaded_groups, offset)
                g = f.loaded_groups[offset]::G
            elseif !isgroup(f, offset)
                throw(ArgumentError("path $(join(dirs[1:i], '/')) points to a dataset, not a group"))
            else
                g = f.loaded_groups[offset] = load_group(f, offset)::G
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
    roffset = lookup_offset(g, name)
    if roffset == UNDEFINED_ADDRESS
        haskey(g.unwritten_child_groups, name) && return g.unwritten_child_groups[name]
        throw(KeyError(name))
    end

    load_dataset(f, roffset)
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
    g.unwritten_links[name] = offset
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
            offset = lookup_offset(g, dir)
            if offset == UNDEFINED_ADDRESS
                if haskey(g.unwritten_child_groups, dir)
                    # It's possible that lookup_offset fails because the group has not yet
                    # been written to the file
                    g = g.unwritten_child_groups[dir]::G
                else
                    return false
                end
            elseif haskey(f.loaded_groups, offset)
                g = f.loaded_groups[offset]::G
            elseif !isgroup(f, offset)
                return false
            else
                g = f.loaded_groups[offset] = load_group(f, offset)::G
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

function load_group(f::JLDFile, offset::RelOffset)
    # Messages
    links = OrderedDict{String,RelOffset}()
    
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
            links[wmsg.link_name] = wmsg.target
        elseif msg.type == HmSymbolTable
            wmsg = HmWrap(HmSymbolTable, msg)
            v1btree_address = wmsg.v1btree_address
            name_index_heap = wmsg.name_index_heap            
        end
    end

    if fractal_heap_address != UNDEFINED_ADDRESS
        records = read_btree(f, fractal_heap_address, name_index_btree)
        for r in records
            links[r[1]] = r[2]
        end
    end

    if v1btree_address != UNDEFINED_ADDRESS
        records = read_oldstyle_group(f, v1btree_address, name_index_heap)
        for r in records
            links[r[1]] = r[2]
        end
    end

    chunk_start, chunk_end = hmitr.chunk
    continuation_msg_address = chunk_end - CONTINUATION_MSG_SIZE
    chunk_start < next_link_offset < chunk_end || (next_link_offset = -1)

    Group{typeof(f)}(f, chunk_start, continuation_msg_address, chunk_end, next_link_offset, 
                     est_num_entries, est_link_name_len,
                     OrderedDict{String,RelOffset}(), OrderedDict{String,Group}(), links)
end

"""
    link_size(name::String)

Returns the size of a link message, excluding message header.
"""
link_size(link_name::String) = sizefun(Val(HmLinkMessage), 0,0,(;link_name, target=UNDEFINED_ADDRESS))

"""
    links_size(pairs)

Returns the size of several link messages. `pairs` is an iterator of
`String => RelOffset` pairs.
"""
function links_size(pairs)
    sz = 0
    for (name::String,) in pairs
        sz += link_size(name) + jlsizeof(HeaderMessage)
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
            g.unwritten_links[name] = retval
        end
        empty!(g.unwritten_child_groups)
    end

    # Save links
    f = g.f
    io = f.io
    # If the group has not been saved yet
    if g.last_chunk_start_offset == -1

        link_info = Hmessage(HmLinkInfo)
        group_info = Hmessage(HmGroupInfo; g.est_num_entries, g.est_link_name_len)

        totalsize = jlsizeof(link_info) + jlsizeof(group_info)
        # Object header continuation placeholder
        totalsize += (jlsizeof(HeaderMessage) + jlsizeof(RelOffset) + jlsizeof(Length))
        # Link messages
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
        jlwrite(io, link_info)
        jlwrite(io, group_info)

        g.next_link_offset = position(io)
    end
    next_msg_offset = g.next_link_offset == -1 ? g.continuation_message_goes_here : g.next_link_offset
    attach_message(f, g.last_chunk_start_offset, collect(g.unwritten_links);
        chunk_start = g.last_chunk_start_offset,
        chunk_end = g.last_chunk_checksum_offset,
        next_msg_offset)
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
        isagroup = haskey(g.unwritten_child_groups, k) || isgroup(g.f, lookup_offset(g, k))
        if (maxnumlines <= 1 && !islast) #|| (maxnumlines < 2 && isagroup))
            print(io, prefix, "└─ ⋯ ($(length(ks)-i+1) more entries)")
            return 0
        end
        print(io, prefix, islast ? "└─" : "├─", isagroup ? "📂 " : "🔢 ", k)
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
