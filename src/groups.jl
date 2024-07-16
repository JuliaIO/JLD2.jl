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
    name = Unicode.normalize(name)
    G = typeof(g)
    if '/' in name
        f = g.f
        dirs = split(name, '/')

        # Handles the absolute path case where the name starts with /
        if isempty(first(dirs))
            g = g.f.root_group::G
            start = 2
        else
            start = 1
        end

        for i = start:length(dirs)-1
            dir = dirs[i]
            isempty(dir) && continue

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

    roffset = lookup_offset(g, name)
    if roffset == UNDEFINED_ADDRESS
        haskey(g.unwritten_child_groups, name) && return g.unwritten_child_groups[name]
        throw(KeyError(name))
    end

    if isgroup(f, roffset)
        let loaded_groups = f.loaded_groups
            get!(()->load_group(f, roffset), loaded_groups, roffset)
        end
    else
        load_dataset(f, roffset)
    end
end

# function Base.write(g::Group, name::AbstractString, obj, wsession::JLDWriteSession=JLDWriteSession())
#     if g.last_chunk_start_offset != -1 && g.continuation_message_goes_here == -1
#         error("objects cannot be added to this group because it was created with a previous version of JLD2")
#     end
#     f = g.f
#     prewrite(f)
#     (g, name) = pathize(g, name, true)
#     g[name] = write_dataset(f, obj, wsession)
#     nothing
# end

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
    name = Unicode.normalize(name)
    if '/' in name
        f = g.f
        dirs = split(name, '/')

        # Handles the absolute path case where the name starts with /
        if isempty(first(dirs))
            g = f.root_group::G
            start = 2
        else
            start = 1
        end

        for i = start:length(dirs)-1
            dir = dirs[i]
            isempty(dir) && continue

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

Base.keytype(f::Group) = String

# LinkInfo struct is used for dispatch
struct LinkInfo end
# In general the size depends on flags
# when writing files we always use 18 bytes 
# this function is only used for writing
jlsizeof(::Type{LinkInfo}) = 18
function jlwrite(io, ::LinkInfo)
    jlwrite(io, zero(UInt16))
    jlwrite(io, typemax(UInt64))
    jlwrite(io, typemax(UInt64))
    return nothing    
end

function jlread(io, ::Type{LinkInfo})
    version = jlread(io, UInt8)
    flags = jlread(io, UInt8)
    (flags & 0b1) == 1 && skip(io, 8)
    fractal_heap_address = jlread(io, RelOffset)
    name_index_btree = jlread(io, RelOffset)
    (flags & 0b10) == 0b10 && skip(io, 8)
    (; version, flags, fractal_heap_address, name_index_btree)
end

@enum(CharacterSet,
      CSET_ASCII,
      CSET_UTF8)

const LM_CREATION_ORDER_PRESENT = UInt8(2^2)
const LM_LINK_TYPE_FIELD_PRESENT = UInt8(2^3)
const LM_LINK_NAME_CHARACTER_SET_FIELD_PRESENT = UInt8(2^4)

function read_link(io::IO)
    # Version
    version = jlread(io, UInt8)
    version == 1 || throw(UnsupportedVersionException())

    # Flags
    flags = jlread(io, UInt8)

    if (flags & LM_LINK_TYPE_FIELD_PRESENT) != 0
        jlread(io, UInt8) == 0 || throw(UnsupportedFeatureException())
    end

    if (flags & LM_CREATION_ORDER_PRESENT) != 0
        skip(io, 8)
    end

    # Link name character set
    cset = CSET_ASCII
    if (flags & LM_LINK_NAME_CHARACTER_SET_FIELD_PRESENT) != 0
        cset_byte = jlread(io, UInt8)
        cset = CharacterSet(cset_byte)
    end

    sz = read_size(io, flags)  # Size
    name = jlread(io, UInt8, sz) # Link name
    target = jlread(io, RelOffset)  # Link information

    (String(name), target)
end

const CONTINUATION_MSG_SIZE = jlsizeof(HeaderMessage) + jlsizeof(RelOffset) + jlsizeof(Length)

function load_group(f::JLDFile, offset::RelOffset)
    io = f.io
    cio, header_version, chunk, groupflags = start_obj_read(f, offset)
    chunk_start, chunk_end = chunk
    # Messages
    continuation_message_goes_here::Int64 = -1
    links = OrderedDict{String,RelOffset}()
    chunks = [chunk]
    chunk_number = 0

    next_link_offset::Int64 = -1
    link_phase_change_max_compact::Int64 = -1 
    link_phase_change_min_dense::Int64 = -1
    est_num_entries::Int64 = 4
    est_link_name_len::Int64 = 8
    fractal_heap_address = UNDEFINED_ADDRESS
    name_index_btree = UNDEFINED_ADDRESS

    v1btree_address = UNDEFINED_ADDRESS
    name_index_heap = UNDEFINED_ADDRESS

    while !isempty(chunks)
        chunk = popfirst!(chunks)
        chunk_start, chunk_end = chunk.chunk_start, chunk.chunk_end

        if chunk_number > 0
            cio = start_chunk_read(io, chunk, header_version)
            header_version == 2 && (chunk_end -= 4)
        end
        chunk_number += 1
        while (curpos = position(cio)) < chunk_end-4
            msg = read_header_message(f, cio, header_version, chunk_start, groupflags)

            if msg.type == HM_NIL
                if continuation_message_goes_here == -1 && 
                    chunk_end - curpos == CONTINUATION_MSG_SIZE
                    #continuation_message_goes_here = curpos # also correct
                    continuation_message_goes_here = fileoffset(f, msg.offset)

                elseif position(cio) + CONTINUATION_MSG_SIZE == chunk_end
                    # This is the remaining space at the end of a chunk
                    # Use only if a message can potentially fit inside
                    # Single Character Name Link Message has 13 bytes payload
                    if msg.size >= 13 
                        #next_link_offset = curpos # also correct
                        next_link_offset = fileoffset(f, msg.offset)
                    end
                end
            else
                continuation_message_goes_here = -1
                if msg.type == HM_LINK_INFO
                    fractal_heap_address = msg.fractal_heap_address
                    name_index_btree = msg.v2_btree_name_index
                elseif msg.type == HM_GROUP_INFO
                    if msg.size > 2
                        # Version Flag
                        msg.version == 0 || throw(UnsupportedFeatureException()) 
                        flag = msg.flags
                        if flag%2 == 1 # first bit set
                            link_phase_change_max_compact = msg.link_phase_change_max_compact
                            link_phase_change_min_dense = msg.link_phase_change_min_dense
                        end
                        if (flag >> 1)%2 == 1 # second bit set
                            # Verify that non-default group size is given
                            est_num_entries = msg.est_num_entries
                            est_link_name_len = msg.est_link_name_len
                        end
                    end
                elseif msg.type == HM_LINK_MESSAGE
                    name, loffset = read_link(msg)
                    links[name] = loffset
                elseif msg.type == HM_OBJECT_HEADER_CONTINUATION
                    push!(chunks, (; chunk_start = fileoffset(f, msg.continuation_offset),
                                     chunk_end = fileoffset(f, msg.continuation_offset + msg.continuation_length)))
                    # For correct behaviour, empty space can only be filled in the 
                    # very last chunk. Forget about previously found empty space
                    next_link_offset = -1
                elseif msg.type == HM_SYMBOL_TABLE
                    v1btree_address = msg.v1btree_address
                    name_index_heap = msg.name_index_heap            
                elseif (msg.hflags & 2^3) != 0
                    throw(UnsupportedFeatureException())
                end
            end
        end

        # Checksum
        seek(cio, chunk_end)
        if header_version == 2
            end_checksum(cio) == jlread(io, UInt32) || throw(InvalidDataException())
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

    Group{typeof(f)}(f, chunk_start, continuation_message_goes_here,        
                     chunk_end, next_link_offset, est_num_entries,
                     est_link_name_len,
                     OrderedDict{String,RelOffset}(), OrderedDict{String,Group}(), links)
end

"""
    link_size(name::String)

Returns the size of a link message, including message header.
"""
link_size(name::String) =
    3 + size_size(jlsizeof(name)) + jlsizeof(name) + jlsizeof(RelOffset)

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

"""
    group_payload_size(g)

Returns the size of a group payload, including link info, group info, and link messages,
but not the object header. Provides
space after the last object message for a continuation message.
"""
group_payload_size(g) =
    jlsizeof(HeaderMessage) + jlsizeof(LinkInfo) + group_info_message_size(g) + links_size(g.unwritten_links) +
    jlsizeof(HeaderMessage) + jlsizeof(RelOffset) + jlsizeof(Length)

group_continuation_size(pairs) =
    jlsizeof(OBJECT_HEADER_CONTINUATION_SIGNATURE) + links_size(pairs) +
    jlsizeof(HeaderMessage) + jlsizeof(RelOffset) + jlsizeof(Length) + 4 # Checksum is included


"""
    write_link(cio, name, offset)
Write a link message at current position in `cio`.
"""
function write_link(cio, name, offset)
    jlwrite(cio, HeaderMessage(HM_LINK_MESSAGE, link_size(name), 0))
    jlwrite(cio, UInt8(1))             # Version

    # Flags
    flags = size_flag(jlsizeof(name)) | LM_LINK_NAME_CHARACTER_SET_FIELD_PRESENT
    jlwrite(cio, flags::UInt8)

    jlwrite(cio, UInt8(CSET_UTF8))     # Link name character set
    write_size(cio, jlsizeof(name))    # Length of link name
    jlwrite(cio, name)                 # Link name
    jlwrite(cio, offset)               # Link target
end

function group_extra_space(g)
    remaining_entries = g.est_num_entries - length(g.unwritten_links)
    extraspace = 0
    if remaining_entries > 0
        extraspace += remaining_entries * (12 + g.est_link_name_len + 4)
    end
     # Can't create a placeholder NIL message larger than that
    min(extraspace, typemax(UInt16))
end

function update_checksum(io, g)
    seek(io, g.last_chunk_start_offset)
    cio = begin_checksum_read(io)
    seek(cio, g.last_chunk_checksum_offset)
    seek(io, g.last_chunk_checksum_offset)
    jlwrite(io, end_checksum(cio))
end

function group_info_message_size(g)
    jlsizeof(HeaderMessage) + 2 + 4*(g.est_num_entries != 4 || g.est_link_name_len != 8)
end

function write_group_info_message(cio, g)
    # Check for non standard value of 
    # est_num_entries [== 4]
    # est_link_name_len [== 8]
    if g.est_num_entries == 4 && g.est_link_name_len == 8
        jlwrite(cio, HeaderMessage(HM_GROUP_INFO, 2, 0))
        jlwrite(cio, UInt16(0))
    else
        jlwrite(cio, HeaderMessage(HM_GROUP_INFO, 6, 0))
        jlwrite(cio, UInt8(0)) # Version
        jlwrite(cio, UInt8(2)) # Flags (non-default sizes)
        jlwrite(cio, UInt16(g.est_num_entries))
        jlwrite(cio, UInt16(g.est_link_name_len))
    end
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
        totalsize = group_payload_size(g)
        # add to size to make space for additional links
        extraspace = group_extra_space(g)
        totalsize += extraspace
        sz = jlsizeof(ObjectStart) + size_size(totalsize) + totalsize

        g.last_chunk_start_offset = f.end_of_data
        g.last_chunk_checksum_offset = f.end_of_data + sz #- 4

        f.end_of_data += sz + 4
        seek(io, g.last_chunk_start_offset)
        #cio = begin_checksum_write(io, sz)
        cio = io
        # Object header
        jlwrite(cio, ObjectStart(size_flag(totalsize)))
        write_size(cio, totalsize)

        # Link info message
        jlwrite(cio, HeaderMessage(HM_LINK_INFO, jlsizeof(LinkInfo), 0))
        jlwrite(cio, LinkInfo())

        # Group info message
        write_group_info_message(cio, g)
        g.next_link_offset = position(cio)
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
