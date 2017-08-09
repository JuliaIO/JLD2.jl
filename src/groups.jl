#
# Groups
#

"""
    Group(f::JLDFile, name::AbstractString)

Construct an empty group named `name` at the top level of `JLDFile` `f`.
"""
Group(f::JLDFile, name::AbstractString) = Group(f.root_group, name)

"""
    Group(g::Group, name::AbstractString)

Construct a group named `name` as a child of group `g`.
"""
Group(g::Group{T}, name::AbstractString) where {T} = (g[name] = Group{T}(g.f))

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
    if '/' in name
        f = g.f
        dirs = split(name, '/')

        # Handles the absolute path case where the name starts with /
        if isempty(first(dirs))
            g = g.f.root_group
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
                    newg = Group{typeof(f)}(f)
                    g.unwritten_child_groups[dir] = newg
                    g = newg
                else
                    throw(KeyError(join(dirs[1:i], '/')))
                end
            elseif haskey(f.loaded_groups, offset)
                g = f.loaded_groups[offset]
            elseif !isgroup(f, offset)
                throw(ArgumentError("path $(join(dirs[1:i], '/')) points to a dataset, not a group"))
            else
                g = f.loaded_groups[offset] = load_group(f, offset)
            end
        end

        name = String(dirs[end])
    end

    if create && haskey(g, name)
        throw(ArgumentError("a group or dataset named $name is already present within this group"))
    end

    return (g, name)
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

function Base.write(g::Group, name::AbstractString, obj, wsession::JLDWriteSession=JLDWriteSession())
    if g.last_chunk_start_offset != -1 && g.continuation_message_goes_here == -1
        error("objects cannot be added to this group because it was created with a previous version of JLD2")
    end
    f = g.f
    prewrite(f)
    (g, name) = pathize(g, name, true)
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
    g.unwritten_links[name] = offset
    g
end

function Base.haskey(g::Group, name::AbstractString)
    (g, name) = pathize(g, name, false)
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

struct LinkInfo
    version::UInt8
    flags::UInt8
    fractal_heap_address::RelOffset
    name_index_btree::RelOffset
end
define_packed(LinkInfo)

LinkInfo() = LinkInfo(0, 0, UNDEFINED_ADDRESS, UNDEFINED_ADDRESS)

@enum(CharacterSet,
      CSET_ASCII,
      CSET_UTF8)

const LM_CREATION_ORDER_PRESENT = UInt8(2^2)
const LM_LINK_TYPE_FIELD_PRESENT = UInt8(2^3)
const LM_LINK_NAME_CHARACTER_SET_FIELD_PRESENT = UInt8(2^4)

function read_link(io::IO)
    # Version
    version = read(io, UInt8)
    version == 1 || throw(UnsupportedVersionException())

    # Flags
    flags = read(io, UInt8)

    if (flags & LM_LINK_TYPE_FIELD_PRESENT) != 0
        read(io, UInt8) == 0 || throw(UnsupportedFeatureException())
    end

    if (flags & LM_CREATION_ORDER_PRESENT) != 0
        skip(io, 8)
    end

    # Link name character set
    cset = CSET_ASCII
    if (flags & LM_LINK_NAME_CHARACTER_SET_FIELD_PRESENT) != 0
        cset_byte = read(io, UInt8)
        cset = CharacterSet(cset_byte)
    end

    sz = read_size(io, flags)  # Size
    name = read(io, UInt8, sz) # Link name
    target = read(io, RelOffset)  # Link information

    (String(name), target)
end

function load_group(f::JLDFile, roffset::RelOffset)
    io = f.io
    chunk_start_offset::Int64 = fileoffset(f, roffset)
    seek(io, chunk_start_offset)

    cio = begin_checksum_read(io)
    sz = read_obj_start(cio)
    chunk_checksum_offset::Int64 = position(cio) + sz

    # Messages
    continuation_message_goes_here::Int64 = -1
    links = OrderedDict{String,RelOffset}()

    continuation_offset::Int64 = -1
    continuation_length::Length = 0

    while true
        if continuation_offset != -1
            seek(io, continuation_offset)
            chunk_checksum_offset = continuation_offset + continuation_length - 4
            continuation_offset = -1

            cio = begin_checksum_read(io)
            read(cio, UInt32) == OBJECT_HEADER_CONTINUATION_SIGNATURE || throw(InvalidDataException())
        end

        while (curpos = position(cio)) <= chunk_checksum_offset - 4
            msg = read(cio, HeaderMessage)
            endpos = curpos + sizeof(HeaderMessage) + msg.size
            if msg.msg_type == HM_NIL
                if continuation_message_goes_here == -1 &&
                   chunk_checksum_offset - curpos >= sizeof(HeaderMessage) + sizeof(RelOffset) + sizeof(Length)
                    continuation_message_goes_here = curpos
                end
            else
                continuation_message_goes_here = -1
                if msg.msg_type == HM_LINK_INFO
                    link_info = read(cio, LinkInfo)
                    link_info.fractal_heap_address == UNDEFINED_ADDRESS || throw(UnsupportedFeatureException())
                elseif msg.msg_type == HM_GROUP_INFO
                    # This message doesn't help us much, so we ignore it for now
                elseif msg.msg_type == HM_LINK_MESSAGE
                    name, loffset = read_link(cio)
                    links[name] = loffset
                elseif msg.msg_type == HM_OBJECT_HEADER_CONTINUATION
                    continuation_offset = chunk_start_offset = fileoffset(f, read(cio, RelOffset))
                    continuation_length = read(cio, Length)
                elseif (msg.flags & 2^3) != 0
                    throw(UnsupportedFeatureException())
                end
            end
            seek(cio, endpos)
        end

        # Checksum
        seek(cio, chunk_checksum_offset)
        end_checksum(cio) == read(io, UInt32) || throw(InvalidDataException())

        continuation_offset == -1 && break
    end

    Group{typeof(f)}(f, chunk_start_offset, continuation_message_goes_here,
                     chunk_checksum_offset, OrderedDict{String,RelOffset}(),
                     OrderedDict{String,Group}(), links)
end

"""
    link_size(name::String)

Returns the size of a link message, including message header.
"""
link_size(name::String) =
    3 + size_size(sizeof(name)) + sizeof(name) + sizeof(RelOffset)

"""
    links_size(pairs)

Returns the size of several link messages. `pairs` is an iterator of
`String => RelOffset` pairs.
"""
function links_size(pairs)
    sz = 0
    for (name::String,) in pairs
        sz += link_size(name) + sizeof(HeaderMessage)
    end
    sz
end

"""
    group_payload_size(pairs)

Returns the size of a group payload, including link info, group info, and link messages,
but not the object header. `pairs` is an iterator of `String => RelOffset` pairs. Provides
space after the last object message for a continuation message.
"""
group_payload_size(pairs) =
    sizeof(HeaderMessage) + sizeof(LinkInfo) + sizeof(HeaderMessage) + 2 + links_size(pairs) +
    sizeof(HeaderMessage) + sizeof(RelOffset) + sizeof(Length)

group_continuation_size(pairs) =
    sizeof(OBJECT_HEADER_CONTINUATION_SIGNATURE) + links_size(pairs) +
    sizeof(HeaderMessage) + sizeof(RelOffset) + sizeof(Length) + 4 # Checksum is included

"""
    save_group(g::Group) -> RelOffset

Stores a group to a file, updating it if it has already been saved. Returns
UNDEFINED_ADDRESS if the group was already stored, or the offset of the new group
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
    if g.last_chunk_start_offset == -1
        psz = group_payload_size(g.unwritten_links)
        sz = sizeof(ObjectStart) + size_size(psz) + psz

        g.last_chunk_start_offset = f.end_of_data
        retval = h5offset(f, f.end_of_data)
        seek(io, f.end_of_data)
        cio = begin_checksum_write(io, sz)

        # Object header
        write(cio, ObjectStart(size_flag(psz)))
        write_size(cio, psz)
        x = position(cio)

        # Link info message
        write(cio, HeaderMessage(HM_LINK_INFO, sizeof(LinkInfo), 0))
        write(cio, LinkInfo())

        # Group info message
        write(cio, HeaderMessage(HM_GROUP_INFO, 2, 0))
        write(cio, UInt16(0))
    else
        # If no changes, no need to save
        isempty(g.unwritten_links) && return UNDEFINED_ADDRESS

        retval = UNDEFINED_ADDRESS
        continuation_start = f.end_of_data
        csz = group_continuation_size(g.unwritten_links)

        # Object continuation message
        seek(io, g.continuation_message_goes_here)
        write(io, HeaderMessage(HM_OBJECT_HEADER_CONTINUATION, sizeof(RelOffset) + sizeof(Length), 0))
        write(io, h5offset(f, continuation_start))
        write(io, Length(csz))

        # Re-calculate checksum
        seek(io, g.last_chunk_start_offset)
        cio = begin_checksum_read(io)
        seek(cio, g.last_chunk_checksum_offset)
        seek(io, g.last_chunk_checksum_offset)
        write(io, end_checksum(cio))

        # Object continuation
        seek(io, continuation_start)
        g.last_chunk_start_offset = continuation_start
        cio = begin_checksum_write(io, csz - 4)
        write(cio, OBJECT_HEADER_CONTINUATION_SIGNATURE)
    end

    # Links
    for (name, offset) in g.unwritten_links
        write(cio, HeaderMessage(HM_LINK_MESSAGE, link_size(name), 0))
        write(cio, UInt8(1))             # Version

        # Flags
        flags = size_flag(sizeof(name)) | LM_LINK_NAME_CHARACTER_SET_FIELD_PRESENT
        write(cio, flags::UInt8)

        write(cio, UInt8(CSET_UTF8))     # Link name character set
        write_size(cio, sizeof(name))    # Length of link name
        write(cio, name)                 # Link name
        write(cio, offset)               # Link target
    end
    empty!(g.unwritten_links)

    # Extra space for object continuation
    g.continuation_message_goes_here = position(cio)
    write(cio, HeaderMessage(HM_NIL, sizeof(RelOffset)+sizeof(Length), 0))
    write(cio, RelOffset(0))
    write(cio, Length(0))

    # Checksum
    write(io, end_checksum(cio))
    f.end_of_data = position(io)
    g.last_chunk_checksum_offset = f.end_of_data - 4

    return retval
end

function show_group(io::IO, g::Group, prefix::String=" ", skiptypes::Bool=false)
    if g.f.n_times_opened == 0
        print(io, "  (closed)")
        return
    end

    ks = collect(keys(g))
    skiptypes && filter!(x -> x != "_types", ks)

    if isempty(ks) && prefix == " "
        print(io, "  (no datasets)")
        return
    end

    for i = 1:length(ks)
        k = ks[i]
        islast = i == length(ks)
        isagroup = haskey(g.unwritten_child_groups, k) || isgroup(g.f, lookup_offset(g, k))
        print(io, prefix, islast ? "└─" : "├─", isagroup ? "📂 " : "🔢 ", k)
        if isagroup
            newg = g[k]
            if !isempty(newg)
                print(io, '\n')
                show_group(io, newg, prefix*(islast ? "   " : "│  "))
            end
        end
        !islast && print(io, '\n')
    end
end

function Base.show(io::IO, g::Group)
    println(io, "JLD2.Group")
    show_group(io, g)
end
