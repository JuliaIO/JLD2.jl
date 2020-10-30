function jlread(io::IO)
    startpos = position(io)
    f = JLDFile(io, "", false, false, false, false)

    @show startpos

    # Extract the file header
    header = String(read!(io, Vector{UInt8}(undef, length(REQUIRED_FILE_HEADER))))
    header == REQUIRED_FILE_HEADER || throw(ArgumentError("Invalid JLD2 header"))

    @show header

    # Read the version number
    ver = VersionNumber(read_bytestring(io))

    @show ver

    if ver > FORMAT_VERSION
        @warn("""Data was written in a newer version of the JLD2 file format.
        Please consider updating JLD2.""", maxlog=1)
    end
    if ver ∉ COMPATIBLE_VERSIONS
        @warn("""Data was written with a different version of JLD2 that may not be compatible.
         Attempting to load data.""", maxlog=1)
    end

    # Jump to the end of the file header
    seek(io, startpos + FILE_HEADER_LENGTH)
    @show position(io)

    superblock = read(io, Superblock)
    f.end_of_data = superblock.end_of_file_address
    f.root_group_offset = superblock.root_group_object_header_address
    f.root_group = load_group(f, superblock.root_group_object_header_address)

    @show position(io)

    if haskey(f.root_group.written_links, "_types")
        types_group_offset = f.root_group.written_links["_types"]
        f.types_group = f.loaded_groups[types_group_offset] = load_group(f, types_group_offset)
        i = 0

        for offset in values(f.types_group.written_links)
            f.datatype_locations[offset] = CommittedDatatype(offset, i += 1)
        end

        resize!(f.datatypes, length(f.datatype_locations))
    else
        f.types_group = Group{typeof(f)}(f)
    end

    @show position(io)

    result = Dict{String, Any}()
    loadtodict!(result, f)
    return result
end

function jlwrite(io::IO, data::AbstractDict)
    startpos = position(io)
    f = JLDFile(io, "", true, true,false,false)
    @show f.end_of_data

    f.root_group = Group{typeof(f)}(f)
    f.types_group = Group{typeof(f)}(f)

    # Write stuff
    wsession = JLDWriteSession()
    for (k, v) in data
        write(f, String(k), v, wsession)
    end

    @show position(io)

    #  The following block is taken from `close(f)` just without actually closing the io
    # Save any groups we know of that have been modified
    for group in values(f.loaded_groups)
        save_group(group)
    end
    if !isempty(f.types_group) && !haskey(f.root_group, "_types")
        f.root_group["_types"] = f.types_group
    end
    res = save_group(f.root_group)
    if f.root_group_offset == UNDEFINED_ADDRESS
        f.root_group_offset = res
    end

    @show position(io)
    # Write JLD2 header
    seek(io, startpos)
    write(io, FILE_HEADER)

    # Write superblock
    seek(io, FILE_HEADER_LENGTH)
    write(io, Superblock(0, FILE_HEADER_LENGTH, UNDEFINED_ADDRESS,
            f.end_of_data, f.root_group_offset))

    @show f.end_of_data
    truncate(io, f.end_of_data)
end
