
using REPL
using REPL.REPLCompletions: Completion, PathCompletion

function REPL.REPLCompletions.complete_path(path::AbstractString, pos::Int; use_envpath=false, shell_escape=false)
    if Base.Sys.isunix() && occursin(r"^~(?:/|$)", path)
        # if the path is just "~", don't consider the expanded username as a prefix
        if path == "~"
            dir, prefix = homedir(), ""
        else
            dir, prefix = splitdir(homedir() * path[2:end])
        end
    else
        dir, prefix = splitdir(path)
    end

    local files
    try
        if isempty(dir)
            files = readdir()
        elseif isdir(dir)
            files = readdir(dir)
        else
            #return Completion[], 0:-1, false
            files = String[]
        end
    catch
        return Completion[], 0:-1, false
    end

    matches = Set{String}()
    for file in files
        if startswith(file, prefix)
            id = try isdir(joinpath(dir, file)) catch; false end
            # joinpath is not used because windows needs to complete with double-backslash
            push!(matches, id ? file * (@static Sys.iswindows() ? "\\\\" : "/") : file)
        end
    end

    for wref in values(OPEN_FILES)
        f = wref.value
        if f === nothing
            continue
        end
        f::JLDFile
        if isempty(dir)
            entries = keys(f.root_group)
        elseif haskey(f, dir) || dir == "/"
            entries = keys(f[dir])
        else
            continue
        end
        for entry in entries
            if startswith(entry, prefix)
                ig = isgroup(f, joinpath(dir,entry))
                push!(matches, ig ? entry*"/" : entry)
            end
        end
    end

    if use_envpath && length(dir) == 0
        # Look for files in PATH as well
        local pathdirs = split(ENV["PATH"], @static Sys.iswindows() ? ";" : ":")

        for pathdir in pathdirs
            local actualpath
            try
                actualpath = realpath(pathdir)
            catch
                # Bash doesn't expect every folder in PATH to exist, so neither shall we
                continue
            end

            if actualpath != pathdir && in(actualpath,pathdirs)
                # Remove paths which (after resolving links) are in the env path twice.
                # Many distros eg. point /bin to /usr/bin but have both in the env path.
                continue
            end

            local filesinpath
            try
                filesinpath = readdir(pathdir)
            catch e
                # Bash allows dirs in PATH that can't be read, so we should as well.
                if isa(e, Base.IOError) || isa(e, Base.ArgumentError)
                    continue
                else
                    # We only handle IOError and ArgumentError here
                    rethrow()
                end
            end

            for file in filesinpath
                # In a perfect world, we would filter on whether the file is executable
                # here, or even on whether the current user can execute the file in question.
                if startswith(file, prefix) && isfile(joinpath(pathdir, file))
                    push!(matches, file)
                end
            end
        end
    end

    matchList = Completion[PathCompletion(shell_escape ? replace(s, r"\s" => s"\\\0") : s) for s in matches]
    startpos = pos - lastindex(prefix) + 1 - count(isequal(' '), prefix)
    # The pos - lastindex(prefix) + 1 is correct due to `lastindex(prefix)-lastindex(prefix)==0`,
    # hence we need to add one to get the first index. This is also correct when considering
    # pos, because pos is the `lastindex` a larger string which `endswith(path)==true`.
    return matchList, startpos:pos, !isempty(matchList)
end


function REPL.REPLCompletions.close_path_completion(str, startpos, r, paths, pos)
    length(paths) == 1 || return false  # Only close if there's a single choice...
    _path = str[startpos:prevind(str, first(r))] * (paths[1]::PathCompletion).path
    path = expanduser(replace(_path, r"\\ " => " "))
    # ...except if it's a directory...
    try
        isdir(path)
    catch e
        e isa Base.IOError || rethrow() # `path` cannot be determined to be a file
    end && return false

    for wref in values(OPEN_FILES)
        f = wref.value
        if f === nothing
            continue
        end
        f::JLDFile
        if isgroup(f, path)
            return false
        end
    end
    # ...and except if there's already a " at the cursor.
    return lastindex(str) <= pos || str[nextind(str, pos)] != '"'
end
