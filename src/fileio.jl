import FileIO
using FileIO: File, @format_str

# Save all the key-value pairs in the dict as top-level variables of the JLD
function fileio_save(f::File{format"JLD2"}, dict::AbstractDict; kwargs...)
    jldopen(FileIO.filename(f), "w"; kwargs...) do file
        wsession = JLDWriteSession()
        for (k,v) in dict
            if !isa(k, AbstractString)
                throw(ArgumentError("keys must be strings (the names of variables), got $k"))
            end
            write(file, String(k), v, wsession)
        end
    end
end

# Or the names and values may be specified as alternating pairs
function fileio_save(f::File{format"JLD2"}, name::AbstractString, value, pairs...; kwargs...)
    if isodd(length(pairs)) || !isa(pairs[1:2:end], Tuple{Vararg{AbstractString}})
        throw(ArgumentError("arguments must be in name-value pairs"))
    end
    jldopen(FileIO.filename(f), "w"; kwargs...) do file
        wsession = JLDWriteSession()
        write(file, String(name), value, wsession)
        for i = 1:2:length(pairs)
            write(file, String(pairs[i]), pairs[i+1], wsession)
        end
    end
end

fileio_save(f::File{format"JLD2"}, value...; kwargs...) = error("must supply a name for each variable")


# load with just a filename returns a dictionary containing all the variables
function fileio_load(f::File{format"JLD2"}; kwargs...)
    jldopen(FileIO.filename(f), "r"; kwargs...) do file
        loadtodict!(Dict{String,Any}(), file)
    end
end

# When called with explicitly requested variable names, return each one
function fileio_load(f::File{format"JLD2"}, varname::AbstractString; kwargs...)
    jldopen(FileIO.filename(f), "r"; kwargs...) do file
        read(file, varname)
    end
end

fileio_load(f::File{format"JLD2"}, varnames::AbstractString...; kwargs...) =
    fileio_load(f, varnames; kwargs...)

function fileio_load(f::File{format"JLD2"}, varnames::Tuple{Vararg{AbstractString}}; kwargs...)
    jldopen(FileIO.filename(f), "r"; kwargs...) do file
        map((var)->jlread(file, var), varnames)
    end
end
