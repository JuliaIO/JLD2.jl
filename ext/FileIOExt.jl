module FileIOExt

import JLD2: fileio_load, fileio_save
using JLD2: load, save
using FileIO: FileIO, File, @format_str, filename

fileio_save(f::File{format"JLD2"}, args...; kwargs...) = save(filename(f), args...; kwargs...)
fileio_load(f::File{format"JLD2"}, args...; kwargs...) = load(filename(f), args...; kwargs...)

end
