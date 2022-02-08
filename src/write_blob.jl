struct SerializedBlob
    blob::Vector{UInt8}
end

struct Blob
    obj::Any
end

Blob(blob::Blob) = blob

function wconvert(::Type{SerializedBlob}, obj::Blob)
    buffer = IOBuffer()
    Serialization.serialize(buffer, obj)
    result = take!(buffer)
    return SerializedBlob(result)
end

function rconvert(::Type{Blob}, blob::SerializedBlob)
   obj = Serialization.deserialize(IOBuffer(blob.blob))
   return Blob(obj) 
end

writeas(::Type{Blob}) = SerializedBlob

write_blob(f::JLDFile, name, obj; compress=nothing) = 
    write_blob(f.root_group, name, obj; compress)


function write_blob(g::Group, name::AbstractString, obj; compress=nothing)
    f = g.f
    prewrite(f)
    (g, name) = pathize(g, name, true)
    if !isnothing(compress)
        cur_compress = f.compress
        f.compress = compress
        g[name] = write_dataset(f, Blob(obj), JLDWriteSession())
        f.compress = cur_compress
    else
        g[name] = write_dataset(f, Blob(obj), JLDWriteSession())
    end
    return nothing
end

