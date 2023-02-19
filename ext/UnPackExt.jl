# —————————————————————————————————————————————————————————————————————————————————————— #
#             Extension of @unpack and @pack! function from UnPack.jl package            #
# —————————————————————————————————————————————————————————————————————————————————————— #
using .UnPack

"""
Unpack fields from JLD2-structs created with e.g. `file = jldopen(path)`. Also works for JLD2-
groups. Useful for quick exrtraction.

# Example
```julia
file = jldopen(path) # contains fields :x, :y
@unpack x, y = file # Extracts x and y from the struct
```
"""
@inline function UnPack.unpack(x::Union{T, Group{T}}, ::Val{f}) where {f, T<:JLDFile{MmapIO}}
    return x[string(f)]
end

"""
Pack variables into JLD2-structs. Also works for JLD2- groups. Useful for quick saving.

# Example
```julia
x = rand(10)
path = "temp.jld2"
file = jldopen(path, "w")
@pack! file = x # Creates entry with name x
close(file)
```
"""
@inline function UnPack.pack!(file::Union{T, Group{T}}, ::Val{f}, val) where {f, T<:JLDFile{MmapIO}}
    file[string(f)] = val
end
