function jldopen(@nospecialize(f::Function), args...; kws...)
    jld = jldopen(args...; kws...)
    try
        return f(jld)
    finally
        close(jld)
    end
end

"""
    @save filename var1 [var2 ...]
    @save filename {compress=true} var1 name2=var2

Write one or more variables `var1,...` from the current scope to a JLD2 file
`filename`.

For interactive use you can save all variables in the current module's global
scope using `@save filename`. More permanent code should prefer the explicit
form to avoid saving unwanted variables.

# Example

To save the string `hello` and array `xs` to the JLD2 file example.jld2:

    hello = "world"
    xs = [1,2,3]
    @save "example.jld2" hello xs

For passing options to the saving command use {}

    @save "example.jld2" {compress=true} hello xs

For saving variables under a different name use regular assignment syntax

    @save "example.jld2" greeting=hello xarray = xs
"""
macro save(filename, vars...)
    fields = []
    options = []
    for var in vars
        # Capture options of the form {compress = true, mmaparrays=false}
        if @capture(var, {opts__})
            for opt in opts
                if @capture(opt, key_ = val_)
                    push!(options, :($key = $(esc(val))))
                else
                    return :(throw(ArgumentError("Invalid option syntax")))
                end
            end
        # Allow assignment syntax a = b
        elseif @capture(var, a_ = b_)
            push!(fields, :(write(f, $(string(a)), $(esc(b)), wsession)))
        # Allow single arg syntax a   → "a" = a
        elseif @capture(var, a_Symbol)
            push!(fields, :(write(f, $(string(a)), $(esc(a)), wsession)))
        else
            return :(throw(ArgumentError("Invalid field syntax")))
        end
    end
    if !isempty(fields)
        return quote
            let
                f = jldopen($(esc(filename)), "w"; $(Expr(:tuple,options...))...)
                wsession = JLDWriteSession()
                try
                    $(Expr(:block, fields...))
                catch e
                    rethrow(e)
                finally
                    close(f)
                end
            end
        end
    else
        # The next part is old code that handles saving the whole workspace
        quote
            let
                m = $(__module__)
                f = jldopen($(esc(filename)), "w"; $(Expr(:tuple,options...))...)
                wsession = JLDWriteSession()
                try
                    for vname in names(m; all=true)
                        s = string(vname)
                        if !occursin(r"^_+[0-9]*$", s) # skip IJulia history vars
                            (startswith(s, "##") || s == "ans") && continue
                            v = getfield(m, vname)
                            if !isa(v, Module)
                                try
                                    write(f, s, v, wsession)
                                catch e
                                    if isa(e, PointerException)
                                        @warn("skipping $vname because it contains a pointer")
                                    else
                                        rethrow(e)
                                    end
                                end
                            end
                        end
                    end
                finally
                    close(f)
                end
            end
        end
    end
end

"""
    @load filename var1 [var2 ...]

Load one or more variables `var1,...` from JLD2 file `filename` into the
current scope and return a vector of the loaded variable names.

For interactive use, the form `@load "somefile.jld2"` will load all variables
from `"somefile.jld2"` into the current scope. This form only supports literal
file names and should be avoided in more permanent code so that it's clear
where the variables come from.

# Example

To load the variables `hello` and `foo` from the file example.jld2, use

    @load "example.jld2" hello foo
"""
macro load(filename, vars...)
    if isempty(vars)
        if isa(filename, Expr)
            throw(ArgumentError("filename argument must be a string literal unless variable names are specified"))
        end
        # Load all variables in the top level of the file
        readexprs = Expr[]
        vars = Symbol[]
        f = jldopen(filename)
        try
            for n in keys(f)
                if !isgroup(f, lookup_offset(f.root_group, n))
                    push!(vars, Symbol(n))
                end
            end
        finally
            close(f)
        end
    end
    return quote
        ($([esc(x) for x in vars]...),) = jldopen($(esc(filename))) do f
            ($([:(read(f, $(string(x)))) for x in vars]...),)
        end
        $(Symbol[v for v in vars]) # convert to Array
    end
end

function loadtodict!(d::Dict, g::Union{JLDFile,Group}, prefix::String="")
    for k in keys(g)
        v = g[k]
        if v isa Group
            loadtodict!(d, v, prefix*k*"/")
        else
            d[prefix*k] = v
        end
    end
    return d
end

# Name used in JLD2 file to identify objects stored with `save_object`
const SINGLE_OBJECT_NAME = "single_stored_object"

"""
    save_object(filename, x)

Stores an object `x` in a new JLD2 file at `filename`. If a file exists at this
path, it will be overwritten.

Since the JLD2 format requires that all objects have a name, the object will be
stored as `single_sotred_object`. If you want to store more than one object, use
[`@save`](@ref) macro, [`jldopen`](@ref) or the FileIO API.

# Example

To save the string `hello` to the JLD2 file example.jld2:

    hello = "world"
    save_object("example.jld2", hello)
"""
function save_object(filename, x)
  jldopen(filename, "w") do file
    file[SINGLE_OBJECT_NAME] = x
  end
  return
end

"""
    load_object(filename)

Returns the only available object from the JLD2 file `filename` (The stored
object name is inconsequential). If the file contains more than one or no
objects, the function throws an `ArgumentError`.

For loading more than one object, use [`@load`](@ref) macro, [`jldopen`](@ref)
or the FileIO API.

# Example

To load the only object from the JLD2 file example.jld2:

    hello = "world"
    save_object("example.jld2", hello)
    hello_loaded = load_object("example.jld2")
"""
function load_object(filename)
  jldopen(filename, "r") do file
    all_keys = keys(file)
    length(all_keys) == 0 && throw(ArgumentError("File $filename does not contain any object"))
    length(all_keys) > 1 && throw(ArgumentError("File $filename contains more than one object. Use `load` or `@load` instead"))
    file[all_keys[1]] #Uses HDF5 functionality of treating the file like a dict
  end
end


"""
    jldsave(filename, compress=false; kwargs...)

Creates a JLD2 file at `filename` and stores the variables given as keyword arguments.

# Examples

    jldsave("example.jld2"; a=1, b=2, c)

is equivalent to

    jldopen("example.jld2, "w") do f
        f["a"] = 1
        f["b"] = 2
        f["c"] = c
    end


To choose the io type `IOStream` instead of the default `MmapIO` use
`jldsave(fn, IOStream; kwargs...)`.
"""
function jldsave(filename::AbstractString, compress=false, iotype::T=MmapIO;
                    kwargs...
                    ) where T<:Union{Type{IOStream},Type{MmapIO}}
    jldopen(filename, "w"; compress=compress, iotype=iotype) do f
        wsession = JLDWriteSession()
        for (k,v) in pairs(kwargs)
            write(f, string(k), v, wsession)
        end
    end
end

jldsave(filename::AbstractString, iotype::Union{Type{IOStream},Type{MmapIO}}; kwargs...) =
    jldsave(filename, false, iotype; kwargs...)

############################################################################################
## Reading slices of arrays
############################################################################################
Base.getindex(f::JLDFile, name::AbstractString, I...) = f.root_group[name, I...]
function Base.getindex(g::Group, name::AbstractString, I...)
    f = g.f
    f.n_times_opened == 0 && throw(ArgumentError("file is closed"))

    (g, name) = pathize(g, name, false)

    roffset = lookup_offset(g, name)
    if roffset == UNDEFINED_ADDRESS
        haskey(g.unwritten_child_groups, name) && return g.unwritten_child_groups[name]
        throw(KeyError(name))
    end

    if isgroup(f, roffset)
        throw(ArgumentError("Attempted to load a slice of a group"))
    else
        load_dataset_slice(f, roffset, I)
    end
end

# just like `load_dataset` but do carry through the extra argument
function load_dataset_slice(f::JLDFile, offset::RelOffset, slice)
    if haskey(f.jloffset, offset)
        # Stored as WeakRefs and may no longer exist
        val = f.jloffset[offset].value
        val !== nothing && return val[slice]
    end

    io = f.io
    seek(io, fileoffset(f, offset))
    cio = begin_checksum_read(io)
    sz = read_obj_start(cio)
    pmax = position(cio) + sz

    # Messages
    dataspace = ReadDataspace()
    attrs = EMPTY_READ_ATTRIBUTES
    datatype_class::UInt8 = 0
    datatype_offset::Int64 = 0
    data_offset::Int64 = 0
    data_length::Int = -1
    chunked_storage::Bool = false
    filter_id::UInt16 = 0
    while position(cio) <= pmax-4
        msg = jlread(cio, HeaderMessage)
        endpos = position(cio) + msg.size
        if msg.msg_type == HM_DATASPACE
            dataspace = read_dataspace_message(cio)
        elseif msg.msg_type == HM_DATATYPE
            datatype_class, datatype_offset = read_datatype_message(cio, f, (msg.flags & 2) == 2)
        elseif msg.msg_type == HM_FILL_VALUE
            (jlread(cio, UInt8) == 3 && jlread(cio, UInt8) == 0x09) || throw(UnsupportedFeatureException())
        elseif msg.msg_type == HM_DATA_LAYOUT
            jlread(cio, UInt8) == 4 || throw(UnsupportedVersionException())
            storage_type = jlread(cio, UInt8)
            if storage_type == LC_COMPACT_STORAGE
                data_length = jlread(cio, UInt16)
                data_offset = position(cio)
            elseif storage_type == LC_CONTIGUOUS_STORAGE
                data_offset = fileoffset(f, jlread(cio, RelOffset))
                data_length = jlread(cio, Length)
            elseif storage_type == LC_CHUNKED_STORAGE
                # TODO: validate this
                flags = jlread(cio, UInt8)
                dimensionality = jlread(cio, UInt8)
                dimensionality_size = jlread(cio, UInt8)
                skip(cio, Int(dimensionality)*Int(dimensionality_size))

                chunk_indexing_type = jlread(cio, UInt8)
                chunk_indexing_type == 1 || throw(UnsupportedFeatureException("Unknown chunk indexing type"))
                data_length = jlread(cio, Length)
                jlread(cio, UInt32)
                data_offset = fileoffset(f, jlread(cio, RelOffset))
                chunked_storage = true
            else
                throw(UnsupportedFeatureException("Unknown data layout"))
            end
        elseif msg.msg_type == HM_FILTER_PIPELINE
            version = jlread(cio, UInt8)
            version == 2 || throw(UnsupportedVersionException("Filter Pipeline Message version $version is not implemented"))
            nfilters = jlread(cio, UInt8)
            nfilters == 1 || throw(UnsupportedFeatureException())
            filter_id = jlread(cio, UInt16)
            issupported_filter(filter_id) || throw(UnsupportedFeatureException("Unknown Compression Filter $filter_id"))
        elseif msg.msg_type == HM_ATTRIBUTE
            if attrs === EMPTY_READ_ATTRIBUTES
                attrs = ReadAttribute[read_attribute(cio, f)]
            else
                push!(attrs, read_attribute(cio, f))
            end
        elseif (msg.flags & 2^3) != 0
            throw(UnsupportedFeatureException())
        end
        seek(cio, endpos)
    end
    seek(cio, pmax)

    filter_id != 0 && !chunked_storage && throw(InvalidDataException("Compressed data must be chunked"))

    # Checksum
    end_checksum(cio) == jlread(io, UInt32) || throw(InvalidDataException("Invalid Checksum"))

    # TODO verify that data length matches
    val = read_data_slice(f, dataspace, datatype_class, datatype_offset, data_offset, data_length,
                    filter_id, offset, attrs, slice)
    val
end

# like `read_data` but with extra argument
function read_data_slice(f::JLDFile, dataspace::ReadDataspace,
                   datatype_class::UInt8, datatype_offset::Int64,
                   data_offset::Int64, data_length::Int=-1, filter_id::UInt16=UInt16(0),
                   header_offset::RelOffset=NULL_REFERENCE,
                   attributes::Union{Vector{ReadAttribute},Nothing}=nothing,
                   slice=nothing)
    # See if there is a julia type attribute
    io = f.io
    if datatype_class == typemax(UInt8) # Committed datatype
        rr = jltype(f, f.datatype_locations[h5offset(f, datatype_offset)])
        seek(io, data_offset)
        read_dataspace = (dataspace, header_offset, data_length, filter_id)
        read_data_slice(f, rr, read_dataspace, attributes, slice)
    else
        seek(io, datatype_offset)
        @read_datatype io datatype_class dt begin
            rr = jltype(f, dt)
            seek(io, data_offset)
            read_dataspace = (dataspace, header_offset, data_length, filter_id)
            read_data_slice(f, rr, read_dataspace, attributes, slice)
        end
    end
end

# like `read_data` but dispatch to read_array_slice for arrays and warn for scalars
function read_data_slice(f::JLDFile,
    @nospecialize(rr),
    read_dataspace::Tuple{ReadDataspace,RelOffset,Int,UInt16},
    attributes::Union{Vector{ReadAttribute},Nothing}=nothing,
    slice=nothing)

   dataspace, header_offset, data_length, filter_id = read_dataspace
   if dataspace.dataspace_type == DS_SCALAR
       filter_id != 0 && throw(UnsupportedFeatureException())
       @warn "Trying to load a slice of a scalar. Loading scalar instead"
       read_scalar(f, rr, header_offset)
   elseif dataspace.dataspace_type == DS_SIMPLE
       read_array_slice(f, dataspace, rr, data_length, filter_id, header_offset, attributes, slice)
   else
       throw(UnsupportedFeatureException())
   end
end

read_array_size(io::IO, ::Type{T}, ::Val{N}) where {T,N} = ds = reverse(ntuple(i->jlread(io, Int64), Val(N)))

# like read_array but take care of the slicing
function read_array_slice(f::JLDFile, dataspace::ReadDataspace,
                    rr::ReadRepresentation{T,RR}, data_length::Int,
                    filter_id::UInt16, header_offset::RelOffset,
                    attributes::Union{Vector{ReadAttribute},Nothing},
                    slice) where {T,RR}
    io = f.io
    data_offset = position(io)
    ndims, offset = get_ndims_offset(f, dataspace, attributes)

    seek(io, offset)
    ds = read_array_size(io, T, Val(Int(ndims)))
    seek(io, data_offset)
    if filter_id !=0
        @warn "Array is compressed and needs to be fully decompressed for slicing."
        v = Array{T, Int(ndims)}(undef, ds...)
        read_compressed_array!(v, f, rr, data_length, filter_id)
        header_offset !== NULL_REFERENCE && (f.jloffset[header_offset] = WeakRef(v))
        return v[slice...]
    else
        # compute linear indices
        lin_idxs = getindex(LinearIndices(ds), slice...)
        v = Array{T, length(size(lin_idxs))}(undef, size(lin_idxs))
        read_array_slice!(v, f, rr, lin_idxs, prod(ds))
        if all(typeof.(slice) .<: Integer)
            return only(v)
        end
    end
    v
end

# like read_array! but only load relevant indices
function read_array_slice!(v::Array{T}, f::JLDFile{IOStream},
                             rr::ReadRepresentation{T,RR}, lin_idxs, totaln) where {T,RR}
    io = f.io
    startoffset = position(io)
    data = Vector{UInt8}(undef, odr_sizeof(RR))
    dataptr = Ptr{Cvoid}(pointer(data))
    @GC.preserve data begin
        for i = 1:n
            offset = startoffset + (lin_idxs[i]-1)*odr_sizeof(RR)
            seek(io, offset)
            read!(io, data)
            if !jlconvert_canbeuninitialized(rr) || jlconvert_isinitialized(rr, dataptr)
                v[i] = jlconvert(rr, f, dataptr, NULL_REFERENCE)
            end
        end
    end
    seek(io, startoffset + odr_sizeof(RR) * totaln)
    v
end

function read_array_slice!(v::Array{T}, f::JLDFile{MmapIO},
    rr::ReadRepresentation{T,RR}, lin_idxs, totaln) where {T,RR}
    io = f.io
    startptr = io.curptr
    n = length(v)
    for i = 1:n
        inptr = startptr + (lin_idxs[i]-1)*odr_sizeof(RR)
        if !jlconvert_canbeuninitialized(rr) || jlconvert_isinitialized(rr, inptr)
            v[i] = jlconvert(rr, f, inptr, NULL_REFERENCE)
        end
    end
    io.curptr = startptr + odr_sizeof(RR) * totaln
    v
end
