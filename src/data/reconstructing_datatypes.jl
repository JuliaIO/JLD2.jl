function read_field_datatypes(f::JLDFile, dt::CompoundDatatype, attrs::Vector{ReadAttribute})
    offsets = nothing
    namevec = nothing
    for attr in attrs
        if attr.name == :field_types
            offsets = read_attr_data(f, attr, ReferenceDatatype(),
                            SameRepr{RelOffset}())
        elseif attr.name == :field_names
            namevec = read_attr_data(f, attr, H5TYPE_VLEN_UTF8,
                            MappedRepr{String,Vlen{String}}())
        elseif attr.name == :field_datatypes
            # Legacy: Files written before JLD2 v0.4.54
            offsets = read_attr_data(f, attr, ReferenceDatatype(),
                            SameRepr{RelOffset}())
        end
    end
    isnothing(namevec) && (namevec = string.(dt.names))
    isnothing(offsets) && (offsets = fill(NULL_REFERENCE, length(namevec)))
    namevec::Vector{String}
    offsets::Vector{RelOffset}
    v = [n=>v for (n,v) in zip(namevec,offsets)]
    OrderedDict{String, RelOffset}(v)
end

"""
    readas(::Type)::Type

**Experimental feature**: 
`JLD2.readas` can be overloaded to override which type a saved type is read as,
and is used together with custom serialization using [`JLD2.writeas`](@ref).

The typical case is custom serialization of parametric types,
where not all type parameters are available during reading. 
Consider the following example for an anonymous function `fun` inside a `Foo`
```julia
struct Foo{F<:Function}
    fun::F
end
struct FooSerialization
    fun
end
JLD2.writeas(::Type{<:Foo}) = FooSerialization
Base.convert(::Type{<:FooSerialization}, f::Foo) = FooSerialization(f.fun)

JLD2.readas(::Type{<:FooSerialization}) = Foo
struct UndefinedFunction <:Function
    fun
end
(f::UndefinedFunction)(args...; kwargs...) = error("The function \$(f.fun) is not defined")
function Base.convert(::Type{<:Foo}, f::FooSerialization)
    isa(f.fun, Function) && return Foo(f.fun)
    return Foo(UndefinedFunction(f.fun))
end
```
If we include these definitions, call `jldsave("foo.jld2"; foo=Foo(x->x^2))`,
restart julia, include the definitions again, and call
`foo = jldopen("foo.jld2") do io; io["foo"]; end`, we get
`foo::Foo{UndefinedFunction}` and `foo::FooSerialization`
with and without defining the `JLD2.readas` above, respectively.
"""
readas(::Any) = nothing # default to nothing to do nothing if no overload is specified. 

function _readas(T_custom, T_in)
    T_out = readas(T_custom)::Union{Type,Nothing}
    return ifelse(isnothing(T_out), T_in, T_out)
end

# jltype is the inverse of h5type, providing a ReadRepresentation for an
# H5Datatype. We handle committed datatypes here, and other datatypes below.
function jltype(f::JLDFile, sdt::Union{SharedDatatype,CommittedDatatype})
    cdt = get(f.datatype_locations, sdt.header_offset, sdt)
    haskey(f.h5jltype, cdt) && return f.h5jltype[cdt]::ReadRepresentation
    
    dt, attrs = read_shared_datatype(f, cdt)

    julia_type_attr = nothing
    written_type_attr = nothing
    for attr in attrs
        if attr.name == :julia_type || attr.name == Symbol("julia type")
            julia_type_attr = attr
        elseif attr.name == :written_type
            written_type_attr = attr
        end
    end
    isnothing(julia_type_attr) && return f.h5jltype[cdt] = jltype(f, dt)

    # Bootstrap: the datatype of datatype is a datatype
    if julia_type_attr.datatype == SharedDatatype(cdt.header_offset)
        if dt != H5TYPE_DATATYPE
            throw(InternalError("""The HDF5 datatype representing a Julia datatype does not match
                     the expectations of this version of JLD2.
                     You may need to update JLD2 to read this file."""))
        end
        f.jlh5type[DataType] = cdt
        f.datatypes[cdt.index] = dt
        return (f.h5jltype[cdt] = MappedRepr{DataType, DataTypeODR}())
    end

    f.plain && return f.h5jltype[cdt] = jltype(f, dt)

    datatype = read_attr_data(f, julia_type_attr)

    if !isnothing(written_type_attr)
        # Custom serialization
        custom_datatype = read_attr_data(f, written_type_attr)
        read_as = _readas(custom_datatype, datatype)
        if read_as <: UnknownType
            @warn("custom serialization of $(typestring(read_as))" *
                  " encountered, but the type does not exist in the workspace; the data will be read unconverted")
            rr, _ = constructrr(f, custom_datatype, dt, attrs)
            canonical = false
        else
            rr, canonical = constructrr(f, custom_datatype, dt, attrs)
            rr = MappedRepr{read_as, CustomSerialization{julia_repr(rr), file_repr(rr)}}()
            canonical &= writeas(read_as) === custom_datatype
        end
    else
        rr, canonical = constructrr(f, datatype, dt, attrs)
    end

    canonical && (f.jlh5type[datatype] = cdt)
    f.datatypes[cdt.index] = dt
    f.h5jltype[cdt] = rr
end



# Constructs a ReadRepresentation for a given opaque (bitstype) type
function constructrr(::JLDFile, T::DataType, dt::BasicDatatype, attrs::Vector{ReadAttribute})
    dt.class == DT_OPAQUE || throw(UnsupportedFeatureException())
    if sizeof(T) == dt.size && isempty(T.types)
        return (SameRepr{T}(), true)
    end
    empty = any(a->a.name==:empty, attrs)
    if empty
        !hasdata(T) && return (MappedRepr{T,nothing}(), true)
        @warn("$T has $(sizeof(T)*8) bytes, but written type was empty; reconstructing")
    elseif isempty(T.types)
        @warn("primitive type $T has $(sizeof(T)*8) bits, but written type has $(dt.size*8) bits; reconstructing")
    else
        @warn("$T is a non-primitive type, but written type is a primitive type with $(dt.size*8) bits; reconstructing")
    end
    reconstruct_bitstype(T.name.name, dt.size, empty)
end

struct TypeMappingException <: Exception end


unpack_odr(::Type{<:OnDiskRepresentation{Offsets,JLTypes,H5Types}}) where {Offsets,JLTypes,H5Types} =
    (Offsets, JLTypes.parameters, H5Types.parameters)



"""
    constructrr(f::JLDFile, T::DataType, dt::CompoundType, attrs::Vector{ReadAttribute},
                hard_failure::Bool=false)

Constructs a [`ReadRepresentation`](@ref) for a given type. This is the generic method for all
types not specially handled below.

If `hard_failure` is true, then throw a `TypeMappingException` instead of attempting
reconstruction. This helps in cases where we can't know if reconstructed parametric types
will have a matching memory layout without first inspecting the memory layout.
"""
@nospecializeinfer function constructrr(f::JLDFile, @nospecialize(T::DataType), dt::CompoundDatatype,
                     attrs::Vector{ReadAttribute},
                     hard_failure::Bool=false)
    field_datatypes = read_field_datatypes(f, dt, attrs)
    # If read type is not a leaf type, reconstruct
    if !isconcretetype(T)
        @warn("read type $T is not a leaf type in workspace; reconstructing")
        return reconstruct_compound(f, string(T), dt, field_datatypes)
    end
    dtnames = Dict{Symbol,Int}(sym => n for (n,sym) in enumerate(dt.names))
    mapped = falses(length(dt.names))

    offsets = Vector{Int}(undef, length(T.types))
    types = Vector{Any}(undef, length(T.types))
    odrs = Vector{Any}(undef, length(T.types))
    fn = fieldnames(T)
    samelayout = isbitstype(T) && sizeof(T) == dt.size
    dtindex = 0
    for i = 1:length(T.types)
        wstype = T.types[i]
        writtenas = writeas(wstype)
        if !hasfielddata(writtenas)
            types[i] = wstype
            odrs[i] = wstype === writtenas ? nothing : CustomSerialization{writtenas,nothing}
            offsets[i] = dtindex == 0 ? 0 : (dt.offsets[dtindex] + dt.members[dtindex].size::UInt32)
        else
            if !haskey(dtnames, fn[i])
                hard_failure && throw(TypeMappingException())
                @warn("saved type $T is missing field $(fn[i]) in workspace type; reconstructing")
                return reconstruct_compound(f, string(T), dt, field_datatypes)
            end

            dtindex = dtnames[fn[i]]
            if (ref = field_datatypes[string(fn[i])]) != NULL_REFERENCE
                dtrr = jltype(f, f.datatype_locations[ref])
            else
                dtrr = jltype(f, dt.members[dtindex])
            end

            readtype = julia_repr(dtrr)
            odrtype = file_repr(dtrr)

            if typeintersect(readtype, wstype) === Union{} &&
               !hasmethod(convert, Tuple{Type{wstype}, readtype})
                # Saved type does not match type in workspace and no
                # convert method exists, so we definitely need to reconstruct.
                hard_failure && throw(TypeMappingException())
                @warn("saved type $T has field $(fn[i])::$(readtype)" *
                      ", but workspace type has field $(fn[i])::$(wstype)" *
                      ", and no applicable convert method exists; reconstructing")
                return reconstruct_compound(f, string(T), dt, field_datatypes)
            end

            types[i] = readtype
            odrs[i] = odrtype
            offsets[i] = dt.offsets[dtindex]

            # The on disk representation of T can only be the same as in memory
            # if the offsets are the same, field type on disk (readtype) and in memory (wstype)
            # are the same and if no CustomSerialization is involved
            samelayout = samelayout && 
                offsets[i] == fieldoffset(T, i) && 
                types[i] === wstype && 
                # An OnDiskRepresentation as odr means that something "fixable" went wrong
                # for this field
                !(odrs[i] <: OnDiskRepresentation) && 
                !(odrs[i] <: CustomSerialization)
            mapped[dtindex] = true
        end
    end

    if !all(mapped)
        @warn("the following fields are present in type $T" *
              " saved in the file but not present in the type in the workspace:\n\n" *
              "$(join(dt.names[.!mapped], "\n"))," *
              "\n\nData in these fields will not be accessible")
    end

    samelayout && return (SameRepr{T}(), true)
    offsets = (offsets...,)
    if (wodr = odr(T)) <: OnDiskRepresentation
        odr_offsets, odr_types, odr_h5types = unpack_odr(wodr)
        tequal = length(odr_types) == length(types)
        for i = 1:length(types)
            tequal || break
            tequal &= odr_types[i] <: types[i]
            tequal &= odr_h5types[i] == odrs[i]
        end
        tequal &= odr_offsets == offsets
        tequal && return (MappedRepr{T,wodr}(), true)
    end
    return (MappedRepr{T,OnDiskRepresentation{offsets, Tuple{types...}, Tuple{odrs...}, Int(offsets[end]+odr_sizeof(odrs[end]))}}(), false)
end

function constructrr(f::JLDFile, u::Upgrade, dt::CompoundDatatype,
                     attrs::Vector{ReadAttribute},
                     hard_failure::Bool=false)
    field_datatypes = read_field_datatypes(f, dt, attrs)
    rodr = reconstruct_odr(f, dt, field_datatypes)
    fnames = tuple((Symbol(k) for k in keys(field_datatypes))...)
    T2 = NamedTuple{fnames, rodr.parameters[2]}
    return (MappedRepr{u.target, CustomSerialization{T2, rodr}}(), false)    
end

function constructrr(f::JLDFile, u::Upgrade, dt::BasicDatatype, 
                     attrs::Vector{ReadAttribute},
                     hard_failure::Bool=false)
    return (MappedRepr{u.target, CustomSerialization{NamedTuple{(), Tuple{}},nothing}}(), false)    
end

function constructrr(f::JLDFile, T::UnionAll, dt::CompoundDatatype,
                     attrs::Vector{ReadAttribute},
                     hard_failure::Bool=false)
    @warn("read type $T is not a leaf type in workspace; reconstructing")
    return reconstruct_compound(f, string(T), dt, read_field_datatypes(f, dt, attrs))
end

# Find types in modules
# returns the result of searching for the type in the specified module m
function _resolve_type_singlemodule(::MappedRepr{T,DataTypeODR},
                                    m,
                                    parts,
                                    mypath,
                                    hasparams::Bool,
                                    params) where T
    for part in parts
        sym = Symbol(part)
        (!isa(m, Module) || !isdefined(m, sym)) && return nothing
        m = getfield(m, sym)
    end
    (!isa(m, DataType) && !isa(m, UnionAll)) && return nothing
    return m
end

isunknowntype(x) = false
isunknowntype(::Type{Union{}}) = false
isunknowntype(x::Type) = x <: UnknownType ? true : false

function _resolve_type(rr::MappedRepr{T,DataTypeODR},
                       f::JLDFile,
                       ptr::Ptr,
                       header_offset::RelOffset,
                       mypath,
                       hasparams::Bool,
                       params) where T
    parts = split(mypath, '.')
    for mod in Base.loaded_modules_array()
        resolution_attempt = _resolve_type_singlemodule(rr,
                                                        mod,
                                                        parts,
                                                        mypath,
                                                        hasparams,
                                                        params)
        !isnothing(resolution_attempt) && return resolution_attempt
    end
    return UnknownType{Symbol(mypath), Tuple{ifelse(hasparams,params,())...}}
end



function types_from_refs(f::JLDFile, ptr::Ptr)
    # Test for a potential null pointer indicating an empty array
    isinit = jlunsafe_load(pconvert(Ptr{UInt32}, ptr)) != 0
    unknown_params = false
    if isinit
        refs = jlconvert(MappedRepr{RelOffset, Vlen{RelOffset}}(), f, ptr, NULL_REFERENCE)
        params =  Any[let
            # If the reference is to a committed datatype, read the datatype
            nulldt = CommittedDatatype(UNDEFINED_ADDRESS, 0)
            cdt = get(f.datatype_locations, ref, nulldt)
            res = cdt !== nulldt ? julia_repr(jltype(f, cdt)) : load_dataset(f, ref)
            unknown_params |= isunknowntype(res) || isreconstructed(res)
            res
        end for ref in refs]
        return params, unknown_params
    end
    return [], unknown_params
end

# Read a type. Returns an instance of UnknownType if the type or parameters
# could not be resolved.
function jlconvert(rr::MappedRepr{<:Type,DataTypeODR},
                   f::JLDFile,
                   ptr::Ptr,
                   header_offset::RelOffset)

    params, unknown_params = types_from_refs(f, ptr+odr_sizeof(Vlen{UInt8}))
    # For cross-platform compatibility convert integer type parameters to system precision
    params = map(params) do p
        if p isa Union{Int64,Int32}
            Int(p)
        elseif p isa Upgrade
            p.target
        else
            p
        end
    end
    hasparams = !isempty(params)
    mypath = String(jlconvert(MappedRepr{UInt8,Vlen{UInt8}}(), f, ptr, NULL_REFERENCE))

    if mypath in keys(f.typemap)
        m = f.typemap[mypath]
        m isa Upgrade && return m
    else
        m = _resolve_type(rr, f, ptr, header_offset, mypath, hasparams, hasparams ? params : nothing)
    end
    isunknowntype(m) && return m
    unknown_params && return UnknownType{m, Tuple{params...}}
    if hasparams
        if f.plain && !(m === Tuple)
            return Any
        end
        try
            m = m{params...}
        catch e
            return UnknownType{m, Tuple{params...}}
        end
    elseif m === Tuple
        # Need to instantiate with no parameters, since Tuple is really
        # Tuple{Vararg{Any}}
        m = Tuple{}
    end
    track_weakref!(f, header_offset, m)
    return m
end

constructrr(::JLDFile, ::Type{T}, dt::CompoundDatatype, ::Vector{ReadAttribute}) where {T<:DataType} =
    dt == H5TYPE_DATATYPE ? (MappedRepr{DataType,DataTypeODR}(), true) :
                            throw(UnsupportedFeatureException())


## Type reconstruction
abstract type AbstractReconstructedType{N} end

struct ReconstructedPrimitive{N, T} <: AbstractReconstructedType{N}
    val::T
end
function Base.show(io::IO, f::ReconstructedPrimitive{N}) where {N}
    print(io, "Reconstruct@$N($(Int(f.val)))")
end


struct ReconstructedSingleton{N} <: AbstractReconstructedType{N} end

function Base.show(io::IO, ::ReconstructedSingleton{N}) where {N}
    print(io, "Reconstruct@$N()")
end

struct ReconstructedStatic{N, FN, NT} <: AbstractReconstructedType{N}
    fields::NamedTuple{FN, NT}
end

Base.getproperty(rc::ReconstructedStatic, s::Symbol) = getproperty(getfield(rc, 1),s)
Base.propertynames(rc::ReconstructedStatic) = propertynames(getfield(rc, 1))

# simple one-line display (without trailing line break)
function Base.show(io::IO, f::ReconstructedStatic{N, FN, FT}) where {N,FN,FT}
    print(io, "Reconstruct@$N($(values(getfield(f,1))))")
end

struct ReconstructedMutable{N, FN, FT} <: AbstractReconstructedType{N}
    fields::Vector{Any}
end

function Base.getproperty(rc::ReconstructedMutable{N,FN,FT}, s::Symbol) where {N,FN,FT}
    i = findfirst(==(s), FN)
    isnothing(i) && throw(ArgumentError("field $s not found"))
    getfield(rc, 1)[i]::FT.parameters[i]
end
Base.propertynames(::ReconstructedMutable{N,FN}) where {N, FN} = FN


function Base.show(io::IO, f::ReconstructedMutable{N, FN, FT}) where {N,FN,FT}
    print(io, "Reconstruct@$N($(getfield(f,1)))")
end



isreconstructed(x) = isreconstructed(typeof(x))
isreconstructed(x::Type{<:AbstractReconstructedType}) = true
isreconstructed(x::Type) = false
isreconstructed(x::Type{Union{}}) = false

function reconstruct_bitstype(name::Union{Symbol,String}, size::Integer, empty::Bool)
    if empty
        return (MappedRepr{ReconstructedSingleton{Symbol(name)}, nothing}(), false)
    else
        T = ReconstructedPrimitive{Symbol(name), uintofsize(size)}
        return (SameRepr{T}(), false)
    end
end

function constructrr(f::JLDFile, unk::Type{<:UnknownType}, dt::BasicDatatype,
                     attrs::Vector{ReadAttribute})
    @warn("type $(typestring(unk)) does not exist in workspace; reconstructing")
    reconstruct_bitstype(shorttypestring(unk), dt.size, any(a->a.name==:empty, attrs))
end

"""
    typestring(::Type{ <:UnknownType})

Convert an `UnknownType` to a corresponding string. This is
only used for warning during reconstruction errors.

See also [`shorttypestring`](@ref).
"""
function typestring(UT)# ::Type{<:UnknownType}
    if UT isa UnionAll
        UT = behead(UT)
        T = UT.parameters[1]
        params = ()
    else
        T = UT.parameters[1]
        params = UT.parameters[2].parameters
    end
    tn = IOBuffer()
    print(tn, T)
    if !isempty(params)
        write(tn, '{')
        join(tn, (isunknowntype(x) ? typestring(x) : x for x in params), ',')
        write(tn, '}')
    end
    String(take!(tn))
end


"""
    shorttypestring(::Type{ <:UnknownType})

Convert an `UnknownType` to a corresponding string. This is
only used to create names for reconstructed types.

See also [`typestring`](@ref).
"""
function shorttypestring(UT) #::Type{<:UnknownType}
    if UT isa UnionAll
        UT = behead(UT)
        T = UT.parameters[1]
        params = ()
    else
        T = UT.parameters[1]
        params = UT.parameters[2].parameters
    end
    tn = IOBuffer()
    print(tn, T isa Symbol ? split(string(T),'.')[end] : T)
    if !isempty(params)
        write(tn, '{')
        join(tn, (isunknowntype(x) ? shorttypestring(x) : x for x in params), ',')
        write(tn, '}')
    end
    String(take!(tn))
end

"""
    behead(T)

Given a `UnionAll` type, recursively eliminates the `where` clauses
"""
behead(T::UnionAll) = behead(T.body)
behead(@nospecialize T) = T


function constructrr(f::JLDFile, unk::Type{UnknownType{T,P}}, dt::CompoundDatatype,
    attrs::Vector{ReadAttribute}) where {T,P}
    field_datatypes = read_field_datatypes(f, dt, attrs)
    if T isa DataType
        if T === Tuple
            # For a tuple with unknown fields, we should reconstruct the fields
            rodr = reconstruct_odr(f, dt, field_datatypes)
            # This is a "pseudo-RR" since the tuple is not fully parametrized, but
            # the parameters must depend on the types actually encoded in the file
            return (MappedRepr{Tuple,rodr}(), false)
        else
            @warn("read type $(typestring(unk)) was parametrized, but type " *
            "$(T) in workspace is not; reconstructing")
        end
    elseif T isa UnionAll
        body = behead(T)
        if length(body.parameters) != length(P.parameters)
            @warn("read type $(T) has a different number of parameters from type " *
                "$(T) in workspace; reconstructing")
            reconstruct_compound(f, string(T), dt, field_datatypes)
        else
            params = [P.parameters...,]
            for i = 1:length(params)
                if isunknowntype(params[i])
                    param = body.parameters[i]::TypeVar
                    params[i] = param.ub
                end
            end

            # Try to construct the rr for the relaxed type. On failure, fall back to
            # reconstruct_compound.`
            T2 = try
                T{params...}
            catch err
                @warn("type parameters for $(typestring(unk)) do not match type $(T) in workspace; reconstructing")
                return reconstruct_compound(f, shorttypestring(unk), dt, field_datatypes)
            end

            try
                (rr,) = constructrr(f, T2, dt, attrs, true)
                @warn("some parameters could not be resolved for type $(typestring(unk)); reading as $T2")
                return (rr, false)
            catch err
                !isa(err, TypeMappingException) && rethrow(err)
                @warn("some parameters could not be resolved for type $(typestring(unk)); reconstructing")
            end
        end
    elseif T isa Symbol
        @warn("type $(typestring(unk)) does not exist in workspace; reconstructing")
    else
        throw(InternalError("Something went very wrong here."))
    end
    reconstruct_compound(f, shorttypestring(unk), dt, field_datatypes)
end



# Reconstruct the ODR of a type from the CompoundDatatype and field_datatypes
# attribute
function reconstruct_odr(f::JLDFile, dt::CompoundDatatype,
                         field_datatypes::OrderedDict{String,RelOffset})
    # Get the type and ODR information for each field
    types = []
    h5types = []
    offsets = Int[]
    offset = 0
    for (k,typeref) in field_datatypes
        i = findfirst(==(Symbol(k)), dt.names)
        if typeref != NULL_REFERENCE
            dtrr = jltype(f, f.datatype_locations[typeref])
        elseif !isnothing(i)
            offset == dt.offsets[i] || throw(InternalError("Field offsets were incorrectly mapped."))
            dtrr = jltype(f, dt.members[i])
        else
            throw(InternalError("Field $k not found in datatype"))
        end
        push!(types, julia_repr(dtrr))
        push!(h5types, file_repr(dtrr))
        push!(offsets, offset)
        offset += odr_sizeof(dtrr)
    end
    OnDiskRepresentation{(offsets...,), Tuple{types...}, Tuple{h5types...},Int(dt.size)}
end

# Reconstruct type that is a "lost cause": either we were not able to resolve
# the name, or the workspace type has additional fields, or cannot convert
# fields to workspace types
function reconstruct_compound(f::JLDFile, T::String, dt::H5Datatype,
                              field_datatypes::OrderedDict{String,RelOffset})
    rodr = reconstruct_odr(f, dt, field_datatypes)
    _, types, odrs = unpack_odr(rodr)
    fnames = tuple((Symbol(k) for k in keys(field_datatypes))...,)
    if !any(jlconvert_canbeuninitialized(ReadRepresentation(types[i], odrs[i])) for i = 1:length(types))
        rt = ReconstructedStatic{Symbol(T), fnames, Tuple{types...}}
        odr = OnDiskRepresentation{(0,), Tuple{NamedTuple{fnames,Tuple{types...}}}, Tuple{rodr}, Int(dt.size)}
        return (MappedRepr{rt, odr}(), false)
    end
    T = ReconstructedMutable{Symbol(T), fnames, Tuple{types...}}
    return MappedRepr{T, rodr}(), false
end

# At present, we write Union{} as an object of Core.TypeofBottom. The method above
# basically works, but `Expr(:new, Type{Union{}})` is a bit weird and causes problems for
# inference. Better to define a separate method.
jlconvert(::MappedRepr{Core.TypeofBottom,nothing}, f::JLDFile, ptr::Ptr,
          header_offset::RelOffset) = Union{}

function jlconvert(::MappedRepr{T,S}, f::JLDFile, ptr::Ptr, header_offset::RelOffset) where {T<:ReconstructedMutable, S<:OnDiskRepresentation}
    offsets, types, odrs = unpack_odr(S)
    res = Vector{Any}(undef, length(types))
    for i = 1:length(types)
        rr = ReadRepresentation(types[i],odrs[i])
        if !(jlconvert_canbeuninitialized(rr)) || jlconvert_isinitialized(rr, ptr+offsets[i]) 
            res[i] = jlconvert(rr, f, ptr+offsets[i], NULL_REFERENCE)
        end
    end
    return T(res)
end

jlconvert(::ReadRepresentation{T, S}, f::JLDFile, ptr::Ptr, header_offset::RelOffset) where {T,S} =
    rconvert(T, jlunsafe_load(pconvert(Ptr{S}, ptr)))

# This jlconvert method handles compound types with padding or references
@generated function jlconvert(::MappedRepr{T,S}, f::JLDFile, ptr::Ptr,
                              header_offset::RelOffset) where {T,S<:OnDiskRepresentation}
    offsets, types, odrs = unpack_odr(S)
    fn = T === Tuple ? [Symbol(i) for i = 1:length(types)] : fieldnames(T)

    if ismutabletype(T)
        blk = quote
            obj = $(Expr(:new, T))
            track_weakref_if_untracked!(f, header_offset, obj)
        end
        for i = 1:length(types)
            rtype = types[i]
            rr = ReadRepresentation(rtype,odrs[i])
            ttype = T.types[i]
            if isnothing(odrs[i])
                # Type is not stored or single instance
                fieldval = Expr(:new, ttype)               
            else
                fieldval = :(rconvert($ttype, jlconvert($rr, f, ptr+$(offsets[i]), NULL_REFERENCE)::$rtype)::$ttype)
            end
            # use jl_set_nth_field instead of setfield! since the former also works for const fields
            # in mutable structs.
            setfield = :(ccall(:jl_set_nth_field, Nothing, (Any, Csize_t, Any), obj, ($i)-1, $fieldval))
            if jlconvert_canbeuninitialized(rr)
                setfield = :(jlconvert_isinitialized($rr, ptr+$(offsets[i])) && $(setfield))
            end
            push!(blk.args, setfield)
        end
        push!(blk.args, (:obj))    
        return blk
    end
    blk = Expr(:block)
    fsyms = Symbol[]
    for i = 1:length(types)
        ptr = :(ptr + $(offsets[i]))
        rtype = types[i]
        fsym = Symbol("field_", fn[i])
        push!(fsyms, fsym)
        rr = ReadRepresentation(rtype,odrs[i])

        if isnothing(odrs[i])
            push!(blk.args, :($fsym = $(Expr(:new, rtype))))
            continue
        end
        if jlconvert_canbeuninitialized(rr)
            push!(blk.args, quote
                if !jlconvert_isinitialized($rr, $ptr)
                    $(if T <: Tuple || i <= ninitialized(T)
                        # Reference must always be initialized
                        :(throw(UndefinedFieldException(T,$(QuoteNode(fn[i])))))
                    else
                        Expr(:return, Expr(:new, T, fsyms[1:i-1]...))
                    end)
                end
            end)
        end
        fieldval = :(jlconvert($rr, f, $ptr, NULL_REFERENCE)::$rtype)
        if T !== Tuple
            # Special case for reconstructed tuples, where we don't know the
            # field types in advance
            fieldval = :(rconvert($(T.types[i]), $fieldval)::$(T.types[i]))
        end
        push!(blk.args, :($fsym = $fieldval))
    end
    push!(blk.args, T <: Tuple ? Expr(:tuple, fsyms...) : Expr(:new, T, fsyms...))
    blk
end
