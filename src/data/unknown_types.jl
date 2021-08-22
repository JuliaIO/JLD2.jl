## Type reconstruction

module ReconstructedTypes end

# Construct a datatype from type parameters, field names, and field types
function create_type(T, typeparams, fieldnames, fieldtypes)
    reconname = gensym(T)
    @warn "Unable to match type $T. Reconstructing to $reconname"
    typeparamnames = Symbol.(('A':'Z')[1:length(typeparams)])
    fieldtypes = [(ft isa SelfReferentialPlaceholder ? reconname : ft) for ft in fieldtypes]
    Core.eval(ReconstructedTypes,
              Expr(:struct, false, :($reconname{$(typeparamnames...)}),
                   Expr(:block, Any[ Expr(Symbol("::"), Symbol(fieldnames[i]), fieldtypes[i]) for i = 1:length(fieldtypes) ]...,
                        # suppress default constructors, plus a bogus `new()` call to make sure
                        # ninitialized is zero.
                        Expr(:if, false, Expr(:call, :new)))))
    T = getfield(ReconstructedTypes, reconname)
    if !isempty(typeparams)
        return T{typeparams...}
    else
        return T
    end
end

isreconstructed(x) = isreconstructed(typeof(x))

function isreconstructed(::Type{T}) where T
    return supertype(T) == Any && parentmodule(T) == ReconstructedTypes
end

function reconstruct_bitstype(name::Union{Symbol,String}, size::Integer, empty::Bool)
    sym = gensym(name)
    Core.eval(ReconstructedTypes, empty ? :(struct $(sym) end) : :(primitive type $(sym) $(Int(size)*8) end))
    T = getfield(ReconstructedTypes, sym)
    (ReadRepresentation{T, empty ? nothing : T}(), false)
end

function constructrr(f::JLDFile, unk::UnknownType, dt::BasicDatatype,
                     attrs::Vector{ReadAttribute})
    @warn("type $(typestring(unk)) does not exist in workspace; reconstructing")
    reconstruct_bitstype(typestring(unk), dt.size, check_empty(attrs))
end

# Convert an ordinary type or an UnknownType to a corresponding string. This is
# only used to create gensymmed names for reconstructed types.
function typestring(T::UnknownType)
    tn = IOBuffer()
    print(tn, T.name)
    if isdefined(T, :parameters) && !isempty(T.parameters)
        write(tn, '{')
        for i = 1:length(T.parameters)
            x = T.parameters[i]
            i != 1 && write(tn, ',')
            if isa(x, UnknownType)
                write(tn, typestring(x))
            else
                print(tn, x)
            end
        end
        write(tn, '}')
    end
    String(take!(tn))
end


"""
    behead(T)

Given a UnionAll type, recursively eliminates the `where` clauses
"""
behead(T::UnionAll) = behead(T.body)
behead(@nospecialize T) = T

function constructrr(f::JLDFile, unk::UnknownType{UnionAll}, dt::CompoundDatatype,
                     attrs::Vector{ReadAttribute})
    field_datatypes = read_field_datatypes(f, attrs)
    body = behead(unk.name)
    if length(body.parameters) != length(unk.parameters)
        @warn("read type $(typestring(unk)) has a different number of parameters from type " *
              "$(unk.name) in workspace; reconstructing")
        reconstruct_compound(f, typestring(unk), dt, field_datatypes)
    else
        params = copy(unk.parameters)
        for i = 1:length(params)
            if isa(params[i], UnknownType)
                param = body.parameters[i]::TypeVar
                params[i] = param.ub
            end
        end

        # Try to construct the rr for the relaxed type. On failure, fall back to
        # reconstruct_compound.
        local T
        try
            T = unk.name{params...}
        catch err
            @warn("type parameters for $(typestring(unk)) do not match type $(unk.name) in workspace; reconstructing")
            return reconstruct_compound(f, typestring(unk), dt, field_datatypes)
        end

        try
            (rr,) = constructrr(f, T, dt, attrs, true)
            @warn("some parameters could not be resolved for type $(typestring(unk)); reading as $T")
            return (rr, false)
        catch err
            !isa(err, TypeMappingException) && rethrow(err)
            @warn("some parameters could not be resolved for type $(typestring(unk)); reconstructing")
            return reconstruct_compound(f, typestring(unk), dt, field_datatypes)
        end
    end
end

# An UnknownType for which we were not able to resolve the name.
function constructrr(f::JLDFile, unk::UnknownType{String}, dt::CompoundDatatype,
                     attrs::Vector{ReadAttribute})
    ts = typestring(unk)
    @warn("type $ts does not exist in workspace; reconstructing")
    reconstruct_compound(f, ts, dt, read_field_datatypes(f, attrs))
end

# Reconstruct the ODR of a type from the CompoundDatatype and field_datatypes
# attribute
function reconstruct_odr(f::JLDFile, dt::CompoundDatatype,
                         field_datatypes::Vector{RelOffset})
    # Get the type and ODR information for each field
    types = Vector{Any}(undef, length(dt.names))
    h5types = Vector{Any}(undef, length(dt.names))
    for i = 1:length(dt.names)
        if !isempty(field_datatypes) && (ref = field_datatypes[i]) != NULL_REFERENCE
            dtrr = jltype(f, f.datatype_locations[ref])
        else
            dtrr = jltype(f, dt.members[i])
        end
        types[i], h5types[i] = typeof(dtrr).parameters
    end
    return OnDiskRepresentation{(dt.offsets...,), Tuple{types...}, Tuple{h5types...}}()
end


# Reconstruct type that is a "lost cause": either we were not able to resolve
# the name, or the workspace type has additional fields, or cannot convert
# fields to workspace types
function reconstruct_compound(f::JLDFile, T::String, dt::H5Datatype,
                              field_datatypes::Union{Vector{RelOffset},Nothing})
    rodr = reconstruct_odr(f, dt, field_datatypes)
    types = typeof(rodr).parameters[2].parameters

    # Now reconstruct the type
    reconname = gensym(T)
    Core.eval(ReconstructedTypes,
              Expr(:struct, false, reconname,
                   Expr(:block, Any[ Expr(Symbol("::"), dt.names[i], types[i]) for i = 1:length(types) ]...,
                        # suppress default constructors, plus a bogus `new()` call to make sure
                        # ninitialized is zero.
                        Expr(:if, false, Expr(:call, :new)))))
    T = getfield(ReconstructedTypes, reconname)

    (ReadRepresentation{T,rodr}(), false)
end

# Logic previously used to reconstruct unknown datatypes

function jlconvert(rr::ReadRepresentation{T,OldDataTypeODR()},
                   f::JLDFile,
                   ptr::Ptr,
                   header_offset::RelOffset) where T

    params = types_from_refs(f, ptr+odr_sizeof(Vlen{UInt8}))
    # For cross-platform compatibility convert integer type parameters to system precision
    params = [p isa Union{Int64,Int32} ? Int(p) : p for p in params]
    hasparams = !isempty(params)
    mypath = String(jlconvert(ReadRepresentation{UInt8,Vlen{UInt8}}(), f, ptr, NULL_REFERENCE))

    m = _resolve_type(mypath, params)
    #m isa UnknownType && return m

    if m isa UnknownType
        return UnknownType{String}(m.name, params, [], [])
    end

    if hasparams
        try
            m = m{params...}
        catch            
            return UnknownType{DataType}(m, params, [],[])
            #return UnknownType(m, params)          
        end
    elseif m === Tuple
        # Need to instantiate with no parameters, since Tuple is really
        # Tuple{Vararg{Any}}
        m = Tuple{}
    end
    track_weakref!(f, header_offset, m)
    return m
end

# This is for legacy only
function constructrr(f::JLDFile, unk::UnknownType{DataType}, dt::CompoundDatatype,
                     attrs::Vector{ReadAttribute})
    field_datatypes = read_field_datatypes(f, attrs)
    if unk.name == Tuple
        # For a tuple with unknown fields, we should reconstruct the fields
        rodr = reconstruct_odr(f, dt, field_datatypes)
        
        # This is a "pseudo-RR" since the tuple is not fully parametrized, but
        # the parameters must depend on the types actually encoded in the 
        (ReadRepresentation{Tuple,rodr}(), false)
    else
        @warn("read type $(typestring(unk)) was parametrized, but type " *
              "$(unk.name) in workspace is not; reconstructing")
        reconstruct_compound(f, typestring(unk), dt, field_datatypes)
    end
end