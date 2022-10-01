function read_field_datatypes(f::JLDFile, attrs::Vector{ReadAttribute})
    for attr in attrs
        if attr.name == :field_datatypes
            return read_attr_data(f, attr, ReferenceDatatype(),
                                  ReadRepresentation{RelOffset,RelOffset}())
        end
    end
    RelOffset[]
end

function check_empty(attrs::Vector{ReadAttribute})
    for attr in attrs
        if attr.name == :empty
            return true
        end
    end
    false
end

# jltype is the inverse of h5type, providing a ReadRepresentation for an
# H5Datatype. We handle committed datatypes here, and other datatypes below.
function jltype(f::JLDFile, cdt::CommittedDatatype)
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

    if isa(julia_type_attr, Nothing)
        throw(InvalidDataException())
    end
    julia_type_attr = julia_type_attr::ReadAttribute

    # If type of datatype is this datatype, then this is the committed
    # datatype that describes a datatype
    if h5offset(f, julia_type_attr.datatype_offset) == cdt.header_offset
        # Verify that the datatype matches our expectations
        if dt != H5TYPE_DATATYPE
            error("""The HDF5 datatype representing a Julia datatype does not match
                     the expectations of this version of JLD.

                     You may need to update JLD to read this file.""")
        end
        f.jlh5type[DataType] = cdt
        f.datatypes[cdt.index] = dt
        return (f.h5jltype[cdt] = ReadRepresentation{DataType, DataTypeODR()}())
    end

    datatype = read_attr_data(f, julia_type_attr)
    if written_type_attr !== nothing
        # Custom serialization
        readas = datatype
        datatype = read_attr_data(f, written_type_attr)
        if isa(readas, UnknownType)
            @warn("custom serialization of $(typestring(readas))" *
                  " encountered, but the type does not exist in the workspace; the data will be read unconverted")
            rr = (constructrr(f, datatype, dt, attrs)::Tuple{ReadRepresentation,Bool})[1]
            canonical = false
        else
            rr, canonical = constructrr(f, datatype, dt, attrs)::Tuple{ReadRepresentation,Bool}
            rrty = typeof(rr)
            rr = ReadRepresentation{readas, CustomSerialization{rrty.parameters[1], rrty.parameters[2]}}()
            canonical = canonical && writeas(readas) === datatype
        end
    else
        rr, canonical = constructrr(f, datatype, dt, attrs)::Tuple{ReadRepresentation,Bool}
    end

    canonical && (f.jlh5type[datatype] = cdt)
    f.datatypes[cdt.index] = dt
    f.h5jltype[cdt] = rr
end


# jltype is the inverse of h5type, providing a ReadRepresentation for an
# H5Datatype. We handle shared datatypes here: ones that were not "committed" by JLD2.
function jltype(f::JLDFile, sdt::SharedDatatype)
    haskey(f.h5jltype, sdt) && return f.h5jltype[sdt]::ReadRepresentation
    dt, attrs = read_shared_datatype(f, sdt)
    rr = jltype(f, dt)
    f.h5jltype[sdt] = rr
end



# Constructs a ReadRepresentation for a given opaque (bitstype) type
function constructrr(::JLDFile, T::DataType, dt::BasicDatatype, attrs::Vector{ReadAttribute})
    dt.class == DT_OPAQUE || throw(UnsupportedFeatureException())
    if T.size == dt.size && isempty(T.types)
        (ReadRepresentation{T,T}(), true)
    else
        empty = check_empty(attrs)
        if empty
            if !hasdata(T)
                (ReadRepresentation{T,nothing}(), true)
            else
                @warn("$T has $(T.size*8) bytes, but written type was empty; reconstructing")
                reconstruct_bitstype(T.name.name, dt.size, empty)
            end
        else
            if isempty(T.types)
                @warn("primitive type $T has $(T.size*8) bits, but written type has $(dt.size*8) bits; reconstructing")
            else
                @warn("$T is a non-primitive type, but written type is a primitive type with $(dt.size*8) bits; reconstructing")
            end
            reconstruct_bitstype(T.name.name, dt.size, empty)
        end
    end
end

"""
constructrr(f::JLDFile, T::DataType, dt::CompoundType, attrs::Vector{ReadAttribute},
            hard_failure::Bool=false)

Constructs a ReadRepresentation for a given type. This is the generic method for all
types not specially handled below.

If hard_failure is true, then throw a TypeMappingException instead of attempting
reconstruction. This helps in cases where we can't know if reconstructed parametric types
will have a matching memory layout without first inspecting the memory layout.
"""
struct TypeMappingException <: Exception; end
function constructrr(f::JLDFile, T::DataType, dt::CompoundDatatype,
                     attrs::Vector{ReadAttribute},
                     hard_failure::Bool=false)
    field_datatypes = read_field_datatypes(f, attrs)

    # If read type is not a leaf type, reconstruct
    if !isconcretetype(T)
        @warn("read type $T is not a leaf type in workspace; reconstructing")
        return reconstruct_compound(f, string(T), dt, field_datatypes)
    end

    # Map names in dt to their indices
    dtnames = Dict{Symbol,Int}()
    for i = 1:length(dt.names)
        dtnames[dt.names[i]] = i
    end
    mapped = falses(length(dt.names))

    offsets = Vector{Int}(undef, length(T.types))
    types = Vector{Any}(undef, length(T.types))
    odrs = Vector{Any}(undef, length(T.types))
    fn = fieldnames(T)
    samelayout = isbitstype(T) && T.size == dt.size
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
            if !isempty(field_datatypes) && (ref = field_datatypes[dtindex]) != NULL_REFERENCE
                dtrr = jltype(f, f.datatype_locations[ref])
            else
                dtrr = jltype(f, dt.members[dtindex])
            end

            readtype, odrtype = typeof(dtrr).parameters

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
                !(odrs[i] isa OnDiskRepresentation) && 
                !(odrs[i] <: CustomSerialization)

            mapped[dtindex] = true
        end
    end

    if !all(mapped)
        @warn("the following fields are present in type $T" *
              " saved in the file but not present in the type the workspace:\n\n" *
              "$(join(dt.names[.!mapped], "\n"))," *
              "\n\nData in these fields will not be accessible")
    end

    if samelayout
        (ReadRepresentation{T,T}(), true)
    else
        wodr = odr(T)
        # This should theoretically be moved inside the if statement, but then it returns
        # the wrong result due to a bug in type inference on 0.6
        typeof_wodr = typeof(wodr)
        offsets = (offsets...,)
        if wodr isa OnDiskRepresentation
            odr_offsets = typeof_wodr.parameters[1]
            odr_types = typeof_wodr.parameters[2].parameters
            odr_h5types = typeof_wodr.parameters[3].parameters
            tequal = length(odr_types) == length(types)
            if tequal
                for i = 1:length(types)
                    if !(odr_types[i] <: types[i]) || odr_h5types[i] != odrs[i]
                        tequal = false
                        break
                    end
                end
                if tequal && odr_offsets == offsets
                    # This should not be necessary, but type inference mistakenly changes
                    # the value of wodr here
                    wodr = typeof_wodr()
                    return (ReadRepresentation{T,wodr}(), true)
                end
            end
        end
        return (ReadRepresentation{T,OnDiskRepresentation{offsets, Tuple{types...}, Tuple{odrs...}, offsets[end]+odr_sizeof(odrs[end])}()}(), false)
    end
end

function constructrr(f::JLDFile, T::UnionAll, dt::CompoundDatatype,
                     attrs::Vector{ReadAttribute},
                     hard_failure::Bool=false)
    @warn("read type $T is not a leaf type in workspace; reconstructing")
    return reconstruct_compound(f, string(T), dt, read_field_datatypes(f, attrs))
end

# Find types in modules
# returns the result of searching for the type in the specified module m
function _resolve_type_singlemodule(::ReadRepresentation{T,DataTypeODR()},
                                    m,
                                    parts,
                                    mypath,
                                    hasparams::Bool,
                                    params) where T
    for part in parts
        sym = Symbol(part)
        if !isa(m, Module) || !isdefined(m, sym)
            return hasparams ? UnknownType(mypath, params) : UnknownType(mypath)
        end
        m = getfield(m, sym)
    end
    if !isa(m, DataType) && !isa(m, UnionAll)
        return hasparams ? UnknownType(mypath, params) : UnknownType(mypath)
    end
    return m
end

_is_not_unknown_type(x::UnknownType) = false
_is_not_unknown_type(x) = true

function _resolve_type(rr::ReadRepresentation{T,DataTypeODR()},
                       f::JLDFile,
                       ptr::Ptr,
                       header_offset::RelOffset,
                       mypath,
                       hasparams::Bool,
                       params) where T
    parts = split(mypath, '.')
    modules = vcat([Main], collect(keys(Base.module_keys)), stdlibmodules(Main))
    unique!(modules)
    for mod in modules
        resolution_attempt = _resolve_type_singlemodule(rr,
                                                        mod,
                                                        parts,
                                                        mypath,
                                                        hasparams,
                                                        params)
        if _is_not_unknown_type(resolution_attempt)
            return resolution_attempt
        end
    end
    return hasparams ? UnknownType(mypath, params) : UnknownType(mypath)
end



function types_from_refs(f::JLDFile, ptr::Ptr)
    # Test for a potential null pointer indicating an empty array
    isinit = jlunsafe_load(convert(Ptr{UInt32}, ptr)) != 0
    unknown_params = false
    if isinit
        refs = jlconvert(ReadRepresentation{RelOffset, Vlen{RelOffset}}(), f, ptr, NULL_REFERENCE)
        params =  Any[let
            # If the reference is to a committed datatype, read the datatype
            nulldt = CommittedDatatype(UNDEFINED_ADDRESS, 0)
            cdt = get(f.datatype_locations, ref, nulldt)
            res = cdt !== nulldt ? (typeof(jltype(f, cdt)::ReadRepresentation)::DataType).parameters[1] : load_dataset(f, ref)
            unknown_params = unknown_params || isa(res, UnknownType)
            res
        end for ref in refs]
        return params, unknown_params
    end
    return [], unknown_params
end

# Read a type. Returns an instance of UnknownType if the type or parameters
# could not be resolved.
function jlconvert(rr::ReadRepresentation{T,DataTypeODR()},
                   f::JLDFile,
                   ptr::Ptr,
                   header_offset::RelOffset) where T

    params, unknown_params = types_from_refs(f, ptr+odr_sizeof(Vlen{UInt8}))
    # For cross-platform compatibility convert integer type parameters to system precision
    params = [p isa Union{Int64,Int32} ? Int(p) : p for p in params]
    hasparams = !isempty(params)
    mypath = String(jlconvert(ReadRepresentation{UInt8,Vlen{UInt8}}(), f, ptr, NULL_REFERENCE))

    if mypath in keys(f.typemap)
        m = f.typemap[mypath]
    else
        m = _resolve_type(rr, f, ptr, header_offset, mypath, hasparams, hasparams ? params : nothing)
        m isa UnknownType && return m
    end

    if hasparams
        try
            m = m{params...}
        catch e
            return UnknownType(m, params)
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
    dt == H5TYPE_DATATYPE ? (ReadRepresentation{DataType,DataTypeODR()}(), true) :
                            throw(UnsupportedFeatureException())


## Type reconstruction

module ReconstructedTypes end

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
    if isdefined(T, :parameters)
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

function constructrr(f::JLDFile, unk::UnknownType{DataType}, dt::CompoundDatatype,
                     attrs::Vector{ReadAttribute})
    field_datatypes = read_field_datatypes(f, attrs)
    if unk.name == Tuple
        # For a tuple with unknown fields, we should reconstruct the fields
        rodr = reconstruct_odr(f, dt, field_datatypes)
        # This is a "pseudo-RR" since the tuple is not fully parametrized, but
        # the parameters must depend on the types actually encoded in the file
        (ReadRepresentation{Tuple,rodr}(), false)
    else
        @warn("read type $(typestring(unk)) was parametrized, but type " *
              "$(unk.name) in workspace is not; reconstructing")
        reconstruct_compound(f, typestring(unk), dt, field_datatypes)
    end
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
    return OnDiskRepresentation{(dt.offsets...,), Tuple{types...}, Tuple{h5types...},dt.size}()
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

# At present, we write Union{} as an object of Core.TypeofBottom. The method above
# basically works, but `Expr(:new, Type{Union{}})` is a bit weird and causes problems for
# inference. Better to define a separate method.
jlconvert(::ReadRepresentation{Core.TypeofBottom,nothing}, f::JLDFile, ptr::Ptr,
          header_offset::RelOffset) = Union{}



# This jlconvert method handles compound types with padding or references
@generated function jlconvert(::ReadRepresentation{T,S}, f::JLDFile, ptr::Ptr,
                              header_offset::RelOffset) where {T,S}
    isa(S, DataType) && return :(convert(T, jlunsafe_load(convert(Ptr{S}, ptr))))
    @assert isa(S, OnDiskRepresentation)

    offsets = typeof(S).parameters[1]
    types = typeof(S).parameters[2].parameters
    odrs = typeof(S).parameters[3].parameters

    blk = Expr(:block)
    args = blk.args
    # Separate treatment for mutable structs for better readability
    if ismutabletype(T)
        push!(args, quote
            obj = $(Expr(:new, T))
            track_weakref!(f, header_offset, obj)
        end)
        fn = fieldnames(T)
        for i = 1:length(types)
            offset = offsets[i]
            rtype = types[i]
            odr = odrs[i]
    
            rr = ReadRepresentation{rtype,odr}()
    
            fni = QuoteNode(fn[i])
            ttype = T.types[i]
            if odr === nothing
                # Type is not stored or single instance
                newi = Expr(:new, ttype)
                # use jl_set_nth_field instead of setfield! since the former also works for const fields
                # in mutable structs.
                push!(args, :(ccall(:jl_set_nth_field, Nothing, (Any, Csize_t, Any), obj, ($i)-1, $newi)))
            else
                loadfield = quote
                    fieldval = rconvert($ttype, jlconvert($rr, f, ptr+$offset, NULL_REFERENCE)::$rtype)::$ttype
                    ccall(:jl_set_nth_field, Nothing, (Any, Csize_t, Any), obj, ($i)-1, fieldval)
                end
                if jlconvert_canbeuninitialized(rr)
                    push!(args, :(jlconvert_isinitialized($rr, ptr+$offset) && $(loadfield)))
                else
                    push!(args, loadfield)
                end
            end
        end

        push!(args, (:obj))    
        return blk
    end
    if isbitstype(T)
        # For bits types, we should always inline, because otherwise we'll just
        # pass a lot of crap around in registers
        push!(args, Expr(:meta, :inline))
    end
    fsyms = []
    fn = T === Tuple ? [Symbol(i) for i = 1:length(types)] : fieldnames(T)
    for i = 1:length(types)
        offset = offsets[i]
        rtype = types[i]
        odr = odrs[i]

        fsym = Symbol("field_", fn[i])
        push!(fsyms, fsym)

        rr = ReadRepresentation{rtype,odr}()

        if odr === nothing
            # Type is not stored or single instance
            if T.types[i] == Union{}
                # This cannot be defined
                @assert !ismutabletype(T)
                push!(args, Expr(:return, Expr(:new, T, fsyms[1:i-1]...)))
                return blk
            else
                newi = Expr(:new, T.types[i])
                push!(args, :($fsym = $newi))
            end
        else
            if jlconvert_canbeuninitialized(rr)
                push!(args, quote
                    if !jlconvert_isinitialized($rr, ptr+$offset)
                        $(if T <: Tuple || i <= ninitialized(T)
                            # Reference must always be initialized
                            :(throw(UndefinedFieldException(T,$(QuoteNode(fn[i])))))
                        else
                            Expr(:return, Expr(:new, T, fsyms[1:i-1]...))
                        end)
                    end
                end)
            end
            if T === Tuple
                # Special case for reconstructed tuples, where we don't know the
                # field types in advance
                push!(args, :($fsym = jlconvert($rr, f, ptr+$offset, NULL_REFERENCE)::$rtype))
            else
                ttype = T.types[i]
                push!(args, :($fsym = rconvert($ttype, jlconvert($rr, f, ptr+$offset, NULL_REFERENCE)::$rtype)::$ttype))
            end
        end
    end

    push!(args, T <: Tuple ? Expr(:tuple, fsyms...) : Expr(:new, T, fsyms...))

    blk
end