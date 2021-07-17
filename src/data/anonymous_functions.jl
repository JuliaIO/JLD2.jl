import Core: TypeName

"""
    isanon(t::DataType)::Bool

Test whether a `DataType` belongs to an anonymous function.
Code borrowed from `BSON` which itself is modelled on `Base.Serialize`.
"""
function isanon(t::DataType)
    tn = t.name
    if isdefined(tn, :mt)
      name = tn.mt.name
      mod = tn.module
      return t.super === Function &&
      unsafe_load(Base.unsafe_convert(Ptr{UInt8}, tn.name)) == UInt8('#') &&
      (!isdefined(mod, name) || t != typeof(getfield(mod, name)))
    end
    return false
end


## Module
# Modules are complex structures and need special casing.
# Write only their name as a string.

writeas(::Type{Module}) = String
wconvert(::Type{String}, m::Module) = string(m)
function rconvert(::Type{Module}, mstr::String)
    
    m = Meta.parse(mstr)
    @assert m isa Symbol || Meta.isexpr(m, :.) "$mstr is not a valid Module identifier"
    f = Symbol(mstr)
    for (u,v) in Base.loaded_modules
        (Symbol(v) == f) && return v
    end
    try 
        mod = eval(m)
        mod isa Module && return mod    
    catch end
    @warn "Module $mstr not found. Attempting to use Main"
    return Main
end

## Methods
# Fields of a julia method. Needs special treatment for serialization
mutable struct SerializedMethod
    name::Symbol
    mod::String # Module
    file::Symbol
    line::Int32
    sig::Type #::DataType # Tuple
    slot_syms::String
    nargs::Int32
    isva::Bool
    source::Core.CodeInfo
    nospecialize::Bool
    pure::Bool
end

JLD2.writeas(::Type{Method}) = SerializedMethod

function JLD2.wconvert(::Type{SerializedMethod}, m::Method)
    SerializedMethod(
        m.name,
        wconvert(String, m.module),
        m.file,
        m.line,
        m.sig,
        m.slot_syms,
        m.nargs,
        m.isva,
        Base._uncompressed_ast(m, m.source),
        m.nospecialize,
        m.pure)
end

function JLD2.rconvert(::Type{Method}, sm::SerializedMethod)
    m = ccall(:jl_new_method_uninit, Ref{Method}, (Any,), Main)
    m.name = sm.name
    m.module = rconvert(Module, sm.mod)
    m.file = sm.file
    m.line = sm.line
    m.sig = sm.sig
    m.slot_syms = sm.slot_syms
    m.nospecialize = sm.nospecialize
    m.nargs = sm.nargs
    m.isva = sm.isva
    m.source = sm.source
    m.pure = sm.pure
    return m
end

"""
    SerializedTypeName(t::TypeName)

Representation of important fields of `TypeName`s for serialization.
"""
mutable struct SerializedTypeName
    jlversion::String
    name::Symbol
    names # Core.SimpleVector
    super::DataType
    parameters # Core.SimpleVector
    types   # Core.SimpleVector
    has_instance::Bool
    abstract::Bool
    mutable::Bool
    ninitialized::Bool
end

function SerializedTypeName(t::TypeName)
    primary = Base.unwrap_unionall(t.wrapper)
    SerializedTypeName(
        Base.string(VERSION), t.name, t.names, primary.super, primary.parameters,
        primary.types, isdefined(primary, :instance), primary.abstract,
        primary.mutable, primary.ninitialized)
end

"""
    SerializedMethodTable(mt)

The method table is typically part of the TypeName. Here, to allow reconstruction
of anonymous functions without problems, first load `SerializedTypeName` to construct
a new TypeName + DataType and then load the methods.
"""
mutable struct SerializedMethodTable
    name::Symbol
    methodlist
    max_args::Int
    kwsorter
end

function SerializedMethodTable(mt)
    SerializedMethodTable(mt.name, collect(Base.MethodList(mt)), mt.max_args,
    isdefined(mt, :kwsorter) ? mt.kwsorter : nothing)
end

"""
    new_typename(::Type{TypeName}, st::SerializedTypeName)

Construct a new TypeName given information in form of a `SerializedTypeName`.
"""
function new_typename(::Type{TypeName}, st::SerializedTypeName)
    # TODO: In the future, test for correct julia version
    # Currently not needed if only v1.6 is supported


    tn = ccall(:jl_new_typename_in, Ref{Core.TypeName}, (Any, Any),
                st.name, ReconstructedTypes)

    tn.names = st.names
    ndt = ccall(:jl_new_datatype, Any, (Any, Any, Any, Any, Any, Any, Cint, Cint, Cint),
                tn, tn.module, st.super, st.parameters, st.names, st.types,
                st.abstract, st.mutable, st.ninitialized)
    ty = tn.wrapper = ndt.name.wrapper

    ccall(:jl_set_const, Cvoid, (Any, Any, Any), tn.module, tn.name, ty)
    if st.has_instance && !isdefined(ty, :instance)
        # use setfield! directly to avoid `fieldtype` lowering expecting to see a Singleton object already on ty
        Core.setfield!(ty, :instance, ccall(:jl_new_struct, Any, (Any, Any...), ty))
    end
    return tn
end

"""
    add_method_table(tn::TypeName, smt::SerializedMethodTable) 

Construct and add new method table to given `TypeName`.
"""
function add_method_table(tn::TypeName, smt::SerializedMethodTable)
    # TODO: `kwsorter` field is currently ignored.
    ndt = tn.wrapper
    tn.mt = ccall(:jl_new_method_table, Any, (Any, Any), tn.name, tn.module)
    tn.mt.name = smt.name 
    tn.mt.max_args = smt.max_args
    for def in smt.methodlist
        isdefined(def, :sig) || continue
        # update function name
        def.name = tn.name
        #type is already correctly replaced on instantiate
        #def.sig = Tuple{ndt, def.sig.types[2:end]...}
        ccall(:jl_method_table_insert, Cvoid, (Any, Any, Ptr{Cvoid}), tn.mt, def, C_NULL)
    end
end