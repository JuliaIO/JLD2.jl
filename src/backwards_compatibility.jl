# The following block allows reading Union types written prior to v0.2
const LEGACY_H5TYPE_UNION = VariableLengthDatatype(H5TYPE_DATATYPE)

function jlconvert(::ReadRepresentation{Union, Vlen{DataTypeODR()}}, f::JLDFile,
                   ptr::Ptr, header_offset::RelOffset)
    v = Union{jlconvert(ReadRepresentation{DataType,Vlen{DataTypeODR()}}(), f, ptr, NULL_REFERENCE)...}
    track_weakref!(f, header_offset, v)
    v
end

constructrr(::JLDFile, ::Type{T}, dt::VariableLengthDatatype, ::Vector{ReadAttribute}) where {T<:Union} =
    dt == LEGACY_H5TYPE_UNION ? (ReadRepresentation{Union,Vlen{DataTypeODR()}}(), true) :
                         throw(UnsupportedFeatureException())

# The following definition is needed to correctly load Strings written
# with JLD2 with versions v0.1.12 - v0.3.1
function read_array(f::JLDFile, dataspace::ReadDataspace,
                    rr::FixedLengthString{String}, layout::DataLayout,
                    filters::FilterPipeline, header_offset::RelOffset,
                    attributes::Union{Vector{ReadAttribute},Nothing})
    rrv = ReadRepresentation{UInt8,odr(UInt8)}()
    v = read_array(f, dataspace, rrv, layout, filters, NULL_REFERENCE, attributes)
    String(v)
end


function julia_type(s::AbstractString)
    s = replace(s, r"ASCIIString|UTF8String|ByteString" => "String")
    if occursin("Base.UTF16String", s)
        error("file contains Base.UTF16String, must be converted and re-saved with JLD 0.9 or less")
    end
    _julia_type(s)
end


function constructrr(f::JLDFile, str::String, dt::CompoundDatatype, attrs::Vector{ReadAttribute})
    jl_type = julia_type(str)
    # for some reason JLD attaches a '_' at the end of member field names
    strnames = [string(name)[1:end-1] for name in dt.names]
    dt.names .= Symbol.(strnames)
    constructrr(f, jl_type, dt, attrs)
end


struct UnsupportedType; end
struct UnconvertedType; end

const _where_macrocall = Symbol("@where")
function expand_where_macro(e::Expr)
    e.head = :where
    popfirst!(e.args)
    popfirst!(e.args)  # source location
    return true
end

is_valid_type_ex(s::Symbol) = true
is_valid_type_ex(s::QuoteNode) = true
is_valid_type_ex(s) = isbitstype(typeof(s))
function is_valid_type_ex(e::Expr)
    if e.head === :curly || e.head == :tuple || e.head == :.
        return all(is_valid_type_ex, e.args)
    elseif e.head === :where
        return is_valid_type_ex(e.args[1])
    elseif e.head === :let && length(e.args) == 2
        return is_valid_type_ex(e.args[2]) &&
               is_valid_type_ex(e.args[1].args[2])
    elseif e.head == :call
        f = e.args[1]
        if f isa Expr
            if f.head === :core
                f = f.args[1]
                return f === :Union || f === :TypeVar || f === :UnionAll
            end
        elseif f isa Symbol
            return f === :Union || f === :TypeVar || f === :symbol
        end
    end
    return false
end

const typemap_Core = Dict(
    :Uint8 => :UInt8,
    :Uint16 => :Uint16,
    :Uint32 => :UInt32,
    :Uint64 => :UInt64,
    :Void => Symbol(Nothing)
)

const _typedict = Dict{String,Type}()

function fixtypes(typ)
    whereall = []
    typ = fixtypes(typ, whereall)
    while !isempty(whereall)
        var = pop!(whereall)
        typ = Expr(:let, var, Expr(:call, Expr(:core, :UnionAll), var.args[1], typ))
    end
    return typ
end
fixtypes(typ, whereall) = typ
function fixtypes(typ::Expr, whereall::Vector{Any})
    if typ.head === :macrocall && typ.args[1] === _where_macrocall
        expand_where_macro(typ) # @where => TypeVar format forwards compatibility
    end
    if typ.head === :.
        if length(typ.args) == 2 && typ.args[1] === :Core
            arg = typ.args[2].value
            return Expr(:., :Core, QuoteNode(get(typemap_Core, arg, arg)))
        else
            return typ
        end
    elseif typ == :(Core.Type{TypeVar(:T,Union(Core.Any,Core.Undef))}) || typ == :(Core.Type{TypeVar(:T)})
        # Work around https://github.com/JuliaLang/julia/issues/8226 and the removal of Top
        return :(Core.Type)
    end

    for i = 1:length(typ.args)
        typ.args[i] = fixtypes(typ.args[i], whereall)
    end

    if (typ.head === :call && !isempty(typ.args) &&
        typ.args[1] === :TypeVar) # TypeVar => where format backwards compatibility
        tv = gensym()
        push!(whereall, Expr(:(=), tv, typ))
        return tv
    end

    if (typ.head === :call && !isempty(typ.args) &&
        typ.args[1] === :Union)
        typ = Expr(:curly, typ.args...)
    end

    if typ.head === :tuple
        if !any(x->isa(x,QuoteNode) || isbits(x), typ.args)
            # guess that we have a tuple type represented as a tuple
            typ = Expr(:curly, :Tuple, typ.args...)
        end
    end

    if typ.head === :curly
        # assume literal TypeVar should work like `T{<:S}`
        while !isempty(whereall)
            var = pop!(whereall)
            typ = Expr(:let, var, Expr(:call, Expr(:core, :UnionAll), var.args[1], typ))
        end
    end
    return typ
end

function _julia_type(s::AbstractString)
    typ = get(_typedict, s, UnconvertedType)
    if typ == UnconvertedType
        sp = Meta.parse(s, raise=false)
        if (isa(sp, Expr) && (sp.head == :error || sp.head == :continue || sp.head == :incomplete))
            println("error parsing type string ", s)
            eval(sp)
        end
        typ = julia_type(fixtypes(sp))
        if typ != UnsupportedType
            _typedict[s] = typ
        end
    end
    if typ == UnconvertedType || typ == UnsupportedType
        return UnknownType(s)
    else
        return typ
    end
end

function julia_type(e::Union{Symbol, Expr})
    if is_valid_type_ex(e)
        try # `try` needed to catch undefined symbols
            # `e` should be fully qualified, and thus reachable from Main
            typ = Core.eval(Main, e)
            typ == Type && return Type
            isa(typ, Type) && return typ
        catch
        end
    end
    return UnsupportedType
end