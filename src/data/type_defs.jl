const Initialized = Union{Type{Val{true}}, Type{Val{false}}}

const Pointers = Union{Ptr{Cvoid}, IndirectPointer}

struct OnDiskRepresentation{Offsets,JLTypes,H5Types, Size} end
odr_sizeof(::Nothing) = 0
@static if VERSION ≥ v"1.9.0-DEV"
    # Modelled after Base.datatype_alignment
    function datatype_size(dt::DataType)
        Base.@_foldable_meta
        dt.layout == C_NULL && throw(UndefRefError())
        size = unsafe_load(pconvert(Ptr{Base.DataTypeLayout}, dt.layout)).size
        return Int(size)
    end
    odr_sizeof(x::DataType) = datatype_size(x)
else
    odr_sizeof(x::DataType) = Int(x.size)
end

struct UnknownType{T, P} end

# Horrible Invalidations
# function Base.show(io::IO, x::Type{UnknownType{T, P}}) where {T, P}
#     print(io, "UnknownType:\"", T,"\"")
#     if !isempty(P.parameters)
#         print(io, "{")
#         for p in P.parameters
#             print(io, p)
#             if p !== P.parameters[end]
#                 print(io, ", ")
#             end
#         end
#         print(io, "}")
#     end
# end

struct Vlen{T}
    size::UInt32
    id::GlobalHeapID
end
odr_sizeof(::Type{T}) where {T<:Vlen} = 4 + jlsizeof(GlobalHeapID)

# Look up the corresponding committed datatype for a given type
macro lookup_committed(f, T)
    quote
        cdt = get($(esc(f)).jlh5type, $(esc(T)), nothing)
        cdt !== nothing && return cdt::CommittedDatatype
    end
end
