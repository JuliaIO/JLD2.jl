const Initialized = Union{Type{Val{true}}, Type{Val{false}}}

const Pointers = Union{Ptr{Cvoid}, IndirectPointer}

struct OnDiskRepresentation{Offsets,JLTypes,H5Types, Size} end
odr_sizeof(::Nothing) = 0
odr_sizeof(x::DataType) = Core.sizeof(x)

struct UnknownType{T, P} end
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
