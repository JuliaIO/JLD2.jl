const Initialized = Union{Type{Val{true}}, Type{Val{false}}}

const Pointers = Union{Ptr{Cvoid}, IndirectPointer}

struct OnDiskRepresentation{Offsets,JLTypes,H5Types, Size} end
odr_sizeof(::Nothing) = 0
@Base.pure odr_sizeof(x::DataType) = Int(x.size)

struct UnknownType{T}
    name::T
    parameters::Vector{Any}

    UnknownType{T}(name) where T = new(name)
    UnknownType{T}(name, parameters) where T = new(name, parameters)
end
UnknownType(name) = UnknownType{typeof(name)}(name)
UnknownType(name, parameters) = UnknownType{typeof(name)}(name, parameters)

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