## Primitive datatypes
# These get special handling only in that they have different HDF5 type
# representations than ordinary opaque types

# This construction prevents these methods from getting called on type unions
const SignedTypes        = [Int8, Int16, Int32, Int64, Int128]
const UnsignedTypes      = [UInt8, UInt16, UInt32, UInt64, UInt128]
const FloatTypes         = [Float16, Float32, Float64]
const PrimitiveTypes     = [SignedTypes; UnsignedTypes; FloatTypes; Bool]
const PrimitiveTypeTypes = Union{(Type{T} for T in PrimitiveTypes)...}

for T in SignedTypes
    @eval h5fieldtype(::JLDFile, ::Type{$T}, ::Type{$T}, ::Initialized) =
        FixedPointDatatype($(sizeof(T)), true)
end
for T in UnsignedTypes
    @eval h5fieldtype(::JLDFile, ::Type{$T}, ::Type{$T}, ::Initialized) =
        FixedPointDatatype($(sizeof(T)), false)
end

struct BENumber{T}
    x::T
end

jlconvert(::MappedRepr{T,BENumber{T}}, ::JLDFile, ptr::Ptr, ::RelOffset) where {T} =
    bswap(jlunsafe_load(pconvert(Ptr{T}, ptr)))

function jltype(f::JLDFile, dt::FixedPointDatatype)
    signed = Bool(dt.bitfield1 >> 3 & 0b1)
    endianness = dt.bitfield1 & 0b1 # 0 → little endian, 1 → big endian
    #endianness == 0 || throw(UnsupportedFeatureException("load big endian numbers is not implemented."))
    ((dt.bitfield2 == 0x00) & (dt.bitfield3 == 0x00) & (dt.bitoffset == 0) & (dt.bitprecision == dt.size*8)) ||
        throw(UnsupportedFeatureException())
    if endianness == 0
        if dt.size == 8
            return signed ? SameRepr{Int64}() : SameRepr{UInt64}()
        elseif dt.size == 1
            return signed ? SameRepr{Int8}() : SameRepr{UInt8}()
        elseif dt.size == 4
            return signed ? SameRepr{Int32}() : SameRepr{UInt32}()
        elseif dt.size == 2
            return signed ? SameRepr{Int16}() : SameRepr{UInt16}()
        elseif dt.size == 16
            return signed ? SameRepr{Int128}() : SameRepr{UInt128}()
        else
            throw(UnsupportedFeatureException())
        end
    else
        if dt.size == 8
            return signed ? MappedRepr{Int64,BENumber{Int64}}() : MappedRepr{UInt64,BENumber{UInt64}}()
        elseif dt.size == 1
            return signed ? MappedRepr{Int8,BENumber{Int8}}() : MappedRepr{UInt8,BENumber{UInt8}}()
        elseif dt.size == 4
            return signed ? MappedRepr{Int32,BENumber{Int32}}() : MappedRepr{UInt32,BENumber{UInt32}}()
        elseif dt.size == 2
            return signed ? MappedRepr{Int16,BENumber{Int16}}() : MappedRepr{UInt16,BENumber{UInt16}}()
        elseif dt.size == 16
            return signed ? MappedRepr{Int128,BENumber{Int128}}() : MappedRepr{UInt128,BENumber{UInt128}}()
        else
            throw(UnsupportedFeatureException())
        end
    end
end

# Special handling for booleans as they are not considered <: Integer in HDF5
h5fieldtype(::JLDFile, ::Type{Bool}, ::Type{Bool}, ::Initialized) =BitFieldDatatype(1)
jltype(::JLDFile, ::BitFieldDatatype) = SameRepr{Bool}()

h5fieldtype(::JLDFile, ::Type{Float16}, ::Type{Float16}, ::Initialized) =
    FloatingPointDatatype(UInt8(DT_FLOATING_POINT) + 0x3<<4, 0x20, 0x0f, 0x00, 2, 0, 16, 10, 5, 0, 10, 0x0000000f)
h5fieldtype(::JLDFile, ::Type{Float32}, ::Type{Float32}, ::Initialized) =
    FloatingPointDatatype(UInt8(DT_FLOATING_POINT) + 0x3<<4, 0x20, 0x1f, 0x00, 4, 0, 32, 23, 8, 0, 23, 0x0000007f)
h5fieldtype(::JLDFile, ::Type{Float64}, ::Type{Float64}, ::Initialized) =
    FloatingPointDatatype(UInt8(DT_FLOATING_POINT) + 0x3<<4, 0x20, 0x3f, 0x00, 8, 0, 64, 52, 11, 0, 52, 0x000003ff)

h5fieldtype(::JLDFile, ::Type{BENumber{Float16}}, ::Type{Float16}, ::Initialized) =
    FloatingPointDatatype(UInt8(DT_FLOATING_POINT) + 0x3<<4, 0x21, 0x0f, 0x00, 2, 0, 16, 10, 5, 0, 10, 0x0000000f)
h5fieldtype(::JLDFile, ::Type{BENumber{Float32}}, ::Type{Float32}, ::Initialized) =
    FloatingPointDatatype(UInt8(DT_FLOATING_POINT) + 0x3<<4, 0x21, 0x1f, 0x00, 4, 0, 32, 23, 8, 0, 23, 0x0000007f)
h5fieldtype(::JLDFile, ::Type{BENumber{Float64}}, ::Type{Float64}, ::Initialized) =
    FloatingPointDatatype(UInt8(DT_FLOATING_POINT) + 0x3<<4, 0x21, 0x3f, 0x00, 8, 0, 64, 52, 11, 0, 52, 0x000003ff)

function jltype(f::JLDFile, dt::FloatingPointDatatype)
    if dt == h5fieldtype(f, Float64, Float64, Val{true})
        return SameRepr{Float64}()
    elseif dt == h5fieldtype(f, Float32, Float32, Val{true})
        return SameRepr{Float32}()
    elseif dt == h5fieldtype(f, Float16, Float16, Val{true})
        return SameRepr{Float16}()
    elseif dt == h5fieldtype(f, BENumber{Float64}, Float64, Val{true})
        return MappedRepr{Float32,BENumber{Float32}}()
    elseif dt == h5fieldtype(f, BENumber{Float32}, Float32, Val{true})
        return MappedRepr{Float32,BENumber{Float32}}()
    elseif dt == h5fieldtype(f, BENumber{Float16}, Float16, Val{true})
        return MappedRepr{Float32,BENumber{Float32}}()
    else
        throw(UnsupportedFeatureException())
    end
end

function h5fieldtype(f::JLDFile, writeas::PrimitiveTypeTypes, readas::Type, init::Initialized)
    @lookup_committed f readas
    commit(f, h5fieldtype(f, writeas, writeas, init), writeas, readas)
end
h5type(f::JLDFile, writeas::PrimitiveTypeTypes, x) =
    h5fieldtype(f, writeas, typeof(x), Val{true})

# Used only for custom serialization
constructrr(f::JLDFile, T::PrimitiveTypeTypes, dt::Union{FixedPointDatatype,FloatingPointDatatype},
            ::Vector{ReadAttribute}) =
    dt == h5fieldtype(f, T, T, Val{true}) ? (SameRepr{T}(), true) :
                                            throw(UnsupportedFeatureException())
