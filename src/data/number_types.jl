## Primitive datatypes
# These get special handling only in that they have different HDF5 type
# representations than ordinary opaque types

# This construction prevents these methods from getting called on type unions
const SignedTypes        = Union{Type{Int8}, Type{Int16}, Type{Int32}, Type{Int64}, Type{Int128}}
const UnsignedTypes      = Union{Type{UInt8}, Type{UInt16}, Type{UInt32}, Type{UInt64}, Type{UInt128}}
const FloatTypes         = Union{Type{Float16}, Type{Float32}, Type{Float64}}
const PrimitiveTypeTypes = Union{SignedTypes, UnsignedTypes, FloatTypes, Type{Bool}}
const PrimitiveTypes     = Union{Bool, Int8, Int16, Int32, Int64, Int128, UInt8, UInt16, UInt32,
                                 UInt64, UInt128, Float16, Float32, Float64}

for T in Base.uniontypes(SignedTypes)
    @eval h5fieldtype(::JLDFile, ::$T, ::$T, ::Initialized) =
        FixedPointDatatype($(T.parameters[1].size), true)
end
for T in Base.uniontypes(UnsignedTypes)
    @eval h5fieldtype(::JLDFile, ::$T, ::$T, ::Initialized) =
        FixedPointDatatype($(T.parameters[1].size), false)
end


function jltype(f::JLDFile, dt::FixedPointDatatype)
    signed = Bool(dt.bitfield1 >> 3 & 0b1)
    endianness = dt.bitfield1 & 0b1 # 0 → little endian, 1 → big endian
    endianness == 0 || throw(UnsupportedFeatureException("load big endian numbers is not implemented."))
    ((dt.bitfield2 == 0x00) & (dt.bitfield3 == 0x00) & (dt.bitoffset == 0) & (dt.bitprecision == dt.size*8)) ||
        throw(UnsupportedFeatureException())
    if dt.size == 8
        return signed ? ReadRepresentation{Int64,Int64}() : ReadRepresentation{UInt64,UInt64}()
    elseif dt.size == 1
        return signed ? ReadRepresentation{Int8,Int8}() : ReadRepresentation{UInt8,UInt8}()
    elseif dt.size == 4
        return signed ? ReadRepresentation{Int32,Int32}() : ReadRepresentation{UInt32,UInt32}()
    elseif dt.size == 2
        return signed ? ReadRepresentation{Int16,Int16}() : ReadRepresentation{UInt16,UInt16}()
    elseif dt.size == 16
        return signed ? ReadRepresentation{Int128,Int128}() : ReadRepresentation{UInt128,UInt128}()
    else
        throw(UnsupportedFeatureException())
    end
end

# Special handling for booleans as they are not considered <: Integer in HDF5
h5fieldtype(::JLDFile, ::Type{Bool}, ::Type{Bool}, ::Initialized) =BitFieldDatatype(1)
jltype(::JLDFile, ::BitFieldDatatype) = ReadRepresentation{Bool, Bool}()

h5fieldtype(::JLDFile, ::Type{Float16}, ::Type{Float16}, ::Initialized) =
    FloatingPointDatatype(DT_FLOATING_POINT, 0x20, 0x0f, 0x00, 2, 0, 16, 10, 5, 0, 10, 0x0000000f)
h5fieldtype(::JLDFile, ::Type{Float32}, ::Type{Float32}, ::Initialized) =
    FloatingPointDatatype(DT_FLOATING_POINT, 0x20, 0x1f, 0x00, 4, 0, 32, 23, 8, 0, 23, 0x0000007f)
h5fieldtype(::JLDFile, ::Type{Float64}, ::Type{Float64}, ::Initialized) =
    FloatingPointDatatype(DT_FLOATING_POINT, 0x20, 0x3f, 0x00, 8, 0, 64, 52, 11, 0, 52, 0x000003ff)

function jltype(f::JLDFile, dt::FloatingPointDatatype)
    if dt == h5fieldtype(f, Float64, Float64, Val{true})
        return ReadRepresentation{Float64,Float64}()
    elseif dt == h5fieldtype(f, Float32, Float32, Val{true})
        return ReadRepresentation{Float32,Float32}()
    elseif dt == h5fieldtype(f, Float16, Float16, Val{true})
        return ReadRepresentation{Float16,Float16}()
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
    dt == h5fieldtype(f, T, T, Val{true}) ? (ReadRepresentation{T,T}(), true) :
                                            throw(UnsupportedFeatureException())
