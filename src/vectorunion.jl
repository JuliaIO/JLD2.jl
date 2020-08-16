
"""
    InlineUnionEl{T1,T2}(mask::UInt8, t1::T1, t2::T2)
Custom serialization struct for two member isbits union
fields e.g. in other structs or arrays.
To indicate that t1 is relevant the mask takes the value `UInt8(0)`
and for t2 `UInt8(255)`
"""
struct InlineUnionEl{T1,T2}
    mask::UInt8
    t1::T1
    t2::T2
end

Base.convert(::Union, x::InlineUnionEl) = iszero(x.mask) ? x.t1 : x.t2


function writeas(T::Union)
    # This function decides which Union fields should be stored in an
    # inlined way. The rule is, the Union has to have to member types
    # and they both need to be isbits.
    types = Base.uniontypes(T)
    length(types) == 2 || return T
    (isbitstype(types[1]) && isbitstype(types[2])) || return T
    return InlineUnionEl{types...}
end

# Custom h5convert / serialization function for isbits union types
# This removes the need for an intermediate conversion to InlineUnionEl
function h5convert!(
    out::Pointers,
    ::Type{CustomSerialization{InlineUnionEl{T1,T2},ODR}},
    f::JLDFile,
    x::Union{T1,T2},
    wsession::JLDWriteSession) where {T1,T2, ODR}

    if x isa T1
        unsafe_store!(convert(Ptr{UInt8}, out), UInt8(0))
        h5convert!(out+1, odr(T1), f, x, wsession)
    else
        unsafe_store!(convert(Ptr{UInt8}, out), UInt8(255))
        h5convert!(out+1+odr_sizeof(T1), odr(T2), f, x, wsession)
    end
end

# Singleton fields (e.g. Missing) don't actually need to be written at all
h5convert!(::Ptr, odr::Nothing, ::JLDFile, ::Missing, ::JLDWriteSession) = nothing
