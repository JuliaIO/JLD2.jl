function allzeros(::Type{T}) where T
    if sizeof(T) > 0
        first(reinterpret(T,zeros(UInt8, sizeof(T))))
    else
        T()
    end
end

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
    InlineUnionEl{T1,T2}(mask::UInt8, x::T1, y::T2) where {T1,T2} = new{T1,T2}(mask, x, y)
    InlineUnionEl{T1,T2}(x::T1) where {T1,T2} = new{T1,T2}(UInt8(0), x, allzeros(T2))
    InlineUnionEl{T1,T2}(x::T2) where {T1,T2} = new{T1,T2}(UInt8(255), allzeros(T1), x)
end

Base.convert(::Union, x::InlineUnionEl) = iszero(x.mask) ? x.t1 : x.t2

function writeasbits(T::Union)
    types = Base.uniontypes(T)
    length(types) == 2 && isbitstype(types[1]) && isbitstype(types[2])
end

function write_dataset(f::JLDFile, x::Array{T}, wsession::JLDWriteSession) where {T}
    if T isa Union && writeasbits(T)
        # Conversion has to be done earlier here because
        # vectors are special cased in dispatch
        y = InlineUnionEl{Base.uniontypes(T)...}.(x)
    else
        y = x
    end
    odr = objodr(y)
    write_dataset(f, WriteDataspace(f, y, odr), h5type(f, y), odr, y, wsession)
end

# This function is identical to the one in data.jl
# exept for the ReadRepresentation and the very last line where the data is
# converted back into a Union Array
function read_array(f::JLDFile, dataspace::ReadDataspace,
                    rr::ReadRepresentation{InlineUnionEl{T1,T2},RR}, data_length::Int,
                    filter_id::UInt16, header_offset::RelOffset,
                    attributes::Union{Vector{ReadAttribute},Nothing}) where {T1, T2,RR}
    io = f.io
    data_offset = position(io)
    ndims, offset = get_ndims_offset(f, dataspace, attributes)
    seek(io, offset)
    v = construct_array(io, InlineUnionEl{T1,T2}, Int(ndims))
    header_offset !== NULL_REFERENCE && (f.jloffset[header_offset] = WeakRef(v))
    n = length(v)
    seek(io, data_offset)
    if filter_id == 1
        read_compressed_array!(v, f, rr, data_length)
    else
        read_array!(v, f, rr)
    end
    Union{T1, T2}[v;]
end
