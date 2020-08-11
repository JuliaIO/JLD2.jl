struct InlineUnionArray{T1,T2}
    mask::Vector{UInt8}
    t1::Vector{T1}
    t2::Vector{T2}
end

function InlineUnionArray(v::Vector{U}) where U
    @assert U isa Union
    types = Base.uniontypes(U)
    @assert length(types) == 2
    T1, T2 = types
    N = length(v)
    mask = zeros(UInt8, N)
    t1 = Vector{T1}(undef, N)
    t2 = Vector{T2}(undef, N)
    for n in 1:N
        el = v[n]
        if typeof(el) === T1
            t1[n] = el
        else
            mask[n] = UInt8(255)
            t2[n] = el
        end
    end
    InlineUnionArray{T1,T2}(mask, t1, t2)
end

function Base.convert(::Type{Vector}, x::InlineUnionArray{T1,T2}) where {T1,T2}
    N = length(x.mask)
    v = Vector{Union{T1,T2}}(undef, N)
    for n=1:N
        if x.mask[n] == 0
            v[n] = x.t1[n]
        else
            v[n] = x.t2[n]
        end
    end
    v
end

Base.convert(::Type{InlineUnionArray}, x) = InlineUnionArray(x)


function writeas(::Type{Vector{T}}) where T
    if T isa Union
        types = Base.uniontypes(T)
        length(types) == 2 || return Vector{T}
        (isbitstype(types[1]) && isbitstype(types[2])) || return Vector{T}
        return InlineUnionArray{types...}
    end
    return Vector{T}
end


function write_dataset(f::JLDFile, x::Vector{T}, wsession::JLDWriteSession) where {T}
    if writeas(typeof(x)) <: InlineUnionArray
        # Conversion has to be done earlier here because
        # vectors are special cased in dispatch
        y = InlineUnionArray(x)
    else
        y = x
    end
    odr = objodr(y)
    write_dataset(f, WriteDataspace(f, y, odr), h5type(f, y), odr, y, wsession)
end


# This is annoying to have to do conversion here
# But the problem is that the standard machinery tries to convert
# elements of a vector at a time instead of the whole thing
@inline function read_scalar(f::JLDFile{MmapIO}, rr::ReadRepresentation{<:InlineUnionArray}, header_offset::RelOffset)
    io = f.io
    inptr = io.curptr
    obj = jlconvert(rr, f, inptr, header_offset)
    io.curptr = inptr + odr_sizeof(rr)
    convert(Vector, obj)
end

@inline function read_scalar(f::JLDFile{IOStream}, rr::ReadRepresentation{<:InlineUnionArray}, header_offset::RelOffset)
    r = Vector{UInt8}(undef, odr_sizeof(rr))
    @GC.preserve r begin
        unsafe_read(f.io, pointer(r), odr_sizeof(rr))
        obj = jlconvert(rr, f, pointer(r), header_offset)
    end
    convert(Vector, obj)
end
