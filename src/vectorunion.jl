struct InlineUnionEl{T1,T2}
    mask::UInt8
    t1::T1
    t2::T2
    InlineUnionEl{T1,T2}(mask::UInt8, x::T1, y::T2) where {T1,T2} = new{T1,T2}(mask, x, y)
    InlineUnionEl{T1,T2}(x::T1) where {T1,T2} = new{T1,T2}(UInt8(0), x)
    function InlineUnionEl{T1,T2}(x::T2) where {T1,T2}
        el = new{T2,T1}(UInt8(0), x)
        convert(InlineUnionEl{T1,T2}, el)
    end
end
function Base.convert(::Type{InlineUnionEl{T2,T1}}, x::InlineUnionEl{T1,T2}) where {T1,T2}
    if x.mask == 0
        InlineUnionEl{T2,T1}(UInt8(255), x.t2, x.t1)
    else
        InlineUnionEl{T2,T1}(UInt8(0), x.t2, x.t1)
    end
end
function Base.convert(::Type{InlineUnionEl{T1,T2}}, x::InlineUnionEl{T1,T2}) where {T1,T2}
    x
end

function Base.show(io::IO, iu::InlineUnionEl)
    print(io, string(typeof(iu)))
    if iu.mask == 0
        print(io, ": ", string(iu.t1))
    else
        print(io, ": ", string(iu.t2))
    end
end

#InlineUnionEl{Float64, Int}(1.0)


# Base.convert(::Type{InlineUnionEl{T1,T2}}, x) where {T1,T2}= InlineUnionEl{T1,T2}(x)
# function Base.convert(::Type, x::InlineUnionEl)
#     if x.mask == 0
#         x.t1
#     else
#         x.t2
#     end
# end
#
# function writeas(::Type{Vector{T}}) where T
#      if T isa Union
#          types = Base.uniontypes(T)
#          length(types) == 2 || return Vector{T}
#          (isbitstype(types[1]) && isbitstype(types[2])) || return Vector{T}
#          return InlineUnionEl{types...}#InlineUnionArray{types...}
#      end
#      return Vector{T}
# end

function writeas(T::Union)
    types = Base.uniontypes(T)
    length(types) == 2 || return T
    (isbitstype(types[1]) && isbitstype(types[2])) || return T
    return InlineUnionEl{types...}#InlineUnionArray{types...}
end

function CustomSerialization(::Type, ::Union, odr)
    #@info "executed this"
    odr
end

function Base.convert(::Union, x::InlineUnionEl)
    if x.mask == 0
        x.t1
    else
        x.t2
    end
end

function h5convert!(out::Pointers,
    ::OnDiskRepresentation{OFFS,Tuple{UInt8,T1,T2},H5types},
    f::JLDFile, x::Union{T1,T2}, wsession::JLDWriteSession) where {OFFS, T1,T2, H5types}
    #error("blb")
    #@info "also did this"
    #@show x
    if x isa T1
        unsafe_store!(convert(Ptr{UInt8}, out), UInt8(0))
        h5convert!(out+1, odr(T1), f, x, wsession)
    else
        #@show x
        unsafe_store!(convert(Ptr{UInt8}, out), UInt8(255))
        h5convert!(out+1+odr_sizeof(T1), odr(T2), f, x, wsession)
    end
end

h5convert!(::Ptr, odr::Nothing, ::JLDFile, ::Missing, ::JLDWriteSession) = nothing
