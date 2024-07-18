#
# Miscellaneous functions
#

# Redefine unsafe_load, unsafe_store!, read, and write so that they pack the type
function define_packed(ty::DataType)
    @assert isbitstype(ty)
    packed_offsets = cumsum([jlsizeof(x) for x in ty.types])
    sz = pop!(packed_offsets)
    pushfirst!(packed_offsets, 0)

    if sz != jlsizeof(ty)
        @eval begin
            function jlunsafe_store!(p::Ptr{$ty}, x::$ty)
                $([:(jlunsafe_store!(pconvert(Ptr{$(ty.types[i])}, p+$(packed_offsets[i])), getfield(x, $i)))
                   for i = 1:length(packed_offsets)]...)
            end
            function jlunsafe_load(p::Ptr{$ty})
                $(Expr(:new, ty, [:(jlunsafe_load(pconvert(Ptr{$(ty.types[i])}, p+$(packed_offsets[i]))))
                                   for i = 1:length(packed_offsets)]...))
            end
            jlsizeof(::Union{$ty,Type{$ty}}) = $(Int(sz))::Int
        end
    end

    @eval begin
        @inline jlwrite(io::Union{MmapIO,BufferedWriter}, x::$ty) = _write(io, x)
        @inline jlread(io::Union{MmapIO,BufferedReader}, x::Type{$ty}) = _read(io, x)
        function jlread(io::IO, ::Type{$ty})
            $(Expr(:new, ty, [:(jlread(io, $(ty.types[i]))) for i = 1:length(packed_offsets)]...))
        end
        function jlwrite(io::IO, x::$ty)
            $([:(jlwrite(io, getfield(x, $i))) for i = 1:length(packed_offsets)]...)
            nothing
        end
    end
    nothing
end

define_packed(RelOffset)


"""
    read_size(io::IO, flags::UInt8)

Loads a variable-length size according to flags

Expects that the first two bits of flags mean:
- 0:   The size of the Length of Link Name field is 1 byte.
- 1:   The size of the Length of Link Name field is 2 bytes.
- 2:   The size of the Length of Link Name field is 4 bytes.
- 3:   The size of the Length of Link Name field is 8 bytes.

Returns the size as an `Int`.
"""
function read_size(io::IO, flags::UInt8)
    if (flags & 3) == 0
        Int(jlread(io, UInt8))
    elseif (flags & 3) == 1
        Int(jlread(io, UInt16))
    elseif (flags & 3) == 2
        Int(jlread(io, UInt32))
    else
        Int(jlread(io, UInt64))
    end
end

"""
    size_flag(sz::Integer)::UInt8

Return the flag that represents the smallest integer type that can represent `sz`.
0 -> UInt8, 1 -> UInt16, 2 -> UInt32, 3 -> UInt64
"""
function size_flag(sz::Integer)
    if sz <= typemax(UInt8)
        UInt8(0)
    elseif sz <= typemax(UInt16)
        UInt8(1)
    elseif sz <= typemax(UInt32)
        UInt8(2)
    else
        UInt8(3)
    end
end

"""
    write_size(io::IO, sz::Integer)

Write the mininum number of bytes required to represent `sz` as (valid) unsigned integer.
"""
function write_size(io::IO, sz::Integer)
    if sz <= typemax(UInt8)
        jlwrite(io, UInt8(sz))
    elseif sz <= typemax(UInt16)
        jlwrite(io, UInt16(sz))
    elseif sz <= typemax(UInt32)
        jlwrite(io, UInt32(sz))
    else
        jlwrite(io, UInt64(sz))
    end
end

"""
    size_size(sz::Integer)

Return the number of bytes required to represent `sz` as an unsigned integer
that actually exists. (e.g. UInt8, UInt16, UInt32, UInt64)
"""
function size_size(sz::Integer)
    if sz <= typemax(UInt8)
        1
    elseif sz <= typemax(UInt16)
        2
    elseif sz <= typemax(UInt32)
        4
    else
        8
    end
end

"""
    size_size2(sz::Integer)

Return the number of bytes required to represent `sz` as an unsigned integer.
Note: this does not check if the integer is a valid julia integer.
"""
function size_size2(sz::Integer)
    if sz < 2^8
        1
    elseif sz < 2^16
        2
    elseif sz < 2^24
        3
    elseif sz < 2^32
        4
    elseif sz < 2^40
        5
    elseif sz < 2^48
        6
    elseif sz < 2^56
        7
    else
        8
    end
end


"""
    symbol_length(x::Symbol)

Returns the length of the string represented by `x`.
"""
symbol_length(x::Symbol) = ccall(:strlen, Int, (Cstring,), x)

"""
    uintofsize(sz::Integer)

Return the `UInt` type that has `sz` bytes.
"""
function uintofsize(sz)
    if sz == 1
        UInt8 
    elseif sz == 2
        UInt16
    elseif sz == 4
        UInt32
    elseif sz == 8 
        UInt64
    else
        throw(ArgumentError("There is no UInt type with $sz bytes"))
    end
end

"""
    to_uint64(bts::Vector{UInt8})

Generate a `UInt64` from a vector of `UInt8` assuming little-endian encoding.
Vector may be shorter than 8 bytes. In that case, the remaining bytes are assumed to be zero.
"""
function to_uint64(bts::Vector{UInt8})
    bts2 = append!(zeros(UInt8, 8-length(bts)), reverse(bts))
    u = zero(UInt64)
    for b in bts2
        u = u << 8
        u += b
    end
    u
end

"""
    skip_to_aligned!(io, rel=0)

Skip to nearest position aligned to a multiple of 8 bytes relative to `rel`.
"""
function skip_to_aligned!(io, rel=0)
    pos = position(io)
    pos += 8 - mod1(pos-rel, 8)
    seek(io, pos)
    return nothing
end


"""
    jlwrite(io::IO, x::Tuple)

Attempt to write a tuple to `io` by writing each element of the tuple in order.
"""
function jlwrite(io::IO, x::Tuple) 
    for y in x
        jlwrite(io, y)
    end
end

"""
    write_zerobytes(io, n)

Write `n` zero bytes to `io`.
"""
function write_zerobytes(io, n)
    for i in 1:n
        jlwrite(io, UInt8(0))
    end
end

"""
    isset(flag, bit)

Return true if the bit-th bit of `flag` is set. (starting from 0)   
"""
function isset(flag, bit)
    #return !iszero(flag & UInt8(2^(bit-1)))
    return Bool(flag >> (bit) & 1)
end

"""
    flag2uint(flag::UInt8)
I
Map the lowest to bits of `flag` to a `UInt` type, mapping 0 to `UInt8`, 1 to `UInt16`, 2 to `UInt32`, and 3 to `UInt64`.
"""
function flag2uint(flag::UInt8)
    # use lowest two bits
    value = flag << 6 >> 6
    if value == 0
        UInt8
    elseif value == 1
        UInt16
    elseif value == 2
        UInt32
    else
        UInt64
    end
end