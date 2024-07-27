#
# Miscellaneous functions
#

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


jlread(io::IO, ::Type{NTuple{N,T}}) where {N,T} = ntuple(_->jlread(io, T), Val{N}())
jlread(io::IO, ::Type{Tuple{}}) = ()

function write_nb_int(io::IO, sz::Integer, nb::Integer)
    for i = 1:nb
        jlwrite(io, UInt8((sz >> (8*(i-1))) & 0xff))
    end
end

function read_nb_uint(io::IO, nb)
    val = zero(UInt)
    for i = 1:nb
        val += jlread(io, UInt8) * 2^(8*(i-1))
    end
    val
end