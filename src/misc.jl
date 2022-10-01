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
                $([:(jlunsafe_store!(convert(Ptr{$(ty.types[i])}, p+$(packed_offsets[i])), getfield(x, $i)))
                   for i = 1:length(packed_offsets)]...)
            end
            function jlunsafe_load(p::Ptr{$ty})
                $(Expr(:new, ty, [:(jlunsafe_load(convert(Ptr{$(ty.types[i])}, p+$(packed_offsets[i]))))
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

"""
    read_size(io::IO, flags::UInt8)

Loads a variable-length size according to flags
Expects that the first two bits of flags mean:
0   The size of the Length of Link Name field is 1 byte.
1   The size of the Length of Link Name field is 2 bytes.
2   The size of the Length of Link Name field is 4 bytes.
3   The size of the Length of Link Name field is 8 bytes.
Returns the size as an Int
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

# Determine what the size flag should be
# Same rules as above
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

# Store a size
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

# Get the size of the size
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

# Get the size of the size
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

function uintofsize(sz)
    if sz == 1
        UInt8 
    elseif sz == 2
        UInt16
    elseif sz == 4
        UInt32
    else 
        UInt64
    end
end

function to_uint64(bts::Vector{UInt8})
    bts2 = [bts; zeros(UInt8, 8-length(bts))]
    u = zero(UInt64)
    for b in reverse(bts2)
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