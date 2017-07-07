#
# Miscellaneous functions
#

# Redefine unsafe_load, unsafe_store!, read, and write so that they pack the type
function define_packed(ty::DataType)
    @assert isbits(ty)
    packed_offsets = cumsum([sizeof(x) for x in ty.types])
    sz = pop!(packed_offsets)
    unshift!(packed_offsets, 0)

    if sz != sizeof(ty)
        @eval begin
            function Base.unsafe_store!(p::Ptr{$ty}, x::$ty)
                $([:(unsafe_store!(convert(Ptr{$(ty.types[i])}, p+$(packed_offsets[i])), getfield(x, $i)))
                   for i = 1:length(packed_offsets)]...)
            end
            function Base.unsafe_load(p::Ptr{$ty})
                $(Expr(:new, ty, [:(unsafe_load(convert(Ptr{$(ty.types[i])}, p+$(packed_offsets[i]))))
                                   for i = 1:length(packed_offsets)]...))
            end
            Base.sizeof(::Union{$ty,Type{$ty}}) = $sz
        end
    end

    @eval begin
        @inline Base.write(io::Union{MmapIO,BufferedWriter}, x::$ty) = _write(io, x)
        @inline Base.read(io::Union{MmapIO,BufferedReader}, x::Type{$ty}) = _read(io, x)
        function Base.read(io::IO, ::Type{$ty})
            $(Expr(:new, ty, [:(read(io, $(ty.types[i]))) for i = 1:length(packed_offsets)]...))
        end
        function Base.write(io::IO, x::$ty)
            $([:(write(io, getfield(x, $i))) for i = 1:length(packed_offsets)]...)
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
        Int(read(io, UInt8))
    elseif (flags & 3) == 1
        Int(read(io, UInt16))
    elseif (flags & 3) == 2
        Int(read(io, UInt32))
    else
        Int(read(io, UInt64))
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
        write(io, UInt8(sz))
    elseif sz <= typemax(UInt16)
        write(io, UInt16(sz))
    elseif sz <= typemax(UInt32)
        write(io, UInt32(sz))
    else
        write(io, UInt64(sz))
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

"""
    symbol_length(x::Symbol)

Returns the length of the string represented by `x`.
"""
symbol_length(x::Symbol) = ccall(:strlen, Int, (Cstring,), x)
