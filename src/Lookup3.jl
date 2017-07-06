module Lookup3

# Original source at http://www.burtleburtle.net/bob/c/lookup3.c

rot(x::UInt32, k) = xor(((x)<<(k)), ((x)>>(32-(k))))

# -------------------------------------------------------------------------------
# mix -- mix 3 32-bit values reversibly.

# This is reversible, so any information in (a,b,c) before mix() is
# still in (a,b,c) after mix().

# If four pairs of (a,b,c) inputs are run through mix(), or through
# mix() in reverse, there are at least 32 bits of the output that
# are sometimes the same for one pair and different for another pair.
# This was tested for:
# * pairs that differed by one bit, by two bits, in any combination
#   of top bits of (a,b,c), or in any combination of bottom bits of
#   (a,b,c).
# * "differ" is defined as +, -, ^, or ~^.  For + and -, I transformed
#   the output delta to a Gray code (a^(a>>1)) so a string of 1's (as
#   is commonly produced by subtraction) look like a single 1-bit
#   difference.
# * the base values were pseudorandom, all zero but one bit set, or
#   all zero plus a counter that starts at zero.

# Some k values for my "a-=c; a^=rot(c,k); c+=b;" arrangement that
# satisfy this are
#     4  6  8 16 19  4
#     9 15  3 18 27 15
#    14  9  3  7 17  3
# Well, "9 15 3 18 27 15" didn't quite get 32 bits diffing
# for "differ" defined as + with a one-bit base and a two-bit delta.  I
# used http://burtleburtle.net/bob/hash/avalanche.html to choose
# the operations, constants, and arrangements of the variables.

# This does not achieve avalanche.  There are input bits of (a,b,c)
# that fail to affect some output bits of (a,b,c), especially of a.  The
# most thoroughly mixed value is c, but it doesn't really even achieve
# avalanche in c.

# This allows some parallelism.  Read-after-writes are good at doubling
# the number of bits affected, so the goal of mixing pulls in the opposite
# direction as the goal of parallelism.  I did what I could.  Rotates
# seem to cost as much as shifts on every machine I could lay my hands
# on, and rotates are much kinder to the top and bottom bits, so I used
# rotates.
# -------------------------------------------------------------------------------
@inline function mix(a, b, c)
  a -= c;  a = xor(a, rot(c, 4));  c += b
  b -= a;  b = xor(b, rot(a, 6));  a += c
  c -= b;  c = xor(c, rot(b, 8));  b += a
  a -= c;  a = xor(a, rot(c,16));  c += b
  b -= a;  b = xor(b, rot(a,19));  a += c
  c -= b;  c = xor(c, rot(b, 4));  b += a
  (a, b, c)
end

# # -------------------------------------------------------------------------------
# final -- final mixing of 3 32-bit values (a,b,c) into c

# Pairs of (a,b,c) values differing in only a few bits will usually
# produce values of c that look totally different.  This was tested for
# * pairs that differed by one bit, by two bits, in any combination
#   of top bits of (a,b,c), or in any combination of bottom bits of
#   (a,b,c).
# * "differ" is defined as +, -, ^, or ~^.  For + and -, I transformed
#   the output delta to a Gray code (a^(a>>1)) so a string of 1's (as
#   is commonly produced by subtraction) look like a single 1-bit
#   difference.
# * the base values were pseudorandom, all zero but one bit set, or
#   all zero plus a counter that starts at zero.

# These constants passed:
#  14 11 25 16 4 14 24
#  12 14 25 16 4 14 24
# and these came close:
#   4  8 15 26 3 22 24
#  10  8 15 26 3 22 24
#  11  8 15 26 3 22 24
# -------------------------------------------------------------------------------
@inline function final(a, b, c)
  c = xor(c, b); c -= rot(b,14)
  a = xor(a, c); a -= rot(c,11)
  b = xor(b, a); b -= rot(a,25)
  c = xor(c, b); c -= rot(b,16)
  a = xor(a, c); a -= rot(c,4)
  b = xor(b, a); b -= rot(a,14)
  c = xor(c, b); c -= rot(b,24)
  c
end

# -------------------------------------------------------------------------------
# hashlittle() -- hash a variable-length key into a 32-bit value
#   k       : the key (the unaligned variable-length array of bytes)
#   length  : the length of the key, counting by bytes
#   initval : can be any 4-byte value
# Returns a 32-bit value.  Every bit of the key affects every bit of
# the return value.  Two keys differing by one or two bits will have
# totally different hash values.

# The best hash table sizes are powers of 2.  There is no need to do
# mod a prime (mod is sooo slow!).  If you need less than 32 bits,
# use a bitmask.  For example, if you need only 10 bits, do
#   h = (h & hashmask(10));
# In which case, the hash table should have hashsize(10) elements.

# If you are hashing n strings (uint8_t **)k, do it like this:
#   for (i=0, h=0; i<n; ++i) h = hashlittle( k[i], len[i], h);

# By Bob Jenkins, 2006.  bob_jenkins@burtleburtle.net.  You may use this
# code any way you wish, private, educational, or commercial.  It's free.

# Use for hash table lookup, or anything where one collision in 2^^32 is
# acceptable.  Do NOT use for cryptographic purposes.
# -------------------------------------------------------------------------------
function hash(k::AbstractVector{UInt8}, istart::Integer=1, n::Integer=length(k), initval::UInt32=UInt32(0))
    n <= length(k) || throw(BoundsError())
    hash(pointer(k, istart), n, initval)
end

function hash(k::Ptr{UInt8}, n::Integer=length(k), initval::UInt32=UInt32(0))
    # Set up the internal state
    a = b = c = 0xdeadbeef + convert(UInt32, n) + initval

    # --------------- all but the last block: affect some 32 bits of (a,b,c)
    ptr = k
    # @inbounds while n > 12
    #     a += k[offset]
    #     a += convert(UInt32, k[offset+1])<<8
    #     a += convert(UInt32, k[offset+2])<<16
    #     a += convert(UInt32, k[offset+3])<<24
    #     @show a
    #     b += k[offset+4]
    #     b += convert(UInt32, k[offset+5])<<8
    #     b += convert(UInt32, k[offset+6])<<16
    #     b += convert(UInt32, k[offset+7])<<24
    #     @show b
    #     c += k[offset+8]
    #     c += convert(UInt32, k[offset+9])<<8
    #     c += convert(UInt32, k[offset+10])<<16
    #     c += convert(UInt32, k[offset+11])<<24
    #     @show c
    #     (a, b, c) = mix(a, b, c)
    #     n -= 12
    #     offset += 12
    # end
    @inbounds while n > 12
        a += unsafe_load(convert(Ptr{UInt32}, ptr))
        ptr += 4
        b += unsafe_load(convert(Ptr{UInt32}, ptr))
        ptr += 4
        c += unsafe_load(convert(Ptr{UInt32}, ptr))
        (a, b, c) = mix(a, b, c)
        ptr += 4
        n -= 12
    end

    # -------------------------------- last block: affect all 32 bits of (c)
    # @inbounds if n > 0
    #     n >= 12 && (c += convert(UInt32, k[offset+11])<<24)
    #     n >= 11 && (c += convert(UInt32, k[offset+10])<<16)
    #     n >= 10 && (c += convert(UInt32, k[offset+9])<<8)
    #     n >= 9  && (c += k[offset+8])
    #     n >= 8  && (b += convert(UInt32, k[offset+7])<<24)
    #     n >= 7  && (b += convert(UInt32, k[offset+6])<<16)
    #     n >= 6  && (b += convert(UInt32, k[offset+5])<<8)
    #     n >= 5  && (b += k[offset+4])
    #     n >= 4  && (a += convert(UInt32, k[offset+3])<<24)
    #     n >= 3  && (a += convert(UInt32, k[offset+2])<<16)
    #     n >= 2  && (a += convert(UInt32, k[offset+1])<<8)
    #     n >= 1  && (a += k[offset])
    #     c = final(a, b, c)
    # end
    @inbounds if n > 0
        if n == 12
            c += unsafe_load(convert(Ptr{UInt32}, ptr+8))
            @goto n8
        elseif n == 11
            c += UInt32(unsafe_load(Ptr{UInt8}(ptr+10)))<<16
            @goto n10
        elseif n == 10
            @label n10
            c += UInt32(unsafe_load(Ptr{UInt8}(ptr+9)))<<8
            @goto n9
        elseif n == 9
            @label n9
            c += unsafe_load(ptr+8)
            @goto n8
        elseif n == 8
            @label n8
            b += unsafe_load(convert(Ptr{UInt32}, ptr+4))
            @goto n4
        elseif n == 7
            @label n7
            b += UInt32(unsafe_load(Ptr{UInt8}(ptr+6)))<<16
            @goto n6
        elseif n == 6
            @label n6
            b += UInt32(unsafe_load(Ptr{UInt8}(ptr+5)))<<8
            @goto n5
        elseif n == 5
            @label n5
            b += unsafe_load(ptr+4)
            @goto n4
        elseif n == 4
            @label n4
            a += unsafe_load(convert(Ptr{UInt32}, ptr))
        elseif n == 3
            @label n3
            a += UInt32(unsafe_load(Ptr{UInt8}(ptr+2)))<<16
            @goto n2
        elseif n == 2
            @label n2
            a += UInt32(unsafe_load(Ptr{UInt8}(ptr+1)))<<8
            @goto n1
        elseif n == 1
            @label n1
            a += unsafe_load(ptr)
        end
        c = final(a, b, c)
    end
    c
end
end
