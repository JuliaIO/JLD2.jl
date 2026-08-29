#
# Global heap
#

const GLOBAL_HEAP_SIGNATURE = htol(0x4c4f4347) # "GCOL"

struct GlobalHeapID
    heap_offset::RelOffset
    index::UInt32
end
define_packed(GlobalHeapID)

isatend(f::JLDFile, gh::GlobalHeap) =
    gh.offset != 0 && f.end_of_data == gh.offset + 8 + jlsizeof(Length) + gh.length

heap_object_length(data::AbstractArray) = length(data)
heap_object_length(::Any) = 1

"""
    allocate_in_global_heap(f::JLDFile, objsz::Int) -> GlobalHeap

Allocate space in the global heap for an object of size `objsz`.
Returns the GlobalHeap that has space for the object.

Allocation strategy:
1. Use existing heap if object fits
2. Extend existing heap if it's at end of file
3. Create new heap otherwise
"""
function allocate_in_global_heap(f::JLDFile, objsz::Int)
    io = f.io

    # Can only fit up to typemax(UInt16) items in a single heap
    heap_filled = length(f.global_heap.objects) >= typemax(UInt16)

    if objsz + 8 + jlsizeof(Length) < f.global_heap.free && !heap_filled
        # Fits in existing global heap
        return f.global_heap
    elseif isatend(f, f.global_heap) && !heap_filled
        # Global heap is at end and can be extended
        gh = f.global_heap
        delta = objsz - gh.free + 8 + jlsizeof(Length)
        gh.free += delta
        gh.length += delta
        seek(io, gh.offset + 8)
        jlwrite(io, gh.length)
        f.end_of_data += delta
        return gh
    else
        # Need to create a new global heap
        heapsz = max(objsz, 4096)
        offset = f.end_of_data + 8 - mod1(f.end_of_data, 8)
        seek(io, offset)
        jlwrite(io, GLOBAL_HEAP_SIGNATURE)
        jlwrite(io, UInt32(1))      # Version & Reserved
        jlwrite(io, Length(heapsz)) # Collection size
        f.end_of_data = position(io) + heapsz
        gh = f.global_heap = f.global_heaps[h5offset(f, offset)] =
            GlobalHeap(offset, heapsz, heapsz, Int64[])
        return gh
    end
end

function write_heap_object(f::JLDFile, odr::ODR, data, wsession::JLDWriteSession) where ODR
    # The type parameter ODR is needed to convince the compiler to specialize on ODR.
    psz = odr_sizeof(odr) * heap_object_length(data)
    objsz = 8 + jlsizeof(Length) + psz
    objsz += 8 - mod1(objsz, 8)
    io = f.io

    # Allocate space in global heap
    gh = allocate_in_global_heap(f, objsz)

    # Write object header
    index = length(gh.objects) + 1
    objoffset = gh.offset + 8 + jlsizeof(Length) + gh.length - gh.free
    seek(io, objoffset)
    jlwrite(io, UInt16(index))           # Heap object index
    jlwrite(io, UInt16(1))               # Reference count
    jlwrite(io, UInt32(0))               # Reserved
    jlwrite(io, Length(psz))             # Object size

    # Update global heap
    gh.free -= objsz
    push!(gh.objects, objoffset)

    # Write free space object
    if gh.free >= 8 + jlsizeof(Length)
        seek(io, objoffset + objsz)
        jlwrite(io, UInt64(0))           # Object index, reference count, reserved
        jlwrite(io, Length(gh.free - 8 - jlsizeof(Length))) # Object size
    end

    # Write actual data
    seek(io, objoffset + 8 + jlsizeof(Length))
    write_data(io, f, data, odr, datamode(odr), wsession)

    GlobalHeapID(h5offset(f, gh.offset), index)
end

# Force specialization on DataType
write_heap_object(f::JLDFile, odr::Type{Union{}}, data, wsession::JLDWriteSession) =
    error("ODR is invalid")

function jlread(io::IO, ::Type{GlobalHeap})
    offset = position(io)
    jlread(io, UInt32) == GLOBAL_HEAP_SIGNATURE || throw(InvalidDataException())
    jlread(io, UInt32) == 1 || throw(UnsupportedVersionException())
    heapsz = jlread(io, Length)
    index = 1
    objects = Int64[]
    startpos = position(io)
    free = heapsz
    while free > 8 + jlsizeof(Length)
        curpos = position(io)
        objidx = jlread(io, UInt16)
        objidx == 0 && break
        if objidx > index 
            append!(objects, fill(typemax(Int), objidx-index))
            index = objidx
        elseif objidx < index
            throw(InvalidDataException("Encountered unordered list of global heap objects."))
        end
        push!(objects, curpos)
        skip(io, 6)                    # Reference count and reserved
        sz = jlread(io, Length)          # Length
        skip(io, sz + 8 - mod1(sz, 8)) # Payload
        free = heapsz - Length(position(io) - startpos)
        index += 1
    end
    GlobalHeap(offset, heapsz, free, objects)
end

function read_heap_object(f::JLDFile, hid::GlobalHeapID, rr::ReadRepresentation)
    io = f.io
    if haskey(f.global_heaps, hid.heap_offset)
        gh = f.global_heaps[hid.heap_offset]
    else
        seek(io, fileoffset(f, hid.heap_offset))
        f.global_heaps[hid.heap_offset] = gh = jlread(io, GlobalHeap)
    end
    seek(io, gh.objects[hid.index]+8)
    len = Int(jlread(io, Length))
    n = div(len, odr_sizeof(rr))
    len == n * odr_sizeof(rr) || throw(InvalidDataException())

    read_array!(Vector{julia_repr(rr)}(undef, n), f, rr)
end
