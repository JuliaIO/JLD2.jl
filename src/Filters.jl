"""
JLD2.Filters

This module contains the interface for using filters in JLD2.jl.
"""
module Filters

using JLD2: JLD2, MemoryBackedIO, ensureroom, Hmessage, HmWrap, HmFilterPipeline, jlread, read_bytestring, HeaderMessage, Length, RelOffset, JLDFile, jlwrite


"""
    Filter

Abstract type to describe HDF5 Filters.
"""
abstract type Filter end

"""
    FILTERS

Maps filter id to filter type.
"""
const FILTERS = Dict{UInt16,Type{<:Filter}}()

"""
    filterid(F) where {F <: Filter}


The internal filter id of a filter of type `F`.
"""
function filterid end
filterid(x::Filter) = filterid(typeof(x))

"""
    filtername(::Type{F}) where {F<:Filter}

What is the name of a filter? Defaults to "Unnamed Filter"
Returns a String describing the filter.
"""
filtername(::Type{F}) where {F<:Filter} = "Unnamed Filter"
filtername(x::Filter) = filtername(typeof(x))

"""
    client_values(::Filter)
Retrieve the client values of a filter.
Depending on the filter type, this may include something like a compression level.
"""
client_values(::Filter) = ()

"""
    register_filter(::Type{F}) where F <: Filter

Register a filter with JLD2.
"""
function register_filter(::Type{F}) where {F<:Filter}
    id = filterid(F)
    FILTERS[id] = F
    return nothing
end

"""
    FilterPipeline(filters)

Container for a sequence of JLD2 compression filters.
"""
struct FilterPipeline{T}
    filters::T
    function FilterPipeline(filters::Vararg)
        @assert all(x -> x isa Filter, filters)
        new{typeof(filters)}(filters)
    end
end
FilterPipeline(filters::Array{<:Filter}) = FilterPipeline(filters...)
FilterPipeline(filters::Tuple) = FilterPipeline(filters...)

iscompressed(fp::FilterPipeline) = !isempty(fp.filters)
Base.iterate(fp::FilterPipeline, state=1) = iterate(fp.filters, state)

function compress(fp::FilterPipeline, buf::Vector{UInt8}, elsize)
    for filter in fp
        buf = compress(filter, buf, elsize)
    end
    buf
end

function decompress(filter::FilterPipeline, io::IO, data_length, elsize)
    buf = read!(io, Vector{UInt8}(undef, data_length))
    decompress(filter, buf, data_length, elsize)
end

function decompress(filter::FilterPipeline, io::MemoryBackedIO, data_length, elsize)
    ensureroom(io, data_length)
    buf = unsafe_wrap(Array, Ptr{UInt8}(io.curptr), data_length)
    decompress(filter, buf, data_length, elsize)
end

function decompress(fp::FilterPipeline, buf::Vector{UInt8}, data_length, args...)
    for filter in reverse(fp.filters)
        buf = decompress(filter, buf, data_length, args...)
        data_length = length(buf)
    end
    buf
end

# For loading need filter_ids as keys
const KNOWN_FILTERS = Dict(
    UInt16(1) => ("Deflate", "JLD2"),
    UInt16(2) => ("Shuffle", "JLD2"),
    UInt16(307) => ("Bzip2Filter", "JLD2Bzip2"),
    UInt16(32001) => ("BloscFilter", "JLD2Blosc"),
    UInt16(32004) => ("LZ4Filter", "JLD2Lz4"),
    UInt16(32008) => ("BitshuffleFilter", "JLD2Bitshuffle"),
    UInt16(32015) => ("ZstdFilter", "JLD2Zstd"),
)

function Filter(id, args...)
    F = get(FILTERS, id, nothing)
    if isnothing(F)
        if haskey(KNOWN_FILTERS, id)
            fil, pkg = KNOWN_FILTERS[id]
            throw(ArgumentError("""
            Written dataset is encoded with filter $(fil) from package $(pkg).
            Make sure to load the package before reading the dataset.
            """))
        else
            throw(ArgumentError("""
            Written dataset is encoded with unknown filter with id $(id).
            """))
        end
    end
    return F(args...)
end

function normalize_filters(compress)
    if compress isa Bool && compress == false
        return FilterPipeline(())
    elseif compress isa Bool && compress == true
        return FilterPipeline(Deflate())
    elseif compress isa FilterPipeline
        return compress
    elseif compress isa Filter
        return FilterPipeline(compress)
    elseif compress isa Vector && eltype(compress) <: Filter
        return FilterPipeline(compress)
    end
    throw(ArgumentError("Failed to interpret the compression argument."))
end


##############################################################################
## Shuffle Filter implementation
##############################################################################
"""
    Shuffle <: Filter

The Shuffle filter can be used as part of a filter pipeline to compress datasets.
It rearranges the bytes of elements in an array to improve compression
efficiency. It is not a compression filter by itself, but can be used in conjunction
with other compression filters like `Deflate`` or `ZstdFilter`.

It can be useful when the array, for example, contains unsigned integer `UInt64` and all
values are small. Then all the upper bytes of the eight byte integer are zero.
This filter will rearrange the bytes so that all the least significant bytes are at the
beginning of the array, followed by the second least significant bytes, and so on, which
simplifies the compression of the data.
"""
mutable struct Shuffle <: Filter
    element_size::UInt32
end
Shuffle() = Shuffle(0)
filterid(::Type{Shuffle}) = UInt16(2)
client_values(filter::Shuffle) = (filter.element_size,)
register_filter(Shuffle)

function compress(fil::Shuffle, data::Vector{UInt8}, element_size)
    fil.element_size = element_size
    # Start with all least significant bytes, then work your way up
    # I'll leave this for someone else to make performant
    data_length = length(data)
    @assert length(data) % element_size == 0
    num_elements = data_length ÷ element_size
    data_new = similar(data)
    for n = eachindex(data_new)
        # Throw in an Int64 to avoid overflow on 32bit systems
        j = 1 + Int64(n - 1) * num_elements
        i = mod1(j, data_length) + (j - 1) ÷ data_length
        data_new[i] = data[n]
    end
    return data_new
end

function decompress(::Shuffle, data::Vector{UInt8}, data_length, element_size)
    # Start with all least significant bytes, then work your way up
    # I'll leave this for someone else to make performant
    @assert data_length == length(data)
    @assert data_length % element_size == 0
    num_elements = data_length ÷ element_size
    data_new = similar(data)
    for n = eachindex(data_new)
            # Throw in an Int64 to avoid overflow on 32bit systems
        j = 1 + Int64(n - 1) * num_elements
        i = mod1(j, data_length) + (j - 1) ÷ data_length
        data_new[n] = data[i]
    end
    return data_new
end


##############################################################################
## Deflate Filter implementation
##############################################################################
using CodecZlib: CodecZlib

"""
    Deflate <: Filter

The Deflate filter can be used to compress datasets.
It uses the well-known and widely used zlib (deflate) compression algorithm.
"""
struct Deflate <: Filter
    level::Cuint
    Deflate(level=5) = new(clamp(level, 0, 9))
end

filterid(::Type{Deflate}) = UInt16(1)
filtername(::Type{Deflate}) = ""
client_values(filter::Deflate) = (filter.level, )
__init__() = register_filter(Deflate)

function compress(filter::Deflate, buf::Vector{UInt8}, args...)
    CodecZlib.transcode(
        CodecZlib.ZlibCompressor(; filter.level),
        buf)
end

function decompress(::Deflate, buf::Vector{UInt8}, args...)
    CodecZlib.transcode(
        CodecZlib.ZlibDecompressor(),
        buf)
end

############################################################################################
## File level functions
############################################################################################

struct WrittenFilter
    id::UInt16
    flags::UInt16
    name::String
    client_data::Vector{UInt32}
end

struct WrittenFilterPipeline
    filters::Vector{WrittenFilter}
end

WrittenFilterPipeline() = WrittenFilterPipeline(WrittenFilter[])
iscompressed(fp::WrittenFilterPipeline) = !isempty(fp.filters)

function WrittenFilterPipeline(msg_::Hmessage)
    msg = HmWrap(HmFilterPipeline, msg_)
    version = msg.version
    nfilters = msg.nfilters
    io = msg.m.io
    seek(io, msg.m.address + 2)
    version == 1 && skip(io, 6)
    filters = map(1:nfilters) do _
        id = jlread(io, UInt16)
        name_length = (version == 2 && id < 255) ? zero(UInt16) : jlread(io, UInt16)
        flags = jlread(io, UInt16)
        nclient_vals = jlread(io, UInt16)
        name = iszero(name_length) ? "" : read_bytestring(io)
        skip(io, max(0, name_length - length(name) - 1))
        client_data = jlread(io, UInt32, nclient_vals)
        (version == 1 && isodd(nclient_vals)) && skip(io, 4)
        WrittenFilter(id, flags, name, client_data)
    end
    return WrittenFilterPipeline(filters)
end

FilterPipeline(hm::Hmessage) =
    FilterPipeline(WrittenFilterPipeline(hm))

FilterPipeline(fp::WrittenFilterPipeline) =
    FilterPipeline([Filter(fil.id, fil.client_data...) for fil in fp.filters])


function pipeline_message_size(fp::FilterPipeline)
    sz = 4 + 2
    for filter in fp
        sz += 6 + 4 * length(Filters.client_values(filter))
        if filterid(filter) > 255
            sz += 2
            fnamelen = length(filtername(filter)) + 1
            fnamelen += 8 - mod1(fnamelen, 8)
            sz += fnamelen
        end
    end
    sz
end

function write_filter_pipeline_message(io, fp::FilterPipeline)
    hmsize = pipeline_message_size(fp) - 4
    jlwrite(io, HeaderMessage(HmFilterPipeline, hmsize, 0))
    jlwrite(io, UInt8(2))                   # Version
    jlwrite(io, UInt8(length(fp.filters)))  # Number of Filters

    for filter in fp.filters
        filter_id = filterid(filter)
        client_data = client_values(filter)
        if filter_id > 255
            filter_name = filtername(filter)
            fnamelen = length(filter_name) + 1
            fnamelen += 8 - mod1(fnamelen, 8)
            padding = fnamelen - length(filter_name)
        end
        jlwrite(io, filter_id)                # Filter Identification Value
        filter_id > 255 && jlwrite(io, UInt16(fnamelen))
        # Length of Filter Name
        jlwrite(io, UInt16(0))                # Flags
        jlwrite(io, UInt16(length(client_data))) # Number of Client Data Values
        filter_id > 255 && jlwrite(io, filter_name) # Filter Name
        filter_id > 255 && (padding > 0) && jlwrite(io, zeros(UInt8, padding))
        for v in client_data
            jlwrite(io, UInt32(v))     # Client Data (Compression Level)
        end
    end
    nothing
end

end # module
