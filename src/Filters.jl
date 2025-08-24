"""
JLD2.Filters

This module contains the interface for using filters in JLD2.jl.
"""
module Filters

using JLD2: JLD2, MemoryBackedIO, ensureroom, Hmessage, HmWrap, HmFilterPipeline, jlread, read_bytestring, HeaderMessage, Length, RelOffset, JLDFile, jlwrite
using JLD2: odr_sizeof, h5convert!

"""
    Filter

Abstract type to describe HDF5 Filters.
"""
abstract type Filter end

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
    filtertype(id)

Retrieve the filter type for a given id.
"""
function filtertype end
filtertype(id::Integer) = filtertype(Val(Int(id)))
filtertype(::Val) = nothing

"""
    apply_filter!(filter, ref, forward::Bool=true)

Apply the filter to the `UInt8` vector stored in `ref`.
By default, the filter is applied in the forward direction (compression).
Setting `forward` to `false` applies the filter in the reverse direction (decompression).
The result is stored back in `ref` and the function returns an integer return code (0 on success).
"""
function apply_filter! end


"""
    set_local!(filter, odr, dataspace, datasetcreationprops)

Some filters use information about the dataset, like element size, element type, and layout. Filters may optionally implement this function for this purpose.
"""
set_local!(filter::Filter, odr, dataspace, datasetcreationprops) = nothing

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
Base.length(fp::FilterPipeline) = length(fp.filters)

# General case: serialize data to a buffer, then compress it
function compress(fp::FilterPipeline, data::Array{T}, odr::S, f::JLDFile, wsession) where {T,S}
    buf = Vector{UInt8}(undef, odr_sizeof(odr) * length(data))
    @GC.preserve buf begin
        cp = Ptr{Cvoid}(pointer(buf))
        @simd for i = 1:length(data)
            @inbounds h5convert!(cp, odr, f, data[i], wsession)
            cp += odr_sizeof(odr)
        end
    end
    ref = Ref(buf)
    retcodes = map(fil -> apply_filter!(fil, ref), fp)
    ref[], retcodes
end

# Special case of `samelayout` data: Use unsafe_wrap to avoid copying
function compress(fp::FilterPipeline, data::Array{T}, odr::T, f::JLDFile, wsession) where {T}
    GC.@preserve data begin
        buf = unsafe_wrap(
            Vector{UInt8},
            Ptr{UInt8}(pointer(data)),
            odr_sizeof(odr) * length(data)
        )
        ref = Ref(buf)
        retcodes = map(fil -> apply_filter!(fil, ref), fp)
        # Do a copy if all compression failed to not return the original unsafe_wrap
        ref[] === buf && (ref[] = copy(buf))
        ref[], retcodes
    end
end

function decompress(filter::FilterPipeline, io::IO, data_length)
    buf = read!(io, Vector{UInt8}(undef, data_length))
    _decompress(filter, buf)
end

function decompress(filter::FilterPipeline, io::MemoryBackedIO, data_length)
    ensureroom(io, data_length)
    buf = unsafe_wrap(Array, Ptr{UInt8}(io.curptr), data_length)
    _decompress(filter, buf)
end

function _decompress(fp::FilterPipeline, buf::Vector{UInt8})
    ref = Ref(buf)
    for filter in reverse(fp.filters)
        apply_filter!(filter, ref, false)
    end
    ref[]
end

# For loading need filter_ids as keys
const KNOWN_FILTERS = Dict(
    UInt16(1) => ("Deflate", "JLD2"),
    UInt16(2) => ("Shuffle", "JLD2"),
    UInt16(307) => ("Bzip2Filter", "JLD2Bzip2"),
    UInt16(32001) => ("BloscFilter", "JLD2Blosc"),
    UInt16(32004) => ("LZ4Filter", "JLD2Lz4"),
    UInt16(32008) => ("BitshuffleFilter", "JLD2Bitshuffle"),
    UInt16(32015) => ("ZstdFilter", "JLD2"),
)

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
filtertype(::Val{2}) = Shuffle

function set_local!(fil::Shuffle, odr, dataspace, datasetcreationprops)
    fil.element_size = odr_sizeof(odr)
    nothing
end

function apply_filter!(fil::Shuffle, ref, forward::Bool=true)
    buf = ref[]
    (; element_size) = fil
    nbytes = length(buf)
    @assert length(buf) % element_size == 0
    nelems = nbytes ÷ element_size
    newbuf = similar(buf)
    for n = eachindex(newbuf)
        # Convert to an Int64 to avoid overflow on 32bit systems
        j = 1 + Int64(n - 1) * nelems
        i = mod1(j, nbytes) + (j - 1) ÷ nbytes
        if forward
            newbuf[i] = buf[n]
        else
            newbuf[n] = buf[i]
        end
    end
    ref[] = newbuf
    return 0
end

##############################################################################
## Deflate Filter implementation
##############################################################################
using ChunkCodecLibZlib: ZlibEncodeOptions, ZlibDecodeOptions, encode, decode

"""
    Deflate <: Filter

The Deflate filter can be used to compress datasets.
It uses the well-known and widely used zlib (deflate) compression algorithm.

## Arguments:
- `level`: Compression level, between 0 and 9. Default is 5.
Larger numbers lead to better compression, but also to longer runtime.
"""
struct Deflate <: Filter
    level::Cuint
    Deflate(level=5) = new(clamp(level, 0, 9))
end

filterid(::Type{Deflate}) = UInt16(1)
filtername(::Type{Deflate}) = ""
client_values(filter::Deflate) = (filter.level, )
filtertype(::Val{1}) = Deflate

function apply_filter!(filter::Deflate, ref, forward::Bool=true)
    if forward
        ref[] = encode(ZlibEncodeOptions(; filter.level), ref[])
    else
        ref[] = decode(ZlibDecodeOptions(), ref[])
    end
    return 0
end

##############################################################################
## Zstd Filter implementation
##############################################################################
using ChunkCodecLibZstd: ChunkCodecLibZstd, ZstdEncodeOptions, ZstdDecodeOptions

"""
    ZstdFilter <: Filter

The ZstdFilter can be used to compress datasets using the Zstandard compression algorithm.

## Arguments:
- `level`: Compression level, between 1 and 22.
Larger numbers lead to better compression, but also to longer runtime.

"""
struct ZstdFilter <: Filter
    level::Cuint
    ZstdFilter(level=ChunkCodecLibZstd.ZSTD_defaultCLevel()) =
        new(clamp(level,
            # This library theoretically supports negative compression levels, which is at odds with hdf5 level repr using UInt32. Limit to 1 instead
            #ChunkCodecLibZstd.ZSTD_minCLevel(),
            1,
            ChunkCodecLibZstd.ZSTD_maxCLevel()
        )
        )
end

filterid(::Type{ZstdFilter}) = UInt16(32015)
filtername(::Type{ZstdFilter}) = "Zstandard compression: http://www.zstd.net"
client_values(filter::ZstdFilter) = (filter.level, )
filtertype(::Val{32015}) = ZstdFilter

function apply_filter!(filter::ZstdFilter, ref, forward::Bool=true)
    if forward
        ref[] = encode(ZstdEncodeOptions(; filter.level), ref[])
    else
        ref[] = decode(ZstdDecodeOptions(), ref[])
    end
    return 0
end



############################################################################################
## Intermediate representation and reading/writing of filter pipelines to file
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
    FilterPipeline([Filter(fil) for fil in fp.filters])


function Filter(fil::WrittenFilter)
    F = filtertype(fil.id)
    if isnothing(F)
        if haskey(KNOWN_FILTERS, id)
            fil, pkg = KNOWN_FILTERS[id]
            throw(ArgumentError("""
            Written dataset is encoded with filter $(fil) from package $(pkg).
            Make sure to load the package before reading the dataset.
            """))
        else
            throw(ArgumentError("""
            Written dataset is encoded with unknown filter with id $(id) and
            description "$(fil.name)".
            If you think this is a bug, please open an issue at https://github.com/JuliaIO/JLD2.jl/
            """))
        end
    end
    return F(fil.client_data...)
end


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
