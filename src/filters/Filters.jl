"""
JLD2.Filters

This module contains the interface for using filters in JLD2.jl.
"""
module Filters

using JLD2: JLD2, MemoryBackedIO, ensureroom

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

What is the name of a filter?
Defaults to "Unnamed Filter"
Returns a String describing the filter. See [`API.h5z_register`](@ref)
"""
filtername(::Type{F}) where {F<:Filter} = "Unnamed Filter"
filtername(x::Filter) = filtername(typeof(x))

"""
    client_values(::Filter)
Retrieve the client values of a filter.
Depending on the filter type, this may include something like a compression level.
"""
client_values(::Filter) = ()

# Generic implementation of register_filter
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

The filter pipeline associated with `plist`.

"""
struct FilterPipeline{T}
    filters::T
    FilterPipeline(filters::Vararg{<:Filter}) = new{typeof(filters)}(filters)
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

function decompress(filter::FilterPipeline, io::IO, data_length, element_size)
    buf = read!(io, Vector{UInt8}(undef, data_length))
    decompress(filter, buf, data_length, element_size)
end

function decompress(filter::FilterPipeline, io::MemoryBackedIO, data_length, element_size)
    ensureroom(io, data_length)
    buf = unsafe_wrap(Array, Ptr{UInt8}(io.curptr), data_length)
    decompress(filter, buf, data_length, element_size)
end

function decompress(fp::FilterPipeline, buf::Vector{UInt8}, data_length, args...)
    for filter in reverse(fp.filters)
        buf = decompress(filter, buf, data_length, args...)
        data_length = length(buf)
    end
    buf
end



## Shuffle Filter implementation
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
        j = 1 + (n-1)*num_elements
        i = mod1(j , data_length) + (j-1)÷data_length
        data_new[i] = data[n]
    end
    return data_new
end

function decompress(::Shuffle, data::Vector{UInt8}, data_length, element_size)
    # Start with all least significant bytes, then work your way up
    # I'll leave this for someone else to make performant
    @assert data_length == length(data)
    @assert data_length % element_size == 0
    num_elements =  data_length÷element_size
    data_new = similar(data)
    for n = eachindex(data_new)
        j = 1 + (n-1)*num_elements
        i = mod1(j , data_length) + (j-1)÷data_length
        data_new[n] = data[i]
    end
    return data_new
end



# For loading need filter_ids as keys
const KNOWN_FILTERS = Dict(
    UInt16(1) => ("Deflate", "JLD2Deflate"),
    UInt16(2) => ("Shuffle", "JLD2"),
    UInt16(307) => (:CodecBzip2, :Bzip2Compressor, :Bzip2Decompressor, "BZIP2"),
    #UInt16(32001) => (:Blosc, :BloscCompressor, :BloscDecompressor, "BLOSC"),
    UInt16(32004) => (:CodecLz4, :LZ4FrameCompressor, :LZ4FrameDecompressor, "LZ4"),
    UInt16(32015) => (:CodecZstd, :ZstdFrameCompressor, :ZstdDecompressor, "ZSTD"),
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
        if !haskey(FILTERS, UInt16(1))
            throw(ArgumentError("""
                Default compresssion filter (Deflate) is not available.
                Make sure to install and load the JLD2Deflate package."""))
        end
        return FilterPipeline(Filter(1))
    elseif compress isa FilterPipeline
        return compress
    elseif compress isa Filter
        return FilterPipeline(compress)
    elseif compress isa Vector && eltype(compress) <: Filter
        return FilterPipeline(compress)
    end
    throw(ArgumentError("Failed to interpret the compression argument."))
end


end # module
