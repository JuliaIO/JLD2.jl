# Compression

JLD2 supports compression of `isbits` arrays. This includes the typical `Array{Float64}`
but also arrays of custom structs that are immutable and only consist of basic number
type fields.

To enable the default compression, you can write:
```julia
using JLD2, JLD2Deflate
save("example.jld2", "large_array", zeros(10000); compress = true)
```
Alternatively use
```julia
jldsave("example.jld2", true; large_array=zeros(10000))
```
or
```julia
jldopen("example.jld2", "w"; compress = true) do f
    f["large_array"] = zeros(10000)
end
```

When reading a file `JLD2` detects compression and automatically decompresses the data
so it is not necessary to pass any extra parameters for that case.
However, JLD2 will prompt you to install and load the necessary filter packages if they are
not yet available.

## Compression Filter API

JLD2 can use a number of different compression algorithms, also called *filter*s.
These can be used individually and even chained which can be useful for some types of data.
The filter used by `compress = true` is the `Deflate()` compression filter.

!!! note
    The default Deflate compression requires the `JLD2Deflate` package to be installed.
    JLD2 will throw an error if the required filter package is not loaded, prompting
    you to install and load the appropriate package: `using JLD2, JLD2Deflate`.

### Installing Filter Packages

To use compression filters, you need to install and load the corresponding packages:

```julia
# For default Deflate compression
using Pkg; Pkg.add("JLD2Deflate")
using JLD2, JLD2Deflate

# For other compression algorithms
Pkg.add(["JLD2Blosc", "JLD2Bzip2", "JLD2Lz4", "JLD2Zstd", "JLD2Bitshuffle"])
using JLD2, JLD2Blosc  # Load the specific package you need
```

### Available Compression Filters

This compression system is analogous to that of HDF5 and uses the same underlying compression
libraries. JLD2 files with compressed datasets can in many cases be opened using HDF5 and
similarly, JLD2 will be able to read most HDF5 files even with compression.
The compression filters available for JLD2 are:

| Filter Package | Filter Type       | Notes                                                        |
|:---------------|:------------------|:-------------------------------------------------------------|
| JLD2 (builtin) | `Shuffle`         | Rearrangement of bytes useful as a preprocess filter for integer arrays |
| JLD2Deflate    | `Deflate`         | Default compression, very widely used, good compatibility    |
| JLD2Blosc      | `BloscFilter`     | High-performance compression with multiple algorithms        |
| JLD2Bzip2      | `Bzip2Filter`     | Good compression ratio, can be slower                        |
| JLD2Lz4        | `LZ4Filter`       | Very fast compression/decompression                          |
| JLD2Zstd       | `ZstdFilter`      | Fast, wide range of compression size vs speed trade-offs     |
| JLD2Bitshuffle | `BitshuffleFilter`| Bit-level shuffling to improve compression of numeric data   |

### Using Specific Filters

To use a specific compression filter, pass an instance of the filter instead of `true`:

```julia
using JLD2, JLD2Blosc

# Using Blosc compression
jldopen("example.jld2", "w"; compress = BloscFilter()) do f
    f["large_array"] = zeros(10000)
end

# Blosc with custom settings
jldopen("example.jld2", "w"; compress = BloscFilter(level=9, compressor="zstd")) do f
    f["large_array"] = zeros(10000)
end
```

```julia
using JLD2, JLD2Zstd

# Using Zstd compression
jldsave("example.jld2", ZstdFilter(level=3); large_array=zeros(10000))
```

### Using Multiple Filters

JLD2 supports combining multiple filters for advanced compression strategies.
This is particularly useful when combining preprocessing filters (like shuffling) with
compression filters. Simply provide a vector of filters:

```julia
using JLD2, JLD2Deflate

# Combine Shuffle preprocessing with Deflate compression
filters = [Shuffle(), Deflate()]

jldopen("example.jld2", "w"; compress = filters) do f
    # Benefits from bit shuffling
    # Only the lowest byte of each element is non-zero
    # Shuffle() reorders the bytes of all elements from e.g.
    # [123123123] to [111222333]
    # where each digit refers to the nth byte of an array element.
    f["numeric_data"] = UInt.(rand(UInt8, 10000)
end
```

!!! note
    Filters in a pipeline are applied in order during compression and in reverse
    order during decompression. Preprocessing filters (like Shuffle)
    should typically come before compression filters.

!!! note
    Both `BloscFilter` and `BitshuffleFilter` have optional builtin support for shuffling
    that may be more performant than the sequential example above.


### Filter Configuration Examples

Different filters support various configuration options:

```julia
using JLD2, JLD2Blosc, JLD2Zstd, JLD2Bzip2

# Blosc with different algorithms and settings
blosc_lz4 = BloscFilter(level=5, shuffle=true, compressor="lz4")
blosc_zstd = BloscFilter(level=3, shuffle=false, compressor="zstd")

# Zstd with different compression levels
zstd_fast = ZstdFilter(level=1)    # Fast compression
zstd_best = ZstdFilter(level=9)   # Best compression

# Bzip2 with custom block size
bzip2_filter = Bzip2Filter(blocksize=9)

# Example usage
jldopen("example.jld2", "w") do f
    write(f, "fast_data", large_array; compress=blosc_lz4)
    write(f, "small_data", small_array; compress=zstd_best)
    write(f, "archive_data", archive_array; compress=bzip2_filter)
end
```

Depending on the characteristics of your datasets, some configurations may be more efficient
than others.

### Manually selecting compression for datasets

Sometimes you may know that some of your arrays are easily compressible and
that for others it is not worth the effort. For precise control, the
`write` function takes an optional keyword argument to override the file compression
settings.

```julia
using JLD2, JLD2Deflate, JLD2Blosc

jldopen("example.jld2", "w") do f
    # This can be efficiently compressed → use compression
    write(f, "compressed_array", zeros(10000); compress=Deflate())

    # Don't compress this
    write(f, "random_array", rand(10000); compress=false)

    # Use a different filter for this specific dataset
    write(f, "fast_compressed", rand(10000); compress=BloscFilter(compressor="lz4"))
end
```

### Compatibility and Migration

The new filter-based API is fully compatible with the previous compression system:

- **File Compatibility**: Files created with the new filter API can be read by older
  versions of JLD2, and files created with the old API can be read with the new system.
- **Performance**: Compression performance and file sizes remain the same as the
  underlying compression libraries are unchanged.
- **HDF5 Compatibility**: The new API is analogous to HDF5.jl, making it easier to
  work with HDF5 files and improving interoperability.

For code migration, the main change is in how you specify compression filters:

```julia
# Old API
# using JLD2, CodecZlib
# jldopen("file.jld2", "w"; compress = ZlibCompressor()) do f

# New API
using JLD2, JLD2Deflate
jldopen("file.jld2", "w"; compress = Deflate()) do f
    # ...
end
```

The simplest usage option of `compress=true` still works, provided that you load
`JLD2Deflate`.
