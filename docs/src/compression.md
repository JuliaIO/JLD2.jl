# Compression

JLD2 supports compression of `isbits` arrays. This includes the typical `Array{Float64}`
but also arrays of custom structs that are immutable and only consist of basic number
type fields.

To enable the default compression, you can write:
```julia
using JLD2
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
However, `JLD2` will prompt you to install and load the necessary filter packages if they are
not yet available.

## Compression Filter API

JLD2 can use a number of different compression algorithms, also called *filter*s.
These can be used individually and even chained which can be useful for some types of data.
The filter used by `compress = true` is the `Deflate()` compression filter.

!!! note
    The default `Deflate()` compression is always available but some others will need to be
    installed separately.
    `JLD2` will throw an error if the required filter package is not loaded, prompting
    you to install and load the appropriate package e.g. : `using JLD2, JLD2Lz4`.

### Installing Filter Packages

To use compression filters, you need to install and load the corresponding packages:

```julia
using Pkg
# For other compression algorithms
Pkg.add("JLD2Lz4")
using JLD2, JLD2Lz4  # Load the package you need
```

### Available Compression Filters

This compression system is analogous to that of HDF5 and uses the same underlying compression
libraries. JLD2 files with compressed datasets can in many cases be opened using HDF5 and
similarly, JLD2 will be able to read most HDF5 files even with compression.
The compression filters available for JLD2 are:

| Filter Package | Filter Type       | Notes                                                        |
|:---------------|:------------------|:-------------------------------------------------------------|
| *built in*     | `Shuffle`         | Rearrangement of bytes useful as a preprocess filter         |
| *built in*     | `Deflate`         | Default compression, very widely used, good compatibility    |
| *built in*     | `ZstdFilter`      | Fast, wide range of compression size vs speed trade-offs     |
| JLD2Bzip2      | `Bzip2Filter`     | Good compression ratio, can be slower                        |
| JLD2Lz4        | `Lz4Filter`       | Very fast compression/decompression                          |

### Using Specific Filters

To use a specific compression filter, pass an instance of the filter instead of `true`:

```julia
using JLD2, JLD2Lz4

# Using Lz4 compression
jldopen("example.jld2", "w"; compress = Lz4Filter()) do f
    f["large_array"] = zeros(10000)
end

# Zstd with non-standard compression level
jldopen("example.jld2", "w"; compress = ZstdFilter(9)) do f
    f["large_array"] = zeros(10000)
end
```


### Using Multiple Filters

JLD2 supports combining multiple filters for advanced compression strategies.
This is particularly useful when combining preprocessing filters (like shuffling) with
compression filters. Simply provide a vector of filters:

```julia
using JLD2

# Combine Shuffle preprocessing with Deflate compression
filters = [Shuffle(), Deflate()]

jldopen("example.jld2", "w"; compress = filters) do f
    # Benefits from byte shuffling
    # Only the lowest byte of each element is non-zero
    # Shuffle() reorders the bytes of all elements from e.g.
    # [123123123] to [111222333]
    # where each digit refers to the nth byte of an array element.
    f["numeric_data"] = UInt.(rand(UInt8, 10000))
end
```

!!! note
    Filters in a pipeline are applied in order during compression and in reverse
    order during decompression. Preprocessing filters (like `Shuffle`)
    should typically come before compression filters.


### Filter Configuration Examples

Different filters support various configuration options:

```julia
using JLD2, JLD2Lz4, JLD2Bzip2

# Zstd with different compression levels
zstd_fast = ZstdFilter(1)    # Fast compression
zstd_best = ZstdFilter(22)   # Best compression

# Bzip2 with custom block size
bzip2_filter = Bzip2Filter(4)

# Example usage
jldopen("example.jld2", "w") do f
    write(f, "fast_data", zeros(UInt8, 10000); compress=zstd_fast)
    write(f, "small_data", randn(10000); compress=zstd_best)
    write(f, "archive_data", randn(1000); compress=bzip2_filter)
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
using JLD2

jldopen("example.jld2", "w"; compress=ZstdFilter()) do f
    # This gets compressed with the ZstdFilter
    write(f, "default_array", zeros(10000))

    # Don't compress this
    write(f, "random_array", rand(10000); compress=false)

    # Override the above compression filter and use a different one
    write(f, "zlib_array", zeros(10000); compress=Deflate())

    # Alternatively, use the same filter but with different configuration
    write(f, "fast_compressed", rand(10000); compress=ZstdFilter(1))
end
```

### Compatibility and Migration from v0.5 to v0.6

- **File Compatibility**: Files created with the old API can be read with the new system.
  Files created with the v0.6 filter API may not be able to read by older versions of JLD2, see the compatibility table below for more information.
- **Performance**: Compression performance and file sizes remain the same as the
  underlying compression libraries are unchanged.
- **HDF5 Compatibility**: The new API is analogous to HDF5.jl, making it easier to
  work with HDF5 files and improving interoperability.

#### Filter Compatibility Table

The following table shows which JLD2 versions can decode data compressed using different filter features:

| Filter feature | JLD2 versions able to decode |
|:------------|:---------------|
| `Deflate` | Since 0.2.0|
| `Bzip2Filter` | Since 0.4.4|
| `ZstdFilter` | Since 0.4.49|
| `Shuffle` | Since 0.6.0 |
| `Lz4Filter` | Since 0.6.0 |
| multiple filters | Since 0.6.0 |

**Notes:**
- Data compressed with `LZ4FrameCompressor` in previous versions
can be read if `JLD2Lz4` is loaded.
Data compressed with `Lz4Filter` cannot be read by JLD2 versions before 0.6.0.

For code migration, the main change is in how you specify compression filters:

```julia
# Old API
# using JLD2, CodecZlib
# jldopen("file.jld2", "w"; compress = ZlibCompressor()) do f

# New API
using JLD2
jldopen("file.jld2", "w"; compress = Deflate()) do f
    # ...
end
```

The simplest usage option of `compress=true` still works as before.


### API Docstrings

```@docs
JLD2.Filters
JLD2.Filters.Deflate
JLD2.Filters.Shuffle
JLD2.Filters.ZstdFilter
```
