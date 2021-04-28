# Compression

JLD2 supports compression of `isbits` arrays. This includes the typical `Array{Float64}`
but also arrays of custom structs that are immutable and only consist of basic number
type fields.

To enable the default compression, you can write
```
FileIO.save("example.jld2", "large_array", zeros(10000); compress = true)
```
using the `FileIO` API. Alternatively use
```
jldsave("example.jld2", true; large_array=zeros(10000))
```
or
```
jldopen("example.jld2", "w"; compress = true) do f
    f["large_array"] = zeros(10000)
end
```

When reading a file `JLD2` detects compression and automatically decompresses the data
so it is not necessary to pass any extra parameters for that case.

`JLD2` uses [`TranscodingStreams.jl`](https://github.com/JuliaIO/TranscodingStreams.jl)
to interact with compression algorithms. Compression is disabled by
default and if `compress = true` is passed, 
[`CodecZlib.jl`](https://github.com/JuliaIO/CodecZlib.jl) will be used.

!!! note
    Note that `CodecZlib.jl` must be explicitly installed for compression/decompression
    to work. If the package is not already loaded `JLD2` will try to dynamically load it,
    but it is recommended to do it explicitly, e.g. `using JLD2, CodecZlib`.

### Choosing a compression algorithm

If you want to use a different compression algorithm that is better suited to
your needs, you can also directly pass a compressor.

| Library | Compressor |    |
|---------|------------|----|
| `CodecZlib.jl` | `ZlibCompressor` | The default as it is very widely used. |
| `CodecBzip2.jl` | `Bzip2Compressor` | Can often times be faster |
| `CodecLz4.jl` | `LZ4FrameCompressor` | Fast, but not compatible to the LZ4 shipped by HDF5 |


To use any of these, replace the `compress = true` argument with an instance of the
compressor, e.g.
```
using JLD2, CodecBzip2
jldopen("example.jld2", "w"; compress = Bzip2Compressor()) do f
    f["large_array"] = zeros(10000)
end
```

### Manually selecting compression for datasets

Sometimes you may know, that some of your arrays are easily compressible and
that for others it is not worth the effort. For precise control, the
`write` function takes an optional keyword argument to override the file compression
settings.

```
using JLD2
jldopen("example.jld2", "w") do f
    # This can be efficiently compressed  → use compression
    write(f, "compressed_array", zeros(10000); compress=true)

    # Don't compress this 
    write(f, "large_array", rand(10000))
end
```
