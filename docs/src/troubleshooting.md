# Gotchas & Troubleshooting

## Objects are cached during loading
JLD2 caches objects during loading. It may give you the same object twice.
This can lead to surprising results if you edit loaded arrays. Note, the underlying file is not being edited!

```@repl
using JLD2 #hide
jldsave("demo.jld2", a=zeros(2))
f = jldopen("demo.jld2")
a = f["a"] # bind loaded array to name `a`
a[1] = 42; # editing the underlying array
f["a"]
a = nothing # remove all references to the loaded array
GC.gc(true) # call GC to remove the cache
f["a"] # a new copy is loaded from the file
close(f) #hide
```

## Cross-compatibility
JLD2 tries to write files in a way that allows you to load them on different operating systems and in particular both on 32bit and 64bit systems.
However, many julia structs may be inherently different on different architectures making this task impossible.
In particular, moving data from a 64bit system to a 32bit system is only guaranteed to work for basic datatypes.

## Security
Beware of opening JLD2 files from untrusted sources. A malicious file may execute code on your computer.
See e.g. this project's [issue #117](https://github.com/JuliaIO/JLD2.jl/issues/117).
To check a file, you can use debug tooling provided by `JLD2` to view what kinds of objects are stored.
Details on the available tools are described below.

## Viewing header messages
Following the HDF5 format specification, JLD2 stores metadata and all information required
to interpret the stored data for each dataset in the form of so-called *header messages*.
Each hdf5 group, dataset, and committed datatype consist of and object header followed by a
variable number of header messages.

There exist different types of these to encode for the data type or the layout i.e. single
element or array.

These can be printed for inspection using JLD2:
```@repl headermsg
using JLD2 #hide
jldsave("test.jld2";
    a = 42,
    b = [1,2,3,4,5],
    c = (1,2),
)
f = jldopen("test.jld2")
JLD2.print_header_messages(f, "a")
```

Here we see, among other things, a
 * dataspace message which states that `"a"` is a single (scalar) element
 * datatype message
 * datalayout message of the *compact* type which means that the data is so small it was
 directly stored as part of the message.


```@repl headermsg
JLD2.print_header_messages(f, "b")
```

Important differences to `"a"` are that the dataspace now reports the dimensions of the
array as `(5,)` and the the data layout has changed to *contiguous* which means that it is
stored as a single block starting at the offset reported in `data_address`.

```@repl headermsg
JLD2.print_header_messages(f, "c")
```

For dataset `c` we see that the datatype is a *shared* datatype which is stored elsewhere
in the file and is referenced by its offset. This is, of course, also a regular hdf5 object
and we can print its header messages by supplying the offset:

```@repl headermsg
JLD2.print_header_messages(f, JLD2.RelOffset(4520))
close(f) #hide
```

This object consists of just two messages:

 * The datatype message defines the hdf5 datatype and therefore describes the byte layout
and field types.
 * The attribute message has the name `julia_type` and as payload the julia `DataType`
 signature `Tuple{Int64, Int64}` which is needed for reconstruction.

