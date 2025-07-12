## 0.5.16
 - Improve `readmmap` to work on more types and misaligned data

## 0.5.15
 - Add full control over type mapping by providing a typemap function

## 0.5.14
 - Add support for `Memory` and `MemoryRef`

## 0.5.13
 - Make UnPack.jl compat into an extension
 - bumps lower compat for julia to v1.9

## 0.5.12
 - Fix precompile workload for loading on latest julia

## 0.5.11
 - Make `Upgrade` work for types that sit inside `Union{..}`

## 0.5.10
 - fix regression for `UInt32`
 - **Deprecation**: Do not rely on JLD2 to load a compression library. This feature will be removed in a future release. Instead explicitly add `using` statements into your scripts.

## 0.5.9
 - fix regression for `Union{Bool,Nothing}` array elements (#617)
 - fix printing issue in `printtoc`

## 0.5.8
 - Stop using `Base.module_keys` as it is removed on nightly

## 0.5.7
 - Fix edge case for uninitialized Vlens

## 0.5.6
 - Add Aqua tests and eliminate method ambiguities

## 0.5.5
 - Experimental support for writing to and reading from `IO` objects e.g. `jldopen(io, "r")`

## 0.5.4
 - Important correctness fix when storing very many equally sized objects
that may get GC'ed while storing is in progress! (#603)

## 0.5.3
 - Bugfix for `<: Function` structs

## 0.5.2
 - Remove left-over warning
 - Restrict warning for storing `Function` objects to `anonymous` functions.

## 0.5.1
 - Bugfix and added test for bug introduced in v0.5.0

## 0.5.0
 - Improved encoding of committed datatypes.
 This fixes longstanding issues but new files will not be loaded correctly with JLD2 versions prior to `v0.5.0`.
 Existing files remain readable.
 - JLD2 now warns when storing structures containing `<: Function` objects.

## 0.4.53
 - Experimental: Slicing and inplace updating of array datasets
 - updated CI workflows
 - improve pretty printing of attribute header message
 - fix storing of datatype info for h5 compat

## 0.4.52
 - fix attribute loading
 - new features: `readmmap` `ismmappable`  and `allocate_early` (api experimental)
 - adds Downgrade testing
 - new feature: disable committing datatypes. (restrict to h5 numbers, strings, and arrays)
 - internal cleanup
 - new experimental feature: reconstruct all committed types as `NamedTuple`s

## 0.4.51
 - remove Unicode normalization support due to excessive performance loss
 - rework of header message internals
 - Implement addition and loading of attributes to datasets and groups
 - Add preliminary `Dataset` structure and API
 - cleanup tests
 - remove `Pkg` dependency
 - fix `Upgrade` for Singleton types

## 0.4.50
 - Don't hide exception data during loading and saving (#569)

## 0.4.49
 - update compat bounds
 - add support Zstdcompression

## 0.4.48
 - fix behaviour for unnormalized strings
 - add missing method for load_attributes
 - clean up `using` statements

## 0.4.47
 - fix loading structs with more than 256 fields (#558)

## 0.4.46
 - remove usage of `IOBuffer` internals

## 0.4.45
 - fix loading of unknown struct with UnionAll (unknown) type parameter (#538)

## 0.4.44
 - fix dispatch around `Union{}` as type parameter

## 0.4.43
 - do not try to `rconvert` `#undef` array elements (#529)

## 0.4.42
 - improvements in time-to-first save/load on julia v1.10 and up
 - discourage from using custom serialization to `Array` and stop using it internally
 as it cannot properly preserve object identity.
 - fix inline group `show`
 - fix a bug in group loading for the `IOStream` backend
 - reduce invalidations to zero

## 0.4.41
 - fix ntuple type with typevar length
 - fix OpaqueData test (HDF5 compat)

## 0.4.40
 - fix unitialized custom-serialized objects
 - allow serializing pkg-modules by name

## 0.4.39
 - backward compatibility support for windows 7 by changing the default IO type to IOStream (#509) (@HongBinYu-hub)

## 0.4.38
 - restrict default Dict encoding to Base implementations

## 0.4.37
 - Update Dict encoding for latest julia

## 0.4.36
 - compat bound for TranscodingStreams.jl

## 0.4.35
 - fix roundtrip of Type{} objects #484

## 0.4.34
 - reclose file when opening fails

## 0.4.33
 - fix `Upgrade` for parametric types
 - new type reconstruction when matching DataType cannot be found (eval-free)
 - new `parallel_read` keyword for creating stand-alone file handles for multithreaded file reading (@ejmeitz)

## 0.4.32
 - add experimental `JLD2.readas` function for customized reading of custom serialized objects (#468)

## 0.4.31
 - fix UInt32 truncation error for absurdly large array sizes
 - move test-files to a separate repo

## 0.4.30
 -  allow loading compressed files during precompilation #446 (@marius311)

## 0.4.29
 - added `Upgrade` feature

## 0.4.28
 - compatibility to julia v1.9-dev (@eschnett)

## 0.4.26
 - fix identity relations with custom serialization

## 0.4.25
 - remove leftover debug statement

## 0.4.24
 - read-only support for `JLD.jl` files
 - read-only support for many HDF5 files. Most test files of HDF5.jl are covered
 - read Opaque bit fields
 - read some other string encodings
 - read big endian numbers
 - read typical chunking formats

## 0.4.23
 - Support for `const` fields in mutable structs

## 0.4.22
 - Fix reconstruction of partially initialized structs

## 0.4.21
 - Add explicit type mapping

## 0.4.20
 - TTFX improvements
 - Add a comment on jldsave (@BoundaryValueProblems)
## 0.4.19
 - Don't inline long ntuples

## 0.4.18
 - Bugfix for dynamic loading of CodecZlib

## 0.4.17
 - Automatically transform symbol keys in strings with a warning (@theogf)

## 0.4.16
 - Fix @save macro
 - Improve auto-conversion of convertible types

## 0.4.15
 - Implement `Base.keytype` for `JLDFile` and `Group`
