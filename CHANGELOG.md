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
