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
 - Fix reconstruction of partially intialized structs

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
