# JLD2

[![Build Status](https://travis-ci.org/simonster/JLD2.jl.svg?branch=master)](https://travis-ci.org/simonster/JLD2.jl)
[![codecov.io](http://codecov.io/github/simonster/JLD2.jl/coverage.svg?branch=master)](http://codecov.io/github/simonster/JLD2.jl?branch=master)

JLD in pure Julia

JLD2 saves and loads Julia data structures in a format comprising a subset of HDF5. While other HDF5 implementations should be able to read the files it produces, there is no guarantee that JLD2 can read files produced by other implementations.

The code here should work on Julia 0.6. It has extensive unit tests and should properly serialize and deserialize in all cases supported by JLD and then some.
