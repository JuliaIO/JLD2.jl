# Internals & Design

## File Interface
The JLDFile object mimics the API of `Base.Dict` as much as it can.
In particular, `keys`, `length`, `haskey`, `isempty`, `get`, `get!` should work as expected.



```@autodocs
Modules = [JLD2]
```
