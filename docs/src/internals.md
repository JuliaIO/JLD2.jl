# Internals & Design

## File Interface
The JLDFile object mimics the API of `Base.Dict` as much as it can.
In particular, `keys`, `length`, `haskey`, `isempty`, `get`, `get!` should work as expected.

## B-tree Functions

```@docs
JLD2.BTrees.write_v2btree_chunked_dataset
JLD2.BTrees.read_symbol_table_node
JLD2.BTrees.read_records_in_node
JLD2.BTrees.read_v2btree_header
```

## Chunked Array Internals

```@docs
JLD2.Chunking.read_chunked_array
```

```@autodocs
Modules = [JLD2]
```
