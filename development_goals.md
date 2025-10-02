# Development goals

- [ ] Code organisation: package all v1 btree and v2 btree implementations in a submodule
- [ ] Add support for all array chunking types (DataLayout version 4)
- [ ] Add support for incremental chunk addition
- [ ] Support specifying fill values for unwritten chunks (HmFillValue) - this (primarily) needs API and reading support
- [ ] Implement more powerful / generic keyword handling in the API for full user control and reasonable program flow complexity.
      i.e. do kwarg validation and passing things around to the right places.
