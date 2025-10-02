# h5py Chunked Dataset API Analysis

## Summary

h5py provides automatic chunk index type selection based on dataset characteristics, primarily the `maxshape` parameter. This analysis documents h5py's API and selection logic for implementing similar functionality in JLD2.

## Test Setup

Created test datasets with various configurations:
- Test data: 30×10 Float32 array (300 elements)
- Chunk sizes tested: (5,5), (1,10), (30,1), (30,10)
- Various maxshape configurations

See: `test_h5py_chunking.py`

## h5py API Signatures

### Basic Syntax

```python
import h5py
import numpy as np

data = np.arange(300).reshape(30, 10).astype('f4')

# Basic chunked dataset
with h5py.File('output.h5', 'w') as f:
    f.create_dataset('name', data=data, chunks=(5, 5))

# With unlimited dimensions
with h5py.File('output.h5', 'w') as f:
    f.create_dataset('name', data=data, chunks=(5, 5), maxshape=(None, 10))

# With compression
with h5py.File('output.h5', 'w') as f:
    f.create_dataset('name', data=data, chunks=(5, 5),
                     compression='gzip', compression_opts=6)

# With fill value
with h5py.File('output.h5', 'w') as f:
    f.create_dataset('name', data=data, chunks=(5, 5), fillvalue=-999.0)

# Auto chunking
with h5py.File('output.h5', 'w') as f:
    f.create_dataset('name', data=data, chunks=True)  # h5py chooses chunk size
```

### Key Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `data` | numpy array | Data to write |
| `chunks` | tuple or True | Chunk dimensions, or True for auto |
| `maxshape` | tuple or None | Maximum dimensions; None in tuple = unlimited |
| `compression` | str or None | Compression filter ('gzip', 'lzf', etc.) |
| `compression_opts` | int | Compression level (e.g., 0-9 for gzip) |
| `fillvalue` | scalar | Fill value for unallocated chunks |

## Chunk Index Type Selection Logic

### Observed Behavior

h5py automatically selects chunk indexing types based on `maxshape`:

| Configuration | Chunk Index Type | HDF5 Dataspace |
|---------------|------------------|----------------|
| `chunks=(30,10)` (data size) | Type 1 (Single Chunk) | `(30, 10) / (30, 10)` |
| `maxshape=None` or `(30,10)` | Type 3 (Fixed Array) | `(30, 10) / (30, 10)` |
| `maxshape=(None,10)` | Type 4 (Extensible Array) | `(30, 10) / (UNLIMITED, 10)` |
| `maxshape=(None,None)` | Type 5 (V2 B-tree) | `(30, 10) / (UNLIMITED, UNLIMITED)` |

**Key Insight**: h5py does NOT use Type 2 (Implicit Index) by default. This type appears to be rarely used or reserved for specific HDF5 library optimizations.

### Selection Algorithm

```
def select_chunk_index_type(data_shape, chunks, maxshape):
    # Single chunk optimization
    if chunks == data_shape:
        return SINGLE_CHUNK  # Type 1

    # Count unlimited dimensions
    if maxshape is None:
        unlimited_count = 0  # All dimensions fixed
    else:
        unlimited_count = sum(1 for dim in maxshape if dim is None)

    # Select based on unlimited dimensions
    if unlimited_count == 0:
        return FIXED_ARRAY  # Type 3
    elif unlimited_count == 1:
        return EXTENSIBLE_ARRAY  # Type 4
    else:  # 2 or more
        return V2_BTREE  # Type 5
```

## Auto Chunking

When `chunks=True`, h5py automatically determines chunk size. From testing:

```python
data = np.arange(300).reshape(30, 10).astype('f4')
f.create_dataset('auto', data=data, chunks=True)
# Result: chunks=(30, 10) - same as data size!
```

h5py's auto-chunking appears to prefer single-chunk layout for small datasets. For larger datasets, it likely uses a target chunk size (often ~1MB) and divides dimensions accordingly.

## Compression Behavior

Compression works with all chunk index types:

```python
# Fixed array with compression
f.create_dataset('comp', data=data, chunks=(5, 5), compression='gzip', compression_opts=6)
# Still uses Type 3 (Fixed Array)

# Extensible with compression
f.create_dataset('comp_ext', data=data, chunks=(5, 5), maxshape=(None, 10),
                 compression='gzip', compression_opts=6)
# Uses Type 4 (Extensible Array) + gzip filter
```

**Key**: Chunk index type selection is independent of whether filters are applied.

## Fill Value Behavior

From testing:

```python
f.create_dataset('filled', data=data, chunks=(5, 5), fillvalue=-999.0)
```

- Fill value is stored in HmFillValue message
- Chunk index type is still Type 3 (Fixed Array) for fixed-size datasets
- Does NOT automatically trigger Type 2 (Implicit Index)

**Note**: HDF5 Type 2 (Implicit Index) is designed for datasets where all chunks are pre-allocated with a fill value, but h5py doesn't use this optimization by default.

## Storage Layout Properties

From h5dump inspection:

```
DATASPACE  SIMPLE { ( 30, 10 ) / ( 30, 10 ) }           # Fixed size
DATASPACE  SIMPLE { ( 30, 10 ) / ( H5S_UNLIMITED, 10 ) } # One unlimited
DATASPACE  SIMPLE { ( 30, 10 ) / ( H5S_UNLIMITED, H5S_UNLIMITED ) } # Two unlimited

STORAGE_LAYOUT {
   CHUNKED ( 5, 5 )
   SIZE 1200         # Total allocated size in bytes
}

FILLVALUE {
   FILL_TIME H5D_FILL_TIME_ALLOC   # Fill on allocation
   VALUE  H5D_FILL_VALUE_DEFAULT    # Default fill value (0.0)
}

ALLOCATION_TIME {
   H5D_ALLOC_TIME_INCR   # Incremental allocation (allocate chunks as written)
}
```

Key observations:
- `ALLOCATION_TIME` is `H5D_ALLOC_TIME_INCR` (incremental) for all chunked datasets
- Fill value is allocated only when chunks are written
- This is optimal for sparse data

## Implications for JLD2

### API Design

1. **Match h5py semantics**: Use `maxshape` with `nothing` for unlimited dimensions
2. **Automatic selection**: Same algorithm as h5py (based on unlimited dimension count)
3. **Single chunk optimization**: Detect when chunks == data size
4. **No implicit index by default**: Only use Type 2 if explicitly requested or specific conditions met

### Proposed JLD2 API

```julia
# Mirror h5py's approach
jldsave("output.jld2";
    fixed = ChunkedArray(data, chunks=(5, 5)),  # Type 3
    extensible = ChunkedArray(data, chunks=(5, 5), maxshape=(nothing, 10)),  # Type 4
    btree = ChunkedArray(data, chunks=(5, 5), maxshape=(nothing, nothing))  # Type 5
)
```

### When to Use Type 2 (Implicit Index)?

Since h5py doesn't use it, we could consider it for:
1. **Explicit request**: User specifies `indexing=:implicit_index`
2. **Pre-allocated sparse data**: When `fill_value` is specified and data is known to be sparse
3. **Optimization opportunity**: JLD2-specific optimization when beneficial

### Auto Chunking Strategy

For `chunks=:auto`, consider:
1. **Target chunk size**: ~32KB (good balance for I/O)
2. **Access pattern hints**: Row-major for Julia's column-major arrays
3. **Dimension-aware**: Avoid chunks larger than dimensions
4. **Type size**: Adjust for element size (fewer elements for large types)

Example heuristic:
```julia
function auto_chunk_size(data_size, element_size)
    target_bytes = 32 * 1024  # 32KB target
    total_elements = prod(data_size)
    target_elements = target_bytes ÷ element_size

    if total_elements <= target_elements
        return data_size  # Single chunk
    end

    # Divide dimensions to get close to target
    # ... balanced division logic ...
end
```

## Compatibility Notes

### Reading h5py Files with JLD2

✅ All test files created by h5py can be read by JLD2:
- Fixed Array (Type 3): ✓ Implemented
- Extensible Array (Type 4): ✓ Implemented
- V2 B-tree (Type 5): ✓ Implemented

### Writing JLD2 Files for h5py

To ensure h5py compatibility:
- Use same chunk index type selection logic
- Write HDF5 v4 data layout (version 3)
- Include proper dataspace messages with maxshape
- Set `H5D_ALLOC_TIME_INCR` for allocation timing
- Include fill value messages

## Test Files Created

- `h5py_single_chunk.h5` - Type 1 (chunks == data size)
- `h5py_fixed_array.h5` - Type 3 (no unlimited dims)
- `h5py_extensible.h5` - Type 4 (one unlimited dim)
- `h5py_v2btree.h5` - Type 5 (two unlimited dims)
- `h5py_compressed.h5` - Type 3 + gzip compression
- `h5py_fillvalue.h5` - Type 3 + custom fill value
- `h5py_chunks_varied.h5` - Various chunk size configurations
- `h5py_auto_chunks.h5` - Auto-chunking example

## Recommendations for JLD2

1. **Phase Priority**:
   - Phase 1: Single Chunk (Type 1) - Simple, good for small arrays
   - Phase 2: Fixed Array (Type 3) - Most common case, matches h5py default
   - Phase 3: Skip or low priority for Implicit Index (Type 2) - Rarely used
   - Phase 4: Extensible Array (Type 4) - Important for time series
   - Phase 5: V2 B-tree (Type 5) - Most flexible, needed for multi-dimensional growth

2. **Default Behavior**: Match h5py's automatic selection for interoperability

3. **Advanced Features**: Allow manual override for power users who understand the trade-offs

4. **Documentation**: Clearly explain chunk index type selection to users

## References

- HDF5 1.10+ specification, Section VII (Data Storage Formats)
- h5py documentation: https://docs.h5py.org/en/stable/high/dataset.html#chunked-storage
- Test scripts: `test_h5py_chunking.py`, `decode_chunk_index_types.jl`
