#!/usr/bin/env python3
"""
Test script to explore h5py chunked dataset creation API.
Creates test files with different chunking configurations.
"""

import h5py
import numpy as np
import sys

def print_dataset_info(filename, dataset_name):
    """Print detailed information about a dataset."""
    with h5py.File(filename, 'r') as f:
        ds = f[dataset_name]
        print(f"\n{filename} - '{dataset_name}':")
        print(f"  Shape: {ds.shape}")
        print(f"  Dtype: {ds.dtype}")
        print(f"  Chunks: {ds.chunks}")
        print(f"  Maxshape: {ds.maxshape}")
        print(f"  Compression: {ds.compression}")
        if ds.compression:
            print(f"  Compression opts: {ds.compression_opts}")
        print(f"  Fill value: {ds.fillvalue}")

# Create test data
data = np.arange(300).reshape(30, 10).astype('f4')
print(f"Test data shape: {data.shape}, dtype: {data.dtype}")

# Test 1: Single chunk (chunk size equals array size)
print("\n" + "="*70)
print("Test 1: Single Chunk")
print("="*70)
with h5py.File('h5py_single_chunk.h5', 'w') as f:
    f.create_dataset('single', data=data, chunks=data.shape)
print_dataset_info('h5py_single_chunk.h5', 'single')

# Test 2: Fixed array (default chunked, no maxshape)
print("\n" + "="*70)
print("Test 2: Fixed Array (default chunking)")
print("="*70)
with h5py.File('h5py_fixed_array.h5', 'w') as f:
    f.create_dataset('fixed', data=data, chunks=(5, 5))
print_dataset_info('h5py_fixed_array.h5', 'fixed')

# Test 3: Extensible array (one unlimited dimension)
print("\n" + "="*70)
print("Test 3: Extensible Array (one unlimited dimension)")
print("="*70)
with h5py.File('h5py_extensible.h5', 'w') as f:
    f.create_dataset('ext', data=data, chunks=(5, 5), maxshape=(None, 10))
print_dataset_info('h5py_extensible.h5', 'ext')

# Test 4: V2 B-tree (multiple unlimited dimensions)
print("\n" + "="*70)
print("Test 4: V2 B-tree (multiple unlimited dimensions)")
print("="*70)
with h5py.File('h5py_v2btree.h5', 'w') as f:
    f.create_dataset('btree', data=data, chunks=(5, 5), maxshape=(None, None))
print_dataset_info('h5py_v2btree.h5', 'btree')

# Test 5: Compressed (with gzip)
print("\n" + "="*70)
print("Test 5: Compressed (gzip)")
print("="*70)
with h5py.File('h5py_compressed.h5', 'w') as f:
    f.create_dataset('comp', data=data, chunks=(5, 5), compression='gzip', compression_opts=6)
print_dataset_info('h5py_compressed.h5', 'comp')

# Test 6: With fill value
print("\n" + "="*70)
print("Test 6: With Fill Value")
print("="*70)
with h5py.File('h5py_fillvalue.h5', 'w') as f:
    f.create_dataset('filled', data=data, chunks=(5, 5), fillvalue=-999.0)
print_dataset_info('h5py_fillvalue.h5', 'filled')

# Test 7: Different chunk sizes
print("\n" + "="*70)
print("Test 7: Different Chunk Sizes")
print("="*70)
with h5py.File('h5py_chunks_varied.h5', 'w') as f:
    # Row-oriented chunks
    f.create_dataset('row_chunks', data=data, chunks=(1, 10))
    # Column-oriented chunks
    f.create_dataset('col_chunks', data=data, chunks=(30, 1))
    # Balanced chunks
    f.create_dataset('balanced', data=data, chunks=(10, 5))

print_dataset_info('h5py_chunks_varied.h5', 'row_chunks')
print_dataset_info('h5py_chunks_varied.h5', 'col_chunks')
print_dataset_info('h5py_chunks_varied.h5', 'balanced')

# Test 8: Auto chunking
print("\n" + "="*70)
print("Test 8: Auto Chunking (chunks=True)")
print("="*70)
with h5py.File('h5py_auto_chunks.h5', 'w') as f:
    f.create_dataset('auto', data=data, chunks=True)
print_dataset_info('h5py_auto_chunks.h5', 'auto')

print("\n" + "="*70)
print("All test files created successfully!")
print("="*70)

# Print summary
print("\nSummary of chunk index types (to be verified with h5debug):")
print("  Single chunk:  Expected index type 1 (single chunk)")
print("  Fixed array:   Expected index type 3 (fixed array)")
print("  Extensible:    Expected index type 4 (extensible array)")
print("  V2 B-tree:     Expected index type 5 (v2 b-tree)")
print("\nUse h5ls to find dataset offsets:")
print("  h5ls -r -v --address h5py_fixed_array.h5")
print("\nThen inspect with h5debug:")
print("  h5debug h5py_fixed_array.h5 <offset>")
