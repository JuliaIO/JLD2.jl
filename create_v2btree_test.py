#!/usr/bin/env python3
"""
Create HDF5 test file with v2 B-tree chunk indexing.

V2 B-tree is used when dataset has multiple unlimited dimensions.
This is different from v1 B-tree which is used for large single-unlimited datasets.
"""

import h5py
import numpy as np

# V2 B-tree requires multiple unlimited dimensions
data = np.arange(300, dtype='float32').reshape(10, 30)

print("Creating test_v2btree.h5...")
print(f"Data shape: {data.shape}")
print(f"Data sum: {data.sum()}")
print(f"First element: {data[0, 0]}")
print(f"Last element: {data[-1, -1]}")

f = h5py.File('test_v2btree.h5', 'w')

# Use multiple unlimited dimensions to trigger v2 B-tree
# This is the key difference from v1 B-tree (single unlimited)
f.create_dataset('btree', data=data, chunks=(2, 3),
                 maxshape=(None, None))  # Both dimensions unlimited

f.close()

print("\nFile created successfully!")
print("\nVerify with:")
print("  h5dump -H test_v2btree.h5 | grep -i 'chunk'")
print("  python3 -c \"import h5py; print(h5py.File('test_v2btree.h5')['btree'][:].sum())\"")
