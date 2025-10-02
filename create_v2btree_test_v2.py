#!/usr/bin/env python3
"""
Create HDF5 test file with v2 B-tree chunk indexing.

V2 B-tree requires HDF5 file format version 1.10+ with specific settings.
Standard h5py may default to v1 B-tree even with multiple unlimited dimensions.

According to HDF5 spec:
- v1 B-tree: Used in layout version 3
- v2 B-tree: Used in layout version 4 (chunk indexing type 5)
- v2 B-tree is for datasets with multiple unlimited dimensions

Let's try using libver='latest' to get newer format.
"""

import h5py
import numpy as np

# Try using latest library version to get v2 B-tree
data = np.arange(300, dtype='float32').reshape(10, 30)

print("Creating test_v2btree_v2.h5 with libver='latest'...")
print(f"Data shape: {data.shape}")
print(f"Data sum: {data.sum()}")

# Use latest library version to enable newer features
f = h5py.File('test_v2btree_v2.h5', 'w', libver='latest')

# Multiple unlimited dimensions should trigger v2 B-tree
f.create_dataset('btree', data=data, chunks=(2, 3),
                 maxshape=(None, None))

f.close()

print("\nFile created successfully!")
print("\nVerify with:")
print("  h5debug test_v2btree_v2.h5 800 | grep 'Index Type'")
