#!/usr/bin/env python3
"""
Create HDF5 test file for Type 2: Implicit Index

Implicit Index requirements:
- Fixed maximum dimension sizes
- No filter applied to the dataset
- Early space allocation (H5D_ALLOC_TIME_EARLY)
"""
import h5py
import numpy as np

# Create file with v1.10 format to enable DataLayout v4
f = h5py.File('test_v4_implicit.h5', 'w', libver='v110')

# Create dataset with:
# - Array: 30×10 (h5py order, will be 10×30 in Julia)
# - Chunks: 3×2
# - No compression
# - Early allocation
data = np.arange(300, dtype='float32').reshape(30, 10)

# Create dataset space with early allocation
dcpl = h5py.h5p.create(h5py.h5p.DATASET_CREATE)
dcpl.set_chunk((3, 2))  # Set chunk dimensions
dcpl.set_alloc_time(h5py.h5d.ALLOC_TIME_EARLY)  # Early allocation triggers implicit index

# Create dataset manually with property list
space = h5py.h5s.create_simple((30, 10))
tid = h5py.h5t.py_create(np.dtype('float32'))
dset = h5py.h5d.create(f.id, b'implicit', tid, space, dcpl=dcpl)

# Write the data
dset.write(h5py.h5s.ALL, h5py.h5s.ALL, data)

f.close()

print("Created test_v4_implicit.h5")
print("Array shape (h5py): (30, 10)")
print("Chunk dims (h5py): (3, 2)")
print("Total chunks: 50")
print("Data range: 0.0 to 299.0")
print("\nUse h5dump -H test_v4_implicit.h5 to verify DataLayout version 4, type 2")
