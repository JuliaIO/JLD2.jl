"""
Create test file with Extensible Array index (type 4)
Used for datasets with ONE unlimited dimension
"""
import h5py
import numpy as np

# Create file with v1.10+ format (required for v4 layouts)
f = h5py.File('test_v4_extensible.h5', 'w', libver='v110')

# Create dataset with ONE unlimited dimension (triggers Extensible Array)
# Sequential data for easy validation: 0, 1, 2, ..., 299
data = np.arange(300, dtype='float32').reshape(30, 10)

# maxshape=(None, 10) means first dimension is unlimited
dataset = f.create_dataset('extensible', data=data, chunks=(3, 2), maxshape=(None, 10))

print("Created test file: test_v4_extensible.h5")
print(f"Dataset shape: {data.shape}")
print(f"Chunk shape: {dataset.chunks}")
print(f"Max shape: {dataset.maxshape}")
print(f"Sum: {data.sum()}")
print(f"First elements: {data.ravel()[:10]}")

f.close()
