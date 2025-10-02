#!/usr/bin/env python3
"""
Create HDF5 test files demonstrating different chunk indexing mechanisms.

This script creates HDF5 files using h5py that demonstrate all the different
chunk indexing types specified in the HDF5 format specification:

1. Single Chunk Index (type 1) - Dataset with only one chunk
2. Implicit Index (type 2) - Contiguous chunk storage
3. Fixed Array Index (type 3) - Fixed dimensions, no unlimited
4. Extensible Array Index (type 4) - One unlimited dimension
5. Version 2 B-tree Index (type 5) - Multiple unlimited dimensions
6. Version 1 B-tree Index (legacy) - Old format with DataLayout v3

Reference: HDF5 Format Specification Section IV.A.2.i (Data Layout Message)
and Appendix C (Types of Indexes for Dataset Chunks)
"""

import h5py
import numpy as np

def create_single_chunk_index():
    """
    Single Chunk Index (type 1)

    Created when the entire dataset fits in a single chunk.
    The chunk information is stored directly in the DataLayout message.
    """
    print("Creating Single Chunk Index test file...")

    with h5py.File('test_single_chunk_index.h5', 'w') as f:
        # Small dataset that fits in one chunk
        data = np.arange(20).reshape(4, 5).astype('f')

        # Chunk size equals dataset size -> single chunk
        f.create_dataset('single_chunk', data=data, chunks=(4, 5))

        print(f"  Created dataset: shape={data.shape}, chunks=(4, 5)")
        print(f"  Number of chunks: 1")

    return 'test_single_chunk_index.h5'

def create_implicit_index():
    """
    Implicit Index (type 2)

    Used when all chunks are stored contiguously and addresses can be
    computed implicitly without an index structure.

    Note: This is rare and may require specific allocation settings.
    """
    print("Creating Implicit Index test file...")

    with h5py.File('test_implicit_index.h5', 'w') as f:
        # Create a chunked dataset with early allocation
        # H5D_ALLOC_TIME_EARLY may trigger implicit indexing
        data = np.arange(100).reshape(10, 10).astype('f')

        dset = f.create_dataset('implicit_chunks', data=data, chunks=(5, 5))

        print(f"  Created dataset: shape={data.shape}, chunks=(5, 5)")
        print(f"  Number of chunks: 4 (2x2)")

    return 'test_implicit_index.h5'

def create_fixed_array_index():
    """
    Fixed Array Index (type 3)

    Used for datasets with fixed dimensions (no unlimited dimensions).
    The number of chunks is known at creation time, so a fixed-size
    array can store chunk addresses.
    """
    print("Creating Fixed Array Index test file...")

    with h5py.File('test_fixed_array_index.h5', 'w') as f:
        # Fixed dimensions (no None in maxshape)
        data = np.arange(300).reshape(30, 10).astype('f')

        # maxshape equals shape -> fixed dimensions
        f.create_dataset('fixed_array',
                        shape=(30, 10),
                        maxshape=(30, 10),  # Fixed, not unlimited
                        chunks=(3, 2),
                        dtype='f')

        # Write data
        f['fixed_array'][:] = data

        print(f"  Created dataset: shape=(30, 10), maxshape=(30, 10)")
        print(f"  Chunks: (3, 2)")
        print(f"  Number of chunks: 50 (10x5)")

    return 'test_fixed_array_index.h5'

def create_extensible_array_index():
    """
    Extensible Array Index (type 4)

    Used for datasets with exactly ONE unlimited dimension.
    Common for time-series data that grows along one axis.
    """
    print("Creating Extensible Array Index test file...")

    with h5py.File('test_extensible_array_index.h5', 'w') as f:
        # One unlimited dimension (first dimension)
        initial_data = np.arange(100).reshape(10, 10).astype('f')

        dset = f.create_dataset('extensible_array',
                               shape=(10, 10),
                               maxshape=(None, 10),  # First dim unlimited
                               chunks=(5, 5),
                               dtype='f')

        # Write initial data
        dset[:] = initial_data

        # Extend the dataset along the unlimited dimension
        dset.resize((20, 10))
        dset[10:20, :] = np.arange(100, 200).reshape(10, 10)

        print(f"  Created dataset: shape=(20, 10), maxshape=(None, 10)")
        print(f"  Chunks: (5, 5)")
        print(f"  Number of chunks after extension: 8 (4x2)")
        print(f"  Unlimited dimension: 0 (first dimension)")

    return 'test_extensible_array_index.h5'

def create_v2_btree_index():
    """
    Version 2 B-tree Index (type 5)

    Used for datasets with MULTIPLE unlimited dimensions.
    Most flexible but has more overhead than other indexing methods.
    """
    print("Creating Version 2 B-tree Index test file...")

    with h5py.File('test_v2_btree_index.h5', 'w') as f:
        # Multiple unlimited dimensions
        initial_data = np.arange(100).reshape(10, 10).astype('f')

        dset = f.create_dataset('v2_btree',
                               shape=(10, 10),
                               maxshape=(None, None),  # Both dims unlimited
                               chunks=(5, 5),
                               dtype='f')

        # Write initial data
        dset[:] = initial_data

        # Extend along both dimensions
        dset.resize((15, 20))
        dset[10:15, :] = np.arange(100, 200).reshape(5, 20)
        dset[:10, 10:20] = np.arange(200, 300).reshape(10, 10)

        print(f"  Created dataset: shape=(15, 20), maxshape=(None, None)")
        print(f"  Chunks: (5, 5)")
        print(f"  Number of chunks after extension: 12 (3x4)")
        print(f"  Unlimited dimensions: both (0 and 1)")

    return 'test_v2_btree_index.h5'

def create_v1_btree_index():
    """
    Version 1 B-tree Index (legacy)

    The legacy indexing method used in older HDF5 files.
    Uses DataLayout message version 3 with V1 B-tree structure.

    Note: Modern h5py uses version 4 DataLayout by default.
    To get V1 B-tree, we may need to use libver='earliest'.
    """
    print("Creating Version 1 B-tree Index test file (legacy)...")

    # Use earliest library version to get old-style format
    with h5py.File('test_v1_btree_index.h5', 'w', libver='earliest') as f:
        # Chunked dataset with many chunks
        data = np.arange(1200).reshape(30, 40).astype('f')

        dset = f.create_dataset('v1_btree',
                               data=data,
                               chunks=(3, 4))

        print(f"  Created dataset: shape=(30, 40)")
        print(f"  Chunks: (3, 4)")
        print(f"  Number of chunks: 100 (10x10)")
        print(f"  Using libver='earliest' to force old format")

    return 'test_v1_btree_index.h5'

def create_filtered_single_chunk():
    """
    Single Chunk Index with Filter (type 1 with compression)

    When DONT_FILTER_PARTIAL_BOUND_CHUNKS flag is set and the chunk is filtered.
    """
    print("Creating Filtered Single Chunk Index test file...")

    with h5py.File('test_filtered_single_chunk.h5', 'w') as f:
        data = np.arange(100).reshape(10, 10).astype('f')

        # Single chunk with gzip compression
        f.create_dataset('filtered_single',
                        data=data,
                        chunks=(10, 10),  # Entire dataset in one chunk
                        compression='gzip',
                        compression_opts=6)

        print(f"  Created dataset: shape=(10, 10), chunks=(10, 10)")
        print(f"  Compression: gzip level 6")
        print(f"  Number of chunks: 1 (filtered)")

    return 'test_filtered_single_chunk.h5'

def main():
    print("=" * 70)
    print("Creating HDF5 Test Files for Chunk Indexing Mechanisms")
    print("=" * 70)
    print()

    files_created = []

    # Create all test files
    files_created.append(create_single_chunk_index())
    print()

    files_created.append(create_implicit_index())
    print()

    files_created.append(create_fixed_array_index())
    print()

    files_created.append(create_extensible_array_index())
    print()

    files_created.append(create_v2_btree_index())
    print()

    files_created.append(create_v1_btree_index())
    print()

    files_created.append(create_filtered_single_chunk())
    print()

    print("=" * 70)
    print("Summary")
    print("=" * 70)
    print("\nFiles created:")
    for i, filename in enumerate(files_created, 1):
        print(f"  {i}. {filename}")

    print("\nNext steps:")
    print("  1. Inspect files with h5dump -H <filename>")
    print("  2. Debug files with h5debug <filename>")
    print("  3. Try reading with JLD2.jl")
    print()

if __name__ == '__main__':
    main()
