#!/usr/bin/env python3
"""
Create HDF5 test files with DataLayout Version 4 (modern chunk indexing).

To force DataLayout v4 with the modern indexing types, we need to:
1. Use libver='v110' or libver='latest' to enable HDF5 1.10+ features
2. Create datasets with specific properties that trigger each indexing type

DataLayout v4 indexing types:
1. Single Chunk (type 1) - Dataset with only one chunk
2. Implicit (type 2) - Rarely used, may require special allocation
3. Fixed Array (type 3) - Fixed dimensions, many chunks
4. Extensible Array (type 4) - One unlimited dimension
5. V2 B-tree (type 5) - Multiple unlimited dimensions
"""

import h5py
import numpy as np

def create_v4_single_chunk():
    """Single Chunk Index with DataLayout v4"""
    print("Creating DataLayout v4 Single Chunk Index...")

    # Use latest library version to get v4 features
    with h5py.File('test_v4_single_chunk.h5', 'w', libver='v110') as f:
        data = np.arange(20).reshape(4, 5).astype('f')

        # Single chunk = dataset size
        f.create_dataset('v4_single', data=data, chunks=(4, 5))

        print(f"  Created: shape=(4, 5), chunks=(4, 5), libver='v110'")

def create_v4_fixed_array():
    """Fixed Array Index with DataLayout v4"""
    print("Creating DataLayout v4 Fixed Array Index...")

    with h5py.File('test_v4_fixed_array.h5', 'w', libver='v110') as f:
        # Fixed dimensions, many chunks
        data = np.arange(300).reshape(30, 10).astype('f')

        # Create with fixed maxshape
        f.create_dataset('v4_fixed',
                        data=data,
                        chunks=(3, 2),
                        maxshape=(30, 10))  # Fixed, not unlimited

        print(f"  Created: shape=(30, 10), maxshape=(30, 10), chunks=(3, 2)")
        print(f"  Number of chunks: 50 (10x5)")

def create_v4_extensible_array():
    """Extensible Array Index with DataLayout v4"""
    print("Creating DataLayout v4 Extensible Array Index...")

    with h5py.File('test_v4_extensible_array.h5', 'w', libver='v110') as f:
        initial_data = np.arange(200).reshape(20, 10).astype('f')

        dset = f.create_dataset('v4_extensible',
                               data=initial_data,
                               chunks=(5, 5),
                               maxshape=(None, 10))  # One unlimited

        # Extend to create more chunks
        dset.resize((30, 10))
        dset[20:30, :] = np.arange(200, 300).reshape(10, 10)

        print(f"  Created: shape=(30, 10), maxshape=(None, 10), chunks=(5, 5)")
        print(f"  Number of chunks after extension: 12 (6x2)")

def create_v4_v2btree():
    """Version 2 B-tree Index with DataLayout v4"""
    print("Creating DataLayout v4 with V2 B-tree Index...")

    with h5py.File('test_v4_v2btree.h5', 'w', libver='v110') as f:
        initial_data = np.arange(200).reshape(20, 10).astype('f')

        dset = f.create_dataset('v4_v2btree',
                               data=initial_data,
                               chunks=(5, 5),
                               maxshape=(None, None))  # Both unlimited

        # Extend in both dimensions
        dset.resize((25, 15))
        dset[20:25, :] = np.arange(200, 275).reshape(5, 15)
        dset[:20, 10:15] = np.arange(275, 375).reshape(20, 5)

        print(f"  Created: shape=(25, 15), maxshape=(None, None), chunks=(5, 5)")
        print(f"  Number of chunks: 15 (5x3)")

def create_v4_filtered_single():
    """Filtered Single Chunk with DataLayout v4"""
    print("Creating DataLayout v4 Filtered Single Chunk...")

    with h5py.File('test_v4_filtered_single.h5', 'w', libver='v110') as f:
        data = np.arange(100).reshape(10, 10).astype('f')

        f.create_dataset('v4_filtered',
                        data=data,
                        chunks=(10, 10),  # Single chunk
                        compression='gzip',
                        compression_opts=6)

        print(f"  Created: shape=(10, 10), chunks=(10, 10), compression=gzip")

def create_latest_libver_comparison():
    """Create files with libver='latest' for comparison"""
    print("Creating files with libver='latest' for comparison...")

    with h5py.File('test_latest_fixed.h5', 'w', libver='latest') as f:
        data = np.arange(300).reshape(30, 10).astype('f')
        f.create_dataset('latest_fixed', data=data, chunks=(3, 2))
        print(f"  Created latest_fixed: shape=(30, 10), chunks=(3, 2)")

    with h5py.File('test_latest_extensible.h5', 'w', libver='latest') as f:
        data = np.arange(200).reshape(20, 10).astype('f')
        dset = f.create_dataset('latest_ext', data=data, chunks=(5, 5),
                                maxshape=(None, 10))
        dset.resize((30, 10))
        dset[20:, :] = np.arange(200, 300).reshape(10, 10)
        print(f"  Created latest_ext: shape=(30, 10), maxshape=(None, 10)")

def main():
    print("=" * 70)
    print("Creating HDF5 Test Files with DataLayout Version 4")
    print("=" * 70)
    print()

    files = []

    files.append(create_v4_single_chunk())
    print()

    files.append(create_v4_fixed_array())
    print()

    files.append(create_v4_extensible_array())
    print()

    files.append(create_v4_v2btree())
    print()

    files.append(create_v4_filtered_single())
    print()

    files.append(create_latest_libver_comparison())
    print()

    print("=" * 70)
    print("Files created with libver='v110' and libver='latest'")
    print("=" * 70)
    print("\nUse h5dump -p -H to inspect DataLayout version")
    print()

if __name__ == '__main__':
    main()
