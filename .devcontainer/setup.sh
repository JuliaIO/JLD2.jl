#!/bin/bash
set -e

echo "Setting up JLD2.jl development environment..."

# Update package list
sudo apt-get update

# Install hexdump (part of bsdmainutils or bsdextrautils)
echo "Installing hexdump..."
sudo apt-get install -y bsdmainutils

# Install HDF5 tools (h5dump, h5debug, h5stat, etc.)
echo "Installing HDF5 CLI tools..."
sudo apt-get install -y hdf5-tools

# Verify installations
echo "Verifying installations..."
echo "Julia versions available:"
juliaup list || echo "Julia installation pending..."

echo "hexdump version:"
hexdump -V 2>&1 || hexdump --version 2>&1 || echo "hexdump installed (no version flag available)"

echo "HDF5 tools installed:"
which h5dump && h5dump --version 2>&1 | head -n 1
which h5stat && echo "h5stat: $(which h5stat)"
which h5debug && echo "h5debug: $(which h5debug)"
which h5ls && echo "h5ls: $(which h5ls)"
which h5diff && echo "h5diff: $(which h5diff)"

# Install Julia dependencies for the project
echo "Installing Julia dependencies..."
julia --project=. -e 'using Pkg; Pkg.instantiate()'

echo "Setup complete!"
