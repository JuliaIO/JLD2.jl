# JLD2.jl Development Container

This directory contains the development container configuration for GitHub Codespaces and VS Code Remote Containers.

## What's Included

### Julia Environment
- **juliaup**: Julia version manager
- **Julia 1.10 (LTS)**: Long-term support version
- **Julia 1.12 (release)**: Latest stable release
- Default channel set to 1.12

### Development Tools
- **hexdump**: Binary file analysis tool
- **HDF5 CLI tools**: Complete suite of HDF5 utilities
  - `h5dump`: Displays HDF5 file contents
  - `h5debug`: HDF5 debugging tool
  - `h5stat`: HDF5 file statistics
  - `h5ls`: List HDF5 file contents
  - `h5diff`: Compare HDF5 files
  - Additional HDF5 utilities

### VS Code Extensions
- Julia Language Support

## Usage

### GitHub Codespaces
When you create a new Codespace for this repository, the environment will be automatically configured with all the required tools.

### VS Code Remote Containers
1. Install the "Remote - Containers" extension in VS Code
2. Open the repository in VS Code
3. Click "Reopen in Container" when prompted (or use Command Palette: "Remote-Containers: Reopen in Container")

## What Happens During Setup

1. Base Ubuntu 22.04 container is created
2. juliaup and Julia versions are installed via devcontainer feature
3. The `setup.sh` script runs to:
   - Install hexdump utility
   - Install HDF5 CLI tools
   - Verify all installations
   - Install Julia package dependencies

## Switching Julia Versions

Use juliaup to switch between installed versions:

```bash
# List available versions
juliaup list

# Switch to Julia 1.10 (LTS)
juliaup default 1.10

# Switch to Julia 1.12 (release)
juliaup default 1.12
```

## Verifying Installations

All tools are verified during container creation. You can manually verify by running:

```bash
# Check Julia
julia --version

# Check hexdump
hexdump --help

# Check HDF5 tools
h5dump --version
which h5stat h5debug h5ls h5diff
```
