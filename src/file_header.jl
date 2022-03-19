import Pkg.Types: VersionRange

# Currently we specify a 512 byte header
const FILE_HEADER_LENGTH = 512

const FORMAT_VERSION = v"0.1.1"
# Range of file format versions that can be read
# Publish patch release relaxing upper version bound
# if the imminent major release is not breaking
const COMPATIBLE_VERSIONS = VersionRange("0.1")
const REQUIRED_FILE_HEADER = "HDF5-based Julia Data Format, version "
const FILE_HEADER = "$(REQUIRED_FILE_HEADER)$(FORMAT_VERSION)\x00 (Julia $(VERSION) $(sizeof(Int)*8)-bit $(htol(1) == 1 ? "LE" : "BE"))\x00"
@assert length(FILE_HEADER) <= FILE_HEADER_LENGTH

# Legacy File Header
const LEGACY_REQUIRED_FILE_HEADER = "Julia data file (HDF5), version 0.2.0"


function verify_file_header(f)
    io = f.io
    fname = f.path
    if f.base_address != FILE_HEADER_LENGTH
        @warn "File likely not written by JLD2. Skipping header verification."
        return
    end
    seek(io, 0)
    headermsg = String(read!(io, Vector{UInt8}(undef, length(REQUIRED_FILE_HEADER))))
    if headermsg != REQUIRED_FILE_HEADER
        if !startswith(headermsg, LEGACY_REQUIRED_FILE_HEADER)
            throw(ArgumentError(string('"', fname, "\" is not a JLD2 file")))
        else
            #@warn("This file was written with an older version of JLD2. Attempting to load data.", maxlog=1)
            return
        end
    end

    ver = VersionNumber(read_bytestring(io))
    if ver > FORMAT_VERSION
        @warn("""This file was written in a newer version of the JLD2 file format.
        Please consider updating JLD2.""", maxlog=1)
    end
    if ver ∉ COMPATIBLE_VERSIONS
        @warn("""This file was written with a different version of JLD2 that may not be compatible.
         Attempting to load data.""", maxlog=1)
    end
end

function write_file_header(f)
    io = f.io
    if f.base_address >= FILE_HEADER_LENGTH
        seek(io, f.base_address - FILE_HEADER_LENGTH)
        jlwrite(io, FILE_HEADER)
    end
    # Write superblock
    seek(io, f.base_address)
    write_superblock(io,f)
    return nothing
end