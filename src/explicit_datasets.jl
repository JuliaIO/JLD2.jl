#=
# Explicit Dataset API

This module provides low-level, explicit control over JLD2 dataset creation, configuration,
and access. Unlike the high-level `jldsave`/`load` interface, the explicit dataset API
allows fine-grained control over compression, chunking, attributes, and other HDF5 features.

## Key Concepts

**Dataset**: A container for data with associated metadata including datatype, dataspace,
layout, attributes, and compression filters. Datasets are the primary storage units in JLD2 files.

**Two-Phase Writing**: Datasets are created first with `create_dataset`, configured with
desired options, then written with `write_dataset`. This allows setting compression and
attributes before data is written.

**Metadata Access**: Use `get_dataset` to access dataset metadata without reading data.
This is efficient for inspecting file contents, attributes, or dataset properties.

## Main Functions

- [`create_dataset`](@ref): Create a new dataset specification
- [`write_dataset`](@ref): Write data using the dataset specification
- [`read_dataset`](@ref): Read data from a written dataset
- [`get_dataset`](@ref): Retrieve dataset metadata without reading data
- [`add_attribute`](@ref): Add metadata attributes to datasets
- [`attributes`](@ref): Retrieve all attributes from a dataset

## Advanced Features

- **Memory Mapping**: Use [`ismmappable`](@ref) and [`readmmap`](@ref) for efficient access to large arrays
- **Array Indexing**: Datasets supporting arrays can be indexed like regular Julia arrays
- **Compression**: Configure compression with the `filters` field before writing
- **Attributes**: Add arbitrary metadata as key-value pairs

## Examples

See individual function documentation for detailed examples.

**Quick Start**:
```julia
jldopen("data.jld2", "w") do f
    # Create dataset with compression
    dset = JLD2.create_dataset(f, "my_data")
    dset.filters = Deflate()
    JLD2.add_attribute(dset, "description", "Compressed array data")
    JLD2.write_dataset(dset, large_array)
end

# Read back with metadata inspection
jldopen("data.jld2", "r") do f
    dset = JLD2.get_dataset(f, "my_data")
    display(dset)  # Show comprehensive metadata
    data = JLD2.read_dataset(dset)
end
```
=#

"""
    Dataset

A mutable struct representing an HDF5/JLD2 dataset with explicit control over metadata.

The `Dataset` type allows low-level access to dataset metadata and provides fine-grained
control over how data is stored, including compression, chunking, and layout options.
This is useful for advanced use cases where you need more control than the standard
`jldsave`/`load` interface provides.

# Fields
- `parent::Group`: The containing group or file
- `name::String`: Dataset name within the parent group
- `offset::RelOffset`: File offset where dataset header is stored (UNDEFINED_ADDRESS if unwritten)
- `datatype`: HDF5 datatype specification for the dataset
- `dataspace`: Dataspace describing the dataset dimensions
- `layout`: Data layout specification (contiguous, compact, or chunked)
- `attributes::OrderedDict{String, Any}`: Dataset attributes as key-value pairs
- `chunk`: Chunking specification (for chunked layouts)
- `filters`: Filter pipeline for compression/transformation
- `header_chunk_info`: Internal metadata for header management

# Usage

Datasets are typically created using [`create_dataset`](@ref), written with [`write_dataset`](@ref),
and read with [`read_dataset`](@ref) or retrieved with [`get_dataset`](@ref).

# Example
```julia
jldopen("data.jld2", "w") do f
    # Create a dataset with compression
    dset = JLD2.create_dataset(f, "compressed_data")
    dset.filters = Deflate()
    JLD2.write_dataset(dset, rand(1000, 1000))

    # Add attributes
    JLD2.add_attribute(dset, "description", "Random data with compression")
end
```

See also: [`create_dataset`](@ref), [`write_dataset`](@ref), [`read_dataset`](@ref), [`get_dataset`](@ref)
"""
mutable struct Dataset
    parent::Group #param..
    name::String
    offset::RelOffset
    datatype
    dataspace
    layout
    attributes::OrderedDict{String, Any}
    chunk
    filters#::Vector{Filter}
    #external
    header_chunk_info # chunk_start, chunk_end, next_msg_offset
end


"""
    create_dataset(parent, path, datatype=nothing, dataspace=nothing; layout=nothing, chunk=nothing, filters=FilterPipeline())

Create a new [`Dataset`](@ref) object with specified metadata, ready for writing data.

This function creates a dataset specification but does not write any data to the file.
The dataset must be written using [`write_dataset`](@ref) to actually store data.
This two-step process allows you to configure compression, attributes, and other
metadata before writing.

# Arguments
- `parent::Union{JLDFile, Group}`: The containing file or group for the new dataset
- `path::Union{String, Nothing}`: Path to the dataset relative to `parent`.
  If `nothing`, creates an unnamed dataset
- `datatype`: HDF5 datatype specification. If `nothing`, will be inferred from data during writing
- `dataspace`: Dataspace describing dimensions. If `nothing`, will be inferred from data during writing

# Keyword Arguments
- `layout`: Data layout specification (`LcContiguous`, `LcCompact`, or `LcChunked`)
- `chunk`: Chunking specification for chunked layouts
- `filters::FilterPipeline`: Compression/transformation pipeline (default: no compression)

# Returns
- `Dataset`: A mutable dataset object ready for configuration and writing

# Examples

## Basic Usage
```julia
jldopen("data.jld2", "w") do f
    dset = JLD2.create_dataset(f, "my_data")
    JLD2.write_dataset(dset, [1, 2, 3, 4, 5])
end
```

## With Compression
```julia
jldopen("data.jld2", "w") do f
    dset = JLD2.create_dataset(f, "compressed_array")
    dset.filters = Deflate()  # Add gzip compression
    JLD2.write_dataset(dset, rand(10000))
end
```

## With Attributes
```julia
jldopen("data.jld2", "w") do f
    dset = JLD2.create_dataset(f, "experiment_results")
    JLD2.add_attribute(dset, "experiment_id", "exp_001")
    JLD2.add_attribute(dset, "date", "2024-01-15")
    JLD2.add_attribute(dset, "temperature", 23.5)
    JLD2.write_dataset(dset, measurement_data)
end
```

## Unnamed Dataset
```julia
jldopen("data.jld2", "w") do f
    dset = JLD2.create_dataset(f, nothing)  # unnamed
    JLD2.write_dataset(dset, temporary_data)
end
```

# Notes
- The dataset is not written to the file until [`write_dataset`](@ref) is called
- Datatype and dataspace are usually inferred automatically from the data
- Compression filters can be added after creation but before writing
- Attributes can be added before or after writing (see [`add_attribute`](@ref))

See also: [`write_dataset`](@ref), [`Dataset`](@ref), [`add_attribute`](@ref), [`Deflate`](@ref)
"""
create_dataset(f::JLDFile, args...; kwargs...) = create_dataset(f.root_group, args...; kwargs...)
function create_dataset(
    g::Group,
    path::Union{Nothing,String},
    datatype=nothing,
    dataspace=nothing;
    layout = nothing,
    chunk=nothing,
    filters=FilterPipeline(),
)
    if !isnothing(path)
        (parent, name) = pathize(g, path, true)
    else
        name = ""
        parent = g
    end

    return Dataset(parent, name, UNDEFINED_ADDRESS, datatype, dataspace,
            layout, OrderedDict{String,Any}(), chunk, filters, nothing)
end

iswritten(dset::Dataset) = (dset.offset != UNDEFINED_ADDRESS)

function Base.show(io::IO, ::MIME"text/plain", dset::Dataset)
    f = dset.parent.f
    print(io, "┌─ Dataset:")
    print(io, isempty(dset.name) ? " (unnamed)" : " \"$(dset.name)\"")
    print(io, iswritten(dset) ? " at $(dset.offset)" : " (unwritten)", "\n")
    prefix = "│  "
    #println(io, prefix*"parent: $(dset.parent)")
    if !isnothing(dset.datatype)
        dt = dset.datatype
        iscommitted = dt isa SharedDatatype && haskey(f.datatype_locations, dt.header_offset)
        print(io, prefix*"datatype: $(typeof(dt))", iscommitted ? " (committed)\n" : "\n")
        iscommitted && println(io, prefix*"\tcommitted at: $(dt.header_offset)")
        rr = jltype(dset.parent.f, dt)
        jt = julia_repr(rr)
        println(io, prefix*"\twritten structure: $jt")
        if iscommitted
            juliatype, writtentype, fields = stringify_committed_datatype(f, f.datatype_locations[dt.header_offset], showfields=true)
            println(io, prefix*"\ttype name: $(juliatype)")
            if !isempty(writtentype)
                println(io, prefix*"\twritten type name: $(writtentype)")
            end
            for field in fields
                println(io, prefix*"\t\t$(field)")
            end
        end
    end
    if !isnothing(dset.dataspace)
        ds = dset.dataspace
        if ds isa HmWrap{HmDataspace}#Hmessage
            println(io, prefix*"dataspace:")
            spacetype = Dict(
                0x00=>"Scalar",
                0x01=>"Simple",
                0x02=>"Null",
                0xff=>"V1")[ds.dataspace_type]
            println(io, prefix*"\ttype: $(spacetype)")
            println(io, prefix*"\tdimensions: $(ds.dimensions)")
        else
            println(io, prefix*"dataspace: $(dset.dataspace)")
        end
    end
    if !isnothing(dset.layout)
        layout = dset.layout
        if layout isa HmWrap{HmDataLayout}
            println(io, prefix*"layout:")
            println(io, prefix*"\tclass: $(layout.layout_class)")
        else
            println(io, prefix*"layout: $(dset.layout)")
        end
    end
    if !isnothing(dset.filters) && !isempty(dset.filters.filters)
        println(io, prefix*"filters: $(dset.filters)")
    end
    if !isempty(dset.attributes)
        println(io, prefix*"Attributes:")
        for (k, attr) in pairs(dset.attributes)
            if attr isa ReadAttribute
                data = read_attr_data(dset.parent.f, attr)
                println(io, prefix*"\t$(attr.name) = ",
                    data isa String ? "\"$data\"" : data)
            else
                println(io, prefix*"\t$(k) = $(attr)")
            end
        end
    end
    println(io, "└─")
end

"""
    write_dataset(dataset::Dataset, data, wsession::JLDWriteSession=JLDWriteSession())

Write data to file using the metadata and configuration stored in the [`Dataset`](@ref) object.

This function performs the actual data writing operation after a dataset has been created
with [`create_dataset`](@ref). The dataset must not have been written before (each dataset
can only be written once). The data type and dataspace will be automatically inferred
from the provided data if they weren't specified during dataset creation.

# Arguments
- `dataset::Dataset`: The dataset object created with [`create_dataset`](@ref)
- `data`: The data to write. Can be any Julia object that JLD2 can serialize
- `wsession::JLDWriteSession`: Optional write session for advanced usage (default: new session)

# Returns
- `RelOffset`: The file offset where the dataset was written

# Throws
- `ArgumentError`: If the dataset has already been written to file

# Examples

## Basic Usage
```julia
jldopen("output.jld2", "w") do f
    dset = JLD2.create_dataset(f, "my_array")
    JLD2.write_dataset(dset, [1, 2, 3, 4, 5])
end
```

## With Compression
```julia
jldopen("output.jld2", "w") do f
    dset = JLD2.create_dataset(f, "large_data")
    dset.filters = Deflate()  # Add gzip compression

    large_array = rand(Float64, 10000, 10000)
    JLD2.write_dataset(dset, large_array)

    println("Compressed data written successfully")
end
```

## With Attributes and Multiple Datasets
```julia
jldopen("experiment.jld2", "w") do f
    # Dataset 1: Results with metadata
    results_dset = JLD2.create_dataset(f, "results")
    JLD2.add_attribute(results_dset, "experiment", "trial_001")
    JLD2.add_attribute(results_dset, "date", "2024-01-15")
    JLD2.write_dataset(results_dset, experimental_results)

    # Dataset 2: Parameters
    params_dset = JLD2.create_dataset(f, "parameters")
    JLD2.write_dataset(params_dset, Dict("learning_rate" => 0.01, "epochs" => 100))
end
```

# Notes
- Each dataset can only be written once. Attempting to write again will throw an error
- Data type and dataspace are automatically inferred if not provided during creation
- Compression filters must be set before writing
- Attributes can be added before or after writing
- The write operation is atomic - either the entire dataset is written or an error is thrown

See also: [`create_dataset`](@ref), [`Dataset`](@ref), [`read_dataset`](@ref), [`add_attribute`](@ref)
"""
function write_dataset(dataset::Dataset, data, wsession::JLDWriteSession=JLDWriteSession())
    f = dataset.parent.f
    if dataset.offset != UNDEFINED_ADDRESS
        throw(ArgumentError("Dataset has already been written to file"))
    end
    # first need to figure out if data type and dataspace are defined / correct
    if isnothing(dataset.datatype)
        dataset.datatype = h5type(f, data)
    end
    datatype = dataset.datatype
    odr = objodr(data)
    if isnothing(dataset.dataspace)
        dataset.dataspace = WriteDataspace(f, data, odr)
    end
    dataspace = dataset.dataspace
    compressor = Filters.normalize_filters(dataset.filters)
    offset = write_dataset(f, dataspace, datatype, odr, data, wsession, compressor)
    !isempty(dataset.name) && (dataset.parent[dataset.name] = offset)
    # Attributes
    attrs = map(collect(keys(pairs(dataset.attributes)))) do name
       WrittenAttribute(f, name, dataset.attributes[name])
    end
    dataset = get_dataset(f, offset, dataset.parent, dataset.name)
    dataset.header_chunk_info =
        attach_message(f, dataset.offset, attrs, wsession;
            chunk_start=dataset.header_chunk_info[1],
            chunk_end=dataset.header_chunk_info[2],
            next_msg_offset=dataset.header_chunk_info[3],
            )

    return offset
end

"""
    read_dataset(dset::Dataset)

Read and return the data stored in a [`Dataset`](@ref).

This function reads the complete dataset from the file and reconstructs the original
Julia object. The dataset must have been previously written to the file using
[`write_dataset`](@ref). For large arrays, consider using [`readmmap`](@ref) for
memory-mapped access if supported.

# Arguments
- `dset::Dataset`: The dataset object, typically obtained from [`get_dataset`](@ref)

# Returns
- The reconstructed Julia object that was originally stored in the dataset

# Examples

## Basic Reading
```julia
jldopen("data.jld2", "r") do f
    dset = JLD2.get_dataset(f, "my_data")
    data = JLD2.read_dataset(dset)
    println("Read data: ", data)
end
```

## Reading with Type Information
```julia
jldopen("data.jld2", "r") do f
    dset = JLD2.get_dataset(f, "matrix_data")

    # Inspect the dataset before reading
    println("Dataset info:")
    display(dset)  # Shows detailed dataset metadata

    # Read the actual data
    matrix = JLD2.read_dataset(dset)
    println("Matrix size: ", size(matrix))
end
```

## Reading Multiple Datasets
```julia
jldopen("experiment.jld2", "r") do f
    # Read experiment results
    results_dset = JLD2.get_dataset(f, "results")
    results = JLD2.read_dataset(results_dset)

    # Read parameters
    params_dset = JLD2.get_dataset(f, "parameters")
    params = JLD2.read_dataset(params_dset)

    println("Results: ", results)
    println("Parameters: ", params)
end
```

## Alternative: Direct Array Access
```julia
jldopen("data.jld2", "r") do f
    dset = JLD2.get_dataset(f, "large_array")

    # For arrays, you can also use indexing
    first_element = dset[1]           # Read single element
    subarray = dset[1:10, 1:5]      # Read subarray
    full_array = dset[]              # Read entire array (same as read_dataset)
end
```

# Notes
- The dataset must have been written to the file before it can be read
- All data is loaded into memory; for large arrays consider [`readmmap`](@ref)
- Compressed datasets are automatically decompressed during reading
- Custom Julia types are automatically reconstructed if the type definitions are available

See also: [`get_dataset`](@ref), [`readmmap`](@ref), [`Dataset`](@ref), [`write_dataset`](@ref)
"""
function read_dataset(dset::Dataset)
    f = dset.parent.f
    read_data(f,
        ReadDataspace(f, dset.dataspace),
        dset.datatype,
        DataLayout(f, dset.layout),
        isnothing(dset.filters) ? FilterPipeline() : dset.filters,
        dset.offset,
        collect(ReadAttribute, values(dset.attributes)))
end

"""
    get_dataset(parent::Union{JLDFile, Group}, name::String)

Retrieve a [`Dataset`](@ref) object from a file without reading the actual data.

This function loads the dataset metadata (datatype, dataspace, attributes, etc.)
but does not read the actual data values. This is useful for inspecting dataset
properties, accessing attributes, or preparing for selective data reading.
The returned `Dataset` object can be used with [`read_dataset`](@ref), [`readmmap`](@ref),
or array indexing operations.

# Arguments
- `parent::Union{JLDFile, Group}`: The file or group containing the dataset
- `name::String`: Name or path of the dataset relative to the parent

# Returns
- `Dataset`: A dataset object containing metadata and providing access to the data

# Throws
- `KeyError`: If no dataset with the specified name exists

# Examples

## Basic Usage
```julia
jldopen("data.jld2", "r") do f
    dset = JLD2.get_dataset(f, "my_array")

    # Inspect the dataset without reading data
    println("Dataset: ", dset.name)
    println("Datatype: ", typeof(dset.datatype))

    # Read the actual data
    data = JLD2.read_dataset(dset)
end
```

## Inspecting Dataset Metadata
```julia
jldopen("experiment.jld2", "r") do f
    dset = JLD2.get_dataset(f, "results")

    # Display comprehensive dataset information
    display(dset)  # Shows datatype, dataspace, layout, attributes, etc.

    # Access specific metadata
    attrs = JLD2.attributes(dset)
    println("Attributes: ", attrs)

    # Check if dataset supports memory mapping
    if JLD2.ismmappable(dset)
        println("Dataset can be memory-mapped")
        mmap_data = JLD2.readmmap(dset)
    else
        println("Dataset requires full loading")
        data = JLD2.read_dataset(dset)
    end
end
```

## Working with Nested Groups
```julia
jldopen("structured_data.jld2", "r") do f
    # Access dataset in nested group
    dset = JLD2.get_dataset(f, "experiments/trial_001/results")

    # Or navigate step by step
    exp_group = f["experiments"]
    trial_group = exp_group["trial_001"]
    dset = JLD2.get_dataset(trial_group, "results")
end
```

## Array Access Patterns
```julia
jldopen("large_array.jld2", "r") do f
    dset = JLD2.get_dataset(f, "big_matrix")

    # Different ways to access the data
    full_data = dset[]                    # Read all data
    single_value = dset[1, 1]            # Read single element
    row = dset[1, :]                     # Read first row
    submatrix = dset[1:10, 1:10]        # Read submatrix

    # Equivalent to read_dataset for full data
    same_data = JLD2.read_dataset(dset)
    @assert full_data == same_data
end
```

## Inspecting File Contents
```julia
function explore_datasets(filename)
    jldopen(filename, "r") do f
        for dataset_name in keys(f)
            try
                dset = JLD2.get_dataset(f, dataset_name)
                println("Dataset: ", dataset_name)
                println("  Type: ", typeof(dset.datatype))

                attrs = JLD2.attributes(dset)
                if !isempty(attrs)
                    println("  Attributes: ", keys(attrs))
                end
                println()
            catch e
                println(dataset_name, ": Not a dataset or error: ", e)
            end
        end
    end
end
```

# Notes
- This function only loads metadata, not the actual data values
- Use [`read_dataset`](@ref) or array indexing to access the data
- The `Dataset` object provides detailed information when displayed
- Supports both absolute and relative paths within the file hierarchy
- Memory-mapped access may be available for suitable datasets (see [`ismmappable`](@ref))

See also: [`read_dataset`](@ref), [`Dataset`](@ref), [`attributes`](@ref), [`readmmap`](@ref), [`ismmappable`](@ref)
"""
get_dataset(f::JLDFile, args...; kwargs...) =
    get_dataset(f.root_group, args...; kwargs...)

function get_dataset(g::Group, name::String)
    f = g.f
    f.n_times_opened == 0 && throw(ArgumentError("file is closed"))

    (g, name) = pathize(g, name, false)
    roffset = lookup_offset(g, name)
    if roffset == UNDEFINED_ADDRESS
        if isempty(name)
            # this is a group
            return get_dataset(f, group_offset(g), g, name)
        end
        throw(KeyError(name))
    end
    get_dataset(f, roffset, g, name)
end

function get_dataset(f::JLDFile, offset::RelOffset, g=f.root_group, name="")
    dset = Dataset(g, name, offset, nothing, nothing, nothing, OrderedDict{String,Any}(), false, FilterPipeline(), nothing)

    hmitr = HeaderMessageIterator(f, offset)
    for msg in hmitr
        if msg.type == HmDataspace
            dset.dataspace = HmWrap(HmDataspace, msg)
        elseif msg.type == HmDatatype
            dset.datatype = HmWrap(HmDatatype, msg).dt
        elseif msg.type == HmDataLayout
            dset.layout = HmWrap(HmDataLayout, msg)
        elseif msg.type == HmFilterPipeline
            dset.filters = FilterPipeline(msg)
        elseif msg.type == HmAttribute
            attr = read_attribute(f, msg)
            dset.attributes[string(attr.name)] = attr
        end
    end

    dset.header_chunk_info = (;
                hmitr.chunk.chunk_start,
                hmitr.chunk.chunk_end,
                next_msg_offset = hmitr.chunk.chunk_end-CONTINUATION_MSG_SIZE)
    return dset
end

# Attributes
message_size(msg::WrittenAttribute) = jlsizeof(HeaderMessage) + jlsizeof(msg)
function write_header_message(io,f::JLDFile, msg::WrittenAttribute, wsession=JLDWriteSession())
    jlwrite(io, HeaderMessage(HmAttribute, jlsizeof(msg), 0))
    write_attribute(io, f, msg, wsession)
    return nothing
end

# Links
message_size(msg::Pair{String, RelOffset}) = jlsizeof(Val(HmLinkMessage); link_name=msg.first)
write_header_message(io, f, msg::Pair{String, RelOffset}, _=nothing) =
    write_header_message(io, Val(HmLinkMessage); link_name=msg.first, target=msg.second)

function attach_message(f::JLDFile, offset, messages, wsession=JLDWriteSession();
    chunk_start,
    chunk_end,
    next_msg_offset,
    minimum_continuation_size::Int = 0
    )
    if chunk_start == UNDEFINED_ADDRESS || chunk_end == UNDEFINED_ADDRESS ||
            next_msg_offset == UNDEFINED_ADDRESS
        throw(UnsupportedFeatureException("Not implemented. pass all info"))
    end
    next_msg_offset == -1 && throw(InternalError("next_msg_offset should not be -1"))

    io = f.io
    seek(io, next_msg_offset)
    remaining_space = chunk_end  - 20 - next_msg_offset
    while !isempty(messages)

        msg = first(messages)
        sz = message_size(msg)
        if remaining_space ≥ sz + 4 || remaining_space == sz
            pos = position(io)
            write_header_message(io, f, msg)
            rsz = position(io) - pos
            if rsz != sz
                throw(InternalError("Message size mismatch. Expected $sz, got $rsz for message $msg"))
            end
            next_msg_offset += sz
            remaining_space -= sz
            popfirst!(messages)
        else
            break
        end
    end

    if isempty(messages)
        # Managed to add all messages
        # Cleanup and return
        # Mark remaining free space with a NIL message
        empty_space = chunk_end-position(io)-4 - 20
        if empty_space != -4
            empty_space < 0 && throw(InternalError("Negative empty space. This should not happen"))
            write_header_message(io, Val(HmNil), 0, empty_space)
        end
        # continuation space
        write_continuation_placeholder(io)

        # Re-calculate checksum
        update_checksum(io, chunk_start, chunk_end)

        return nothing
    end
    if !iszero(remaining_space)
        # Mark remaining free space with a NIL message
        write_header_message(io, Val(HmNil), 0, remaining_space-4)
    end
    # If we got to here then a new continuation needs to be created
    continuation_start = f.end_of_data
    continuation_size = jlsizeof(OBJECT_HEADER_CONTINUATION_SIGNATURE)
    continuation_size += sum(message_size(msg) for msg in messages)
    continuation_size += CONTINUATION_MSG_SIZE + 4 # Checksum
    tmp = max(continuation_size, minimum_continuation_size)
    # only replace if the gap is larger than 4 bytes
    tmp - continuation_size > 4 && (continuation_size = tmp)

    # Object continuation message
    write_header_message(io, Val(HmObjectHeaderContinuation);
        continuation_offset=h5offset(f, continuation_start),
        continuation_length=Length(continuation_size))

    # Re-calculate checksum
    update_checksum(io, chunk_start, chunk_end)

    # Object continuation
    seek(io, continuation_start)
    chunk_start = continuation_start
    chunk_end = continuation_start + continuation_size -4
    cio = begin_checksum_write(io, continuation_size - 4)
    jlwrite(cio, OBJECT_HEADER_CONTINUATION_SIGNATURE)
    remaining_space = continuation_size - jlsizeof(OBJECT_HEADER_CONTINUATION_SIGNATURE) - CONTINUATION_MSG_SIZE -4
    while !isempty(messages)
        msg = popfirst!(messages)
        sz = message_size(msg)
        write_header_message(io, f, msg, wsession)
        next_msg_offset += sz
        remaining_space -= sz
    end
    if remaining_space > 0
        @assert remaining_space ≥ 4 "Gaps smaller than 4 bytes should not occur"
        write_header_message(cio, Val(HmNil), 0, remaining_space)
    end
    # Extra space for object continuation
    write_continuation_placeholder(cio)
    # Checksum
    jlwrite(io, end_checksum(cio))
    f.end_of_data = position(io)
    #g.last_chunk_checksum_offset = f.end_of_data - 4
    # TODO: last_chunk_checksum_offset is not updated
    return header_chunk_info = (
        chunk_start,
        chunk_end,
        position(io)-24)
end

"""
    add_attribute(dset::Dataset, name::String, data, wsession=JLDWriteSession())

Add an attribute with the specified name and data to a [`Dataset`](@ref).

Attributes are metadata key-value pairs associated with datasets. They can store
additional information about the data such as units, descriptions, creation dates,
or any other relevant metadata. Attributes can be added before or after the dataset
has been written to the file.

# Arguments
- `dset::Dataset`: The dataset to add the attribute to
- `name::String`: The attribute name (must be unique within the dataset)
- `data`: The attribute value (can be any JLD2-serializable Julia object)
- `wsession::JLDWriteSession`: Optional write session for advanced usage (default: new session)

# Throws
- `ArgumentError`: If an attribute with the same name already exists

# Examples

## Basic Attribute Usage
```julia
jldopen("data.jld2", "w") do f
    dset = JLD2.create_dataset(f, "experiment_data")

    # Add various types of attributes
    JLD2.add_attribute(dset, "description", "Temperature measurements")
    JLD2.add_attribute(dset, "units", "°C")
    JLD2.add_attribute(dset, "measurement_date", "2024-01-15")
    JLD2.add_attribute(dset, "sensor_id", 42)
    JLD2.add_attribute(dset, "calibrated", true)

    # Write the actual data
    JLD2.write_dataset(dset, temperature_readings)
end
```

## Complex Attribute Data
```julia
jldopen("analysis.jld2", "w") do f
    dset = JLD2.create_dataset(f, "results")

    # Attributes can store complex data
    JLD2.add_attribute(dset, "parameters", Dict(
        "learning_rate" => 0.01,
        "batch_size" => 32,
        "epochs" => 100
    ))

    JLD2.add_attribute(dset, "processing_steps", [
        "normalization",
        "feature_extraction",
        "model_training"
    ])

    JLD2.write_dataset(dset, model_results)
end
```

## Reading Attributes
```julia
jldopen("data.jld2", "r") do f
    dset = JLD2.get_dataset(f, "experiment_data")

    # Get all attributes
    attrs = JLD2.attributes(dset)
    for (attr_name, value) in attrs
        println(attr_name, ": ", value)
    end

    # Access specific attributes
    description = attrs["description"]
    units = attrs["units"]
    println("Data: ", description, " (", units, ")")
end
```

## Adding Attributes to Written Datasets
```julia
# First session: create and write dataset
jldopen("data.jld2", "w") do f
    dset = JLD2.create_dataset(f, "measurements")
    JLD2.write_dataset(dset, sensor_data)
end

# Second session: add attributes to existing dataset
jldopen("data.jld2", "a") do f  # append mode
    dset = JLD2.get_dataset(f, "measurements")
    JLD2.add_attribute(dset, "analysis_date", string(today()))
    JLD2.add_attribute(dset, "processed_by", "analysis_v2.1")
end
```

# Notes
- Attribute names must be unique within each dataset
- Attributes can be added before or after writing the dataset data
- Attribute data can be any Julia object that JLD2 can serialize
- Use [`attributes`](@ref) to retrieve all attributes from a dataset
- Existing attributes cannot be modified; the dataset must be recreated to change them

See also: [`attributes`](@ref), [`Dataset`](@ref), [`create_dataset`](@ref)
"""
function add_attribute(dset::Dataset, name::String, data, wsession=JLDWriteSession())
    f = dset.parent.f
    prewrite(f) # assert writability

    for attrname in keys(dset.attributes)
        if name == attrname
            throw(ArgumentError("Attribute \"$name\" already exists. Attribute names must be unique."))
        end
    end
    dset.attributes[name] = data
    if iswritten(dset)
        dset.header_chunk_info =
        attach_message(f, dset.offset, [WrittenAttribute(f,name,data)], wsession;
            chunk_start=dset.header_chunk_info[1],
            chunk_end=dset.header_chunk_info[2],
            next_msg_offset=dset.header_chunk_info[3],
            )
        return nothing
    end
end

"""
    attributes(dset::Dataset; plain::Bool=false)

Return the attributes of a dataset as an `OrderedDict`.
If `plain` is set to `true` then the values are returned as stored in the dataset object.
"""
function attributes(dset::Dataset; plain::Bool=false)
    plain && return dset.attributes
    OrderedDict(keys(dset.attributes) .=> map(values(dset.attributes)) do attr
        read_attr_data(dset.parent.f, attr)
    end)
end

"""
    ismmappable(dset::Dataset)

Check if a dataset can be memory-mapped. This can be useful for large arrays and for editing written arrays.

An Array dataset may be mmapped if:
    - `JLD2.samelayout(T) == true`: The element type is `isbits` and has a size that either 1, 2, 4, or a multiple of 8 bytes.
    - Uncompressed: Compressed arrays cannot be memory-mapped
    - Uses a contiguous layout: This is true for all array datasets written by JLD2 with version ≥ v0.4.52
    - Windows: The file must be opened in read-only mode. This is a limitation of Mmap on Windows.
"""
function ismmappable(dset::Dataset)
    iswritten(dset) || return false
    f = dset.parent.f
    dt = dset.datatype
    rr = jltype(f, dt)
    T = julia_repr(rr)
    !(samelayout(T)) && return false
    !isempty(dset.filters.filters) && return false
    ret = false
    if (layout = dset.layout) isa HmWrap{HmDataLayout}
        ret = (layout.layout_class == LcContiguous && layout.data_address != UNDEFINED_ADDRESS)
    end
    if ret == true && Sys.iswindows() && dset.parent.f.writable
        @warn "On Windows memory-mapping is only possible for files in read-only mode."
        ret = false
    end
    return ret
end


"""
    readmmap(dset::Dataset)

Memory-map a dataset. This can be useful for large arrays and for editing written arrays.
See [`ismmappable`](@ref) for requirements.
"""
function readmmap(dset::Dataset)
    ismmappable(dset) || throw(ArgumentError("Dataset is not mmappable"))
    f = dset.parent.f

    # figure out the element type
    dt = dset.datatype
    rr = jltype(f, dt)
    T = julia_repr(rr)
    ndims, offset = get_ndims_offset(f, ReadDataspace(f, dset.dataspace), collect(values(dset.attributes)))

    io = f.io
    seek(io, offset)
    dims = reverse([jlread(io, Int64) for i in 1:ndims])
    iobackend = io isa IOStream ? io : io.f
    seek(iobackend, DataLayout(f, dset.layout).data_offset)
    # May be pos != data_offset for files with custom offset
    pos = position(iobackend)
    if (pos % 8 == 0) ||
       (pos % 2 == 0 && sizeof(T) == 2) ||
       (pos % 4 == 0 && sizeof(T) == 4) ||
       (sizeof(T) == 1)
        # These are cases where we can directly mmap the data.
        Mmap.mmap(iobackend, Array{T,Int(ndims)}, (dims...,))
    else
        dims[1] *= sizeof(T)
        # A fallback for all other cases is to mmap the data as UInt8 and reinterpret it.
        reinterpret(T,
            Mmap.mmap(iobackend, Array{UInt8,Int(ndims)}, (dims...);)
        )
    end
end

@static if !Sys.iswindows()
"""
    allocate_early(dset::Dataset, T::DataType)

Write a dataset to file without any actual data. Reserve space according to element type and dimensions.
This may be useful in conjunction with [`readmmap`](@ref).

Note: Not available on Windows.
"""
function allocate_early(dset::Dataset, T::DataType)
    iswritten(dset) && throw(ArgumentError("Dataset has already been written to file"))
    # for this to work, require all information to be provided
    isnothing(dset.datatype) && throw(ArgumentError("datatype must be provided"))
    isnothing(dset.dataspace) && throw(ArgumentError("dataspace must be provided"))
    datatype = dset.datatype
    dataspace = dset.dataspace

    f = dset.parent.f
    attributes = map(collect(dset.attributes)) do (name, attr)
        attr isa WrittenAttribute && return attr
        return WrittenAttribute(f, name, attr)
        throw(ArgumentError("Invalid attribute: $a"))
    end
    writtenas = writeas(T)
    odr_ = _odr(writtenas, T, odr(writtenas))
    datasz = odr_sizeof(odr_)::Int * numel(dataspace)::Int
    psz = payload_size_without_storage_message(dataspace, datatype)::Int
    psz += sum(message_size.(attributes), init=0)
    # minimum extra space for continuation message
    psz += jlsizeof(HeaderMessage) + jlsizeof(RelOffset) + jlsizeof(Length)

    # Layout class: Use contiguous for now
    layout_class = LcContiguous
    psz += jlsizeof(Val(HmDataLayout); layout_class)
    fullsz = jlsizeof(ObjectStart) + size_size(psz) + psz + 4

    header_offset = f.end_of_data
    io = f.io
    seek(io, header_offset)
    f.end_of_data = header_offset + fullsz

    cio = begin_checksum_write(io, fullsz - 4)
    jlwrite(cio, ObjectStart(size_flag(psz)))
    write_size(cio, psz)
    write_header_message(cio, Val(HmFillValue); flags=0x09)
    write_header_message(cio, Val(HmDataspace); dataspace.dataspace_type, dimensions=dataspace.size)
    for attr in dataspace.attributes
        write_header_message(cio, f, attr)
    end
    write_header_message(cio, Val(HmDatatype), 1 | (2*isa(datatype, CommittedDatatype)); dt=datatype)
    for a in attributes
        write_header_message(cio, f, a, wsession)
    end
    # Align contiguous chunk to 8 bytes in the file
    address = f.end_of_data + 8 - mod1(f.end_of_data, 8)
    data_address = h5offset(f, address)
    write_header_message(cio, Val(HmDataLayout);
        layout_class, data_address, data_size=datasz)

    dset.header_chunk_info = (header_offset, position(cio)+20, position(cio))
    # Add NIL message replacable by continuation message
    write_continuation_placeholder(cio)
    jlwrite(io, end_checksum(cio))

    f.end_of_data = address + datasz
    seek(io, f.end_of_data)

    offset = h5offset(f, header_offset)
    !isempty(dset.name) && (dset.parent[dset.name] = offset)
    #dset.offset = offset

    # load current dataset as new dataset
    ddset = get_dataset(f, offset, dset.parent, dset.name)
    for field in fieldnames(Dataset)
        setproperty!(dset, field, getfield(ddset, field))
    end
    return offset
end
end

struct ArrayDataset{T, N, RR<:ReadRepresentation{T}, io} <: AbstractArray{T, N}
    f::JLDFile{io}
    dset::Dataset
    dims::NTuple{N, Int}
    data_address::Int64
    rr::RR
    writable::Bool
end
function ArrayDataset(dset::Dataset)
    isarraydataset(dset) || throw(ArgumentError("Dataset is not an array"))
    iscompressed(dset.filters) && throw(UnsupportedFeatureException("Compressed datasets are not supported."))
    f = dset.parent.f
    dt = dset.datatype
    writable = f.writable && (dset.layout.layout_class == LcContiguous)
    rr = jltype(f, !(f.plain) && dt isa SharedDatatype ? get(f.datatype_locations, dt.header_offset, dt) : dt)
    return ArrayDataset(
        f, dset,
        Int.(reverse(dset.dataspace.dimensions)),
        fileoffset(f, dset.layout.data_address),
        rr,
        writable
        )
end

function isarraydataset(dset::Dataset)
    isnothing(dset.dataspace) && return false
    ds = dset.dataspace
    if ds isa HmWrap{HmDataspace}
        return ds.dataspace_type == DS_SIMPLE || ds.dataspace_type == DS_V1
    end
    return false
end

Base.IndexStyle(::Type{<:ArrayDataset}) = IndexLinear()
Base.size(A::ArrayDataset) = A.dims
Base.getindex(dset::Dataset, I...) = ArrayDataset(dset)[I...]
Base.getindex(dset::Dataset) = read_dataset(dset)
Base.setindex!(dset::Dataset, v, i, I...) = Base.setindex!(ArrayDataset(dset), v, i, I...)

function Base.getindex(A::ArrayDataset, i::Integer)
    @boundscheck checkbounds(A, i)
    seek(A.f.io, A.data_address + (i-1)*odr_sizeof(A.rr))
    return read_scalar(A.f, A.rr, UNDEFINED_ADDRESS)
end

function Base.setindex!(A::ArrayDataset{T,N,ODR}, v, i::Integer) where {T,N,ODR}
    @boundscheck checkbounds(A, i)
    A.f.writable || throw(ArgumentError("Cannot edit in read-only mode"))
    A.writable || throw(ArgumentError("Dataset cannot be edited"))
    seek(A.f.io, A.data_address + (i-1)*odr_sizeof(A.rr))
    write_data(A.f.io, A.f, v, T, datamode(ODR), JLDWriteSession())
    return v
end
