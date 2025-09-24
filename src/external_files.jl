# External File Management for JLD2
# This module handles opening, caching, and accessing external HDF5/JLD2 files

# External file handle cache
const EXTERNAL_FILE_CACHE = Dict{String, WeakRef}()
const MAX_CACHE_SIZE = 32  # Maximum number of external files to keep open

# Task-local reference chain tracking for circular reference detection
const REFERENCE_CHAIN_KEY = gensym(:external_file_reference_chain)

# Settings for enhanced error handling
const MAX_RETRY_ATTEMPTS = 3
const RETRY_DELAY_MS = 100
const NETWORK_TIMEOUT_MS = 5000

"""
    get_reference_chain() -> Vector{String}

Get the current external file reference chain for this task.
"""
function get_reference_chain()
    return get(task_local_storage(), REFERENCE_CHAIN_KEY, String[])
end

"""
    set_reference_chain(chain::Vector{String})

Set the external file reference chain for this task.
"""
function set_reference_chain(chain::Vector{String})
    task_local_storage(REFERENCE_CHAIN_KEY, chain)
end

"""
    push_reference_chain!(file_path::String)

Add a file to the reference chain and return the updated chain.
"""
function push_reference_chain!(file_path::String)
    chain = copy(get_reference_chain())
    push!(chain, normpath(abspath(file_path)))
    set_reference_chain(chain)
    return chain
end

"""
    pop_reference_chain!()

Remove the last file from the reference chain.
"""
function pop_reference_chain!()
    chain = copy(get_reference_chain())
    if !isempty(chain)
        pop!(chain)
        set_reference_chain(chain)
    end
    return chain
end

"""
    ExternalFileHandle

Represents a handle to an external file that can be shared across multiple external links.
Includes error handling for various failure conditions.
"""
mutable struct ExternalFileHandle
    file_path::String
    jld_file::Union{JLDFile, Nothing}
    last_access_time::Float64
    error_state::Union{Exception, Nothing}

    function ExternalFileHandle(file_path::String)
        new(file_path, nothing, time(), nothing)
    end
end

"""
    get_external_file(current_file_path::String, external_file_path::String) -> JLDFile

Open or retrieve a cached external file handle.

# Arguments
- `current_file_path`: Path to the current file (for resolving relative external paths)
- `external_file_path`: Path to the external file (from the external link)

# Returns
An open JLDFile handle to the external file.

# Error Handling
Throws appropriate exceptions for various failure modes:
- `SystemError`: File not found or permission denied
- `ArgumentError`: Invalid file format or security violation
- `UnsupportedFeatureException`: Circular reference detection
"""
function get_external_file(current_file_path::String, external_file_path::String)
    # Resolve and validate the external file path
    resolved_path = resolve_external_file_path(current_file_path, external_file_path)

    # Check cache first
    if haskey(EXTERNAL_FILE_CACHE, resolved_path)
        handle_ref = EXTERNAL_FILE_CACHE[resolved_path]
        handle = handle_ref.value
        if handle !== nothing && handle.jld_file !== nothing
            handle.last_access_time = time()
            return handle.jld_file
        else
            # Handle was garbage collected or file was closed
            delete!(EXTERNAL_FILE_CACHE, resolved_path)
        end
    end

    # Create new handle
    handle = ExternalFileHandle(resolved_path)

    try
        # Check if file exists and is readable
        if !isfile(resolved_path)
            throw(SystemError("External file not found: $resolved_path"))
        end

        # Detect circular references with full chain tracking
        detect_circular_reference(current_file_path, resolved_path)

        # Add to reference chain before opening
        chain = push_reference_chain!(resolved_path)

        try
            # Open the external file with enhanced error handling
            handle.jld_file = open_external_file_with_retry(resolved_path)
            handle.last_access_time = time()

            # Cache the handle (with weak reference to allow garbage collection)
            manage_cache_size()
            EXTERNAL_FILE_CACHE[resolved_path] = WeakRef(handle)

            return handle.jld_file

        finally
            # Always remove from reference chain when done
            pop_reference_chain!()
        end

    catch e
        handle.error_state = e
        # Enhanced error handling with context preservation
        rethrow_with_context(e, resolved_path, current_file_path)
    end
end

"""
    manage_cache_size()

Keep the external file cache size under control by removing least recently used entries.
"""
function manage_cache_size()
    if length(EXTERNAL_FILE_CACHE) >= MAX_CACHE_SIZE
        # Remove entries with dead weak references first
        dead_keys = String[]
        for (path, ref) in EXTERNAL_FILE_CACHE
            if ref.value === nothing
                push!(dead_keys, path)
            end
        end

        for key in dead_keys
            delete!(EXTERNAL_FILE_CACHE, key)
        end

        # If still too many, remove least recently used
        if length(EXTERNAL_FILE_CACHE) >= MAX_CACHE_SIZE
            # Get all valid handles with their access times
            handles_with_times = Pair{String, Float64}[]
            for (path, ref) in EXTERNAL_FILE_CACHE
                handle = ref.value
                if handle !== nothing
                    push!(handles_with_times, path => handle.last_access_time)
                end
            end

            # Sort by access time and remove oldest
            sort!(handles_with_times, by=x->x.second)
            num_to_remove = length(handles_with_times) - MAX_CACHE_SIZE + 1

            for i in 1:num_to_remove
                delete!(EXTERNAL_FILE_CACHE, handles_with_times[i].first)
            end
        end
    end
end

"""
    detect_circular_reference(current_file_path::String, external_file_path::String)

Sophisticated circular reference detection that tracks the full chain of external references.

# Arguments
- `current_file_path`: Path to the current file
- `external_file_path`: Path to the external file being opened

# Throws
`UnsupportedFeatureException` if a circular reference is detected.

# Algorithm
Uses task-local storage to maintain a reference chain of currently opening files.
This catches both direct circular references (A -> A) and longer chains (A -> B -> C -> A).
"""
function detect_circular_reference(current_file_path::String, external_file_path::String)
    # Normalize paths for comparison
    current_norm = normpath(abspath(current_file_path))
    external_norm = normpath(abspath(external_file_path))

    # Check for direct circular reference
    if current_norm == external_norm
        throw(UnsupportedFeatureException("Circular external link reference detected: file references itself"))
    end

    # Get current reference chain
    chain = get_reference_chain()

    # Check if external file is already in the reference chain
    if external_norm in chain
        # Found circular reference - construct helpful error message
        chain_str = join(chain, " -> ")
        throw(UnsupportedFeatureException(
            "Circular external link reference chain detected: $chain_str -> $external_norm"
        ))
    end

    # Check for excessive chain depth (protection against pathological cases)
    if length(chain) > 10
        chain_str = join(chain, " -> ")
        throw(UnsupportedFeatureException(
            "External link chain too deep ($(length(chain)) levels): $chain_str -> $external_norm"
        ))
    end
end

"""
    resolve_external_link(current_file::JLDFile, external_link::ExternalLink) -> Union{Any, Nothing}

Resolve an external link and return the target object.

# Arguments
- `current_file`: The current JLD2 file containing the external link
- `external_link`: The external link to resolve

# Returns
The object referenced by the external link, or throws an exception if resolution fails.

# Error Handling
- File access errors (not found, permission denied) are passed through as SystemError
- Invalid object paths throw KeyError
- Security violations throw ArgumentError
- Circular references throw UnsupportedFeatureException
"""
function resolve_external_link(current_file::JLDFile, external_link::ExternalLink)
    # Get the external file handle
    external_file = get_external_file(current_file.path, external_link.file_path)

    # Navigate to the target object within the external file
    try
        # Use the object path to access the target
        # The object_path should be an absolute path within the external file
        object_path = external_link.object_path

        # Remove leading slash for indexing (JLD2 paths don't use leading slash for indexing)
        if startswith(object_path, "/")
            object_path = object_path[2:end]
        end

        # Handle empty path (root group)
        if isempty(object_path)
            return external_file.root_group
        end

        # Navigate through the path
        return navigate_external_path(external_file, object_path)

    catch e
        if isa(e, KeyError)
            throw(KeyError("Object not found in external file $(external_link.file_path): $(external_link.object_path)"))
        else
            rethrow(e)
        end
    end
end

"""
    navigate_external_path(external_file::JLDFile, object_path::String) -> Any

Navigate through an object path within an external file to find the target object.

# Arguments
- `external_file`: The external JLD2 file
- `object_path`: Path within the file (without leading slash)

# Returns
The object at the specified path.
"""
function navigate_external_path(external_file::JLDFile, object_path::String)
    # Split the path into components
    path_components = split(object_path, '/', keepempty=false)

    current_object = external_file.root_group
    current_path = ""

    for component in path_components
        current_path = isempty(current_path) ? component : current_path * "/" * component

        # Check if the component exists in the current group
        if haskey(current_object, component)
            current_object = current_object[component]
        else
            throw(KeyError("Path component '$component' not found in external file (full path: $object_path)"))
        end
    end

    return current_object
end

"""
    clear_external_file_cache()

Clear the external file cache and close all cached files.
This is useful for cleanup or when memory usage needs to be reduced.
"""
function clear_external_file_cache()
    for (path, ref) in EXTERNAL_FILE_CACHE
        handle = ref.value
        if handle !== nothing && handle.jld_file !== nothing
            try
                close(handle.jld_file)
            catch
                # Ignore errors during cleanup
            end
        end
    end
    empty!(EXTERNAL_FILE_CACHE)
end

"""
    open_external_file_with_retry(file_path::String) -> JLDFile

Open an external file with retry logic for transient failures.

# Arguments
- `file_path`: Path to the external file to open

# Returns
An open JLDFile handle.

# Error Handling
Implements exponential backoff retry logic for transient network and I/O failures.
Distinguishes between permanent failures (file not found) and transient failures (network timeouts).
"""
function open_external_file_with_retry(file_path::String)
    last_error = nothing

    for attempt in 1:MAX_RETRY_ATTEMPTS
        try
            # Attempt to open the file
            return jldopen(file_path, "r")

        catch e
            last_error = e

            # Determine if this is a retryable error
            if !is_retryable_error(e) || attempt == MAX_RETRY_ATTEMPTS
                rethrow(e)
            end

            # Wait before retrying with exponential backoff
            delay_ms = RETRY_DELAY_MS * (2^(attempt-1))
            sleep(delay_ms / 1000)

            @debug "Retrying external file access" file_path attempt delay_ms error=e
        end
    end

    # Should never reach here, but handle it gracefully
    throw(last_error)
end

"""
    is_retryable_error(error::Exception) -> Bool

Determine if an error is worth retrying for external file access.

# Arguments
- `error`: The exception that occurred

# Returns
`true` if the error might be transient and worth retrying, `false` otherwise.
"""
function is_retryable_error(error::Exception)
    # System errors that might be transient
    if isa(error, SystemError)
        # Network-related errors that might be temporary
        retryable_errnos = [
            11,  # EAGAIN - Resource temporarily unavailable
            110, # ETIMEDOUT - Connection timed out
            111, # ECONNREFUSED - Connection refused
            113, # EHOSTUNREACH - No route to host
            115, # EINPROGRESS - Operation in progress
        ]

        return error.errnum in retryable_errnos
    end

    # IO errors might be transient
    if isa(error, Base.IOError)
        return true
    end

    # Other errors are typically permanent
    return false
end

"""
    rethrow_with_context(error::Exception, resolved_path::String, current_file_path::String)

Rethrow an error with enhanced context information for debugging.

# Arguments
- `error`: The original exception
- `resolved_path`: The resolved external file path
- `current_file_path`: The current file path for context

# Error Enhancement
Preserves the original error type while adding helpful context information.
"""
function rethrow_with_context(error::Exception, resolved_path::String, current_file_path::String)
    if isa(error, SystemError)
        if error.errnum == 2  # ENOENT - file not found
            throw(SystemError("External file not found: $resolved_path (referenced from: $current_file_path)", error.errnum))
        elseif error.errnum == 13  # EACCES - permission denied
            throw(SystemError("Permission denied accessing external file: $resolved_path (referenced from: $current_file_path)", error.errnum))
        else
            throw(SystemError("Error accessing external file: $resolved_path (referenced from: $current_file_path): $(error.msg)", error.errnum))
        end
    elseif isa(error, ArgumentError)
        throw(ArgumentError("External file access error: $resolved_path (referenced from: $current_file_path): $(error.msg)"))
    else
        # For other error types, re-throw with added context in a way that preserves the original type
        try
            # Try to add context to the error message if possible
            if hasfield(typeof(error), :msg) && isa(getfield(error, :msg), AbstractString)
                new_msg = "External file error: $resolved_path (referenced from: $current_file_path): $(getfield(error, :msg))"
                # Create new instance with updated message
                new_error = typeof(error)(new_msg, [getfield(error, f) for f in fieldnames(typeof(error))[2:end]]...)
                throw(new_error)
            else
                rethrow(error)
            end
        catch
            # If context enhancement fails, just rethrow the original
            rethrow(error)
        end
    end
end

"""
    get_cache_stats() -> NamedTuple

Get statistics about the external file cache for debugging and monitoring.

# Returns
A NamedTuple with cache statistics:
- `cache_size`: Number of entries in cache
- `active_files`: Number of actually open files
- `dead_references`: Number of garbage-collected references
"""
function get_cache_stats()
    active_count = 0
    dead_count = 0

    for (path, ref) in EXTERNAL_FILE_CACHE
        handle = ref.value
        if handle !== nothing && handle.jld_file !== nothing
            active_count += 1
        else
            dead_count += 1
        end
    end

    return (
        cache_size = length(EXTERNAL_FILE_CACHE),
        active_files = active_count,
        dead_references = dead_count
    )
end

"""
    get_reference_chain_info() -> NamedTuple

Get information about the current reference chain for debugging.

# Returns
A NamedTuple with:
- `chain_length`: Current chain depth
- `chain_files`: List of files in the current chain
- `max_depth_reached`: Whether we're near the maximum allowed depth
"""
function get_reference_chain_info()
    chain = get_reference_chain()
    return (
        chain_length = length(chain),
        chain_files = copy(chain),
        max_depth_reached = length(chain) > 8  # Near the limit of 10
    )
end