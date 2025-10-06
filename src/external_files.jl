function load_external_dataset(current_file::JLDFile, external_link::ExternalLink)
    # Get the external file handle
    external_file = get_external_file(current_file.path, external_link.file_path)
    isfile(external_file) || throw(ArgumentError("External file not found: $external_file"))

    # Navigate to the target object within the external file
    try
        # Use the object path to access the target
        # The object_path should be an absolute path within the external file
        object_path = external_link.object_path
        lock(OPEN_FILES_LOCK) do
            if external_file in keys(OPEN_FILES)
                return OPEN_FILES[external_link][object_path]
            end
        end

        jldopen(external_file, "r") do f
            return f[object_path]
        end
    catch e
        if isa(e, KeyError)
            throw(KeyError("Object not found in external file $(external_link.file_path): $(external_link.object_path)"))
        else
            rethrow(e)
        end
    end
end
