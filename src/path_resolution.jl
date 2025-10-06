function resolve_external_file_path(current_file_path::String, external_file_path::String)
    current_dir = dirname(current_file_path)
    return abspath(joinpath(current_dir, external_file_path))
end

function resolve_soft_link(g::Group, path::String)
    if !startswith(path, '/')
        path = group_path(g) * "/" * path
    end
    return path
end
