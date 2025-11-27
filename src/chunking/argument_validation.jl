function validate_chunks(chunks::NTuple{N,Int}, data_size::NTuple{N,Int}) where N
    for (i, chunk_dim) in enumerate(chunks)
        if chunk_dim <= 0
            throw(ArgumentError("Chunk size must be positive, got chunks[$i]=$chunk_dim"))
        end
    end

    for (i, (chunk_dim, data_dim)) in enumerate(zip(chunks, data_size))
        if chunk_dim > data_dim
            @warn "Chunk dimension $i ($chunk_dim) is larger than data dimension ($data_dim). " *
                  "This may not be optimal." maxlog=1
        end
    end

    return nothing
end

function validate_chunks(chunks::NTuple{M,Int}, data_size::NTuple{N,Int}) where {M,N}
    throw(ArgumentError(
        "Chunk dimensions ($M) must match data dimensions ($N). " *
        "Got chunks=$chunks for data of size $data_size"
    ))
end

function validate_maxshape(maxshape::Nothing, data_size)
    return nothing
end

function validate_maxshape(maxshape::NTuple{N,Union{Int,Nothing}}, data_size::NTuple{N,Int}) where N
    for (i, (max_dim, data_dim)) in enumerate(zip(maxshape, data_size))
        if !isnothing(max_dim) && max_dim < data_dim
            throw(ArgumentError(
                "maxshape[$i]=$max_dim is less than current data size $data_dim. " *
                "maxshape must be >= current size or nothing (unlimited)."
            ))
        end
    end
    return nothing
end

function validate_maxshape(maxshape::NTuple{M,Union{Int,Nothing}}, data_size::NTuple{N,Int}) where {M,N}
    throw(ArgumentError(
        "maxshape dimensions ($M) must match data dimensions ($N). " *
        "Got maxshape=$maxshape for data of size $data_size"
    ))
end

function convert_maxshape_to_hdf5(maxshape::Nothing)
    return nothing
end

function convert_maxshape_to_hdf5(maxshape::NTuple{N,Union{Int,Nothing}}) where N
    hdf5_max = map(maxshape) do dim
        isnothing(dim) ? H5S_UNLIMITED : Int64(dim)
    end
    return reverse(hdf5_max)
end

function validate_fill_value(fill_value::Nothing, data_eltype)
    return nothing
end

function validate_fill_value(fill_value, data_eltype)
    if !(fill_value isa data_eltype)
        throw(ArgumentError(
            "fill_value type $(typeof(fill_value)) doesn't match data element type $data_eltype. " *
            "fill_value must be of the same type as array elements."
        ))
    end
    return nothing
end

function validate_index_type(index_type::Symbol)
    valid_types = [:single_chunk, :implicit_index, :fixed_array, :extensible_array, :v2btree, :v1btree]
    if !(index_type in valid_types)
        throw(ArgumentError(
            "Invalid chunk index type: $index_type. " *
            "Must be one of: $(join(valid_types, ", ", " or "))"
        ))
    end
    return nothing
end
