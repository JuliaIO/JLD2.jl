# This file implements methods to look up the type signatures
# of committed julia data types in JLD2 files without
# needing the types to exist in the current session.

function read_string_from_attribute(f::JLDFile, attr)
    attr.dataspace.dataspace_type != DS_SCALAR && return "unsupported_dataspace"

    try
        isa(attr.datatype, VariableLengthDatatype) && return read_vlen_string_attribute(f, attr)
        isa(attr.datatype, SharedDatatype) && return read_shared_datatype_attribute(f, attr)
        return read_fixed_string_attribute(f, attr)
    catch e
        return "unreadable_type_string: $(typeof(e)): $e"
    end
end

function read_vlen_string_attribute(f::JLDFile, attr)
    seek(f.io, attr.data_offset)
    vlen_length = jlread(f.io, UInt32)
    global_heap_id = jlread(f.io, UInt32)

    global_heap_id != 0 && return "global_heap_string"
    (vlen_length == 0 || vlen_length >= 10000) && return "invalid_length"

    string_bytes = Vector{UInt8}(undef, vlen_length)
    unsafe_read(f.io, pointer(string_bytes), vlen_length)
    null_pos = findfirst(isequal(0), string_bytes)
    !isnothing(null_pos) && (string_bytes = string_bytes[1:null_pos-1])
    return String(string_bytes)
end

function read_shared_datatype_attribute(f::JLDFile, attr)
    datatype_location = f.datatype_locations[attr.datatype.header_offset]
    actual_dt, _ = read_shared_datatype(f, datatype_location)

    !isa(actual_dt, CompoundDatatype) && return "non_compound_shared"

    seek(f.io, attr.data_offset)
    compound_data = read(f.io, UInt8, actual_dt.size)

    @GC.preserve compound_data begin
        ptr = pointer(compound_data)
        try
            mypath_bytes = jlconvert(MappedRepr{UInt8,Vlen{UInt8}}(), f, ptr, NULL_REFERENCE)
            mypath = String(mypath_bytes)

            startswith(mypath, r"Core|Main") && (mypath = last(split(mypath, "."; limit=2)))

            full_type = try
                params = typestring_from_refs(f, ptr + odr_sizeof(Vlen{UInt8}))
                isempty(params) ? mypath : "$mypath{$(join(params, ","))}"
            catch
                mypath
            end

            return apply_namedtuple_conversion(full_type)
        catch e
            return "jlconvert_error: $(typeof(e)): $e"
        end
    end
end

function read_fixed_string_attribute(f::JLDFile, attr)
    seek(f.io, attr.data_offset)
    remaining = f.end_of_data - position(f.io)
    max_read = min(1000, remaining)

    max_read <= 0 && return "no_data"

    string_bytes = Vector{UInt8}(undef, max_read)
    unsafe_read(f.io, pointer(string_bytes), max_read)
    null_pos = findfirst(isequal(0), string_bytes)
    !isnothing(null_pos) && (string_bytes = string_bytes[1:null_pos-1])
    result = String(string_bytes)

    (length(result) > 0 && all(c -> isprint(c) || c == '\n' || c == '\t', result)) ? result : "invalid_string"
end

function apply_namedtuple_conversion(type_str::AbstractString)
    startswith(type_str, "NamedTuple{") ? convert_namedtuple_to_macro_syntax(type_str) : type_str
end

# Enhanced describe function that can read the actual RelOffset from dataset context
function describe_reference_datatype_with_offset(f::JLDFile, dt::BasicDatatype, data_offset::Int64)
    # Read the actual RelOffset value from the data
    seek(f.io, data_offset)
    offset_value = jlread(f.io, RelOffset)
    return "Object reference -> $rel_offset"
end

# Convert NamedTuple{(:a, :b), Tuple{Int,Int}} to @NamedTuple{a::Int, b::Int}
function convert_namedtuple_to_macro_syntax(type_str::String)
    try
        # Parse NamedTuple{(:names...), Tuple{types...}}
        if !startswith(type_str, "NamedTuple{")
            return type_str
        end

        # Extract the content between the outer braces
        content = type_str[12:end-1]  # Remove "NamedTuple{" and "}"

        # Find the comma that separates names from types
        # We need to carefully parse to handle nested braces and parentheses
        paren_count = 0
        brace_count = 0
        comma_pos = 0

        for i in 1:length(content)
            c = content[i]
            if c == '('
                paren_count += 1
            elseif c == ')'
                paren_count -= 1
            elseif c == '{'
                brace_count += 1
            elseif c == '}'
                brace_count -= 1
            elseif c == ',' && paren_count == 0 && brace_count == 0
                comma_pos = i
                break
            end
        end

        if comma_pos == 0
            return type_str  # Couldn't parse
        end

        names_part = strip(content[1:comma_pos-1])
        types_part = strip(content[comma_pos+1:end])

        # Parse names: (:a, :b, :c) -> ["a", "b", "c"]
        if !startswith(names_part, "(:") || !endswith(names_part, ")")
            return type_str  # Couldn't parse names
        end

        names_content = names_part[3:end-1]  # Remove "(:" and ")"
        # Handle single field case where there might not be a comma
        if contains(names_content, ',')
            name_strings = [strip(strip(n), ':') for n in split(names_content, ',') if !isempty(strip(n))]
        else
            name_strings = [strip(strip(names_content), ':')]
        end

        # Parse types: Tuple{Int, Float64, String} -> ["Int", "Float64", "String"]
        if !startswith(types_part, "Tuple{") || !endswith(types_part, "}")
            return type_str  # Couldn't parse types
        end

        types_content = types_part[7:end-1]  # Remove "Tuple{" and "}"
        # Parse types with proper handling of nested braces - inline implementation
        if isempty(strip(types_content))
            type_strings = String[]
        else
            type_strings = String[]
            current = ""
            brace_count_inner = 0

            for c in types_content
                if c == '{'
                    brace_count_inner += 1
                    current *= c
                elseif c == '}'
                    brace_count_inner -= 1
                    current *= c
                elseif c == ',' && brace_count_inner == 0
                    push!(type_strings, strip(current))
                    current = ""
                else
                    current *= c
                end
            end

            # Add the final part
            if !isempty(strip(current))
                push!(type_strings, strip(current))
            end
        end

        # Ensure names and types match in count
        if length(name_strings) != length(type_strings)
            return type_str  # Mismatch in counts
        end

        # Build macro syntax: @NamedTuple{a::Int, b::Float64}
        field_specs = [name_strings[i] * "::" * type_strings[i] for i in 1:length(name_strings)]
        return "@NamedTuple{" * join(field_specs, ", ") * "}"

    catch e
        # If parsing fails, return original
        return type_str
    end
end

function stringify_h5datatype(f, dt; showfields=false)
    io = f.io
    attrs = ReadAttribute[]
    if isshared(dt)
        dt, attrs = read_shared_datatype(f, dt)
    end

    written_type_str = ""
    julia_type_str = ""
    for attr in attrs
        if !(attr.name == :julia_type || attr.name == :written_type)
            continue
        end
        datatype = attr.datatype
        @assert datatype isa SharedDatatype
        rr = jltype(f, f.datatype_locations[datatype.header_offset])
        @assert attr.dataspace.dataspace_type == DS_SCALAR
        str = jlconvert_string_wrap(rr, f, attr.data_offset)

        if attr.name == :written_type
            written_type_str = str
        elseif attr.name ==:julia_type
            julia_type_str = str
        end
    end

    if !(dt isa CompoundDatatype)
        type = jltype(f, dt)
        julia_type_str = string(julia_repr(type))
        #written_type_str = string(file_repr(type))
    end

    if !showfields ||
        !isa(dt, CompoundDatatype)
        dt isa VariableLengthDatatype ||
        startswith(julia_type_str, r"Tuple|Union") ||
        julia_type_str == "DataType"
        return julia_type_str, written_type_str, String[]
    end

    field_datatypes = read_field_datatypes(f, dt, attrs)
    field_strs = String[]
    #do_report = false
    for (i, key) in enumerate(keys(field_datatypes))
        # Find the corresponding index in dt.names
        name_index = findfirst(name -> string(name) == string(key), dt.names)
        if name_index === nothing
            continue  # Skip if name not found
        end

        if (ref = field_datatypes[string(key)]) != NULL_REFERENCE
            fieldtype = stringify_h5datatype(f, f.datatype_locations[ref])[1]
            fieldtype *= " (committed at $(ref))"
        else
            # These are normal julia types
            if name_index <= length(dt.members)
                dtrr = jltype(f, dt.members[name_index])
                fieldtype = string(julia_repr(dtrr))
                if fieldtype == "Any"
                    fieldtype = "Any (untyped reference)"
                end
            else
                fieldtype = "unknown"
            end
        end
        push!(field_strs, "$(dt.names[name_index])::$(fieldtype)")
    end
    return julia_type_str, written_type_str, field_strs
end

function stringify_object(f, offset)
    # Messages
    dataspace = ReadDataspace()
    attrs = EMPTY_READ_ATTRIBUTES
    datatype::H5Datatype = PlaceholderH5Datatype()
    layout::DataLayout = DataLayout(0,LcCompact,0,-1)
    filter_pipeline::WrittenFilterPipeline = WrittenFilterPipeline()
    for msg in HeaderMessageIterator(f, offset)
        if msg.type == HmDataspace
            dataspace = ReadDataspace(f, msg)
        elseif msg.type == HmDatatype
            datatype = HmWrap(HmDatatype, msg).dt
        elseif msg.type == HmDataLayout
            layout = DataLayout(f, msg)
        elseif msg.type == HmFilterPipeline
            filter_pipeline = WrittenFilterPipeline(msg)
        elseif msg.type == HmAttribute
            if attrs === EMPTY_READ_ATTRIBUTES
                attrs = ReadAttribute[read_attribute(f, msg)]
            else
                push!(attrs, read_attribute(f,msg))
            end
        elseif (msg.hflags & 2^3) != 0
            throw(UnsupportedFeatureException())
        end
    end

    iscompressed(filter_pipeline) && !ischunked(layout)  && throw(InvalidDataException("Compressed data must be chunked"))

    if datatype isa SharedDatatype && haskey(f.datatype_locations, datatype.header_offset)
        # Committed datatype
        rr = jltype(f, f.datatype_locations[datatype.header_offset])
        jlconvert_string_wrap(rr, f, layout.data_offset)
    else
        rr = jltype(f, datatype)
        seek(f.io, layout.data_offset)
        read_dataspace = (dataspace, NULL_REFERENCE, layout, FilterPipeline(filter_pipeline))
        res = read_data(f, rr, read_dataspace, nothing)
        string(res)
    end
end

function typestring_from_refs(f::JLDFile, ptr::Ptr)
    # Test for a potential null pointer indicating an empty array
    isinit = jlunsafe_load(convert(Ptr{UInt32}, ptr)) != 0
    if isinit
        refs = jlconvert(MappedRepr{RelOffset, Vlen{RelOffset}}(), f, ptr, NULL_REFERENCE)
        #println("datatypes at refs $(Int.(getproperty.(refs,:offset)))")
        params =  Any[let
            # If the reference is to a committed datatype, read the datatype
            nulldt = CommittedDatatype(UNDEFINED_ADDRESS, 0)
            cdt = get(f.datatype_locations, ref, nulldt)
            res = if cdt !== nulldt
                stringify_h5datatype(f, cdt)[1]
            else
                stringify_object(f, ref)
            end
            if startswith(res, r"Core|Main")
                res = last(split(res, "."; limit=2))
            end
            res
        end for ref in refs]
        return params
    end
    return []
end

function jlconvert_string_wrap(rr, f, offset)
    seek(f.io, offset)
    r = Vector{UInt8}(undef, odr_sizeof(rr))
    @GC.preserve r begin
        unsafe_read(f.io, pointer(r), odr_sizeof(rr))
        jlconvert_string(rr, f, pointer(r))
    end
end

function jlconvert_string(::MappedRepr{T,DataTypeODR},
                        f::JLDFile,
                        ptr::Ptr) where T
    mypath = String(jlconvert(MappedRepr{UInt8,Vlen{UInt8}}(), f, ptr, NULL_REFERENCE))
    params = typestring_from_refs(f, ptr+odr_sizeof(Vlen{UInt8}))
    if startswith(mypath, r"Core|Main")
        mypath = last(split(mypath, "."; limit=2))
    end

    if !isempty(params)
        return mypath*"{"*join(params, ",")*"}"
    else
        return mypath
    end
end

function jlconvert_string(::MappedRepr{Union, UnionTypeODR}, f::JLDFile,
    ptr::Ptr)#, header_offset::RelOffset)
    # Skip union type description in the beginning
    ptr += odr_sizeof(Vlen{String})
    # Reconstruct a Union by reading a list of DataTypes and UnionAlls
    # Lookup of RelOffsets is taken from jlconvert of DataTypes
    datatypes = typestring_from_refs(f, ptr)
    unionalls = typestring_from_refs(f, ptr+odr_sizeof(Vlen{RelOffset}))
    "Union{"*join(vcat(datatypes, unionalls), ",")*"}"
end

function jlconvert_string(rr::ReadRepresentation,
                        f::JLDFile,
                        ptr::Ptr)
    # so apparently this is a custom struct with plain fields that wants to be loaded
    string(jlconvert(rr, f, ptr, UNDEFINED_ADDRESS))
end

# Safe attribute reading that provides useful info without risking reconstruction errors
function safe_read_attribute_info(f::JLDFile, attr::ReadAttribute)
    try
        # For simple attributes (strings, numbers), try to read the value
        if attr.dataspace.dataspace_type == DS_SCALAR
            # Check if it's a simple type we can safely read
            if attr.datatype isa VariableLengthDatatype ||
               (attr.datatype isa BasicDatatype && attr.datatype.class >> 4 == 1) ||  # String types
               (attr.datatype isa SharedDatatype)  # Might be string type

                # Try safe string reading
                str_result = read_string_from_attribute(f, attr)
                if !startswith(str_result, "unreadable_type_string") &&
                   !startswith(str_result, "unsupported_dataspace") &&
                   !startswith(str_result, "jlconvert_error")
                    return "\"$str_result\""
                end
            elseif attr.datatype isa BasicDatatype
                # Check for reference types in attributes
                class = attr.datatype.class
                if class%16 == DT_REFERENCE
                    # This attribute contains a reference to another object
                    try
                        seek(f.io, attr.data_offset)
                        offset_value = jlread(f.io, UInt64)
                        rel_offset = RelOffset(offset_value)

                        # Try to read the referenced object as a string
                        referenced_str = try
                            stringify_object(f, rel_offset)
                        catch
                            "referenced_object@$rel_offset"
                        end
                        return "\"$referenced_str\""
                    catch
                        return "<reference@unknown>"
                    end
                elseif class >> 4 == 0  # Numeric types
                    if class & 0x0f == 0  # Integer
                        try
                            seek(f.io, attr.data_offset)
                            if attr.datatype.size == 8
                                value = jlread(f.io, Int64)
                                return string(value)
                            elseif attr.datatype.size == 4
                                value = jlread(f.io, Int32)
                                return string(value)
                            end
                        catch
                            # Fall through to default handling
                        end
                    elseif class & 0x0f == 1  # Float
                        try
                            seek(f.io, attr.data_offset)
                            if attr.datatype.size == 8
                                value = jlread(f.io, Float64)
                                return string(value)
                            elseif attr.datatype.size == 4
                                value = jlread(f.io, Float32)
                                return string(value)
                            end
                        catch
                            # Fall through to default handling
                        end
                    end
                end
            end
        end

        # For complex attributes, provide descriptive info instead of trying to read
        datatype_desc = if attr.datatype isa SharedDatatype
            "committed_type@$(attr.datatype.header_offset)"
        elseif attr.datatype isa BasicDatatype
            "basic_type(class=0x$(string(attr.datatype.class, base=16)), size=$(attr.datatype.size))"
        elseif attr.datatype isa VariableLengthDatatype
            "variable_length"
        elseif attr.datatype isa CompoundDatatype
            "compound_type($(length(attr.datatype.names))_fields)"
        else
            "$(typeof(attr.datatype))"
        end

        dataspace_desc = if attr.dataspace.dataspace_type == DS_SCALAR
            "scalar"
        elseif attr.dataspace.dataspace_type == DS_SIMPLE
            dims = attr.dataspace.dimensions
            "array$(tuple(dims...))"
        else
            "$(attr.dataspace.dataspace_type)"
        end

        return "<$datatype_desc, $dataspace_desc, @$(attr.data_offset)>"

    catch e
        return "<read_error: $(typeof(e))>"
    end
end
