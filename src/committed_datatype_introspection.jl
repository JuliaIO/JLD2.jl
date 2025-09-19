# This file implements methods to look up the type signatures
# of committed julia data types in JLD2 files without
# needing the types to exist in the current session.

# Safe introspection function that only reads string representations
# without triggering type reconstruction
function safe_introspect_datatype(f::JLDFile, dt::H5Datatype; showfields=false)
    # For committed datatypes, use enhanced safe introspection
    if dt isa SharedDatatype && haskey(f.datatype_locations, dt.header_offset)
        julia_type_str, written_type_str, field_strs = safe_introspect_committed_datatype(f, f.datatype_locations[dt.header_offset]; showfields=showfields)

        if showfields && !isempty(field_strs)
            # Show detailed field information for structs
            return "$julia_type_str{$(join(field_strs, ", "))}"
        else
            # Just return the type name (including parameters if present)
            return julia_type_str
        end
    else
        # For non-committed datatypes, try safe reconstruction for basic types
        return safe_reconstruct_basic_type(f, dt)
    end
end

# Read type strings from committed datatype without reconstruction
function safe_read_committed_type_strings(f::JLDFile, cdt)
    dt, attrs = read_shared_datatype(f, cdt)

    for attr in attrs
        if attr.name == :julia_type || attr.name == Symbol("julia type")
            # Try manual string reading
            result = read_string_from_attribute(f, attr)
            if !startswith(result, "unreadable_type_string") && !startswith(result, "unsupported_dataspace")
                return result
            end
        end
    end

    # Fallback: try to infer from datatype structure
    return safe_describe_datatype(f, dt)
end

# Helper to read string data from an attribute without type reconstruction
function read_string_from_attribute(f::JLDFile, attr)
    try
        # Handle different attribute datatype cases
        if attr.dataspace.dataspace_type == DS_SCALAR
            if isa(attr.datatype, VariableLengthDatatype)
                # Direct variable-length string
                seek(f.io, attr.data_offset)
                vlen_length = jlread(f.io, UInt32)
                global_heap_id = jlread(f.io, UInt32)

                if vlen_length > 0 && global_heap_id == 0 && vlen_length < 10000
                    string_bytes = Vector{UInt8}(undef, vlen_length)
                    unsafe_read(f.io, pointer(string_bytes), vlen_length)
                    null_pos = findfirst(isequal(0), string_bytes)
                    if !isnothing(null_pos)
                        string_bytes = string_bytes[1:null_pos-1]
                    end
                    return String(string_bytes)
                elseif global_heap_id != 0
                    return "global_heap_string"
                end
            elseif isa(attr.datatype, SharedDatatype)
                # The attribute datatype is a committed datatype
                # Need to look up the actual datatype and read accordingly
                datatype_location = f.datatype_locations[attr.datatype.header_offset]
                actual_dt, _ = read_shared_datatype(f, datatype_location)

                if isa(actual_dt, CompoundDatatype)
                    # This is a DataType ODR structure - read the name field manually
                    # Create a buffer for the compound data
                    seek(f.io, attr.data_offset)
                    compound_size = actual_dt.size  # Should be 32 bytes
                    compound_data = Vector{UInt8}(undef, compound_size)
                    unsafe_read(f.io, pointer(compound_data), compound_size)

                    # Now manually call jlconvert for the Vlen{UInt8} at offset 0
                    @GC.preserve compound_data begin
                        ptr = pointer(compound_data)
                        try
                            # This should call the same conversion logic as the working code
                            mypath_bytes = jlconvert(MappedRepr{UInt8,Vlen{UInt8}}(), f, ptr, NULL_REFERENCE)
                            mypath = String(mypath_bytes)

                            # Clean up the module prefix like the original code does
                            if startswith(mypath, r"Core|Main")
                                mypath = last(split(mypath, "."; limit=2))
                            end

                            # Also try to read type parameters like the original code
                            full_type = try
                                params = typestring_from_refs(f, ptr + odr_sizeof(Vlen{UInt8}))
                                if !isempty(params)
                                    mypath*"{"*join(params, ",")*"}"
                                else
                                    mypath
                                end
                            catch
                                # If parameter reading fails, just return the base type name
                                mypath
                            end

                            # Apply NamedTuple macro conversion if applicable
                            if startswith(full_type, "NamedTuple{")
                                return convert_namedtuple_to_macro_syntax(full_type)
                            else
                                return full_type
                            end
                        catch e
                            return "jlconvert_error: $(typeof(e)): $e"
                        end
                    end
                end
            else
                # Try reading as fixed-length string
                seek(f.io, attr.data_offset)
                current_pos = position(f.io)
                remaining = f.end_of_data - current_pos
                max_read = min(1000, remaining)

                if max_read > 0
                    string_bytes = Vector{UInt8}(undef, max_read)
                    unsafe_read(f.io, pointer(string_bytes), max_read)
                    null_pos = findfirst(isequal(0), string_bytes)
                    if !isnothing(null_pos)
                        string_bytes = string_bytes[1:null_pos-1]
                    end
                    result = String(string_bytes)

                    if length(result) > 0 && all(c -> isprint(c) || c == '\n' || c == '\t', result)
                        return result
                    end
                end
            end
        end

        return "unsupported_dataspace"
    catch e
        return "unreadable_type_string: $(typeof(e)): $e"
    end
end

# Safe description of basic datatypes with reconstruction for fundamental types
function safe_describe_datatype(f::JLDFile, dt::H5Datatype)
    if dt isa BasicDatatype
        # Analyze the basic datatype properties
        class = dt.class
        if class >> 4 == 1  # String types
            return "String"
        elseif class >> 4 == 0  # Numeric types
            if class & 0x0f == 0  # Integer
                return dt.size == 8 ? "Int64" : dt.size == 4 ? "Int32" : "Int$(dt.size*8)"
            elseif class & 0x0f == 1  # Float
                return dt.size == 8 ? "Float64" : dt.size == 4 ? "Float32" : "Float$(dt.size*8)"
            end
        end
        return "BasicDatatype"
    elseif dt isa CompoundDatatype
        # For compound types, show field structure with types
        field_strs = String[]
        for (i, name) in enumerate(dt.names)
            if i <= length(dt.members)
                # Try safe reconstruction first for better type names
                field_type = safe_reconstruct_basic_type(f, dt.members[i])
                push!(field_strs, "$name::$field_type")
            else
                push!(field_strs, "$name::Unknown")
            end
        end
        # Return both header and field list for proper multi-line formatting
        if isempty(field_strs)
            return "compound struct (empty)"
        else
            # Return a special format that the show function can detect and format properly
            return "COMPOUND_STRUCT:" * join(field_strs, "|")
        end
    elseif dt isa ArrayDatatype
        return "Array"
    elseif dt isa VariableLengthDatatype
        # Check if it's a variable-length string
        if dt.type isa BasicDatatype && dt.type.class >> 4 == 0 && dt.type.class & 0x0f == 0  # Integer base type
            return "String"  # Variable-length string
        else
            return "VariableLength"
        end
    else
        return string(typeof(dt))
    end
end

# Safe reconstruction of basic Julia types that we know are always available
function safe_reconstruct_basic_type(f::JLDFile, dt::H5Datatype)
    # Check for reference datatypes first (before trying reconstruction)
    if dt isa BasicDatatype && dt.class == 0x37  # DT_REFERENCE | 0x3<<4
        return describe_reference_datatype(f, dt)
    end

    try
        # Only reconstruct fundamental Julia types that are guaranteed to exist
        rr = jltype(f, dt)
        jtype = julia_repr(rr)

        # Check if this is a basic type we're confident about
        type_str = string(jtype)
        if type_str in ["Int8", "Int16", "Int32", "Int64", "Int128",
                       "UInt8", "UInt16", "UInt32", "UInt64", "UInt128",
                       "Float16", "Float32", "Float64",
                       "Bool", "Char", "String", "Symbol",
                       "Nothing", "Missing"]
            return type_str
        end

        # Also handle some basic composite types
        if startswith(type_str, "Tuple{") || startswith(type_str, "Pair{") ||
           startswith(type_str, "Complex{") || startswith(type_str, "Rational{")
            return type_str
        end

        # Handle NamedTuple with macro syntax conversion
        if startswith(type_str, "NamedTuple{")
            return convert_namedtuple_to_macro_syntax(type_str)
        end

        # If it's not a basic type, fall back to safe description
        return safe_describe_datatype(f, dt)
    catch
        # If reconstruction fails, fall back to safe description
        return safe_describe_datatype(f, dt)
    end
end

# Describe reference datatypes with useful information
function describe_reference_datatype(f::JLDFile, dt::BasicDatatype)
    try
        # For reference datatypes, we can safely load the RelOffset value
        # to show exactly where the referenced object is stored
        rr = jltype(f, dt)
        jtype = julia_repr(rr)
        type_str = string(jtype)

        # Get the actual RelOffset value for debugging/investigation
        rel_offset_info = try
            # We need to find where this reference is stored in the data
            # This requires context from the dataset, but we can provide a generic approach
            "RelOffset(...)"
        catch
            "offset unknown"
        end

        # If we get "Any", this means it's a generic reference
        if type_str == "Any"
            return "Object reference -> $rel_offset_info"
        end

        # Allow reconstruction for basic reference types like arrays and known composites
        if startswith(type_str, "Array{") || startswith(type_str, "Vector{") ||
           startswith(type_str, "Matrix{") || startswith(type_str, "AbstractArray{") ||
           startswith(type_str, "Union{") || startswith(type_str, "Tuple{") ||
           startswith(type_str, "NamedTuple{")

            # Apply NamedTuple conversion if needed
            result_type = if startswith(type_str, "NamedTuple{")
                convert_namedtuple_to_macro_syntax(type_str)
            else
                type_str
            end
            return "$result_type -> $rel_offset_info"
        end

        # For unknown reference types, return a descriptive message with offset
        return "Reference{$type_str} -> $rel_offset_info"
    catch
        # If reconstruction fails completely, return a generic reference description
        return "Reference{unknown}"
    end
end

# Enhanced describe function that can read the actual RelOffset from dataset context
function describe_reference_datatype_with_offset(f::JLDFile, dt::BasicDatatype, data_offset::Int64)
    try
        # Read the actual RelOffset value from the data
        seek(f.io, data_offset)
        offset_value = jlread(f.io, UInt64)
        rel_offset = RelOffset(offset_value)

        # Get type information
        rr = jltype(f, dt)
        jtype = julia_repr(rr)
        type_str = string(jtype)

        # If we get "Any", this means it's a generic reference
        if type_str == "Any"
            return "Object reference -> $rel_offset"
        end

        # Allow reconstruction for basic reference types like arrays and known composites
        if startswith(type_str, "Array{") || startswith(type_str, "Vector{") ||
           startswith(type_str, "Matrix{") || startswith(type_str, "AbstractArray{") ||
           startswith(type_str, "Union{") || startswith(type_str, "Tuple{") ||
           startswith(type_str, "NamedTuple{")

            # Apply NamedTuple conversion if needed
            result_type = if startswith(type_str, "NamedTuple{")
                convert_namedtuple_to_macro_syntax(type_str)
            else
                type_str
            end
            return "$result_type -> $rel_offset"
        end

        # For unknown reference types, return a descriptive message with offset
        return "Reference{$type_str} -> $rel_offset"
    catch e
        # If reading fails, fall back to generic description
        return "Reference{unknown} -> offset unreadable"
    end
end

# Helper function to parse comma-separated values with proper brace handling
function parse_comma_separated_with_braces(content::String)
    if isempty(strip(content))
        return String[]
    end

    parts = String[]
    current = ""
    brace_count = 0

    for c in content
        if c == '{'
            brace_count += 1
            current *= c
        elseif c == '}'
            brace_count -= 1
            current *= c
        elseif c == ',' && brace_count == 0
            push!(parts, strip(current))
            current = ""
        else
            current *= c
        end
    end

    # Add the final part
    if !isempty(strip(current))
        push!(parts, strip(current))
    end

    return parts
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

# Enhanced safe introspection for custom and parametric structs
function safe_introspect_committed_datatype(f::JLDFile, cdt; showfields=false)
    dt, attrs = read_shared_datatype(f, cdt)
    written_type_str = ""
    julia_type_str = ""

    # Read type strings from attributes safely
    for attr in attrs
        if !(attr.name == :julia_type || attr.name == :written_type)
            continue
        end

        # Read the string safely without type reconstruction
        str = read_string_from_attribute(f, attr)

        if attr.name == :written_type
            written_type_str = str
        elseif attr.name == :julia_type
            julia_type_str = str
        end
    end

    if !showfields ||
       dt isa BasicDatatype ||
       dt isa VariableLengthDatatype ||
       startswith(julia_type_str, r"Tuple|Union") ||
       julia_type_str == "DataType"
        return julia_type_str, written_type_str, String[]
    end

    # For compound types, safely inspect field types recursively
    field_strs = String[]
    if dt isa CompoundDatatype
        field_datatypes = safe_read_field_datatypes(f, dt, attrs)

        for (i, key) in enumerate(keys(field_datatypes))
            # Find the corresponding index in dt.names
            name_index = findfirst(name -> string(name) == string(key), dt.names)
            if name_index === nothing
                continue  # Skip if name not found
            end

            if (ref = field_datatypes[string(key)]) != NULL_REFERENCE
                # Recursively introspect field type safely
                fieldtype = safe_introspect_committed_datatype(f, f.datatype_locations[ref])[1]
            else
                # Handle basic field types safely - use safe reconstruction for basic types
                if name_index <= length(dt.members)
                    fieldtype = safe_reconstruct_basic_type(f, dt.members[name_index])
                    # Clean up any compound struct formatting in nested fields
                    if startswith(fieldtype, "COMPOUND_STRUCT:")
                        field_data = fieldtype[17:end]
                        nested_fields = split(field_data, "|")
                        if length(nested_fields) <= 2
                            fieldtype = "struct{$(join(nested_fields, ", "))}"
                        else
                            fieldtype = "struct{$(length(nested_fields)) fields}"
                        end
                    end
                else
                    fieldtype = "Unknown"
                end
            end
            push!(field_strs, "$(dt.names[name_index])::$(fieldtype)")
        end
    end

    return julia_type_str, written_type_str, field_strs
end

# Safe reading of field datatypes without reconstruction
function safe_read_field_datatypes(f::JLDFile, dt::CompoundDatatype, attrs)
    field_datatypes = Dict{String, RelOffset}()

    for attr in attrs
        if attr.name == :field_datatypes
            # This attribute contains references to field datatypes
            # Read them safely without triggering reconstruction
            try
                seek(f.io, attr.data_offset)
                # The field_datatypes attribute contains an array of references
                # We need to read this safely without jlconvert

                # For now, initialize all fields to NULL_REFERENCE
                # This will fall back to safe_describe_datatype for basic types
                for name in dt.names
                    field_datatypes[string(name)] = NULL_REFERENCE
                end

                # TODO: Implement safe reading of the actual references
                # This would require understanding the exact layout of the field_datatypes attribute
            catch e
                # Fallback: assume all fields are basic types
                for name in dt.names
                    field_datatypes[string(name)] = NULL_REFERENCE
                end
            end
            break
        end
    end

    # If no field_datatypes attribute found, assume basic types
    if isempty(field_datatypes)
        for name in dt.names
            field_datatypes[string(name)] = NULL_REFERENCE
        end
    end

    return field_datatypes
end
function stringify_committed_datatype(f, cdt; showfields=false)
    io = f.io
    dt, attrs = read_shared_datatype(f, cdt)
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

    if !showfields ||
        dt isa BasicDatatype ||
        dt isa VariableLengthDatatype ||
        startswith(julia_type_str, r"Tuple|Union") ||
        julia_type_str == "DataType"
        return julia_type_str, written_type_str, String[]
    end

    field_datatypes = read_field_datatypes(f, dt, attrs)
    field_strs = String[]
    do_report = false
    for (i, key) in enumerate(keys(field_datatypes))
        # Find the corresponding index in dt.names
        name_index = findfirst(name -> string(name) == string(key), dt.names)
        if name_index === nothing
            continue  # Skip if name not found
        end

        if (ref = field_datatypes[string(key)]) != NULL_REFERENCE
            fieldtype = stringify_committed_datatype(f, f.datatype_locations[ref])[1]
            do_report = true
        else
            # These are normal julia types
            if name_index <= length(dt.members)
                dtrr = jltype(f, dt.members[name_index])
                fieldtype = string(julia_repr(dtrr))
            else
                fieldtype = "Unknown"
            end
        end
        push!(field_strs, "$(dt.names[name_index])::$(fieldtype)")
    end
    if do_report == false
        empty!(field_strs)
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
                stringify_committed_datatype(f, cdt)[1]
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
                if class == 0x37  # DT_REFERENCE
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
