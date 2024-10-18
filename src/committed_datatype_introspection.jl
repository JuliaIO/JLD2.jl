# This file implements methods to look up the type signatures
# of committed julia data types in JLD2 files without
# needing the types to exist in the current session.
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
        if (ref = field_datatypes[string(key)]) != NULL_REFERENCE
            fieldtype = stringify_committed_datatype(f, f.datatype_locations[ref])[1]
            do_report = true
        else
            # These are normal julia types
            dtrr = jltype(f, dt.members[i])
            fieldtype = string(julia_type(dtrr))
        end
        push!(field_strs, "$(dt.names[i])::$(fieldtype)")
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
    filter_pipeline::FilterPipeline = FilterPipeline(Filter[])
    for msg in HeaderMessageIterator(f, offset)
        if msg.type == HmDataspace
            dataspace = ReadDataspace(f, msg)
        elseif msg.type == HmDatatype
            datatype = HmWrap(HmDatatype, msg).dt
        elseif msg.type == HmDataLayout
            layout = DataLayout(f, msg)
        elseif msg.type == HmFilterPipeline
            filter_pipeline = FilterPipeline(msg)
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
        read_dataspace = (dataspace, NULL_REFERENCE, layout, filter_pipeline)
        res = read_data(f, rr, read_dataspace, nothing)
        string(res)
    end    
end

function typestring_from_refs(f::JLDFile, ptr::Ptr)
    # Test for a potential null pointer indicating an empty array
    isinit = jlunsafe_load(convert(Ptr{UInt32}, ptr)) != 0
    if isinit
        refs = jlconvert(ChangedLayout{RelOffset, Vlen{RelOffset}}(), f, ptr, NULL_REFERENCE)
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

function jlconvert_string(::ChangedLayout{T,DataTypeODR},
                        f::JLDFile,
                        ptr::Ptr) where T
    mypath = String(jlconvert(ChangedLayout{UInt8,Vlen{UInt8}}(), f, ptr, NULL_REFERENCE))
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

function jlconvert_string(::ChangedLayout{Union, UnionTypeODR}, f::JLDFile,
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