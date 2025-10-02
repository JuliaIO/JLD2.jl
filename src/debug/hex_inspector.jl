# HDF5 Hex Inspector - Advanced byte-level debugging tool
# Provides hex dumps with intelligent HDF5 structure annotations

using JLD2

# HDF5 format constants for annotation
const HDF5_SIGNATURES = Dict(
    b"TREE" => "V1 B-tree Node",
    b"\x89HDF" => "HDF5 File Signature",
    b"OHDR" => "Object Header",
    b"HEAP" => "Global Heap",
    b"FRHP" => "Fractal Heap Header",
    b"FHDB" => "Fractal Heap Direct Block",
    b"FHIB" => "Fractal Heap Indirect Block",
    b"SNOD" => "Symbol Table Node",
    b"GCOL" => "Global Heap Collection"
)

const HDF5_MESSAGE_TYPES = Dict(
    0x00 => "NIL",
    0x01 => "Dataspace",
    0x02 => "Link Info",
    0x03 => "Datatype",
    0x04 => "Fill Value (Old)",
    0x05 => "Fill Value",
    0x06 => "Link",
    0x08 => "Data Layout",
    0x09 => "Bogus",
    0x0A => "Group Info",
    0x0B => "Filter Pipeline",
    0x0C => "Attribute",
    0x0D => "Object Comment",
    0x0E => "Object Modification Time (Old)",
    0x0F => "Shared Message Table",
    0x10 => "Object Header Continuation",
    0x11 => "Symbol Table",
    0x12 => "Object Modification Time",
    0x13 => "B-tree K-values",
    0x14 => "Driver Info",
    0x15 => "Attribute Info",
    0x16 => "Object Reference Count"
)

struct HDF5Section
    offset::Int
    length::Int
    type::String
    description::String
    color::Symbol
end

"""
    annotated_hex_dump(file_path; start_offset=0, length=1024, auto_detect=true)

Display a hex dump with intelligent HDF5 structure annotations.
Automatically detects and highlights HDF5 structures.
"""
function annotated_hex_dump(file_path; start_offset=0, length=1024, auto_detect=true)
    open(file_path, "r") do io
        seek(io, start_offset)
        bytes = read(io, length)

        println("=== Annotated HDF5 Hex Dump ===")
        println("File: $file_path")
        println("Offset: 0x$(string(start_offset, base=16, pad=8)) - 0x$(string(start_offset + length - 1, base=16, pad=8))")
        println("Length: $length bytes")
        println()

        if auto_detect
            sections = detect_hdf5_structures(bytes, start_offset)
            print_annotated_sections(bytes, start_offset, sections)
        else
            print_basic_hex_dump(bytes, start_offset)
        end
    end
end

"""
    detect_hdf5_structures(bytes, base_offset)

Automatically detect HDF5 structures in the byte array.
"""
function detect_hdf5_structures(bytes, base_offset)
    sections = HDF5Section[]
    i = 1

    while i <= length(bytes) - 4
        # Check for known signatures
        for (sig, description) in HDF5_SIGNATURES
            if length(bytes) >= i + length(sig) - 1
                if bytes[i:i+length(sig)-1] == sig
                    section_length = estimate_structure_length(bytes, i, sig)
                    push!(sections, HDF5Section(
                        base_offset + i - 1,
                        section_length,
                        "signature",
                        description,
                        :green
                    ))
                    i += section_length
                    @goto continue_outer
                end
            end
        end

        # Check for HDF5 message headers (if in object header context)
        if i <= length(bytes) - 8
            potential_msg_type = bytes[i]
            if haskey(HDF5_MESSAGE_TYPES, potential_msg_type)
                msg_size = reinterpret(UInt16, bytes[i+2:i+3])[1]  # Assuming little-endian
                if msg_size > 0 && msg_size < 65536  # Reasonable message size
                    push!(sections, HDF5Section(
                        base_offset + i - 1,
                        8 + msg_size,  # Header + data
                        "message",
                        "$(HDF5_MESSAGE_TYPES[potential_msg_type]) Message",
                        :blue
                    ))
                    i += 8 + msg_size
                    @goto continue_outer
                end
            end
        end

        i += 1
        @label continue_outer
    end

    # Fill gaps with "unknown" sections
    fill_unknown_sections!(sections, base_offset, length(bytes))

    return sections
end

"""
    estimate_structure_length(bytes, start_pos, signature)

Estimate the length of an HDF5 structure based on its signature.
"""
function estimate_structure_length(bytes, start_pos, signature)
    if signature == b"TREE"
        # V1 B-tree node - read the entries count and estimate size
        if length(bytes) >= start_pos + 15
            # Read node type (offset 4), level (offset 5), entries_used (offset 6-7)
            entries_used = reinterpret(UInt16, bytes[start_pos+6:start_pos+7])[1]
            # Estimate based on entries_used (this is approximate)
            return 16 + entries_used * 24  # Basic estimate for V1 B-tree node
        end
        return 64  # Minimum reasonable size
    elseif signature == b"\x89HDF"
        return 8  # HDF5 signature is 8 bytes
    elseif signature == b"OHDR"
        # Object header - would need to parse size fields
        return 16  # Minimum estimate
    else
        return 16  # Default estimate
    end
end

"""
    fill_unknown_sections!(sections, base_offset, total_length)

Fill gaps between detected sections with "unknown" sections.
"""
function fill_unknown_sections!(sections, base_offset, total_length)
    sort!(sections, by=s -> s.offset)

    new_sections = HDF5Section[]
    current_pos = base_offset

    for section in sections
        # Add unknown section before this section
        if section.offset > current_pos
            push!(new_sections, HDF5Section(
                current_pos,
                section.offset - current_pos,
                "unknown",
                "Unknown/Data",
                :light_black
            ))
        end
        push!(new_sections, section)
        current_pos = section.offset + section.length
    end

    # Add unknown section at the end if needed
    if current_pos < base_offset + total_length
        push!(new_sections, HDF5Section(
            current_pos,
            base_offset + total_length - current_pos,
            "unknown",
            "Unknown/Data",
            :light_black
        ))
    end

    empty!(sections)
    append!(sections, new_sections)
end

"""
    print_annotated_sections(bytes, base_offset, sections)

Print hex dump with section annotations and colors.
"""
function print_annotated_sections(bytes, base_offset, sections)
    for section in sections
        start_idx = section.offset - base_offset + 1
        end_idx = min(start_idx + section.length - 1, length(bytes))

        # Section header
        println("\n$(color_text("▼ $(section.description)", section.color)) " *
                "[0x$(string(section.offset, base=16, pad=8)) - " *
                "0x$(string(section.offset + section.length - 1, base=16, pad=8)), " *
                "$(section.length) bytes]")

        # Hex dump of this section
        section_bytes = bytes[start_idx:end_idx]
        print_hex_dump_section(section_bytes, section.offset, section.color)
    end
end

"""
    print_hex_dump_section(bytes, base_offset, color)

Print hex dump for a specific section with color highlighting.
"""
function print_hex_dump_section(bytes, base_offset, color)
    for i in 1:16:length(bytes)
        chunk_end = min(i + 15, length(bytes))
        chunk = bytes[i:chunk_end]

        # Offset
        offset_str = string(base_offset + i - 1, base=16, pad=8)

        # Hex representation
        hex_str = join([string(b, base=16, pad=2) for b in chunk], " ")
        hex_str = hex_str * " " ^ (47 - length(hex_str))  # Pad to fixed width

        # ASCII representation
        ascii_str = join([32 <= b <= 126 ? Char(b) : '.' for b in chunk], "")

        # Apply color
        colored_hex = color_text(hex_str, color)
        colored_ascii = color_text(ascii_str, color)

        println("  $offset_str: $colored_hex |$colored_ascii|")
    end
end

"""
    print_basic_hex_dump(bytes, base_offset)

Print basic hex dump without structure detection.
"""
function print_basic_hex_dump(bytes, base_offset)
    for i in 1:16:length(bytes)
        chunk_end = min(i + 15, length(bytes))
        chunk = bytes[i:chunk_end]

        offset_str = string(base_offset + i - 1, base=16, pad=8)
        hex_str = join([string(b, base=16, pad=2) for b in chunk], " ")
        hex_str = hex_str * " " ^ (47 - length(hex_str))
        ascii_str = join([32 <= b <= 126 ? Char(b) : '.' for b in chunk], "")

        println("$offset_str: $hex_str |$ascii_str|")
    end
end

"""
    color_text(text, color)

Apply ANSI color codes to text (simplified version).
"""
function color_text(text, color)
    color_codes = Dict(
        :red => "\e[31m",
        :green => "\e[32m",
        :blue => "\e[34m",
        :yellow => "\e[33m",
        :magenta => "\e[35m",
        :cyan => "\e[36m",
        :light_black => "\e[90m",
        :default => ""
    )

    reset = "\e[0m"
    code = get(color_codes, color, "")
    return "$code$text$reset"
end

"""
    compare_hex_dumps(file1, file2; offset1=0, offset2=0, length=1024)

Compare hex dumps of two files side by side, highlighting differences.
"""
function compare_hex_dumps(file1, file2; offset1=0, offset2=0, length=1024)
    bytes1 = open(file1, "r") do io
        seek(io, offset1)
        read(io, length)
    end

    bytes2 = open(file2, "r") do io
        seek(io, offset2)
        read(io, length)
    end

    min_length = min(Base.length(bytes1), Base.length(bytes2))

    println("=== Comparative Hex Dump ===")
    println("File 1: $file1 (offset: 0x$(string(offset1, base=16, pad=8)))")
    println("File 2: $file2 (offset: 0x$(string(offset2, base=16, pad=8)))")
    println()

    differences = 0
    for i in 1:16:min_length
        chunk_end = min(i + 15, min_length)

        # File 1 chunk
        chunk1 = bytes1[i:chunk_end]
        hex1 = join([string(b, base=16, pad=2) for b in chunk1], " ")
        ascii1 = join([32 <= b <= 126 ? Char(b) : '.' for b in chunk1], "")

        # File 2 chunk
        chunk2 = bytes2[i:chunk_end]
        hex2 = join([string(b, base=16, pad=2) for b in chunk2], " ")
        ascii2 = join([32 <= b <= 126 ? Char(b) : '.' for b in chunk2], "")

        # Check for differences
        has_diff = chunk1 != chunk2
        if has_diff
            differences += 1
            hex1 = color_text(hex1, :red)
            hex2 = color_text(hex2, :red)
            ascii1 = color_text(ascii1, :red)
            ascii2 = color_text(ascii2, :red)
        end

        offset_str = string(offset1 + i - 1, base=16, pad=8)

        println("$offset_str: $(rpad(hex1, 60)) |$ascii1|")
        println("$offset_str: $(rpad(hex2, 60)) |$ascii2|")
        if has_diff
            println("         " * color_text("^^^^ DIFFERENCE ^^^^", :yellow))
        end
        println()
    end

    println("Total differences: $differences chunks")
end

"""
    find_hdf5_structure(file_path, structure_type)

Find all occurrences of a specific HDF5 structure type in a file.
"""
function find_hdf5_structure(file_path, structure_type)
    signatures_to_find = []
    if structure_type == "v1btree"
        push!(signatures_to_find, b"TREE")
    elseif structure_type == "object_header"
        push!(signatures_to_find, b"OHDR")
    elseif structure_type == "heap"
        push!(signatures_to_find, b"HEAP")
    else
        error("Unknown structure type: $structure_type")
    end

    locations = []
    open(file_path, "r") do io
        file_size = filesize(file_path)
        chunk_size = 64 * 1024  # 64KB chunks

        for offset in 0:chunk_size:file_size-1
            seek(io, offset)
            chunk = read(io, min(chunk_size, file_size - offset))

            for sig in signatures_to_find
                pos = 1
                while pos <= length(chunk) - length(sig) + 1
                    idx = findfirst(sig, chunk[pos:end])
                    if idx !== nothing
                        absolute_offset = offset + pos + idx[1] - 2
                        push!(locations, absolute_offset)
                        pos += idx[1]
                    else
                        break
                    end
                end
            end
        end
    end

    return unique(locations)
end

# Export functions
export annotated_hex_dump, compare_hex_dumps, find_hdf5_structure
