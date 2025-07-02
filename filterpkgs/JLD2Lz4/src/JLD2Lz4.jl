"""
    JLD2Lz4

Transitional package to the extension package JLD2/CodecLz4Ext which implements the
Lz4 compression filter for JLD2.
Loading this package will load the package extension.
"""
module JLD2Lz4

using JLD2: JLD2
using CodecLz4: CodecLz4
const CodecLz4Ext = Base.get_extension(JLD2, :CodecLz4Ext)

using .CodecLz4Ext: Lz4Filter

export Lz4Filter

end # module JLD2Lz4
