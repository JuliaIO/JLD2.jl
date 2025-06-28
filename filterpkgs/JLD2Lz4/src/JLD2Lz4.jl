"""
    JLD2Lz4

Transitional package to JLD2/CodecLz4Ext.

The contents of this package are now contained within the package extension
CodecLz4Ext. Loading this package will load the package extension.
"""
module JLD2Lz4

using JLD2: JLD2
using CodecLz4: CodecLz4
const Lz4Ext = Base.get_extension(JLD2, :Lz4Ext)

using .Lz4Ext: Lz4Filter

export Lz4Filter

end # module JLD2Lz4
