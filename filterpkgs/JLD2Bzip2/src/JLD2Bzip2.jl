"""
    JLD2Bzip2

Transitional package to JLD2/CodecBzip2Ext.

The contents of this package are now contained within the package extension
CodecBzip2Ext. Loading this package will load the package extension.
"""
module JLD2Bzip2

using JLD2: JLD2
using CodecBzip2: CodecBzip2
const CodecBzip2Ext = Base.get_extension(JLD2, :CodecBzip2Ext)

using .CodecBzip2Ext: Bzip2Filter

export Bzip2Filter

end # module JLD2Bzip2
