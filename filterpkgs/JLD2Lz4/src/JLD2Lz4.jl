"""
    JLD2Lz4

Transitional package to the extension package JLD2/Lz4Ext which implements the
Lz4 compression filter for JLD2.
Loading this package will load the package extension and export `Lz4Filter`.
"""
module JLD2Lz4

using JLD2: JLD2
using ChunkCodecLibLz4: ChunkCodecLibLz4
const CodecLz4Ext = Base.get_extension(JLD2, :Lz4Ext)

using .CodecLz4Ext: Lz4Filter

export Lz4Filter

end # module JLD2Lz4
