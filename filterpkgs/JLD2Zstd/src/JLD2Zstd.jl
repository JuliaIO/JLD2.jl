"""
    JLD2Zstd

Transitional package to the extension package JLD2/CodecZstdExt which implements the
Zstd compression filter for JLD2.
Loading this package will load the package extension.
"""
module JLD2Zstd

using JLD2: JLD2
using CodecZstd: CodecZstd
const CodecZstdExt = Base.get_extension(JLD2, :CodecZstdExt)

using .CodecZstdExt: ZstdFilter

export ZstdFilter

end # module JLD2Zstd
