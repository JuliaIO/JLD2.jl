"""
    JLD2Zstd

Transitional package to JLD2/CodecZstdExt.

The contents of this package are now contained within the package extension
CodecZstdExt. Loading this package will load the package extension.
"""
module JLD2Zstd

using JLD2: JLD2
using CodecZstd: CodecZstd
const CodecZstdExt = Base.get_extension(JLD2, :CodecZstdExt)

using .CodecZstdExt: H5Z_FILTER_ZSTD
using .CodecZstdExt: zstd_name
using .CodecZstdExt: ZstdFilter

export ZstdFilter

end # module JLD2Zstd
