"""
    JLD2Deflate

Transitional package to JLD2/CodecZlibExt.

Loading this package will load the package extension and enable the compression filter.
"""
module JLD2Deflate

using JLD2: JLD2
using CodecZlib: CodecZlib
const CodecZlibExt = Base.get_extension(JLD2, :CodecZlibExt)

using .CodecZlibExt: H5Z_FILTER_ZLIB
using .CodecZlibExt: zlib_name
using .CodecZlibExt: Deflate

export Deflate

end # module JLD2Deflate
