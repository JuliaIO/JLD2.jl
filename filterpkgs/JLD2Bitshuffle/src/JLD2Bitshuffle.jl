"""
    JLD2Bitshuffle

Transitional package to JLD2/bitshuffle_jllExt.

The contents of this package are now contained within the package extension
bitshuffle_jllExt. Loading this package will load the package extension.
"""
module JLD2Bitshuffle

using JLD2: JLD2
using bitshuffle_jll: bitshuffle_jll
const bitshuffle_jllExt = Base.get_extension(JLD2, :bitshuffle_jllExt)

using .bitshuffle_jllExt: BitshuffleFilter

export BitshuffleFilter

end # module JLD2Bitshuffle
