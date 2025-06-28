"""
    JLD2Bitshuffle

Transitional package to JLD2/BitshuffleExt.

The contents of this package are now contained within the package extension
BitshuffleExt. Loading this package will load the package extension.
"""
module JLD2Bitshuffle

using JLD2: JLD2
using bitshuffle_jll: bitshuffle_jll
const BitshuffleExt = Base.get_extension(JLD2, :BitshuffleExt)

using .BitshuffleExt: BitshuffleFilter

export BitshuffleFilter

end # module JLD2Bitshuffle
