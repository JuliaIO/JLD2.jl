"""
    JLD2Blosc

Transitional package to JLD2/BloscExt.

The contents of this package are contained within the package extension
BloscExt. Loading this package will load the package extension.
"""
module JLD2Blosc

using JLD2: JLD2
using Blosc: Blosc, SHUFFLE, BITSHUFFLE, NOSHUFFLE
const BloscExt = Base.get_extension(JLD2, :BloscExt)

using .BloscExt: BloscFilter

export BloscFilter

end # module JLD2Blosc
