# The `safe_realpath` function is based on:
# https://github.com/JuliaLang/Pkg.jl/blob/8f97b18e2ceac28a2ac241385dcef1613328924e/src/utils.jl
function safe_realpath(path)
    ispath(path) && return realpath(path)
    a, b = splitdir(path)
    return joinpath(safe_realpath(a), b)
end
