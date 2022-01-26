module A

export AType

struct AType
	x::Int
end

struct SameNameType{T}
    x::T
end

end