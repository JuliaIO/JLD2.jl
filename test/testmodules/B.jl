module B

using A

export BType

struct BType
	x::AType
end

struct SameNameType
    y::Int
end

end