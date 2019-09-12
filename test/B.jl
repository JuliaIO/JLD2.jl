module B

using A

export BType

struct BType
	x::AType
end

end