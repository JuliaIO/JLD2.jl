using JLD2, Base.Test
@test JLD2.Lookup3.hash(UInt8[]) == 0xdeadbeef
@test JLD2.Lookup3.hash(UInt8[], 1, 0, 0xdeadbeef) == 0xbd5b7dde
@test JLD2.Lookup3.hash(UInt8[23]) == 0xa209c931
@test JLD2.Lookup3.hash(UInt8[0]) == 0x8ba9414b
@test JLD2.Lookup3.hash(UInt8[23, 187]) == 0x8ba7a6c9
@test JLD2.Lookup3.hash(UInt8[0, 0]) == 0x62cd61b3
@test JLD2.Lookup3.hash(UInt8[23, 187, 98]) == 0xcebdf4f0
@test JLD2.Lookup3.hash(UInt8[0, 0, 0]) == 0x6bd0060f
@test JLD2.Lookup3.hash(UInt8[23, 187, 98, 217]) == 0x2c88bb51
@test JLD2.Lookup3.hash(UInt8[0, 0, 0, 0]) == 0x049396b8
large_buf = Vector{UInt8}(3093)
for i = 1:length(large_buf)
    large_buf[i] = ((i-1) * 3) % UInt8
end
@test JLD2.Lookup3.hash(large_buf) == 0x1bd2ee7b
@test JLD2.Lookup3.hash(fill!(large_buf, 0)) == 0x930c7afc
