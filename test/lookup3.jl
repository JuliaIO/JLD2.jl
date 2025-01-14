using JLD2, Test
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
large_buf = Vector{UInt8}(undef, 3093)
for i = 1:length(large_buf)
    large_buf[i] = ((i-1) * 3) % UInt8
end
@test JLD2.Lookup3.hash(large_buf) == 0x1bd2ee7b
@test JLD2.Lookup3.hash(fill!(large_buf, 0)) == 0x930c7afc

lookup3_test_hashes = [
    0x276a0407, 0xc4f3b847, 0x3253e887, 0xf0dbeea6, 0xa496ca89, 0xa2773e81,
    0xa88b6e6c, 0x2ca474f0, 0xe38ce8aa, 0xdb610bd1, 0x17f84daf, 0xccda323b,
    0x114345a2, 0xc01e7c57, 0x636342ab, 0x17bd0180, 0xbed86ff9, 0xae721167,
    0x9bc026d8, 0xaf53f65a, 0xbad83ad7, 0xcf6e279e, 0x8806c437, 0x4eaa9b13,
    0x2b885021, 0x769e8a62, 0x7e5372be, 0xa70fa8be, 0x8b7a3c59, 0x17770551,
]
for (i, h) in enumerate(lookup3_test_hashes)
    @test JLD2.Lookup3.hash(b"Four score and seven years ago"[begin:i]) == h
end
