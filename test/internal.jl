using JLD2, Base.Test

function writeloop(f, sz)
    for i = 1:sz
        write(f, UInt8(6))
    end
end

# Force growing an MmapIO
name, io = mktemp()
f = JLD2.MmapIO(name, true, true, true)
close(io)
sz = JLD2.MMAP_GROW_SIZE+5
writeloop(f, sz)
JLD2.truncate_and_close(f, position(f))

@test filesize(name) == sz
f = open(name)
rd = read(f, UInt8, sz)
@test all(rd .== UInt8(6))
close(f)

f = JLD2.MmapIO(name, true, true, true)
write(f, rd)
JLD2.truncate_and_close(f, position(f))

@test filesize(name) == sz
f = open(name)
rd = read(f, UInt8, sz)
@test all(rd .== UInt8(6))
close(f)

@test JLD2.size_flag(1) === UInt8(0)
@test JLD2.size_flag(256) === UInt8(1)
@test JLD2.size_flag(65536) === UInt8(2)
@test JLD2.size_flag(UInt64(4294967296)) === UInt8(3)

buf = IOBuffer()
JLD2.write_size(buf, 175)
seek(buf, 0)
@test read(buf, UInt8) == 175
seek(buf, 0)
@test JLD2.read_size(buf, UInt8(0)) == 175
seek(buf, 0)

JLD2.write_size(buf, 42580)
seek(buf, 0)
@test read(buf, UInt16) == 42580
seek(buf, 0)
@test JLD2.read_size(buf, UInt8(1)) == 42580
seek(buf, 0)

JLD2.write_size(buf, 1902153053)
seek(buf, 0)
@test read(buf, UInt32) == 1902153053
seek(buf, 0)
@test JLD2.read_size(buf, UInt8(2)) == 1902153053
seek(buf, 0)

if Int == Int64
    # Only test this on 64-bit platforms. We can't read data of size >4GB on 32-bit anyway.
    JLD2.write_size(buf, 3804306107)
    seek(buf, 0)
    @test read(buf, UInt32) == 3804306107
    seek(buf, 0)
    @test JLD2.read_size(buf, UInt8(2)) == 3804306107
    seek(buf, 0)

    JLD2.write_size(buf, 7832227080891617460)
    seek(buf, 0)
    @test read(buf, UInt64) == 7832227080891617460
    seek(buf, 0)
    @test JLD2.read_size(buf, UInt8(3)) == 7832227080891617460
    seek(buf, 0)
end

@test JLD2.size_size(1) === 1
@test JLD2.size_size(256) === 2
@test JLD2.size_size(65536) === 4
@test JLD2.size_size(UInt64(4294967296)) === 8
