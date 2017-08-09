using JLD2, Base.Test

for ioty in [JLD2.MmapIO, IOStream]
    fn = joinpath(tempdir(), "test.jld")

    # Ensure we can handle cases where the same file is open multiple times
    f = jldopen(fn, true, true, true, ioty)
    write(f, "hello", "world")
    @test_throws ArgumentError jldopen(fn, true, true, true, ioty)
    f2 = jldopen(fn, true, false, false, ioty)
    write(f2, "this", "works")
    close(f2)
    write(f, "still", "works")
    close(f)

    f = jldopen(fn, false, false, false, ioty)
    @test read(f, "hello") == "world"
    @test read(f, "this") == "works"
    @test read(f, "still") == "works"
    @test_throws ArgumentError jldopen(fn, true, false, false, ioty)
    @test_throws ArgumentError jldopen(fn, false, false, false, ioty; compress=true)
    @test_throws ArgumentError jldopen(fn, false, false, false, ioty; mmaparrays=true)
    close(f)

    # Make sure that the file gets written properly regardless of the order in which the
    # IO and JLDFile are finalized
    for order in [[1, 2], [2, 1]]
        f = jldopen(fn, true, true, true, ioty)
        write(f, "x", 1:10)

        objs = isa(f, JLD2.JLDFile{IOStream}) ? [f, f.io] : [f, f.io.f]
        for i in order
            finalize(objs[i])
        end

        f = jldopen(fn, false, false, false, ioty)
        @test read(f, "x") == 1:10
        close(f)
    end

    cmd = """
    using JLD2
    f = jldopen($(repr(fn)), true, true, true, $(ioty))
    write(f, "x", 1:10)
    """
    run(`$(Base.julia_cmd()) -e $cmd`)

    f = jldopen(fn, false, false, false, ioty)
    @test read(f, "x") == 1:10
    close(f)
end
