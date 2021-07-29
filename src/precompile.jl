# Precompile statements generated using SnoopCompile, via:
#   julia> using SnoopCompile, Test, FileIO
#   julia> tinf = @snoopi_deep begin include("test/rw.jl"); include("test/loadsave.jl"); end
#   julia> ttot, pcs = ttot, pcs = SnoopCompile.parcel(tinf; tmin=0.05)
#   julia> SnoopCompile.write("tmp", pcs)
# With the cutoff at tmin = 0.05, this accounts for about 40% of overall inference time
# spent in executing /test/rw.jl and /test/loadsave.jl
# A single CodecZlib argument call was filtered out manually.

function _precompile_()
    ccall(:jl_generating_output, Cint, ()) == 1 || return nothing

    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Float64})   # time: 0.8394732
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Base.ReshapedArray{Int, 2, UnitRange{Int}, Tuple{}}})   # time: 0.5689673
    Base.precompile(Tuple{Core.kwftype(typeof(jldopen)),NamedTuple{(:compress,), Tuple{Bool}},typeof(jldopen),String,Bool,Bool,Bool,Type{MmapIO}})   # time: 0.3199544
    Base.precompile(Tuple{Core.kwftype(typeof(write)),NamedTuple{(:compress,), Tuple{Nothing}},typeof(write),Group{JLDFile{MmapIO}},String,Base.ReshapedArray{Int, 2, UnitRange{Int}, Tuple{}},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.2051841
    Base.precompile(Tuple{typeof(read_data),JLDFile{MmapIO},ReadRepresentation{ComplexF64, ComplexF64},Tuple{ReadDataspace, RelOffset, Int, UInt16},Vector{ReadAttribute}})   # time: 0.187065
    Base.precompile(Tuple{typeof(read_data),JLDFile{MmapIO},ReadRepresentation{BitMatrix, OnDiskRepresentation{(0, 8, 16), Tuple{Vector{UInt}, Int, Tuple{Int, Int}}, Tuple{RelOffset, Int, Tuple{Int, Int}}}()},Tuple{ReadDataspace, RelOffset, Int, UInt16},Vector{ReadAttribute}})   # time: 0.1830797
    Base.precompile(Tuple{Core.kwftype(typeof(write)),NamedTuple{(:compress,), Tuple{Nothing}},typeof(write),Group{JLDFile{IOStream}},String,Base.ReshapedArray{Int, 2, UnitRange{Int}, Tuple{}},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.172313
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Dict{Symbol, String}})   # time: 0.1482486
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,IdDict{Symbol, String}})   # time: 0.14119
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Vector{Vector{Float64}}})   # time: 0.141007
    Base.precompile(Tuple{typeof(constructrr),JLDFile{MmapIO},DataType,CompoundDatatype,Vector{ReadAttribute}})   # time: 0.1277466
    Base.precompile(Tuple{typeof(h5fieldtype),JLDFile{MmapIO},Type{UnitRange{Int}},Type{UnitRange{Int}},Type{Val{true}}})   # time: 0.1257442
    Base.precompile(Tuple{typeof(write_dataset),JLDFile{MmapIO},WriteDataspace{0, Tuple{}},CommittedDatatype,OnDiskRepresentation{(0, 8, 16), Tuple{Vector{UInt}, Int, Tuple{Int, Int}}, Tuple{RelOffset, Int, Tuple{Int, Int}}},BitMatrix,JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.1243035
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Dict{String, String},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.1156945
    Base.precompile(Tuple{typeof(read_data),JLDFile{MmapIO},ReadRepresentation{DataType, OnDiskRepresentation{(0, 16), Tuple{String, Vector{Any}}, Tuple{Vlen{String}, Vlen{RelOffset}}}()},Tuple{ReadDataspace, RelOffset, Int, UInt16},Nothing})   # time: 0.115149
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Vector{Vector{U} where U<:Integer}})   # time: 0.10986
    Base.precompile(Tuple{typeof(getindex),Group{JLDFile{IOStream}},String})   # time: 0.1069834
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Vector{Array{Int}}})   # time: 0.1035001
    Base.precompile(Tuple{typeof(setindex!),JLDFile{MmapIO},Vector{Union{Missing, Bool}},String})   # time: 0.0995374
    Base.precompile(Tuple{typeof(close),JLDFile{MmapIO}})   # time: 0.0988005
    Base.precompile(Tuple{typeof(read_data),JLDFile{MmapIO},ReadRepresentation{Base.ImmutableDict{Symbol, String}, CustomSerialization{Array, RelOffset}},Tuple{ReadDataspace, RelOffset, Int, UInt16},Vector{ReadAttribute}})   # time: 0.0972386
    Base.precompile(Tuple{Core.kwftype(typeof(write)),NamedTuple{(:compress,), Tuple{Nothing}},typeof(write),Group{JLDFile{MmapIO}},String,Dict{String, String},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0919869
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Tuple{Int, Int, Tuple{Int, Int, Vector{Int}}, Vector{Int}}})   # time: 0.0905843
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Vector{Tuple{}}})   # time: 0.0843623
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Tuple{Int, String}})   # time: 0.0840445
    Base.precompile(Tuple{typeof(read),JLDFile{MmapIO},String})   # time: 0.0816563
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,BigFloat})   # time: 0.0796746
    Base.precompile(Tuple{typeof(h5convert!),IndirectPointer,Type{CustomSerialization{Vector{Pair{Any, Any}}, RelOffset}},JLDFile{MmapIO},IdDict,JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0794924
    Base.precompile(Tuple{typeof(write_dataset),JLDFile{IOStream},WriteDataspace{0, Tuple{}},CommittedDatatype,OnDiskRepresentation{(0, 8, 16), Tuple{Vector{UInt}, Int, Tuple{Int, Int}}, Tuple{RelOffset, Int, Tuple{Int, Int}}},BitMatrix,JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0782433
    Base.precompile(Tuple{Core.kwftype(typeof(write)),NamedTuple{(:compress,), Tuple{Nothing}},typeof(write),Group{JLDFile{MmapIO}},String,Float64,JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0779112
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Vector{Vector{Int}}})   # time: 0.0767249
    Base.precompile(Tuple{typeof(write_dataset),JLDFile{MmapIO},WriteDataspace{1, Tuple{WrittenAttribute{WriteDataspace{0, Tuple{}}, BasicDatatype, RelOffset}}},BasicDatatype,Type{RelOffset},Vector{Union{Missing, Float32, Float64, Int32}},JLDWriteSession{Dict{UInt, RelOffset}},Bool})   # time: 0.0764436
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Base.ReshapedArray{ComplexF64, 1, Base.ReinterpretArray{ComplexF64, 2, Float64, Matrix{Float64}, false}, Tuple{}}})   # time: 0.0764319
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,BitMatrix})   # time: 0.0733186
    Base.precompile(Tuple{Core.kwftype(typeof(write)),NamedTuple{(:compress,), Tuple{Nothing}},typeof(write),Group{JLDFile{MmapIO}},String,Vector{Array{Int}},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0726416
    Base.precompile(Tuple{typeof(h5type),JLDFile{MmapIO},Type,Base.ReshapedArray{Int, 2, UnitRange{Int}, Tuple{}}})   # time: 0.0719121
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,ComplexF32})   # time: 0.07121
    Base.precompile(Tuple{Core.kwftype(typeof(write)),NamedTuple{(:compress,), Tuple{Nothing}},typeof(write),Group{JLDFile{IOStream}},String,Vector{Array{Int}},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0702584
    Base.precompile(Tuple{typeof(write_dataset),JLDFile{MmapIO},WriteDataspace{0, Tuple{}},CommittedDatatype,OnDiskRepresentation{(0, 8), Tuple{TypeVar, Any}, Tuple{RelOffset, RelOffset}},Type,JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0697
    Base.precompile(Tuple{Core.kwftype(typeof(write)),NamedTuple{(:compress,), Tuple{Nothing}},typeof(write),Group{JLDFile{MmapIO}},String,Dict{Symbol, String},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0691246
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,SubArray{Int, 1, Vector{Int}, Tuple{UnitRange{Int}}, true}})   # time: 0.0687224
    Base.precompile(Tuple{typeof(write_ref),JLDFile{IOStream},Vector{Int},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0681966
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Base.ReshapedArray{Int, 4, UnitRange{Int}, Tuple{}}})   # time: 0.068107
    Base.precompile(Tuple{typeof(setindex!),JLDFile{MmapIO},Vector{Union{Float64, Int}},String})   # time: 0.0679686
    Base.precompile(Tuple{typeof(write_ref),JLDFile{MmapIO},LineNumberNode,JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0676823
    Base.precompile(Tuple{typeof(read_array),JLDFile{MmapIO},ReadDataspace,ReadRepresentation{Union{Int, Vector}, RelOffset},Int,UInt16,RelOffset,Vector{ReadAttribute}})   # time: 0.0675551
    Base.precompile(Tuple{typeof(setindex!),JLDFile{MmapIO},Vector{Union{Missing, Float32, Float64, Int32}},String})   # time: 0.0671966
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Vector{Union{Nothing, Int}}})   # time: 0.0671739
    Base.precompile(Tuple{typeof(setindex!),JLDFile{MmapIO},Vector{Union{Int, Vector}},String})   # time: 0.0666241
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Complex{Int}})   # time: 0.066388
    Base.precompile(Tuple{typeof(write_ref),JLDFile{MmapIO},Tuple{Symbol},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0653194
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Base.GitVersionInfo})   # time: 0.0652343
    Base.precompile(Tuple{typeof(write),JLDFile{IOStream},String,Vector{Char}})   # time: 0.0641642
    Base.precompile(Tuple{Core.kwftype(typeof(write)),NamedTuple{(:compress,), Tuple{Nothing}},typeof(write),Group{JLDFile{MmapIO}},String,NamedTuple{(:ptr,), Tuple{Ptr{Float64}}},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0624435
    Base.precompile(Tuple{Core.kwftype(typeof(write)),NamedTuple{(:compress,), Tuple{Nothing}},typeof(write),Group{JLDFile{IOStream}},String,Dict{Symbol, String},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0622999
    Base.precompile(Tuple{typeof(jldopen),AbstractString})   # time: 0.061449
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Expr})   # time: 0.0613437
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,NamedTuple{(:ptr,), Tuple{Ptr{Float64}}},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0605991
    Base.precompile(Tuple{Core.kwftype(typeof(write)),NamedTuple{(:compress,), Tuple{Nothing}},typeof(write),Group{JLDFile{IOStream}},String,Tuple{Int, Int, Tuple{Int, Int, Vector{Int}}, Vector{Int}},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0597551
    Base.precompile(Tuple{typeof(write_ref_mutable),JLDFile{IOStream},Matrix{Int},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0595954
    Base.precompile(Tuple{Core.kwftype(typeof(write)),NamedTuple{(:compress,), Tuple{Nothing}},typeof(write),Group{JLDFile{IOStream}},String,Vector{Vector{U} where U<:Integer},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0593596
    Base.precompile(Tuple{Core.kwftype(typeof(write)),NamedTuple{(:compress,), Tuple{Nothing}},typeof(write),Group{JLDFile{IOStream}},String,Vector{Tuple{}},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0589484
    Base.precompile(Tuple{typeof(read_data),JLDFile{MmapIO},ReadRepresentation{ComplexF32, ComplexF32},Tuple{ReadDataspace, RelOffset, Int, UInt16},Vector{ReadAttribute}})   # time: 0.0588968
    Base.precompile(Tuple{Core.kwftype(typeof(write)),NamedTuple{(:compress,), Tuple{Nothing}},typeof(write),Group{JLDFile{IOStream}},String,Base.ReshapedArray{ComplexF64, 1, Base.ReinterpretArray{ComplexF64, 2, Float64, Matrix{Float64}, false}, Tuple{}},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0586527
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Matrix{Int}})   # time: 0.058283
    Base.precompile(Tuple{typeof(setindex!),JLDFile{IOStream},Vector{UInt8},String})   # time: 0.058052
    Base.precompile(Tuple{typeof(write_ref),JLDFile{IOStream},LineNumberNode,JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0580383
    Base.precompile(Tuple{typeof(constructrr),JLDFile{IOStream},DataType,CompoundDatatype,Vector{ReadAttribute}})   # time: 0.0569429
    Base.precompile(Tuple{typeof(write_heap_object),JLDFile{IOStream},Type,Vector{RelOffset},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.056889
    Base.precompile(Tuple{typeof(write_ref),JLDFile{MmapIO},Matrix{Float64},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0561273
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Array{Int, 0}})   # time: 0.0560988
    Base.precompile(Tuple{Core.kwftype(typeof(write)),NamedTuple{(:compress,), Tuple{Nothing}},typeof(write),Group{JLDFile{IOStream}},String,Float64,JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0560133
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Vector{Vector{U} where U>:Int}})   # time: 0.0542099
    Base.precompile(Tuple{Core.kwftype(typeof(write)),NamedTuple{(:compress,), Tuple{Nothing}},typeof(write),Group{JLDFile{MmapIO}},String,Expr,JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0540752
    Base.precompile(Tuple{Core.kwftype(typeof(write)),NamedTuple{(:compress,), Tuple{Nothing}},typeof(write),Group{JLDFile{IOStream}},String,Vector{Vector{Float64}},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0536389
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Vector{Vector{U} where Int<:U<:Real}})   # time: 0.0536368
    Base.precompile(Tuple{Core.kwftype(typeof(write)),NamedTuple{(:compress,), Tuple{Nothing}},typeof(write),Group{JLDFile{IOStream}},String,Vector{Any},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0534297
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Base.ReshapedArray{Int, 3, UnitRange{Int}, Tuple{}}})   # time: 0.0530808
    Base.precompile(Tuple{Core.kwftype(typeof(write)),NamedTuple{(:compress,), Tuple{Nothing}},typeof(write),Group{JLDFile{MmapIO}},String,SubArray{Int, 1, Vector{Int}, Tuple{UnitRange{Int}}, true},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0526628
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Matrix{Float64}})   # time: 0.0525738
    Base.precompile(Tuple{typeof(write_dataset),JLDFile{MmapIO},WriteDataspace{0, Tuple{}},CommittedDatatype,OnDiskRepresentation{(0, 8), Tuple{Int, Union{Nothing, Symbol}}, Tuple{Int, RelOffset}},LineNumberNode,JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0525689
    Base.precompile(Tuple{typeof(h5fieldtype),JLDFile{MmapIO},Type{Tuple{Int, Int, Int}},Type{Tuple{Int, Int, Int}},Type{Val{true}}})   # time: 0.0525214
    Base.precompile(Tuple{typeof(h5convert!),IndirectPointer,Type{RelOffset},JLDFile{MmapIO},Array{Float64},JLDWriteSession{Dict{UInt, RelOffset}}})   # time: 0.0522247
    Base.precompile(Tuple{typeof(write),JLDFile{MmapIO},String,Vector{DataType}})   # time: 0.0515368
    Base.precompile(Tuple{typeof(fileio_save),File{FileIO.DataFormat{:JLD2}, String},String,String,String,Vararg{Any}})   # time: 0.0513424
    Base.precompile(Tuple{typeof(h5type),JLDFile{IOStream},Type,Base.ReshapedArray{Int, 2, UnitRange{Int}, Tuple{}}})   # time: 0.050349
    Base.precompile(Tuple{typeof(read_data),JLDFile{IOStream},FixedLengthString{String},Tuple{ReadDataspace, RelOffset, Int, UInt16},Vector{ReadAttribute}})   # time: 0.0502741
    Base.precompile(Tuple{typeof(read_data),JLDFile{IOStream},ReadRepresentation{LineNumberNode, OnDiskRepresentation{(0, 8), Tuple{Int, Union{Nothing, Symbol}}, Tuple{Int, RelOffset}}()},Tuple{ReadDataspace, RelOffset, Int, UInt16},Vector{ReadAttribute}})   # time: 0.0500727
end
