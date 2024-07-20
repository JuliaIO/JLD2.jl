
# Redefine unsafe_load, unsafe_store!, read, and write so that they pack the type
function define_packed(ty::DataType)
    @assert isbitstype(ty)
    packed_offsets = cumsum([jlsizeof(x) for x in ty.types])
    sz = pop!(packed_offsets)
    pushfirst!(packed_offsets, 0)

    if sz != jlsizeof(ty)
        @eval begin
            function jlunsafe_store!(p::Ptr{$ty}, x::$ty)
                $([:(jlunsafe_store!(pconvert(Ptr{$(ty.types[i])}, p+$(packed_offsets[i])), getfield(x, $i)))
                   for i = 1:length(packed_offsets)]...)
            end
            function jlunsafe_load(p::Ptr{$ty})
                $(Expr(:new, ty, [:(jlunsafe_load(pconvert(Ptr{$(ty.types[i])}, p+$(packed_offsets[i]))))
                                   for i = 1:length(packed_offsets)]...))
            end
            jlsizeof(::Union{$ty,Type{$ty}}) = $(Int(sz))::Int
        end
    end

    @eval begin
        @inline jlwrite(io::Union{MmapIO,BufferedWriter}, x::$ty) = _write(io, x)
        @inline jlread(io::Union{MmapIO,BufferedReader}, x::Type{$ty}) = _read(io, x)
        function jlread(io::IO, ::Type{$ty})
            $(Expr(:new, ty, [:(jlread(io, $(ty.types[i]))) for i = 1:length(packed_offsets)]...))
        end
        function jlwrite(io::IO, x::$ty)
            $([:(jlwrite(io, getfield(x, $i))) for i = 1:length(packed_offsets)]...)
            nothing
        end
    end
    nothing
end


###########################################################################################
# The following macro is used for declarative definition of Header Messages
###########################################################################################

function getprop end
function construct_hm_payload end
function sizefun end
function messageshow end

function linefun(ex)
    v = nothing
    kw = esc(:kw)
    ptr = esc(:ptr)
    io = esc(:io)
    offset = esc(:offset)
    if ex isa LineNumberNode
        return [ex, ex, ex, ex]
    elseif @capture(ex, cond_ && body_)
        rets = linefun(ex.args[2])
        return [ Expr(:&&, esc(cond), ret ) for ret in rets ]
    elseif @capture(ex, if cond_; body_; end )
        accs = build_fun_body((Any[], Any[], Any[], Any[]), body)
        return [ Expr(:if, esc(cond), Expr(:block, accs[i]...)) for i in 1:length(accs) ]
    elseif @capture(ex, s_Symbol::T_) || @capture(ex, s_Symbol::T_ = v_)
        getprop_ = :($(esc(s)) = $kw.$(s))
        default = Symbol(s,"_default")
        default_ = :($default = $(esc(v)))
        get_ = :($(esc(s)) = get($kw, $(QuoteNode(s)), $default))
        haskey_ = :(haskey($kw, $(QuoteNode(s))) || throw(ArgumentError($("Argument $(QuoteNode(s)) is required"))))

        if @capture(T, @FixedLengthString(len_))
            len = esc(len)
            read_statement = :(unsafe_string($ptr+$offset, Int($(len))::Int))
            read_io = :(String(jlread($io, UInt8, $(len))))
            increment = :($len)
            write_statement = :(jlwrite(io, $(esc(s))))
        elseif @capture(T, @Blob(len_))
            len = esc(len)
            read_statement = quote
                ptr = Ptr{UInt8}($ptr + $offset)
                blob = Vector{UInt8}(undef, $len)
                unsafe_copyto!(pointer(blob), ptr, $len)
                blob
            end
            read_io = :(jlread($io, UInt8, $(len)))
            increment = :($len)
            write_statement = :(jlwrite(io, $(esc(s))))
        elseif @capture(T, @Int(len_))
            len = esc(len)
            read_statement = quote
                ptr = Ptr{UInt8}($ptr + $offset)
                uint = zero(UInt64)
                for i in 1:$len
                    uint = uint | unsafe_load(ptr+i-1) << ((i-1)*8)
                end
                uint
            end
            read_io = quote
                uint = zero(UInt64)
                for i in 1:$len
                    uint = uint | jlread($io, UInt8) << ((i-1)*8)
                end
                uint
            end
            increment = :($len)
            #write_statement = :(jlwrite(io, $(esc(s))))
            write_statement = :(write_nb_int(io, $(esc(s)), $len))
        elseif @capture(T, @Offset)
            read_statement = :(getfield(hm,:payload_offset) + $offset)
            read_io = :(RelOffset(getfield(m,:offset) + position($io) - getfield(m,:address)))
            increment = 0
            getprop_ = nothing
            haskey_=nothing
            write_statement = nothing
        elseif @capture(T, @computed(expr_))
            @assert isnothing(v) "Defaults for @computed fields are not supported"
            read_io = read_statement = esc(expr)
            getprop_ = :($(esc(s)) = $(esc(expr)))
            increment = 0
            write_statement = nothing
            haskey_ = nothing
        elseif @capture(T, @read(type_, rsize_)) || @capture(T, @read(type_))
            read_statement = quote
                _io = IOBuffer(getfield(hm,:body))
                seek(_io, $offset)
                jlread(_io, $(esc(type)))
            end
            read_io = quote
                jlread($io, $(esc(type)))
            end
            write_statement = :(jlwrite(_io, $(esc(s))))
            if !isnothing(rsize)
                increment = esc(rsize)
            else
                increment = :(sizeof(typeof($(esc(s)))))
            end
        else
            T = esc(T)
            read_statement = :(unsafe_load(Ptr{$T}($ptr+$offset)))
            read_io = :(jlread($io, $T))
            increment = :(sizeof($(T)))
            write_statement = :(jlwrite(io, $T($(esc(s)))))
        end

        assign_statement = :($(esc(s)) = $read_statement)
        io_assign = :($(esc(s)) = $read_io)
        offset_incr = :($offset += $increment)
        
        return [
            # io_based getproperty function
            Expr(:block,
                io_assign,
                :(s == $(QuoteNode(s)) && return $(esc(s))),
                #offset_incr
                ),
            # writing function
            if !isnothing(v)
                Expr(:block, default_, get_, write_statement)
            else
                Expr(:block, haskey_, getprop_, write_statement)
            end,
            # size function
            if !isnothing(v)
                Expr(:block, default_, get_, offset_incr)
            else
                Expr(:block, haskey_, getprop_, offset_incr)
            end,
            # pretty printer
            Expr(:block, io_assign,
                :(push!(keyvalue, $(QuoteNode(s)) => $(esc(s)))))
        ]
    elseif @capture(ex, @skip(n_))
        increment = esc(n)
        off_inc = :($offset += $increment)
        io_inc = :(skip($io, $increment))
        write_inc = :(write_zerobytes(io, $increment))
        return [io_inc, write_inc, off_inc, off_inc]
    elseif @capture(ex, @skiptoaligned(n_))
        increment = :(8 - mod1($offset-$(esc(n)), 8))
        off_inc = :($offset += $increment)
        io_inc = :(skip($io, $increment))
        write_inc = :(write_zerobytes(io, $increment))
        return [io_inc, write_inc, off_inc, io_inc]
    end
    throw(ArgumentError("Invalid field syntax: $ex"))
end

function build_fun_body(accs, blk)
    for ex in blk.args
        rets = linefun(ex)
        for i in 1:length(accs)
            push!(accs[i], rets[i])
        end
    end
    return accs
end

macro pseudostruct(name, blck)
    iogetprop, funbody, sizefun, messageshowfun = 
        build_fun_body((Any[], Any[], Any[], Any[]), blck)

    iogetpropfun = (quote
        function $(esc(:iogetprop))(::Val{$name}, $(esc(:m))::Message, s::Symbol, $(esc(:hflags))=0x0, $(esc(:hsize))=0x0000)
            $(__source__)
            $(esc(:io)) = getfield(m, :io)
            seek($(esc(:io)), getfield(m, :address))
            $(iogetprop...)
            throw(ArgumentError("Field $s not found"))
        end
    end).args[2]

    constructfun = (quote
        function $(esc(:construct_hm_payload))(::Val{$name}, $(esc(:hflags)), $(esc(:hsize)), $(esc(:kw)))
            $(__source__)
            io = IOBuffer()
            $(funbody...)
            io
        end
    end).args[2]


    size_fun = (quote
        function $(esc(:sizefun))(::Val{$name}, $(esc(:hflags)), $(esc(:hsize)), $(esc(:kw)))
            $(__source__)
            $(esc(:offset)) = 0
            $(sizefun...)
            return $(esc(:offset))
        end
    end).args[2]

    message_show = (quote
        function $(esc(:messageshow))(::Val{$name}, $(esc(:m))::Message, $(esc(:hflags))=0x0, $(esc(:hsize))=0x0000)
            $(__source__)
            $(esc(:io)) = m.io
            seek($(esc(:io)), m.address)
            $(esc(:offset)) = 0
            keyvalue = Pair{Symbol,Any}[]
            $(messageshowfun...)
            return keyvalue
        end
    end).args[2]

    return Expr(:block, iogetpropfun, constructfun, size_fun, message_show, nothing)
end
