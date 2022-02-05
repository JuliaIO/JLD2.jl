@static if VERSION < v"1.7.0-A"
    # Location of `mutable` flag is moved from datatype to typename in julia v1.7
    # Switch to using accessor function added in v1.7

    # Borrowed from julia base
    function ismutabletype(@nospecialize(t::Type))
        t = Base.unwrap_unionall(t)
        # TODO: what to do for `Union`?
        return isa(t, DataType) && t.mutable
    end
end

@static if VERSION < v"1.7.0-A"
    #if :ninitialized in fieldnames(DataType)
    # https://github.com/JuliaIO/JLD2.jl/issues/327
    function ninitialized(@nospecialize(T::Type))::Int
        T.ninitialized
    end
else
    function ninitialized(@nospecialize(T::Type))::Int
        fieldcount(T) - T.name.n_uninitialized
    end
end