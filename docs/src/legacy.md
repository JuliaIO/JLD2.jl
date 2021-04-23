# Legacy

This page lists features of JLD2 that are kept for legacy purposes.
In particular, the following sections describes the `@load` and `@save` macros.
They have been the default for many users but they unnecessarily introduce
new macro-based syntax. Over time a range of issues have been opened by
new users struggling with them. Since their inception, the julia language 
has improved significantly and macros may no longer be necessary in this case.


### `@save` and `@load` macros

The `@save` and `@load` macros are the simplest way to interact with a JLD2 file. The `@save` macro writes one or more variables from the current scope to the JLD2 file. For example:

```julia
using JLD2
hello = "world"
foo = :bar
@save "example.jld2" hello foo
```

This writes the variables `hello` and `foo` to datasets in a new JLD2 file named `example.jld2`. The `@load` macro loads variables out of a JLD2 file:

```julia
@load "example.jld2" hello foo
```

This assigns the contents of the `hello` and `foo` datasets to variables of the same name in the current scope.

It is best practice to explicitly name the variables to be loaded and saved from a file, so that it is clear from whence these variables arise. However, for convenience, JLD2 also provides variants of `@load` and `@save` that do not require variables to be named explicitly. When called with no variable arguments, `@save <filename>` writes all variables in the global scope of the current module to file `<filename>`, while `@load <filename>` loads all variables in file `<filename>`. When called with no variable arguments, `@load` requires that the file name is provided as a string literal, i.e., it is not possible to select the file at runtime.

Additional customization is possible using assignment syntax and option passing:

```
@save "example.jld2" bye=hello bar=foo
@save "example.jld2" {compress=true} hello bar=foo
```

```@docs
@save
@load
```