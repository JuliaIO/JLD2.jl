# Welcome to **JLD2.jl**

**JLD2.jl** is a high-performance, pure Julia library for saving and loading arbitrary Julia data structures. It's designed as a fast binary serialization format that produces files according to the well-known HDF5 format but doesn't rely on external libraries like the HDF5 C-library — making it an ideal choice for native Julia workflows.

---

## 📦 What is JLD2.jl?

JLD2 stands for **Julia Data Format 2**, a file format and serialization library tailored to work seamlessly with native Julia types. Whether you're working with arrays, dictionaries, structs, or custom types, JLD2 provides a reliable way to store your data to disk and retrieve it later — all in pure Julia.

---

## 🚀 Why Use JLD2?

* ✅ **Save and load any Julia type**, including custom structs
* ✅ **Fast and efficient**, thanks to native Julia code
* ✅ **Custom IO support** — can read and write to either `Vector{UInt8}` or custom IO implementations
* ✅ **Portable within Julia versions** — great for caching, prototyping, and long-term storage
* ✅ **Customizable type loading** — offers fine-grained control to update outdated stored structures upon load


---

## ✨ Example: Saving and Loading Data

```julia
using JLD2

# Some sample data
model = (name = "Transformer", layers = 12, params = 300_000_000)
scores = [0.91, 0.87, 0.93]

# Save to a file
@save "model_state.jld2" model scores

# Load back later
@load "model_state.jld2" model scores

println(model.name)  # Output: Transformer
```

You can also save individual variables or load just what you need:

```julia
@save "data.jld2" a=1 b=[1,2,3] c="hi"
@load "data.jld2" b c
```

---

## 📊 Related Serialization libraries in Julia

Below is a comparison of JLD2 and other common Julia libraries for data storage and serialization.
Choose the tool that best fits your needs:

| Library         | Best For                                         | Notes                                                                                 |
|-----------------|--------------------------------------------------|---------------------------------------------------------------------------------------|
| **JLD2.jl**        | Native Julia data (structs, arrays, Dicts, etc.) | Fast, flexible, pure Julia, HDF5-compatible, no external dependencies                 |
| **Serialization.jl** | Any Julia object         | Built-in, very fast, but not cross-version safe or portable outside Julia             |
| **MAT.jl**      | Interop with MATLAB `.mat` files                 | Read/write MATLAB files, good for sharing with MATLAB users                           |
| **HDF5.jl**     | Full-featured HDF5 access                        | Direct interface to HDF5 C library, cross-language, supports advanced HDF5 features   |
| **Arrow.jl**    | Tabular data, dataframes, analytics              | Excellent for columnar data, cross-language (Python, R, etc.), DataFrame support      |
| **CSV.jl**      | Lightweight table exports/imports                | Great for human-readable, row-based data, simple and widely supported                 |
| **JSON.jl**     | Web services, config files, nested dicts         | Portable, text-based, less efficient for complex Julia types                          |

If your goal is to store structured tables (like `DataFrame`s) for analysis or sharing between tools, **Arrow.jl** or **CSV.jl** may be a better fit. But if you're dealing with **rich, nested, Julia-native data**, or just want a fast way to persist your models, simulations, or internal state, **JLD2** is the right tool.

---

## 💬 Contributing

JLD2 is community-driven! We welcome contributions, bug reports, and suggestions at the [GitHub repository](https://github.com/JuliaIO/JLD2.jl).

If you're interested in improving documentation, fixing issues, or proposing new features, we’d love your help.

---

## 🧭 Next Steps

Use the sidebar to navigate:

* [Usage Guide](./basic_usage.md)
* [Custom serialization](./advanced.md)
* [Compression](./compression.md)
* [Advanced Tips](./advanced.md)
* [HDF5 compatibility](./hdf5compat.md)
* [Gotchas & Troubleshooting](./troubleshooting.md)
