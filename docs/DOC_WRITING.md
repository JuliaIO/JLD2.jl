# JLD2.jl Documentation Writing Guide

This guide provides comprehensive information about writing and maintaining documentation for JLD2.jl using Documenter.jl.

## Table of Contents

- [Documenter.jl Features](#documenterjl-features)
- [JLD2-Specific Guidelines](#jld2-specific-guidelines)
- [Code Block Types](#code-block-types)
- [Best Practices](#best-practices)
- [File Organization](#file-organization)
- [Building and Testing](#building-and-testing)
- [Common Patterns](#common-patterns)
- [Troubleshooting](#troubleshooting)

## Documenter.jl Features

### Overview

Documenter.jl is Julia's documentation generator that converts markdown files and docstrings into beautiful HTML documentation. It supports several special code block types for creating interactive, executable documentation.

### Key Code Block Types

#### 1. `@example` Blocks

**Purpose**: Execute code and display both input and output in the documentation.

**Syntax**:
```markdown
```@example [name]
# Your Julia code here
```
```

**Features**:
- Executes code during documentation build
- Shows both input code and resulting output
- Variables persist across blocks with same name
- Automatic module isolation between different names

**Example**:
```markdown
```@example demo
using JLD2
data = [1, 2, 3, 4, 5]
jldsave("test.jld2"; data)
```
```

#### 2. `@repl` Blocks

**Purpose**: Simulate REPL sessions with `julia>` prompts.

**Syntax**:
```markdown
```@repl [name]
julia> code_here
output_here

julia> more_code
```
```

**Features**:
- Shows `julia>` prompts like real REPL
- Semicolons suppress output (like real REPL)
- Good for demonstrating interactive usage
- Shares scope when using same name

#### 3. `@docs` Blocks

**Purpose**: Include docstrings from Julia code.

**Syntax**:
```markdown
```@docs
FunctionName
ModuleName.TypeName
```
```

**Features**:
- Automatically pulls docstrings from code
- Ensures docstrings stay in sync
- Can target specific method signatures
- Warns about missing docstrings

#### 4. `@setup` Blocks

**Purpose**: Execute setup code without showing it in documentation.

**Syntax**:
```markdown
```@setup name
# Hidden setup code
```
```

**Features**:
- Code executes but doesn't appear in docs
- Must use same `name` as associated `@example` blocks
- Useful for importing modules, setting up test data

### Variable Scoping and Naming

#### Named Blocks
```markdown
```@example myname
x = 5
```

```@example myname
y = x + 3  # x is available from previous block
```
```

#### Anonymous Blocks
```markdown
```@example
# This runs in its own isolated module
```
```

## JLD2-Specific Guidelines

### Package Context

JLD2.jl is a data persistence library with these key characteristics:
- **Complex API**: Multiple ways to save/load data
- **Performance-critical**: Users care about speed and memory usage
- **File format compatibility**: Must work with HDF5 ecosystem
- **Experimental features**: Some features like chunked arrays are evolving

### Target Audiences

1. **Data Scientists**: Need practical examples for common workflows
2. **Performance Users**: Want benchmarks and optimization guidance
3. **HDF5 Users**: Need compatibility and migration information
4. **Package Developers**: Building on top of JLD2

### Documentation Structure Principles

1. **User Journey**: Start simple, build complexity gradually
2. **Runnable Examples**: Every feature should have working code
3. **Real-world Focused**: Prefer practical over toy examples
4. **Performance Aware**: Include timing and memory considerations
5. **Cross-platform**: Consider Windows/Linux/macOS differences

## Code Block Types for JLD2

### Standard Example Pattern

Use this pattern for most JLD2 documentation:

```markdown
```@example feature_name
using JLD2

# Create realistic test data
data = rand(Float64, 100, 100)  # Avoid tiny examples

# Demonstrate the feature
jldopen("example.jld2", "w") do f
    f["dataset"] = data
end

# Show verification
result = jldopen("example.jld2", "r") do f
    f["dataset"]
end

println("Success: $(size(result))")

# Always clean up
rm("example.jld2", force=true)
nothing # hide
```
```

### File Cleanup Pattern

**Always clean up temporary files**:
```julia
rm("tempfile.jld2", force=true)  # force=true prevents errors
nothing # hide                   # hide suppresses output
```

### Error Demonstration Pattern

```markdown
```@example errors
try
    # Code that should fail
    jldopen("nonexistent.jld2", "r") do f
        f["data"]
    end
catch e
    println("Expected error: $(typeof(e))")
    println("Message: $(e.msg)")
end
```
```

### Performance Comparison Pattern

```markdown
```@example perf
using BenchmarkTools

# Create test data
small_data = rand(100)
large_data = rand(10000)

# Compare approaches
@btime jldsave("small.jld2"; data=$small_data)
@btime jldsave("large.jld2"; data=$large_data)

# Cleanup
rm("small.jld2", force=true)
rm("large.jld2", force=true)
nothing # hide
```
```

## Best Practices

### Content Guidelines

#### 1. Start with Working Examples
- Begin each section with a complete, runnable example
- Use realistic data sizes (avoid `[1,2,3]` examples)
- Show both writing and reading operations

#### 2. Build Complexity Gradually
```markdown
## Basic Usage
```@example basics
# Simple case first
```

## Advanced Usage
```@example basics
# Build on the simple case
```

## Expert Tips
```@example basics
# Complex scenarios using same data
```
```

#### 3. Include Error Cases
```julia
# Show what happens when things go wrong
try
    # Problematic code
catch e
    # Explain the error
end
```

#### 4. Performance Considerations
- Always mention performance implications
- Include file size comparisons when relevant
- Show memory usage for large datasets

### Technical Guidelines

#### 1. File Management
```julia
# Use unique filenames to avoid conflicts
filename = tempname() * ".jld2"

# Always clean up
rm(filename, force=true)
```

#### 2. Output Control
```julia
result = expensive_operation()
nothing # hide - suppresses large output
```

#### 3. Cross-platform Compatibility
```julia
# Use Julia's path functions
filepath = joinpath(tempdir(), "data.jld2")

# Avoid hardcoded paths
# BAD: "C:\\temp\\data.jld2"
# GOOD: joinpath(tempdir(), "data.jld2")
```

## File Organization

### Documentation Structure

```
docs/
├── src/
│   ├── index.md              # Overview and quick start
│   ├── basic_usage.md        # Core functionality
│   ├── advanced.md           # Advanced features
│   ├── customserialization.md # Custom types
│   ├── compression.md        # Performance features
│   ├── chunked_arrays.md     # Experimental features
│   ├── hdf5compat.md        # Ecosystem integration
│   ├── external_links.md    # Cross-file references
│   ├── internals.md         # Implementation details
│   ├── legacy.md            # Backwards compatibility
│   └── troubleshooting.md   # Common problems
├── make.jl                  # Build script
└── Project.toml            # Doc dependencies
```

### Content Guidelines by File

- **index.md**: Quick start, installation, basic examples
- **basic_usage.md**: Core save/load operations, common patterns
- **advanced.md**: Performance tuning, complex workflows
- **chunked_arrays.md**: Experimental features with clear status
- **troubleshooting.md**: Common errors and solutions

## Building and Testing

### Local Build Process

```bash
# Navigate to project root
cd JLD2.jl

# Build documentation
julia --project=docs docs/make.jl
```

### Verification Checklist

1. **All examples execute**: No errors in `@example` blocks
2. **Files cleaned up**: No leftover `.jld2` files
3. **Cross-references work**: All `@ref` links resolve
4. **Output reasonable**: No excessive output in examples
5. **Navigation works**: TOC and internal links functional

### CI Integration

Documentation builds automatically on:
- Pull requests (verification only)
- Main branch pushes (deployment to GitHub Pages)
- Tagged releases (versioned documentation)

## Common Patterns

### API Documentation Pattern

```markdown
## Function Name

Brief description of what the function does.

### Basic Usage

```@example api
# Simple, common use case
```

### Parameters

- `param1`: Description
- `param2`: Description

### Advanced Usage

```@example api
# More complex scenario
```

### See Also

- [`related_function`](@ref)
- [Related Section](#section-link)
```

### Troubleshooting Pattern

```markdown
## Problem: Error Message

**Symptoms**: What the user sees

**Cause**: Why this happens

**Solution**:
```@example troubleshoot
# Working code that fixes the issue
```

**Prevention**: How to avoid this in the future
```

### Migration Guide Pattern

```markdown
## Migrating from X to Y

### Old Approach (Deprecated)

```julia
# Don't use this anymore
old_function(args)
```

### New Approach (Recommended)

```@example migration
# Use this instead
new_function(args)
```

### Why the Change

Explanation of benefits: performance, features, etc.
```

## Important Caveats

### Custom Type Reconstruction Limitation

**Critical Issue**: Documenter.jl's `@example` and `@repl` blocks execute code within anonymous modules, which breaks JLD2's struct reconstruction mechanism.

**Problem**:
```julia
# This will NOT work in @example blocks
struct MyType
    data::Vector{Float64}
end

# Save works fine
jldsave("test.jld2"; obj=MyType([1.0, 2.0, 3.0]))

# But loading fails because MyType is not accessible
# from the anonymous module context
result = load("test.jld2", "obj")  # ERROR: type MyType not found
```

**Why This Happens**:
- Documenter creates isolated anonymous modules for each example block
- JLD2 needs access to the original type definition during deserialization
- The type `MyType` defined in the example block is not visible to JLD2's reconstruction process

**Workarounds**:

1. **Use built-in types only in examples**:
```julia
# Safe - uses only built-in Julia types
data = Dict("numbers" => [1, 2, 3], "text" => "hello")
jldsave("example.jld2"; data)
result = load("example.jld2", "data")  # Works fine
```

2. **Demonstrate concept without loading**:
```julia
# Show saving custom types but not loading them
struct MyType
    value::Int
end

obj = MyType(42)
jldsave("custom.jld2"; obj)
println("Custom type saved successfully")
# Don't attempt to load back in the example
```

3. **Use text descriptions for custom type workflows**:
```markdown
To load custom types, ensure the type definition is available:

1. Define your struct in a module or main scope
2. Save the object with `jldsave`
3. In a new session, load the module containing the type definition
4. Then load the data - JLD2 will reconstruct the object correctly
```

4. **Reference external working examples**:
```julia
# Point to test files or separate examples
println("See test/custom_serialization.jl for complete custom type examples")
```

**Documentation Strategy**:
- Use this limitation as a teaching opportunity about Julia's module system
- Emphasize the importance of module/package structure for custom types
- Provide external working examples in the test suite
- Focus documentation examples on built-in types and common patterns

This limitation affects any package that relies on type availability during deserialization, not just JLD2.

## Troubleshooting

### Common Documentation Issues

#### 1. Build Failures

**Problem**: `@example` blocks fail during build

**Solutions**:
- Check that all required packages are imported
- Verify file paths are correct
- Ensure cleanup code runs properly
- Test examples in fresh Julia session

#### 2. Missing Cross-references

**Problem**: `@ref` links don't resolve

**Solutions**:
- Check exact spelling of target
- Verify target has docstring or is in `@docs` block
- Use explicit section links: `[text](#section-name)`

#### 3. Excessive Output

**Problem**: Examples produce too much output

**Solutions**:
```julia
result = noisy_function()
nothing # hide

# Or capture specific parts
println("Key result: $(summary(result))")
```

#### 4. File Conflicts

**Problem**: Multiple examples interfere with each other

**Solutions**:
```julia
# Use unique names
filename = tempname() * ".jld2"

# Or use different example block names
```

### Performance Considerations

#### 1. Build Time
- Limit expensive operations in examples
- Use small but realistic data sizes
- Cache setup where possible with `@setup` blocks

#### 2. Example Execution
- Examples run every build - keep them fast
- Use `# hide` to suppress unnecessary output
- Clean up resources promptly

## Style Guidelines

### Writing Style

1. **Active Voice**: "JLD2 saves data" not "Data is saved by JLD2"
2. **Present Tense**: "The function returns" not "The function will return"
3. **Clear Structure**: Use headers, lists, and code blocks effectively
4. **Audience Aware**: Match complexity to intended users

### Code Style

1. **Consistent Naming**: Use meaningful variable names
2. **Realistic Examples**: Avoid toy data when possible
3. **Complete Examples**: Show full workflow, not just fragments
4. **Error Handling**: Demonstrate both success and failure cases

### Markdown Formatting

```markdown
# Main Sections (H1)
## Subsections (H2)
### Details (H3)

**Bold** for emphasis
`code` for inline code
[Links](url) for references

- Lists for options
- Lists for features
- Lists for steps

!!! warning "Callouts"
    For important notes

!!! info "Information"
    For helpful tips
```

This guide should be updated as Documenter.jl evolves and as JLD2.jl's documentation needs change. Always test documentation changes locally before committing.