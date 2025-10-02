# Performance Benchmarks for JLD2 Link System
# Tests to ensure no regression in hard link performance with new link system

# using JLD2
# using Test
# using Statistics

# # Import the internal link types for testing
# using JLD2: HardLink, SoftLink, ExternalLink, lookup_offset, lookup_link

# @testitem "Link Type Dispatch Performance" begin
#     using JLD2
#     using Statistics
#     using JLD2: HardLink, SoftLink, ExternalLink, lookup_offset, lookup_link

#     # Create test data
#     temp_file = tempname() * ".jld2"

#     try
#         # Create a file with mixed link types for testing
#         jldopen(temp_file, "w") do f
#             # Create regular datasets (hard links)
#             f["data1"] = collect(1:1000)
#             f["data2"] = collect(1:1000)
#             f["data3"] = collect(1:1000)

#             # Create groups with hard links
#             g = create_group(f, "test_group")
#             g["nested_data1"] = collect(1:1000)
#             g["nested_data2"] = collect(1:1000)
#             g["nested_data3"] = collect(1:1000)
#         end

#         # Benchmark hard link performance (baseline)
#         println("Running hard link performance benchmarks...")

#         hard_link_times = Float64[]
#         jldopen(temp_file, "r") do f
#             for i in 1:100
#                 start_time = time_ns()

#                 # Access multiple hard links
#                 _ = f["data1"]
#                 _ = f["data2"]
#                 _ = f["data3"]
#                 _ = f["test_group"]["nested_data1"]
#                 _ = f["test_group"]["nested_data2"]
#                 _ = f["test_group"]["nested_data3"]

#                 end_time = time_ns()
#                 push!(hard_link_times, (end_time - start_time) / 1e6)  # Convert to ms
#             end
#         end

#         # Create a test file with external and soft links
#         temp_external = tempname() * ".jld2"
#         temp_main = tempname() * ".jld2"

#         try
#             # Create external file
#             jldopen(temp_external, "w") do f
#                 f["external_data1"] = collect(1:1000)
#                 f["external_data2"] = collect(1:1000)
#                 f["external_data3"] = collect(1:1000)
#             end

#             # Create main file with links
#             jldopen(temp_main, "w") do f
#                 # Hard links (baseline)
#                 f["hard_data1"] = collect(1:1000)
#                 f["hard_data2"] = collect(1:1000)
#                 f["hard_data3"] = collect(1:1000)

#                 # Soft links
#                 create_soft_link!(f, "soft_link1", "/hard_data1")
#                 create_soft_link!(f, "soft_link2", "/hard_data2")
#                 create_soft_link!(f, "soft_link3", "/hard_data3")

#                 # External links
#                 create_external_link!(f, "ext_link1", temp_external, "/external_data1")
#                 create_external_link!(f, "ext_link2", temp_external, "/external_data2")
#                 create_external_link!(f, "ext_link3", temp_external, "/external_data3")
#             end

#             # Benchmark link resolution performance
#             soft_link_times = Float64[]
#             external_link_times = Float64[]
#             mixed_access_times = Float64[]

#             jldopen(temp_main, "r") do f
#                 # Test soft link performance
#                 for i in 1:50  # Fewer iterations for more expensive operations
#                     start_time = time_ns()

#                     _ = f["soft_link1"]
#                     _ = f["soft_link2"]
#                     _ = f["soft_link3"]

#                     end_time = time_ns()
#                     push!(soft_link_times, (end_time - start_time) / 1e6)
#                 end

#                 # Test external link performance
#                 for i in 1:50
#                     start_time = time_ns()

#                     _ = f["ext_link1"]
#                     _ = f["ext_link2"]
#                     _ = f["ext_link3"]

#                     end_time = time_ns()
#                     push!(external_link_times, (end_time - start_time) / 1e6)
#                 end

#                 # Test mixed access patterns (realistic usage)
#                 for i in 1:50
#                     start_time = time_ns()

#                     # Mix of all link types
#                     _ = f["hard_data1"]      # Hard link
#                     _ = f["soft_link1"]      # Soft link
#                     _ = f["ext_link1"]       # External link
#                     _ = f["hard_data2"]      # Hard link
#                     _ = f["soft_link2"]      # Soft link
#                     _ = f["ext_link2"]       # External link

#                     end_time = time_ns()
#                     push!(mixed_access_times, (end_time - start_time) / 1e6)
#                 end
#             end

#             # Calculate statistics
#             hard_mean = mean(hard_link_times)
#             hard_std = std(hard_link_times)

#             soft_mean = mean(soft_link_times)
#             soft_std = std(soft_link_times)

#             external_mean = mean(external_link_times)
#             external_std = std(external_link_times)

#             mixed_mean = mean(mixed_access_times)
#             mixed_std = std(mixed_access_times)

#             # Performance reporting
#             println("Performance Benchmark Results:")
#             println("==============================")
#             println("Hard Links (6 accesses):     $(round(hard_mean, digits=3)) ± $(round(hard_std, digits=3)) ms")
#             println("Soft Links (3 accesses):     $(round(soft_mean, digits=3)) ± $(round(soft_std, digits=3)) ms")
#             println("External Links (3 accesses):  $(round(external_mean, digits=3)) ± $(round(external_std, digits=3)) ms")
#             println("Mixed Access (6 accesses):    $(round(mixed_mean, digits=3)) ± $(round(mixed_std, digits=3)) ms")
#             println()

#             # Performance per access
#             println("Per-Access Performance:")
#             println("Hard Link per access:    $(round(hard_mean/6, digits=4)) ms")
#             println("Soft Link per access:    $(round(soft_mean/3, digits=4)) ms")
#             println("External Link per access: $(round(external_mean/3, digits=4)) ms")
#             println("Mixed Access per item:   $(round(mixed_mean/6, digits=4)) ms")
#             println()

#             # Regression tests - ensure hard links aren't significantly slower
#             hard_per_access = hard_mean / 6

#             # Hard links should be fastest (baseline)
#             @test hard_per_access < soft_mean / 3  # Hard links should be faster than soft links

#             # Soft links should be reasonable (within 10x of hard links for simple resolution)
#             soft_per_access = soft_mean / 3
#             @test soft_per_access < hard_per_access * 10  # Soft links shouldn't be more than 10x slower

#             # External links will be slower but should still be reasonable for cached access
#             external_per_access = external_mean / 3
#             @test external_per_access < hard_per_access * 50  # External links shouldn't be more than 50x slower

#             # Print performance ratios
#             println("Performance Ratios (relative to hard links):")
#             println("Soft Link ratio:     $(round(soft_per_access / hard_per_access, digits=1))x")
#             println("External Link ratio: $(round(external_per_access / hard_per_access, digits=1))x")

#             # Ensure we're not seeing catastrophic performance regression
#             @test hard_per_access < 1.0  # Hard links should take less than 1ms per access

#         finally
#             isfile(temp_external) && rm(temp_external, force=true)
#             isfile(temp_main) && rm(temp_main, force=true)
#         end

#     finally
#         isfile(temp_file) && rm(temp_file, force=true)
#     end
# end

# @testitem "Link Lookup Function Performance" begin
#     using JLD2
#     using Statistics
#     using JLD2: HardLink, SoftLink, ExternalLink, lookup_offset, lookup_link

#     # Test the performance of lookup functions directly
#     temp_file = tempname() * ".jld2"

#     try
#         jldopen(temp_file, "w") do f
#             f["test_data"] = collect(1:1000)
#             create_soft_link!(f, "soft_test", "/test_data")
#         end

#         jldopen(temp_file, "r") do f
#             # Time lookup_offset (traditional path)
#             lookup_offset_times = Float64[]
#             for i in 1:1000
#                 start_time = time_ns()
#                 offset = lookup_offset(f.root_group, "test_data")
#                 end_time = time_ns()
#                 push!(lookup_offset_times, (end_time - start_time) / 1e6)
#             end

#             # Time lookup_link (new path)
#             lookup_link_times = Float64[]
#             for i in 1:1000
#                 start_time = time_ns()
#                 link = lookup_link(f.root_group, "test_data")
#                 end_time = time_ns()
#                 push!(lookup_link_times, (end_time - start_time) / 1e6)
#             end

#             offset_mean = mean(lookup_offset_times)
#             link_mean = mean(lookup_link_times)

#             println("Lookup Function Performance:")
#             println("lookup_offset: $(round(offset_mean, digits=6)) ms")
#             println("lookup_link:   $(round(link_mean, digits=6)) ms")
#             println("Ratio: $(round(link_mean / offset_mean, digits=2))x")

#             # lookup_link should not be significantly slower than lookup_offset for hard links
#             @test link_mean < offset_mean * 2  # Should be within 2x performance
#         end

#     finally
#         isfile(temp_file) && rm(temp_file, force=true)
#     end
# end

# @testitem "External File Cache Performance" begin
#     using JLD2
#     using Statistics
#     using JLD2: clear_external_file_cache, get_cache_stats

#     # Test external file caching performance
#     temp_files = String[]
#     temp_main = tempname() * ".jld2"

#     try
#         # Create multiple external files
#         for i in 1:5
#             temp_external = tempname() * "_external_$i.jld2"
#             push!(temp_files, temp_external)

#             jldopen(temp_external, "w") do f
#                 f["data"] = collect(1:100) * i
#             end
#         end

#         # Create main file with external links
#         jldopen(temp_main, "w") do f
#             for (i, external_file) in enumerate(temp_files)
#                 create_external_link!(f, "link_$i", external_file, "/data")
#             end
#         end

#         # Clear cache to start fresh
#         clear_external_file_cache()

#         # Test first access (no cache)
#         first_access_times = Float64[]
#         jldopen(temp_main, "r") do f
#             for i in 1:5
#                 start_time = time_ns()
#                 _ = f["link_$i"]  # This should cache the external file
#                 end_time = time_ns()
#                 push!(first_access_times, (end_time - start_time) / 1e6)
#             end
#         end

#         # Test cached access performance
#         cached_access_times = Float64[]
#         jldopen(temp_main, "r") do f
#             for i in 1:5
#                 start_time = time_ns()
#                 _ = f["link_$i"]  # This should use cached external file
#                 end_time = time_ns()
#                 push!(cached_access_times, (end_time - start_time) / 1e6)
#             end
#         end

#         first_mean = mean(first_access_times)
#         cached_mean = mean(cached_access_times)

#         println("External File Cache Performance:")
#         println("First access (no cache): $(round(first_mean, digits=3)) ms")
#         println("Cached access:           $(round(cached_mean, digits=3)) ms")
#         println("Speedup ratio:           $(round(first_mean / cached_mean, digits=1))x")

#         # Cached access should be faster than first access
#         @test cached_mean < first_mean

#         # Check cache stats
#         stats = get_cache_stats()
#         @test stats.active_files >= 5  # Should have cached our external files

#     finally
#         rm(temp_main, force=true)
#         for temp_file in temp_files
#             isfile(temp_file) && rm(temp_file, force=true)
#         end
#         clear_external_file_cache()
#     end
# end
