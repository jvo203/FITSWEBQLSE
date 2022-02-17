using CSV
using DataFrames
using Plots

data = CSV.read("memory_usage.csv", DataFrame)
println(data[1:5, :])

timestamp = data[:, 1]
allocated = 4 * data[:, 2] ./ (1024^2) # kB for rss, bytes otherwise (Rust and FORTRAN)

# plot(timestamp, allocated, label = "jemalloc stats.allocated memory [GB]", xlabel = "elapsed time [s]", ylabel = "memory [GB]", legend = :bottomright, title = "memory consumption")
plot(timestamp, allocated, label = "allocated memory [GB]", xlabel = "elapsed time [s]", ylabel = "memory [GB]", legend = :bottomright, title = "Julia memory consumption")

savefig(homedir() * "/mem_allocated_v5.pdf")