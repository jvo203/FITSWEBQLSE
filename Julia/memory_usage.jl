using CSV
using DataFrames
using Plots

data = CSV.read("memory_usage.csv", DataFrame)
println(data[1:5, :])

timestamp = data[:, 1]
allocated = data[:, 2] ./ (1024^3)

plot(timestamp, allocated, label = "jemalloc stats.allocated memory [GB]", xlabel = "elapsed time [s]", ylabel = "memory [GB]", legend = :bottomright, title = "memory consumption")
savefig(homedir() * "/mem_allocated_v5.pdf")