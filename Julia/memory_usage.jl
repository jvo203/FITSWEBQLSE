using CSV
using Plots

gr(size = (1000, 500), linewidth = 2)

data = CSV.read("memory_usage.csv")
println(data[1:5, :])

timestamp = data[:, 1]
allocated = data[:, 2] ./ (1024^3)

plot(timestamp, allocated, label = "jemalloc stats.allocated memory [GB]", xlabel = "elapsed time [s]", ylabel = "memory [MB]")
savefig("mem_allocated_v5.pdf")