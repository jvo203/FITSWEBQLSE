using CSV
using DataFrames
using Plots

data_r = CSV.read(homedir() * "/memory_usage_rust.csv", DataFrame)
data_f = CSV.read(homedir() * "/memory_usage_fortran.csv", DataFrame)

timestamp_r = data_r[:, 1]
allocated_r = data_r[:, 2] ./ (1024^3)

timestamp_f = data_f[:, 1]
allocated_f = data_f[:, 2] ./ (1024^3)

common = min(size(timestamp_r)[1], size(timestamp_f)[1])

plot(timestamp_f[1:common], [allocated_r[1:common], allocated_f[1:common]], label = ["Rust fits_web_ql v4" "C / FORTRAN FITSWEBQLSE v5"], xlabel = "elapsed time [s]", ylabel = "allocated memory [GB]", legend = :bottomright, title = "memory consumption comparison")
savefig(homedir() * "/mem_comparison.pdf")