using FITSIO
using LinearAlgebra
using Optim
using Peaks
using Plots
using Statistics

dir = homedir()
#dir = "/mnt/c/Users/クリストファー"
fitspath = dir * "/Downloads/fsclmo_HDSA00042941.fits"

f = FITS(fitspath)
N = ndims(f[1])
println("ndims: ", N, ", size: ", size(f[1]))

header = read_header(f[1])

width = 0
height = 0

if haskey(header, "NAXIS1")
    width = header["NAXIS1"]
end

if haskey(header, "NAXIS2")
    height = header["NAXIS2"]
end

println("width: $(width), height: $(height)")

data = read(f[1], :, :)
println("data size: ", size(data))

close(f)

x = Int(width / 2)
y = Int(height / 2)
println("x: $(x), y: $(y)")

# take the row at the center of the image
row = data[:, y]
println("row size: ", size(row))

plot(row, label="Spectrum", xlabel="Pixel", ylabel="Intensity", legend=:topleft)

# the approximate number of peaks in the spectrum to be fitted
#N = Int(round(length(row) / 20))

# fit the time-series data with <peaks> radial basis functions and return the rms error
function rbf_cost_function(params::Vector{Float64}, centres::Vector{Float32}, targets::Vector{Float32})
    # a common gamma
    gamma = Float32(params[1])

    # the weights of the peaks
    weights = Float32.(params[2:end])

    # the predicted values for a range of inputs between 1 and the length of the data    
    predictions = [dot(weights, exp.(-gamma * (Float32(i) .- centres) .^ 2)) for i in 1:length(targets)]

    #return plot(predictions, label="Predictions", xlabel="Pixel", ylabel="Intensity", legend=:topleft)

    # the cost is the sum of the squared differences between the predictions and the targets
    return sqrt(sum((predictions .- targets) .^ 2))
end

function rbf_forward(params::Vector{Float64}, centres::Vector{Float32}, targets::Vector{Float32})
    # a common gamma
    gamma = Float32(params[1])

    # the weights of the peaks
    weights = Float32.(params[2:end])

    # the predicted values for a range of inputs between 1 and the length of the data    
    predictions = [dot(weights, exp.(-gamma * (Float32(i) .- centres) .^ 2)) for i in 1:length(targets)]

    return predictions
end

# get a mean and std of the data in row
mean_row = mean(row)
std_row = std(row)
threshold = mean_row + 3 * std_row

pks, vals = findmaxima(row)

# prune the peaks
_, proms = peakproms!(pks, row; minprom=threshold)

N = length(pks)
println("N. peaks: ", N)

gamma = 0.1
#centres = Float32.([i * length(row) / (N + 1) for i in 1:N])
centres = Float32.(pks)
#weights = ones(N)
# small random weights
weights = rand(N)

params = [gamma; weights]

println("centres: ", centres)
println("#params: ", length(params))

@time rbf_cost_function(params, centres, row)

# optimize the parameters with Optim
#result = optimize(x -> rbf_cost_function(x, centres, row), params, Optim.Options(iterations=1000000, show_trace=true))
#result = optimize(x -> rbf_cost_function(x, centres, row), params, LBFGS(), Optim.Options(iterations=1000, show_trace=true))
result = optimize(x -> rbf_cost_function(x, centres, row), params, SimulatedAnnealing(), Optim.Options(iterations=1000000, show_trace=true))
println(result)
new_params = Optim.minimizer(result)
println("new_params: ", new_params)

predicted = rbf_forward(new_params, centres, row)
plot(predicted, label="Predictions", xlabel="Pixel", ylabel="Intensity", legend=:topleft)
scatter!(centres, row[round.(Int, centres)], label="Peaks")

# plot the peaks
#plot(row)
#scatter!(pks, row[round.(Int, pks)], label="Peaks")