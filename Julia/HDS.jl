using FITSIO
using Optim
using Plots

dir = homedir()
dir = "/mnt/c/Users/クリストファー"
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
N = Int(round(length(row) / 20))
println("N: ", N)