using FITSIO
using ImageTransformations

src = homedir() * "/Downloads/ALMA00000006.fits"
dest = homedir() * "/upsampled.fits"

f = FITS(src)
hdu = f[1]

N = ndims(hdu)
println("ndims: ", N, ", size: ", size(hdu))

header = read_header(hdu)
headerStr = read_header(hdu, String)
println(header)

width = 0
height = 0
depth = 1

if haskey(header, "NAXIS1")
    width = header["NAXIS1"]
end

if haskey(header, "NAXIS2")
    height = header["NAXIS2"]
end

if haskey(header, "NAXIS3")
    depth = header["NAXIS3"]
end

println("width: $(width), height: $(height), depth: $(depth)")

if depth < 1
    println("depth must be >= 1")
    close(f)
    exit()
end

bitpix = header["BITPIX"]

if bitpix != -32
    println("BITPIX:", bitpix)
    println("bitpix must be == -32")
    close(f)
    exit()
end

# new dimensions
new_width = 2 * width
new_height = 2 * height

# change the header
header["NAXIS1"] = new_width
header["NAXIS2"] = new_height


new_data = zeros(Float32, new_width, new_height, depth)

for frame = 1:depth
    global N

    if N == 4
        data = reshape(read(hdu, :, :, frame, 1), (width, height))
    elseif N == 3
        data = reshape(read(hdu, :, :, frame), (width, height))
    elseif N == 2
        data = reshape(read(hdu, :, :), (width, height))
    end

    println("HDU $(frame): ", size(data))

    new_data[:, :, frame] = Float32.(imresize(data, (new_width, new_height)))
end

println("upsampled size:", size(new_data))

# open a destination FITS file
df = FITS(dest, "w")
write(df, new_data, header = header)