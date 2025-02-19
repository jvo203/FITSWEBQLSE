using AstroImages
using FITSIO
using Plots

basedir = homedir() * "/NAO/POLARISATION/ALMA"
target = "VYCMa"
band = "Band5"
image = "FullPol_ReferenceImages"
name = ".spw3"

Stokes = ["I", "Q", "U", "V"]
#Stokes = ["I", "Q", "U", "I"] # for testing

function read_data(filename)
    f = FITS(filename)
    data = read(f[1])
    close(f)

    return data
end

# for each Stokes parameter, read the FITS data
planes = []
for stokes in Stokes
    local data

    fn = basedir * "/" * target * "_" * band * "_" * image * "/" * target * name * "." * stokes * ".pbcorr.fits"

    try
        data = read_data(fn)
        println("Read file: ", fn, " with size: ", size(data))
        push!(planes, data)
    catch
        println("Error reading file: ", fn)
    end
end
println(size(planes))
println(planes[1][1:10])

exit()

# create a 4D cube from the planes
cube = cat(planes..., dims=4)
println("cube:", size(cube))
println(cube[1:10])

# read FITS header from the first Stokes parameter FITS file (I)
filename = basedir * "/OrionKL/" * band * "/" * name * "." * Stokes[1] * ".fits"
header = FITSIO.read_header(filename)

# modify the header to have the same cube dimensions as the data
header["NAXIS4"] = size(cube, 4)

# write the modified header with new data to a new FITS file
filename = basedir * "/OrionKL/" * band * "/" * name * ".StokesIQU.fits"
FITS(filename, "w") do f
    write(f, cube; header=header)    
end

#implot(planes[1][:,:,1,1])