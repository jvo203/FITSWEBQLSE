using FITSIO

basedir = homedir() * "/NAO/POLARISATION/ALMA"
target = "OrionKL"
band = "band6"
name = "concat.ms.cal.mfs.impbcor"

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

    fn = basedir * "/" * target * "/" * band * "/" * name * "." * stokes * ".fits"

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

# create a 4D cube from the planes
cube = cat(planes..., dims=4)
println("cube:", size(cube))
println(cube[1:10])

# read FITS header from the first Stokes parameter FITS file (I)
filename = basedir * "/" * target * "/" * band * "/" * name * "." * Stokes[1] * ".fits"
header = FITSIO.read_header(filename)

# modify the header to have the same cube dimensions as the data
header["NAXIS4"] = size(cube, 4)

# write the modified header with new data to a new FITS file
filename = basedir * "/" * target * "." * band * "." * name * ".StokesIQU.fits"
FITS(filename, "w") do f
    write(f, cube; header=header)
end
