using FITSIO

basedir = homedir() * "/NAO/POLARISATION/ALMA"
band = "band3"

# read FITS header from the I intensity FITS file
filename = basedir * "/OrionKL/" * band * "/concat.ms.cal.mfs.impbcor.I.fits"
header = FITSIO.read_header(filename)
println(header)

# read a data cube from the Stokes IQU FITS file (the first image)
filename = basedir * "/OrionKL/" * band * "/wolfram.StokesIQU.fits"
f = FITS(filename)
data = read(f[1])
println(size(data))

# modify the header to have the same cube dimensions as the data
header["NAXIS4"] = size(data, 4)

# write the modified header with new data to a new FITS file
filename = basedir * "/OrionKL/" * band * "/concat.ms.cal.mfs.impbcor.StokesIQU.fits"
FITS(filename, "w") do f
    write(f, data; header=header)    
end

