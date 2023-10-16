using FITSIO
using WCS

# Read in the FITS file
file = homedir() * "/Downloads/ALMA01018218.fits"
f = FITS(file)

# read-in the FITS header
header = read_header(f[1], String)

# create a WCS object
wcs = WCS.from_header(header)

# print the WCS object
println(wcs)