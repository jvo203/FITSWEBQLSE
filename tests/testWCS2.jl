using FITSIO
using WCS

# Read in the FITS file
file = homedir() * "/Downloads/SUPM19EAEFCA02F0C500.fits"
f = FITS(file)

# read-in the FITS header
header = read_header(f[1], String)

# create a WCS object
wcs_array = WCS.from_header(header)
wcs = wcs_array[1]

# print the WCS object
println(wcs)

pix = [937.6906124764719, 7012.886595879788]
pix2 = [2235.7681311858246, 6188.131977699766]