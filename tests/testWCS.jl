using FITSIO
using WCS

# Read in the FITS file
file = homedir() * "/Downloads/ALMA01018218.fits"
f = FITS(file)

# read-in the FITS header
header = read_header(f[1], String)

# create a WCS object
wcs_array = WCS.from_header(header)
wcs = wcs_array[1]

# print the WCS object
println(wcs)

physical = [856.49056, 438.4528, 136, 1.0] # 0-indexed (but ds9 seems to use 1-indexed)
fk5 = [261.2105354, -34.2435452, 9.317640936376723e10, 1.0] # 0-indexed (but ds9 seems to use 1-indexed)

println("Physical: ", physical)
println("FK5: ", fk5)

# convert from physical to fk5
worldcoords = pix_to_world(wcs, physical)
println("Physical to FK5: ", worldcoords)

# convert from fk5 to physical
pixcoords = world_to_pix(wcs, fk5)
println("FK5 to Physical: ", pixcoords)