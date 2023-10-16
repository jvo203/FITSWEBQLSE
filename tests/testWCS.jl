using FITSIO
using WCS

function wcs2pix(deg, crval, crpix, cdelt)
    return (deg - crval) / cdelt + crpix
end

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

physical = [856.49056, 438.4528, 136, 1.0] # 0-indexed (but ds9 seems to use 1-indexed for the z-axis)
fk5 = [261.2105354, -34.2435452, 9.317640936376723e10, 1.0] # 0-indexed (but ds9 seems to use 1-indexed for the z-axis)

println("Physical: ", physical)
println("FK5: ", fk5)

# convert from physical to fk5
worldcoords = pix_to_world(wcs, physical)
println("Physical to FK5: ", worldcoords)

# convert from fk5 to physical
pixcoords = world_to_pix(wcs, fk5)
println("FK5 to Physical: ", pixcoords)

CDELT1 = wcs.cdelt[1]
CDELT2 = wcs.cdelt[2]

CRPIX1 = wcs.crpix[1]
CRPIX2 = wcs.crpix[2]

CRVAL1 = wcs.crval[1]
CRVAL2 = wcs.crval[2]

# JavaScript
println("JavaScript: ", wcs2pix(fk5[1], CRVAL1, CRPIX1, CDELT1), ", ", wcs2pix(fk5[2], CRVAL2, CRPIX2, CDELT2))