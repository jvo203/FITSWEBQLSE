from astropy.io import fits
from astropy.wcs import WCS
import numpy as np

# get a user home directory
from pathlib import Path

home = str(Path.home())

header = fits.open(home + "/Downloads/SUPM19EAEFCA02F0C500.fits")[0].header
print(header)

wcs = WCS(header)
print(wcs)

pixcrd = np.array(
    [[937.6906124764719, 7012.886595879788], [2235.7681311858246, 6188.131977699766]],
    dtype=np.float64,
)

print(pixcrd)

world = wcs.wcs_pix2world(pixcrd, 0)
print(world)
