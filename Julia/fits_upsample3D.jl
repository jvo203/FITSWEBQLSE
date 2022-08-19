using CFITSIO
using WCS
using ImageTransformations

println("CFITSIO version: ", libcfitsio_version())

src = homedir() * "/Downloads/numbers.fits"
dst = homedir() * "/Downloads/numbers_upsampled.fits"

f = fits_open_diskfile(src)

num = fits_get_num_hdus(f)
println("Number of HDUs in the file: ", num)

for i = 1:num
    hdu_type = fits_movabs_hdu(f, i)
    println(i, ") hdu_type = ", hdu_type)
end

bitpix, = fits_read_keyword(f, "BITPIX")
println("BITPIX = ", bitpix)
bitpix = parse(Int64, bitpix)

if bitpix != -32
    println("bitpix must be == -32")
    close(f)
    exit()
end

width, = fits_read_keyword(f, "NAXIS1")
height, = fits_read_keyword(f, "NAXIS2")
depth, = fits_read_keyword(f, "NAXIS3")

width = parse(Int64, width)
height = parse(Int64, height)
depth = parse(Int64, depth)

println("width: $(width), height: $(height), depth: $(depth)")

# new dimensions
new_width = 1 * width
new_height = 1 * height

println("new width: $new_width, new_height: $new_height")

keysexist, morekeys = fits_get_hdrspace(f)

out = fits_clobber_file(dst)

for i = 1:keysexist
    rec = fits_read_record(f, i)
    name, value, comment = fits_read_keyn(f, i)

    # println(rec)
    # println(name, "|", value, "|", comment)

    if name == "NAXIS1"
        fits_write_key(out, "NAXIS1", new_width, "")
        continue
    end

    if name == "NAXIS2"
        fits_write_key(out, "NAXIS2", new_height, "")
        continue
    end

    fits_write_record(out, rec)
end

# add the WCS keywords
wcs = WCSTransform(3;
    cdelt=[-2.777777777778E-05, 2.777777777778E-05, -2.929537207031E+04],
    ctype=["RA---SIN", "DEC--SIN", "FREQ"],
    crpix=[1.510000000000E+02, 1.510000000000E+02, 1.0],
    crval=[2.690886656602E+02, -2.195623208439E+01, 2.195706138430E+11])

# convert a WCSTransform to a FITS header
header = WCS.to_header(wcs)
println(header)
println("header length: ", length(header))

# divide the header into 80-character strings
nkeywords = Int(length(header) / 80)

for i = 1:nkeywords
    global rec
    rec = SubString(header, (i - 1) * 80 + 1, i * 80)

    if !isempty(strip(rec))
        println(rec, length(rec))
        fits_write_record(out, String(rec))
    end
end

fits_write_key(out, "RESTFRQ", "2.195632900000E+11", "Rest Frequency (Hz)")
fits_write_key(out, "SPECSYS", "LSRK", "Spectral reference frame")
fits_write_key(out, "BUNIT", "JY/BEAM", "Brightness (pixel) unit")
fits_write_key(out, "OBSRA", "2.690886656602E+02", "Right Ascension of observation")
fits_write_key(out, "OBSDEC", "-2.195623208439E+01", "Declination of observation")
fits_write_key(out, "OBJECT", "NUMBERS", "Name of observed object")

# the extra one is the <END> keyword
rec = fits_read_record(f, keysexist + 1)
fits_write_record(out, rec)

println("FITS image dimensions: ", fits_get_img_size(f))

data = Array{Float32}(undef, width, height)
new_data = Array{Float32}(undef, new_width, new_height)

nelements = width * height

for frame = 1:depth
    global new_data

    fpixel = [1, 1, frame, 1]
    fits_read_pix(f, fpixel, nelements, data)

    println("HDU $(frame): ", size(data))

    new_data = Float32.(imresize(data, (new_width, new_height)))

    fits_write_pix(out, fpixel, new_width * new_height, new_data)
end

println("upsampled size:", size(new_data))

fits_close_file(f)
fits_close_file(out)