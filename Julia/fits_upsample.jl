using CFITSIO
using ImageTransformations

println(libcfitsio_version())

# src = homedir() * "/Downloads/ALMA00000006.fits"
# dst = homedir() * "/upsampled.fits"

src = "/Volumes/OWC/fits_web_ql/FITSCACHE/ALMA01567567.fits"
dst = "/Volumes/OWC/I_AM_BIG.fits"

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
new_width = 2 * width
new_height = 2 * height

println("new width: $new_width, new_height: $new_height")

keysexist, morekeys = fits_get_hdrspace(f)

out = fits_clobber_file(dst)

for i = 1:keysexist+1 # the extra one is the <END> keyword
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