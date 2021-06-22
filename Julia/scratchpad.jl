# reduce (integrate) the image
pixels = @distributed (+) for i = 1:depth

    local val , pixels

    try
        fits_file = FITS(filepath)

        pixels = reshape(read(fits_file[hdu_id], :, :, i, 1), (width, height))
        val = sum(pixels)

        close(fits_file)
    catch e
        println("i:$i::error: $e")
        val = 0.0

        # the type should be decided based on <bitpix>
        pixels = zeros(Float32, width, height)
    end

    put!(progress, (i, val))

    pixels

end

################################
pixels = zeros(Float32, width, height)
mask = map(isnan, pixels)

image = RemoteChannel(() -> Channel{Tuple}(32))

image_task = @async while true
    try
        thread_pixels, thread_mask = take!(image)
        pixels .+= thread_pixels
        mask .&= thread_mask
        println("received (pixels,mask)")
    catch e
        println("image task completed")
        break
    end
end

# send back the result to the root
put!(results, (pixels, mask))

close(image)
wait(image_task)

fits.pixels = pixels
fits.mask = mask
################################

frame_mask = invalidate_pixel.(frame_pixels, datamin, datamax, ignrval)

# replace NaNs with 0.0
frame_pixels[frame_mask] .= 0.0

pixels .+= frame_pixels
mask .&= frame_mask

# pick out the valid values only
valid_mask = .!frame_mask
valid_pixels = frame_pixels[valid_mask]

pixel_sum = sum(valid_pixels)
pixel_count = length(valid_pixels)

if pixel_count > 0
    frame_min, frame_max = extrema(valid_pixels)
    mean_spectrum = pixel_sum / pixel_count
    integrated_spectrum = pixel_sum * cdelt3
else
    # no mistake here, reverse the min/max values
    # so that global dmin/dmax can get correct values
    # in the face of all-NaN frames
    frame_min = prevfloat(typemax(Float32))
    frame_max = -prevfloat(typemax(Float32))

    mean_spectrum = 0.0
    integrated_spectrum = 0.0
end

# insert back NaNs ahead of conversion to half-float (Float16)
frame_pixels[frame_mask] .= NaN32


######################################
very slow:
pixel_count = 0
                            pixel_sum = 0.0

                            frame_min = prevfloat(typemax(Float32))
                            frame_max = -prevfloat(typemax(Float32))

                            # a single pass through the data
                            for idx in eachindex(frame_pixels)
                                x = frame_pixels[idx]

                                is_nan =
                                    !isfinite(x) ||
                                    (x < datamin) ||
                                    (x > datamax) ||
                                    (x <= ignrval)

                                if is_nan
                                    x = NaN32
                                else
                                    pixel_count += 1
                                    pixel_sum += x

                                    pixels[idx] += x
                                    mask[idx] |= true

                                    if x < frame_min
                                        frame_min = x
                                    end

                                    if x > frame_max
                                        frame_max = x
                                    end
                                end
                            end

                            if pixel_count > 0
                                mean_spectrum = pixel_sum / pixel_count
                                integrated_spectrum = pixel_sum * cdelt3
                            else
                                mean_spectrum = 0.0
                                integrated_spectrum = 0.0
                            end

######################################
