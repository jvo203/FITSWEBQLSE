# reduce (integrate) the image
pixels = @distributed (+) for i = 1:depth

    local val , pixels

    try
        fits_file = FITS(filepath)

        pixels = reshape(
                            read(fits_file[hdu_id], :, :, i, 1),
                            (width, height),
                        )
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