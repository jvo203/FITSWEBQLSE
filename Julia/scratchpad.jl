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
