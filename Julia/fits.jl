using FITSIO;

function loadFITS(filepath::String)
    println("loading $filepath")

    local f

    try
        f = FITS(filepath)
        println(f)
    catch e
        println(e)
        return
    end

    for hdu in f
        println(typeof(hdu))

        naxes = ndims(hdu)

        if naxes < 2
            continue
        end

        # we have at least two dimensions
        # read the data and break the for loop
        break
    end
end
