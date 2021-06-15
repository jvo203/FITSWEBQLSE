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
    end
end
