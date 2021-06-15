using Dates;
using FITSIO;

mutable struct FITSDataSet
    datasetid::Any
    header::Any
    has_header::Bool
    has_data::Bool
    last_accessed::Float64

    function FITSDataSet()
        new("", Nothing, false, false, datetime2unix(now()))
    end

    function FITSDataSet(datasetid)
        new(datasetid, Nothing, false, false, 0, datetime2unix(now()))
    end
end

function loadFITS(filepath::String)
    println("loading $filepath")

    local f , width , height , depth
    local header

    try
        f = FITS(filepath)
        println(f)
    catch e
        println(e)
        return
    end

    width = 0
    height = 0
    depth = 1

    for hdu in f
        println(typeof(hdu))

        naxes = ndims(hdu)

        if naxes < 2
            # continue searching for the "right" HDU
            continue
        end

        # we have at least two dimensions
        try
            width = size(hdu, 1)
            height = size(hdu, 2)
            depth = size(hdu, 3)
        catch e
        end

        println(
            "naxes: ",
            naxes,
            "\twidth: ",
            width,
            "\theight: ",
            height,
            "\tdepth: ",
            depth,
        )

        # read the header & data; break the <for> loop
        header = read_header(hdu)
        println(header)

        break
    end
end
