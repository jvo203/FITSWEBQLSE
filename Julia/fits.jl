using Dates;
using FITSIO;

mutable struct FITSDataSet
    datasetid::String
    header::Any
    width::Integer
    height::Integer
    depth::Integer
    has_header::Bool
    has_data::Bool
    has_error::Bool
    last_accessed::Float64
    mutex::Any

    function FITSDataSet()
        new(
            "",
            Nothing,
            0,
            0,
            0,
            false,
            false,
            false,
            datetime2unix(now()),
            ReentrantLock(),
        )
    end

    function FITSDataSet(datasetid)
        new(
            datasetid,
            Nothing,
            0,
            0,
            0,
            false,
            false,
            false,
            datetime2unix(now()),
            ReentrantLock(),
        )
    end
end

function update_timestamp(fits::FITSDataSet)

    lock(fits.mutex)

    fits.last_accessed = datetime2unix(now())

    unlock(fits.mutex)

end

function has_header(fits::FITSDataSet)::Bool
    has_header = false

    lock(fits.mutex)

    has_header = fits.has_header

    unlock(fits.mutex)

    return has_header
end

function has_data(fits::FITSDataSet)::Bool
    has_data = false

    lock(fits.mutex)

    has_data = fits.has_data

    unlock(fits.mutex)

    return has_data
end

function has_error(fits::FITSDataSet)::Bool
    has_error = false

    lock(fits.mutex)

    has_error = fits.has_error

    unlock(fits.mutex)

    return has_error
end

function dataset_exists(datasetid::String, fits_objects, fits_lock)::Bool
    key_exists = false

    lock(fits_lock)

    try
        key_exists = haskey(fits_objects, datasetid)
    finally
        unlock(fits_lock)
    end

    return key_exists
end

function insert_dataset(dataset::FITSDataSet, fits_objects, fits_lock)
    lock(fits_lock)

    try
        datasetid = dataset.datasetid
        fits_objects[datasetid] = dataset
    catch e
        println("Failed to insert a dataset: $e")
    finally
        unlock(fits_lock)
    end
end

function get_dataset(datasetid::String, fits_objects, fits_lock)::FITSDataSet
    local dataset::FITSDataSet

    lock(fits_lock)

    try
        dataset = fits_objects[datasetid]
    catch e
        dataset = FITSDataSet()
        println("Failed to retrieve a dataset: $e")
    finally
        unlock(fits_lock)
    end

    return dataset
end

function loadFITS(filepath::String, fits::FITSDataSet)

    if fits.datasetid == ""
        return
    end

    println("loading $filepath::$(fits.datasetid)")

    local f , width::Integer , height::Integer , depth::Integer
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
        # println(header)

        break
    end

    update_timestamp(fits)
end
