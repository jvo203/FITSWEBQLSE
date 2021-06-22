using BSON;
using Dates;
using DistributedArrays;
using FITSIO;
using Mmap;
using Serialization;

mutable struct testD
    x::Float64
    y::Int32

    function testD()
        new(1.0, 7)
    end
end

mutable struct FITSDataSet
    # metadata
    datasetid::String
    header::Any
    width::Integer
    height::Integer
    depth::Integer

    is_optical::Bool
    is_xray::Bool
    has_frequency::Bool
    has_velocity::Bool
    frame_multiplier::Float32
    _cdelt3::Float32
    datamin::Float32
    datamax::Float32
    flux::String
    ignrval::Float32

    # pixels, spectrum
    pixels::Any
    mask::Any
    indices::Any
    frame_min::Any
    frame_max::Any
    mean_spectrum::Any
    integrated_spectrum::Any

    # house-keeping
    has_header::Bool
    has_data::Bool
    has_error::Bool
    last_accessed::Threads.Atomic{Float64}
    progress::Threads.Atomic{Int}
    total::Threads.Atomic{Int}
    elapsed::Threads.Atomic{Float64}
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
            false,
            0.0,
            0.0,
            0.0,
            0.0,
            "",
            0.0,
            Nothing,
            Nothing,
            Nothing,
            Nothing,
            Nothing,
            Nothing,
            Nothing,
            false,
            false,
            false,
            Threads.Atomic{Float64}(0.0),
            Threads.Atomic{Int}(0),
            Threads.Atomic{Int}(0),
            Threads.Atomic{Float64}(0.0),
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
            true,
            false,
            false,
            false,
            1.0,
            1.0,
            -prevfloat(typemax(Float32)),
            prevfloat(typemax(Float32)),
            "",
            -prevfloat(typemax(Float32)),
            Nothing,
            Nothing,
            Nothing,
            Nothing,
            Nothing,
            Nothing,
            Nothing,
            false,
            false,
            false,
            Threads.Atomic{Float64}(datetime2unix(now())),
            Threads.Atomic{Int}(0),
            Threads.Atomic{Int}(0),
            Threads.Atomic{Float64}(0.0),
            ReentrantLock(),
        )
    end
end

function update_timestamp(fits::FITSDataSet)
    fits.last_accessed[] = datetime2unix(now())
end

function update_progress(fits::FITSDataSet, total::Integer)
    fits.elapsed[] = datetime2unix(now()) - fits.last_accessed[]
    Threads.atomic_add!(fits.progress, 1)
    fits.total[] = total
end

function serialize_to_bson(fits::FITSDataSet)
    try
        filename = ".cache/" * fits.datasetid * ".bson"
        bson(filename, fits)
    catch e
        println("error serialising the FITS object::$e")
    end
end

function deserialize_from_bson(datasetid)::FITSDataSet
    filename = ".cache/" * datasetid * ".bson"
    return BSON.load(filename)
end

function serialize_to_file(fits::FITSDataSet)
    try
        filename = ".cache/" * fits.datasetid * ".jls"
        serialize(filename, fits)
    catch e
        println("error serialising the FITS object::$e")
    end
end

function deserialize_from_file(datasetid)
    filename = ".cache/" * datasetid * ".jls"
    return deserialize(filename)
end

function get_progress(fits::FITSDataSet)
    progress = 0.0

    if fits.total[] > 0
        progress = 100.0 * Float64(fits.progress[]) / Float64(fits.total[])
    end

    return progress, fits.elapsed[]
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

function process_header(fits::FITSDataSet)
    println("FITS header #records: $(length(fits.header))")

    for i = 1:length(fits.header)
        record = fits.header[i]

        # comments evaluate to 'nothing'
        if !isnothing(record)

            if typeof(record) != String
                continue
            end

            if occursin("ASTRO-F", record)
                fits.is_optical = true
                fits.flux = "logistic"
            end

            if occursin("HSCPIPE", record)
                fits.is_optical = true
                fits.flux = "ratio"
            end

            record = lowercase(record)

            if occursin("suzaku", record) ||
               occursin("hitomi", record) ||
               occursin("x-ray", record)
                fits.is_optical = false
                fits.is_xray = true
                fits.flux = "legacy"
                fits.ignrval = -1.0
            end
        end
    end

    # further examine the header
    try
        record = fits.header["FRAMEID"]

        if occursin("SUPM", record) || occursin("MCSM", record)
            fits.is_optical = true
            fits.flux = "ratio"
        end
    catch e
    end

    try
        fits.ignrval = Float32(fits.header["IGNRVAL"])
    catch e
    end

    try
        fits.datamin = Float32(fits.header["DATAMIN"])
    catch e
    end

    try
        fits.datamax = Float32(fits.header["DATAMAX"])
    catch e
    end

    try
        record = lowercase(fits.header["TELESCOP"])

        if occursin("alma", record) || occursin("vla", record) || occursin("ska", record)
            fits.is_optical = false
        end

        if occursin("nro45", record)
            fits.is_optical = false
            fits.flux = "ratio"
        end

        if occursin("chandra", record)
            fits.is_optical = false
            fits.is_xray = true
        end

        if occursin("kiso", record)
            fits.is_optical = true
            fits.flux = "ratio"
        end
    catch e
    end

    try
        ctype3 = lowercase(fits.header["CTYPE3"])

        if occursin("f", ctype3)
            fits.has_frequency = true
        end

        if occursin("v", ctype3)
            fits.has_velocity = true
        end
    catch e
    end

    try
        cunit3 = lowercase(fits.header["CUNIT3"])

        if occursin("hz", cunit3)
            fits.has_frequency = true
            fits.frame_multiplier = 1.0E0
        end

        if occursin("khz", cunit3)
            fits.has_frequency = true
            fits.frame_multiplier = 1.0E3
        end

        if occursin("mhz", cunit3)
            fits.has_frequency = true
            fits.frame_multiplier = 1.0E6
        end

        if occursin("ghz", cunit3)
            fits.has_frequency = true
            fits.frame_multiplier = 1.0E9
        end

        if occursin("thz", cunit3)
            fits.has_frequency = true
            fits.frame_multiplier = 1.0E12
        end

        if occursin("m/s", cunit3)
            fits.has_velocity = true
            fits.frame_multiplier = 1.0E0
        end

        if occursin("km/s", cunit3)
            fits.has_velocity = true
            fits.frame_multiplier = 1.0E3
        end
    catch e
    end

    try
        cdelt3 = Float32(fits.header["CDELT3"])

        if fits.has_velocity
            fits._cdelt3 = cdelt3 * fits.frame_multiplier / 1000.0
        else
            fits._cdelt3 = 1.0
        end
    catch e
    end
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

    hdu_id = 0

    for hdu in f

        hdu_id = hdu_id + 1

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
        try
            lock(fits.mutex)

            fits.width = width
            fits.height = height
            fits.depth = depth
            fits.header = read_header(hdu)

            fits.has_header = true
        catch e
        finally
            unlock(fits.mutex)
        end

        if !has_header(fits)

            lock(fits.mutex)
            fits.has_error = true
            unlock(fits.mutex)

            break
        end

        try
            process_header(fits)
        catch e
            println("error processing FITS header: $e")
        end

        println(
            "datamin: $(fits.datamin), datamax: $(fits.datamax), ignrval: $(fits.ignrval), _cdelt3: $(fits._cdelt3)",
        )

        # callable in the main thread (invisible to workers...)
        function invalidate(x, datamin, datamax, ignrval)::Bool
            val = Float32(x)

            !isfinite(val) || (val < datamin) || (val > datamax) || (val <= ignrval)
        end

        # only visible to workers...
        @everywhere function invalidate_pixel(x, datamin, datamax, ignrval)::Bool
            val = Float32(x)

            !isfinite(val) || (val < datamin) || (val > datamax) || (val <= ignrval)
        end

        # read a 2D image
        if depth == 1
            println("reading a $width X $height 2D image")

            try
                pixels = zeros(Float32, width, height)
                mask = map(isnan, pixels)

                @time begin

                    pixels = reshape(read(hdu), (width, height))

                    mask = invalidate.(pixels, fits.datamin, fits.datamax, fits.ignrval)

                    # replace NaNs with 0.0
                    pixels[mask] .= 0.0

                    valid_mask = .!mask
                    valid_pixels = pixels[valid_mask]

                    dmin, dmax = extrema(valid_pixels)
                    println("dmin: $dmin, dmax: $dmax")

                    fits.pixels = pixels
                    fits.mask = mask
                end

                println("FITS image dimensions: ", size(fits.pixels))

                lock(fits.mutex)
                fits.has_data = true
                unlock(fits.mutex)

                update_progress(fits, 1)

            catch e
                println("Error reading pixels: $e")
            end

        else
            n = length(workers())

            println(
                "reading a $width X $height X $depth 3D data cube using $n parallel worker(s)",
            )

            try
                indices = Dict{Int32,BitArray{1}}()

                # distributed arrays
                chunks = (1, 1, n)
                pixels = DArray(
                    I -> zeros(Float32, map(length, I)),
                    (width, height, n),
                    workers(),
                    chunks,
                )
                mask = DArray(
                    I -> fill(false, map(length, I)),
                    (width, height, n),
                    workers(),
                    chunks,
                )

                frame_min = zeros(Float32, depth)
                frame_max = zeros(Float32, depth)

                mean_spectrum = zeros(Float32, depth)
                integrated_spectrum = zeros(Float32, depth)

                # TO-DO: reducing pixels, using cdelt3
                # TO-DO: integrated_spectrum, mean_spectrum

                jobs = RemoteChannel(() -> Channel{Int}(32))
                progress = RemoteChannel(() -> Channel{Tuple}(32))

                # fill-in the jobs queue
                @async for i = 1:depth
                    put!(jobs, i)

                    # close the channel after the last value had been sent
                    if i == depth
                        close(jobs)
                    end
                end

                # process the incoming results in the background
                progress_task = @async while true
                    try
                        local queue::BitArray{1}

                        frame, min_val, max_val, mean_val, integrated_val, tid =
                            take!(progress)

                        frame_min[frame] = Float32(min_val)
                        frame_max[frame] = Float32(max_val)
                        mean_spectrum[frame] = Float32(mean_val)
                        integrated_spectrum[frame] = Float32(integrated_val)

                        update_progress(fits, depth)

                        try
                            queue = indices[tid]
                        catch e
                            println("adding a new BitArray@$tid")
                            queue = falses(depth)
                            indices[tid] = queue
                        finally
                            queue[frame] = true
                        end

                    catch e
                        println("progress task completed")
                        break
                    end
                end

                @everywhere function load_fits_frame(
                    datasetid,
                    jobs,
                    progress,
                    path,
                    width,
                    height,
                    datamin,
                    datamax,
                    ignrval,
                    cdelt3,
                    hdu_id,
                    global_pixels::DArray,
                    global_mask::DArray,
                )

                    local frame , frame_pixels , frame_mask
                    local valid_pixels , valid_mask
                    local frame_min , frame_max
                    local mean_spectrum , integrated_spectrum

                    pixels = zeros(Float32, width, height)
                    mask = map(!isnan, pixels)

                    #compressed_pixels = zeros(Float16, width, height)

                    try

                        fits_file = FITS(path)

                        hdu = fits_file[hdu_id]

                        naxes = ndims(hdu)

                        while true
                            frame = take!(jobs)

                            # check #naxes, only read (:, :, frame) if and when necessary
                            if naxes >= 4
                                frame_pixels =
                                    reshape(read(hdu, :, :, frame, 1), (width, height))
                            else
                                frame_pixels =
                                    reshape(read(hdu, :, :, frame), (width, height))
                            end

                            frame_mask =
                                invalidate_pixel.(frame_pixels, datamin, datamax, ignrval)

                            # replace NaNs with 0.0
                            frame_pixels[frame_mask] .= 0.0

                            pixels .+= frame_pixels
                            mask .&= frame_mask

                            # pick out the valid values only
                            valid_mask = .!frame_mask
                            valid_pixels = @view frame_pixels[valid_mask]

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

                            # convert to half-float
                            filename = ".cache/" * datasetid * "." * string(frame) * ".bin"
                            io = open(filename, "w+")
                            compressed_pixels =
                                Mmap.mmap(io, Matrix{Float16}, (width, height))
                            compressed_pixels = map(x -> Float16(x), frame_pixels)
                            Mmap.sync!(compressed_pixels)
                            close(io)

                            # send back the reduced values
                            put!(
                                progress,
                                (
                                    frame,
                                    frame_min,
                                    frame_max,
                                    mean_spectrum,
                                    integrated_spectrum,
                                    myid(),
                                ),
                            )

                            # println("processing frame #$frame")
                        end

                    catch e
                    # println("task $(myid())/$frame::error: $e")
                    finally

                        # copy (pixels,mask) into the distributed arrays (global_pixels,global_mask)
                        try
                            # obtain worker-local references
                            local_pixels = localpart(global_pixels)
                            local_mask = localpart(global_mask)

                            local_pixels[:, :] = pixels
                            local_mask[:, :] = mask
                        catch e
                            println("DArray::$e")
                        end

                        println("loading FITS cube finished")
                    end

                end

                # spawn remote jobs
                @time @sync for w in workers()
                    @spawnat w load_fits_frame(
                        fits.datasetid,
                        jobs,
                        progress,
                        filepath,
                        width,
                        height,
                        fits.datamin,
                        fits.datamax,
                        fits.ignrval,
                        fits._cdelt3,
                        hdu_id,
                        pixels,
                        mask,
                    )
                end

                close(progress)
                wait(progress_task)

                for (key, value) in indices
                    idx = findall(value)
                    println("tid $key::", idx, "($(length(idx)))")
                end

                # distributed pixels & mask + distribution indices
                fits.pixels = pixels
                fits.mask = mask
                fits.indices = indices

                fits.frame_min = frame_min
                fits.frame_max = frame_max
                fits.mean_spectrum = mean_spectrum
                fits.integrated_spectrum = integrated_spectrum

                dmin = minimum(frame_min)
                dmax = maximum(frame_max)
                println("dmin: $dmin, dmax: $dmax")

                println("fits.pixels:", size(fits.pixels))
                # println("fits.pixels:", fits.pixels[1:5,1:5,:])
                # println("mask:", fits.mask)
                # println("frame_min:", fits.frame_min)
                # println("frame_max:", fits.frame_max)
                # println("mean spectrum:", fits.mean_spectrum)
                # println("integrated spectrum:", fits.integrated_spectrum)

                lock(fits.mutex)
                fits.has_data = true
                unlock(fits.mutex)

            catch e
                println("distributed computing error: $e")
            end

        end

        # stop reading FITS HDUs
        break
    end

    update_timestamp(fits)

    # serialize_to_bson(fits)
    serialize_to_file(fits)

end

function preloadFITS(fits::FITSDataSet)
    if (fits.datasetid == "") || (fits.depth <= 1)
        return
    end

    for (key, value) in fits.indices
        idx = findall(value)
        println("tid $key::", idx, "($(length(idx)))")
    end
end