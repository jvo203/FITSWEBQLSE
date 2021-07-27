using Dates;
using DistributedArrays;
using FITSIO;
using JSON;
using CodecLz4;
using Mmap;
using Serialization;
using Statistics;
using Images, ImageTransformations, Interpolations;
using ZfpCompression;
using PhysicalConstants.CODATA2018;

include("classifier.jl")

const NBINS = 1024

@enum Quality low medium high
@enum Intensity mean integrated
@enum Beam CIRCLE SQUARE # "square" is a reserved Julia function

struct ImageToneMapping
    flux::String
    pmin::Float32
    pmax::Float32
    med::Float32
    sensitivity::Float32
    ratio_sensitivity::Float32
    white::Float32
    black::Float32
end

mutable struct FITSDataSet
    # metadata
    datasetid::String
    filesize::Integer
    header::Any
    headerStr::String
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
    compressed_pixels::Any
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
            0,
            Nothing,
            "NULL",
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
            0,
            Nothing,
            "NULL",
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

function serialize_fits(fits::FITSDataSet)
    n = length(workers())

    try
        lock(fits.mutex)

        filename =
            ".cache" *
            Base.Filesystem.path_separator *
            fits.datasetid *
            Base.Filesystem.path_separator *
            "state.jls"
        io = open(filename, "w+")

        serialize(io, n)
        serialize(io, fits.datasetid)
        serialize(io, fits.filesize)
        serialize(io, fits.header)
        serialize(io, fits.headerStr)
        serialize(io, fits.width)
        serialize(io, fits.height)
        serialize(io, fits.depth)

        serialize(io, fits.is_optical)
        serialize(io, fits.is_xray)
        serialize(io, fits.has_frequency)
        serialize(io, fits.has_velocity)
        serialize(io, fits.frame_multiplier)
        serialize(io, fits._cdelt3)
        serialize(io, fits.datamin)
        serialize(io, fits.datamax)
        serialize(io, fits.flux)
        serialize(io, fits.ignrval)

        if fits.depth == 1
            serialize(io, fits.pixels)
            serialize(io, fits.mask)
        end

        # depth > 1
        # skipping fits.pixels (DArray does not serialize well)
        # skipping fits.mask (DArray does not serialize well)
        # skipping fits.compressed_pixels (Futures do not serialize)

        serialize(io, fits.indices)
        serialize(io, fits.frame_min)
        serialize(io, fits.frame_max)
        serialize(io, fits.mean_spectrum)
        serialize(io, fits.integrated_spectrum)

        serialize(io, fits.has_header)
        serialize(io, fits.has_data)
        serialize(io, fits.has_error)
        # skipping fits.last_accessed
        serialize(io, fits.progress)
        serialize(io, fits.total)
        # skipping fits.elapsed
        # skipping fits.mutex 

        close(io)
    catch e
        println("error serialising the FITS object::$e")
    finally
        unlock(fits.mutex)
    end
end

function deserialize_fits(datasetid)
    fits = FITSDataSet(datasetid)

    filename =
        ".cache" *
        Base.Filesystem.path_separator *
        fits.datasetid *
        Base.Filesystem.path_separator *
        "state.jls"
    io = open(filename)

    n = length(workers())

    if deserialize(io) != n
        println("The number of parallel processes does not match. Invalidating the cache.")
        close(io)

        dirname = ".cache" * Base.Filesystem.path_separator * fits.datasetid
        rm(dirname, recursive = true)

        error("The number of parallel processes does not match. Invalidating the cache.")
    end

    fits.datasetid = deserialize(io)
    fits.filesize = deserialize(io)
    fits.header = deserialize(io)
    fits.headerStr = deserialize(io)
    fits.width = deserialize(io)
    fits.height = deserialize(io)
    fits.depth = deserialize(io)

    fits.is_optical = deserialize(io)
    fits.is_xray = deserialize(io)
    fits.has_frequency = deserialize(io)
    fits.has_velocity = deserialize(io)
    fits.frame_multiplier = deserialize(io)
    fits._cdelt3 = deserialize(io)
    fits.datamin = deserialize(io)
    fits.datamax = deserialize(io)
    fits.flux = deserialize(io)
    fits.ignrval = deserialize(io)

    if fits.depth == 1
        fits.pixels = deserialize(io)
        fits.mask = deserialize(io)
    end

    # depth > 1
    # skipping fits.pixels (DArray does not serialize well)
    # skipping fits.mask (DArray does not serialize well)
    # skipping fits.compressed_pixels

    fits.indices = deserialize(io)
    fits.frame_min = deserialize(io)
    fits.frame_max = deserialize(io)
    fits.mean_spectrum = deserialize(io)
    fits.integrated_spectrum = deserialize(io)

    fits.has_header = deserialize(io)
    fits.has_data = deserialize(io)
    fits.has_error = deserialize(io)
    # skipping fits.last_accessed
    fits.progress = deserialize(io)
    fits.total = deserialize(io)
    # skipping fits.elapsed
    # skipping fits.mutex

    close(io)

    return fits
end

function serialize_to_file(fits::FITSDataSet)
    try
        filename = ".cache" * Base.Filesystem.path_separator * fits.datasetid * "/state.jls"
        serialize(filename, fits)
    catch e
        println("error serialising the FITS object::$e")
    end
end

function deserialize_from_file(datasetid)
    filename = ".cache" * Base.Filesystem.path_separator * datasetid * "/state.jls"
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

function get_frequency_range(fits::FITSDataSet)
    local crval3 , cdelt3 , crpix3

    header = fits.header

    # any errors will be propagated back and handled higher up
    crval3 = header["CRVAL3"]
    cdelt3 = header["CDELT3"]
    crpix3 = header["CRPIX3"]

    c = SpeedOfLightInVacuum
    f1 = f2 = NaN

    if fits.has_velocity

        # we need the rest frequency too
        restfrq = NaN # default value

        try
            restfrq = header["RESTFRQ"]
        catch e
        end

        try
            restfrq = header["RESTFREQ"]
        catch e
        end

        if !isfinite(restfrq)
            error("Could not obtain the rest frequency.")
        end

        v1 =
            crval3 * fits.frame_multiplier + cdelt3 * fits.frame_multiplier * (1.0 - crpix3)
        v2 =
            crval3 * fits.frame_multiplier +
            cdelt3 * fits.frame_multiplier * (fits.depth - crpix3)

        f1 = restfrq * sqrt((1.0 - v1 / c) / (1.0 + v1 / c))
        f2 = restfrq * sqrt((1.0 - v2 / c) / (1.0 + v2 / c))

    end

    if fits.has_frequency

        f1 =
            crval3 * fits.frame_multiplier + cdelt3 * fits.frame_multiplier * (1.0 - crpix3)
        f2 =
            crval3 * fits.frame_multiplier +
            cdelt3 * fits.frame_multiplier * (fits.depth - crpix3)

    end

    if !isfinite(f1) || !isfinite(f2)
        error("Could not obtain {f1,f2}.")
    end

    freq_start = min(f1, f2) / 1.0E9 # [Hz -> GHz]
    freq_end = max(f1, f2) / 1.0E9 # [Hz -> GHz]

    return (freq_start, freq_end)
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

    if Sys.iswindows()
        filepath = replace(filepath, "/" => "\\")
    end

    println("loading $filepath::$(fits.datasetid)")

    local f , width::Integer , height::Integer , depth::Integer
    local header

    try
        fits.filesize = filesize(filepath)
        f = FITS(filepath)

        println(f)
    catch e
        println(e)
        return
    end

    try
        cache_dir = ".cache" * Base.Filesystem.path_separator * fits.datasetid

        if !isdir(cache_dir)
            mkdir(cache_dir)
        end
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
            fits.headerStr = read_header(hdu, String)

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
                    fits.mask = .!mask # negate the mask so that <true> indicates valid pixels
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

                    compressed_frames = Dict{Int32,Matrix{Float16}}()
                    # compressed_pixels = zeros(Float16, width, height)

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

                            compressed_pixels = map(x -> Float16(x), frame_pixels)
                            compressed_frames[frame] = compressed_pixels

                            # convert to half-float
                            cache_dir =
                                ".cache" * Base.Filesystem.path_separator * datasetid
                            filename =
                                cache_dir *
                                Base.Filesystem.path_separator *
                                string(frame) *
                                ".f16"

                            io = open(filename, "w+")
                            write(io, compressed_pixels)
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

                            # negate the mask so that <true> indicates valid pixels
                            mask = .!mask

                            local_pixels[:, :] = pixels
                            local_mask[:, :] = mask

                            cache_dir =
                                ".cache" * Base.Filesystem.path_separator * datasetid

                            filename =
                                cache_dir *
                                Base.Filesystem.path_separator *
                                string(myid()) *
                                ".pixels"
                            serialize(filename, pixels)

                            filename =
                                cache_dir *
                                Base.Filesystem.path_separator *
                                string(myid()) *
                                ".mask"
                            serialize(filename, mask)

                        catch e
                            println("DArray::$e")
                        end

                        println("loading FITS cube finished")
                    end

                    # do not wait, trigger garbage collection *NOW*
                    GC.gc()

                    return compressed_frames
                end

                # spawn remote jobs, collecting the Futures along the way
                # Remote Access Service
                ras = [
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
                    ) for w in workers()
                ]

                println("ras: ", ras)

                @time wait.(ras)
                # println("results: ", fetch.(ras))

                close(progress)
                wait(progress_task)

                for (key, value) in indices
                    idx = findall(value)
                    println("tid $key::", idx, "($(length(idx)))")
                end

                # distributed pixels & mask + distribution indices
                fits.pixels = pixels
                fits.mask = mask
                fits.compressed_pixels = ras
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
    serialize_fits(fits)

end

function restoreImage(fits::FITSDataSet)
    if (fits.datasetid == "") || (fits.depth <= 1)
        return
    end

    n = length(workers())
    width = fits.width
    height = fits.height

    chunks = (1, 1, n)
    pixels =
        DArray(I -> zeros(Float32, map(length, I)), (width, height, n), workers(), chunks)
    mask = DArray(I -> fill(false, map(length, I)), (width, height, n), workers(), chunks)

    # restore DArrays
    @everywhere function preload_image(datasetid, global_pixels, global_mask)

        cache_dir = ".cache" * Base.Filesystem.path_separator * datasetid

        try
            filename =
                cache_dir * Base.Filesystem.path_separator * string(myid()) * ".pixels"
            pixels = deserialize(filename)

            filename = cache_dir * Base.Filesystem.path_separator * string(myid()) * ".mask"
            mask = deserialize(filename)

            # obtain worker-local references
            local_pixels = localpart(global_pixels)
            local_mask = localpart(global_mask)

            local_pixels[:, :] = pixels
            local_mask[:, :] = mask

        catch e
            println("DArray::$e")
            return false
        end

        return true
    end

    # Remote Access Service
    ras = [@spawnat w preload_image(fits.datasetid, pixels, mask) for w in workers()]

    # wait for the pixels & mask to be restored
    bSuccess = all(fetch.(ras))

    fits.pixels = pixels
    fits.mask = mask

    # adjust has_data and has_error
    lock(fits.mutex)
    fits.has_data = bSuccess
    fits.has_error = !bSuccess
    unlock(fits.mutex)
end

function restoreData(fits::FITSDataSet)
    if (fits.datasetid == "") || (fits.depth <= 1)
        return
    end

    @everywhere function preload_frames(datasetid, width, height, idx)
        compressed_frames = Dict{Int32,Matrix{Float16}}()

        for frame in idx
            try
                cache_dir = ".cache" * Base.Filesystem.path_separator * datasetid
                filename =
                    cache_dir * Base.Filesystem.path_separator * string(frame) * ".f16"

                io = open(filename) # default is read-only
                compressed_pixels = Mmap.mmap(io, Matrix{Float16}, (width, height))
                close(io)

                compressed_frames[frame] = compressed_pixels

                # println("restored frame #$frame")

            catch e
                println(e)
            end
        end

        return compressed_frames
    end

    # Remote Access Service
    ras = [
        @spawnat w preload_frames(fits.datasetid, fits.width, fits.height, findall(value)) for (w, value) in fits.indices
    ]

    println("ras: ", ras)

    fits.compressed_pixels = ras
end

function get_screen_scale(x::Integer)

    return floor(0.9 * Float32(x))

end

function get_image_scale_square(
    width::Integer,
    height::Integer,
    img_width::Integer,
    img_height::Integer,
)

    screen_dimension = get_screen_scale(min(width, height))
    image_dimension = Float32(max(img_width, img_height))

    return screen_dimension / image_dimension

end

function get_image_scale(
    width::Integer,
    height::Integer,
    img_width::Integer,
    img_height::Integer,
)

    scale = Float32(1.0)

    if img_width == img_height
        return get_image_scale_square(width, height, img_width, img_height)
    end

    if img_height < img_width
        screen_dimension = 0.9 * Float32(height)
        image_dimension = Float32(img_height)
        scale = screen_dimension / image_dimension
        new_image_width = scale * img_width

        if new_image_width > 0.8 * Float32(width)
            screen_dimension = 0.8 * Float32(width)
            image_dimension = Float32(img_width)
            scale = screen_dimension / image_dimension
        end

        return scale
    end

    if img_width < img_height

        screen_dimension = 0.8 * Float32(width)
        image_dimension = Float32(img_width)
        scale = screen_dimension / image_dimension
        new_image_height = scale * img_height

        if new_image_height > 0.9 * Float32(height)
            screen_dimension = 0.9 * Float32(height)
            image_dimension = img_height
            scale = screen_dimension / image_dimension
        end

        return scale
    end

    # default scale
    return scale
end

@views function inherent_image_dimensions(mask)

    width = size(mask)[1]
    height = size(mask)[2]

    x1 = 1
    x2 = width
    y1 = 1
    y2 = height

    # go through the 2D image mask
    # truncating the NaN values along the X & Y axes

    # x1
    for k = 1:width
        x1 = k

        if any(mask[k, :])
            break
        end
    end

    # x2
    for k = width:-1:1
        x2 = k

        if any(mask[k, :])
            break
        end
    end

    # y1
    for k = 1:height
        y1 = k

        if any(mask[:, k])
            break
        end
    end

    # y2
    for k = height:-1:1
        y2 = k

        if any(mask[:, k])
            break
        end
    end

    # println("original dimensions: $width x $height")

    width = x2 - x1 + 1
    height = y2 - y1 + 1

    # println("inherent dimensions: $width x $height")

    return (width, height)

end

function getImage(fits::FITSDataSet, width::Integer, height::Integer)
    local scale::Float32 , pixels , mask
    local image_width::Integer , image_height::Integer
    local inner_width::Integer , inner_height::Integer

    inner_width = 0
    inner_height = 0
    bDownsize = false

    println("getImage::$(fits.datasetid)/($width)/($height)")

    # calculate scale, downsize when applicable

    # get the maximum common bounding box
    if isa(fits.mask, DArray)

        # go through the collated mask fetching inner dims from all workers
        @everywhere function get_inner_dimensions(global_mask::DArray)
            fits_dims = size(global_mask)
            fits_width = fits_dims[1]
            fits_height = fits_dims[2]

            # obtain a worker-local mask
            local_mask = reshape(localpart(global_mask), fits_dims[1:2])

            return inherent_image_dimensions(local_mask)

        end

        try
            # Remote Access Service
            ras = [@spawnat w get_inner_dimensions(fits.mask) for w in workers()]

            # fetch results
            res = fetch.(ras)

            # reduce the results
            for dims in res
                dimx, dimy = dims

                inner_width = max(inner_width, dimx)
                inner_height = max(inner_height, dimy)
            end

            println(res)

        catch e
            println(e)

            # on error revert to the original FITS dimensions
            inner_width = fits.width
            inner_height = fits.height
        end
    else
        # mask is a "normal" local Array
        try
            inner_width, inner_height = inherent_image_dimensions(fits.mask)
        catch e
            println(e)

            # on error revert to the original FITS dimensions
            inner_width = fits.width
            inner_height = fits.height
        end
    end

    try
        scale = get_image_scale(width, height, inner_width, inner_height)
    catch e
        println(e)
        scale = 1.0
    end

    if scale < 1.0
        image_width = round(Integer, scale * fits.width)
        image_height = round(Integer, scale * fits.height)
        bDownsize = true
    else
        image_width = fits.width
        image_height = fits.height
    end

    println("scale = $scale, image: $image_width x $image_height, bDownsize: $bDownsize")

    if isa(fits.pixels, DArray) && isa(fits.mask, DArray)
        println("distributed pixels,mask")

        pixels = zeros(Float32, image_width, image_height)
        mask = map(isnan, pixels)

        # create a remote channel for receiving the results
        image_res = RemoteChannel(() -> Channel{Tuple}(32))

        image_task = @async while true
            try
                thread_pixels, thread_mask = take!(image_res)

                if (typeof(pixels) != typeof(thread_pixels)) ||
                   (typeof(mask) != typeof(thread_mask))
                    # println("pixels/mask type mismatch")
                    # println(typeof(pixels), typeof(thread_pixels))
                    # println(typeof(mask), typeof(thread_mask))                
                end

                if (size(pixels) != size(thread_pixels)) ||
                   (size(mask) != size(thread_mask))
                    println("pixels/mask dimension mismatch")
                    # error("pixels/mask dimension mismatch")
                else
                    pixels .+= thread_pixels
                    mask .|= thread_mask
                end

                println("received (pixels,mask)")
            catch e
                println("image task completed")
                break
            end
        end

        @everywhere function collate_images(
            results,
            global_pixels::DArray,
            global_mask::DArray,
            width::Integer,
            height::Integer,
            downsize::Bool,
        )

            fits_dims = size(global_pixels)
            fits_width = fits_dims[1]
            fits_height = fits_dims[2]

            # obtain worker-local references
            local_pixels = reshape(localpart(global_pixels), fits_dims[1:2])
            local_mask = reshape(localpart(global_mask), fits_dims[1:2])

            # println("FITS dimensions: $fits_width x $fits_height; ", size(local_pixels))

            # send back the (optionally downsized) image
            if !downsize
                put!(results, (local_pixels, local_mask))
            else
                # downsize the pixels & mask            
                try
                    pixels = Float32.(imresize(local_pixels, (width, height)))
                    mask = Bool.(imresize(local_mask, (width, height), method = Constant())) # use Nearest-Neighbours for the mask
                    put!(results, (pixels, mask))
                catch e
                    println(e)
                end
            end

        end

        @time @sync for w in workers()
            @spawnat w collate_images(
                image_res,
                fits.pixels,
                fits.mask,
                image_width,
                image_height,
                bDownsize,
            )
        end

        close(image_res)
        wait(image_task)

    else
        println("local pixels,mask")

        # copy and optionally downsize local pixels,mask
        if !bDownsize
            pixels = fits.pixels
            mask = fits.mask
        else
            # downsize the pixels & mask            
            try
                pixels = Float32.(imresize(fits.pixels, (image_width, image_height)))
                mask =
                    Bool.(
                        imresize(
                            fits.mask,
                            (image_width, image_height),
                            method = Constant(),
                        ),
                    ) # use Nearest-Neighbours for the mask            
            catch e
                println(e)
            end
        end
    end

    # next make a histogram
    valid_pixels = @view pixels[mask]

    println("#valid_pixels: ", length(valid_pixels))

    @time edges, bins = imhist(valid_pixels)
    nbins = length(edges)

    println("pixel range: ", edges, "; bins: ", bins, "; nbins = ", nbins)

    pmin = first(edges)
    pmax = last(edges)
    # @time pmin, pmax = extrema(valid_pixels)
    @time med = median(valid_pixels)
    println("extrema: $pmin ~ $pmax; median = $med")

    pixelsN = filter(x -> x < med, valid_pixels)
    pixelsP = filter(x -> x > med, valid_pixels)

    countN = length(pixelsN)
    countP = length(pixelsP)

    sumN = sum(map(x -> abs(x - med), pixelsN))
    sumP = sum(map(x -> abs(x - med), pixelsP))

    # println("countN: $countN, countP: $countP, sumN: $sumN, sumP: $sumP")

    mad = 0.0
    madN = 0.0
    madP = 0.0

    if countN > 0
        madN = sumN / countN
    end

    if countP > 0
        madP = sumP / countP
    end

    if countN + countP > 0
        mad = (sumN + sumP) / (countN + countP)
    end

    println("madN = $madN, madP = $madP, mad = $mad")

    # ALMAWebQL v2 - style
    u = 7.5
    black = max(pmin, med - u * madN)
    white = min(pmax, med + u * madP)
    sensitivity = 1.0 / (white - black)
    ratio_sensitivity = sensitivity

    if fits.is_optical
        u = 0.5
        v = 15.0
        black = max(pmin, med - u * madN)
        white = min(pmax, med + u * madP)
        sensitivity = 1.0 / (white - black)
        ratio_sensitivity = sensitivity

        # TO-DO: auto-brightness
    end

    println(
        "black: $black, white: $white, sensitivity: $sensitivity, ratio_sensitivity: $ratio_sensitivity",
    )

    if fits.flux == ""
        acc = accumulate(+, bins)
        acc_tot = sum(bins)
        println("accumulator length: $(length(acc)); total = $acc_tot")
        slots = Float64.(acc) ./ Float64(acc_tot)

        # upsample the slots array to <NBINS>
        cum = imresize(slots, (NBINS,), method = Linear())
        println("slots length: $(length(cum))")

        try
            fits.flux = histogram_classifier(cum)
            println("flux: ", fits.flux)

            # save the updated flux
            serialize_fits(fits)
        catch e
            println(e)
        end
    end

    println("getImage done")

    tone_mapping = ImageToneMapping(
        fits.flux,
        pmin,
        pmax,
        med,
        sensitivity,
        ratio_sensitivity,
        white,
        black,
    )

    return (bins, tone_mapping, pixels, mask)
end

function getJSON(fits::FITSDataSet)
    local CD1_1 , CD1_2 , CD2_1 , CD2_2
    local CRVAL1 , CDELT1 , CRPIX1 , CUNIT1 , CTYPE1
    local CRVAL2 , CDELT2 , CRPIX2 , CUNIT2 , CTYPE2
    local CRVAL3 , CDELT3 , CRPIX3 , CUNIT3 , CTYPE3
    local BMAJ , BMIN , BPA , BUNIT , BTYPE , SPECSYS
    local RESTFRQ , OBSRA , OBSDEC
    local OBJECT , DATEOBS , TIMESYS , LINE , FILTER

    try
        buf = IOBuffer()

        header = fits.header

        try
            CD1_1 = header["CD1_1"]
        catch e
            CD1_1 = NaN
        end

        try
            CD1_2 = header["CD1_2"]
        catch e
            CD1_2 = NaN
        end

        try
            CD2_1 = header["CD2_1"]
        catch e
            CD2_1 = NaN
        end

        try
            CD2_2 = header["CD2_2"]
        catch e
            CD2_2 = NaN
        end

        try
            CRVAL1 = header["CRVAL1"]
        catch e
            CRVAL1 = NaN
        end

        try
            CDELT1 = header["CDELT1"]
        catch e
            CDELT1 = NaN
        end

        try
            CRPIX1 = header["CRPIX1"]
        catch e
            CRPIX1 = NaN
        end

        try
            CUNIT1 = header["CUNIT1"]
        catch e
            CUNIT1 = ""
        end

        try
            CTYPE1 = header["CTYPE1"]
        catch e
            CTYPE1 = ""
        end

        try
            CRVAL2 = header["CRVAL2"]
        catch e
            CRVAL2 = NaN
        end

        try
            CDELT2 = header["CDELT2"]
        catch e
            CDELT2 = NaN
        end

        try
            CRPIX2 = header["CRPIX2"]
        catch e
            CRPIX2 = NaN
        end

        try
            CUNIT2 = header["CUNIT2"]
        catch e
            CUNIT2 = ""
        end

        try
            CTYPE2 = header["CTYPE2"]
        catch e
            CTYPE2 = ""
        end

        try
            CRVAL3 = header["CRVAL3"]
        catch e
            CRVAL3 = NaN
        end

        try
            CDELT3 = header["CDELT3"]
        catch e
            CDELT3 = NaN
        end

        try
            CRPIX3 = header["CRPIX3"]
        catch e
            CRPIX3 = NaN
        end

        try
            CUNIT3 = header["CUNIT3"]
        catch e
            CUNIT3 = ""
        end

        try
            CTYPE3 = header["CTYPE3"]
        catch e
            CTYPE3 = ""
        end

        try
            BMAJ = header["BMAJ"]
        catch e
            BMAJ = NaN
        end

        try
            BMIN = header["BMIN"]
        catch e
            BMIN = NaN
        end

        try
            BPA = header["BPA"]
        catch e
            BPA = NaN
        end

        try
            BUNIT = header["BUNIT"]
        catch e
            BUNIT = ""
        end

        try
            BTYPE = header["BTYPE"]
        catch e
            BTYPE = ""
        end

        try
            SPECSYS = header["SPECSYS"]
        catch e
            SPECSYS = ""
        end

        RESTFRQ = NaN # default value
        try
            RESTFRQ = header["RESTFRQ"]
        catch e
        end

        try
            RESTFRQ = header["RESTFREQ"]
        catch e
        end

        try
            OBSRA = header["OBSRA"]
        catch e
            OBSRA = NaN
        end

        try
            OBSDEC = header["OBSDEC"]
        catch e
            OBSDEC = NaN
        end

        try
            OBJECT = header["OBJECT"]
        catch e
            OBJECT = ""
        end

        try
            DATEOBS = header["DATE-OBS"]
        catch e
            DATEOBS = ""
        end

        try
            TIMESYS = header["TIMESYS"]
        catch e
            TIMESYS = ""
        end

        LINE = "" # default value
        try
            LINE = header["LINE"]
        catch e
        end

        try
            LINE = header["J_LINE"]
        catch e
        end

        try
            FILTER = header["FILTER"]
        catch e
            FILTER = ""
        end

        dict = Dict(
            "width" => fits.width,
            "height" => fits.height,
            "depth" => fits.depth,
            "polarisation" => 1,
            "filesize" => fits.filesize,
            "IGNRVAL" => fits.ignrval,
            "CD1_1" => CD1_1,
            "CD1_2" => CD1_2,
            "CD2_1" => CD2_1,
            "CD2_2" => CD2_2,
            "CRVAL1" => CRVAL1,
            "CDELT1" => CDELT1,
            "CRPIX1" => CRPIX1,
            "CUNIT1" => CUNIT1,
            "CTYPE1" => CTYPE1,
            "CRVAL2" => CRVAL2,
            "CDELT2" => CDELT2,
            "CRPIX2" => CRPIX2,
            "CUNIT2" => CUNIT2,
            "CTYPE2" => CTYPE2,
            "CRVAL3" => CRVAL3,
            "CDELT3" => CDELT3,
            "CRPIX3" => CRPIX3,
            "CUNIT3" => CUNIT3,
            "CTYPE3" => CTYPE3,
            "BMAJ" => BMAJ,
            "BMIN" => BMIN,
            "BPA" => BPA,
            "BUNIT" => BUNIT,
            "BTYPE" => BTYPE,
            "SPECSYS" => SPECSYS,
            "RESTFRQ" => RESTFRQ,
            "OBSRA" => OBSRA,
            "OBSDEC" => OBSDEC,
            "OBJECT" => OBJECT,
            "DATEOBS" => DATEOBS,
            "TIMESYS" => TIMESYS,
            "LINE" => LINE,
            "FILTER" => FILTER,
        )

        write(buf, JSON.json(dict))

        json = String(take!(buf))

        return json
    catch e
        println("getJSON::$e")
        return "{}"
    end
end

function get_freq2vel_bounds(
    fits::FITSDataSet,
    frame_start::Float64,
    frame_end::Float64,
    ref_freq::Float64,
)

    header = fits.header

    # any errors will be propagated back and handled higher up
    crval3 = header["CRVAL3"]
    cdelt3 = header["CDELT3"]
    crpix3 = header["CRPIX3"]

    c = SpeedOfLightInVacuum

    fRatio = frame_start / ref_freq
    v1 = (1.0 - fRatio * fRatio) / (1.0 + fRatio * fRatio) * c

    fRatio = frame_end / ref_freq
    v2 = (1.0 - fRatio * fRatio) / (1.0 + fRatio * fRatio) * c

    x1 = crpix3 + (v1 - crval3 * fits.frame_multiplier) / (cdelt3 * fits.frame_multiplier)
    x2 = crpix3 + (v2 - crval3 * fits.frame_multiplier) / (cdelt3 * fits.frame_multiplier)

    first_frame = Integer(round(x1))
    last_frame = Integer(round(x2))

    # reverse the direction
    if cdelt3 < 0.0
        first_frame = 1 + fits.depth - first_frame
        last_frame = 1 + fits.depth - last_frame
    end

    # impose ordering
    if last_frame < first_frame
        tmp = first_frame
        first_frame = last_frame
        last_frame = tmp
    end

    first_frame = max(1, first_frame)
    last_frame = min(fits.depth, last_frame)

    return (first_frame, last_frame)

end

function get_frequency_bounds(fits::FITSDataSet, freq_start::Float64, freq_end::Float64)

    if (freq_start <= 0.0) || (freq_end <= 0.0)
        error("Frequency <= 0.0!!!")
    end

    header = fits.header

    # any errors will be propagated back and handled higher up
    crval3 = header["CRVAL3"]
    cdelt3 = header["CDELT3"]
    crpix3 = header["CRPIX3"]

    f1 = crval3 * fits.frame_multiplier + cdelt3 * fits.frame_multiplier * (1.0 - crpix3)
    f2 =
        crval3 * fits.frame_multiplier +
        cdelt3 * fits.frame_multiplier * (fits.depth - crpix3)

    band_lo = min(f1, f2)
    band_hi = max(f1, f2)

    local first_frame , last_frame

    if cdelt3 > 0.0
        first_frame =
            1 +
            Integer(round((freq_start - band_lo) / (band_hi - band_lo) * (fits.depth - 1)))
        last_frame =
            1 +
            Integer(round((freq_end - band_lo) / (band_hi - band_lo) * (fits.depth - 1)))
    else
        first_frame =
            1 +
            Integer(round((band_hi - freq_start) / (band_hi - band_lo) * (fits.depth - 1)))
        last_frame =
            1 +
            Integer(round((band_hi - freq_end) / (band_hi - band_lo) * (fits.depth - 1)))
    end

    # impose ordering
    if last_frame < first_frame
        tmp = first_frame
        first_frame = last_frame
        last_frame = tmp
    end

    first_frame = max(1, first_frame)
    last_frame = min(fits.depth, last_frame)

    return (first_frame, last_frame)

end

function get_velocity_bounds(fits::FITSDataSet, vel_start::Float64, vel_end::Float64)

    header = fits.header

    # any errors will be propagated back and handled higher up
    crval3 = header["CRVAL3"]
    cdelt3 = header["CDELT3"]
    crpix3 = header["CRPIX3"]

    v1 = crval3 * fits.frame_multiplier + cdelt3 * fits.frame_multiplier * (1.0 - crpix3)
    v2 =
        crval3 * fits.frame_multiplier +
        cdelt3 * fits.frame_multiplier * (Float64(fits.depth) - crpix3)

    band_lo = min(v1, v2)
    band_hi = max(v1, v2)

    local first_frame , last_frame

    if cdelt3 > 0.0
        first_frame =
            1 +
            Integer(round((vel_start - band_lo) / (band_hi - band_lo) * (fits.depth - 1)))
        last_frame =
            1 + Integer(round((vel_end - band_lo) / (band_hi - band_lo) * (fits.depth - 1)))
    else
        first_frame =
            1 +
            Integer(round((band_hi - vel_start) / (band_hi - band_lo) * (fits.depth - 1)))
        last_frame =
            1 + Integer(round((band_hi - vel_end) / (band_hi - band_lo) * (fits.depth - 1)))
    end

    # impose ordering
    if last_frame < first_frame
        tmp = first_frame
        first_frame = last_frame
        last_frame = tmp
    end

    first_frame = max(1, first_frame)
    last_frame = min(fits.depth, last_frame)

    return (first_frame, last_frame)

end

function get_spectrum_range(
    fits::FITSDataSet,
    frame_start::Float64,
    frame_end::Float64,
    ref_freq::Float64,
)

    if fits.depth <= 1
        return (fits.depth, fits.depth)
    end

    if fits.has_velocity && ref_freq > 0.0
        return get_freq2vel_bounds(fits, frame_start, frame_end, ref_freq)
    end

    if fits.has_frequency
        return get_frequency_bounds(fits, frame_start, frame_end)
    end

    if fits.has_velocity
        return get_velocity_bounds(fits, frame_start, frame_end)
    end

    error("$(fits.datasetid)::Cannot get a spectrum range")

end

function getViewportSpectrum(fits::FITSDataSet, req::Dict{String,Any})

    x1 = req["x1"]
    x2 = req["x2"]
    y1 = req["y1"]
    y2 = req["y2"]

    image = req["image"]
    width = req["width"]
    height = req["height"]

    quality::Quality = medium # by default use medium quality
    try
        quality = eval(Meta.parse(req["quality"]))
    catch e
    end

    frame_start = Float64(req["frame_start"])
    frame_end = Float64(req["frame_end"])

    ref_freq = 0.0 # by default ref_freq is missing
    try
        ref_freq = Float64(req["ref_freq"])
    catch e
    end

    first_frame, last_frame = get_spectrum_range(fits, frame_start, frame_end, ref_freq)
    frame_length = last_frame - first_frame + 1
    println(
        "[get_spectrum_range] :: [$first_frame, $last_frame] <$frame_length> ($(fits.depth))",
    )

    # sanity checks
    x1 = max(1, x1)
    y1 = max(1, y1)
    x2 = min(fits.width, x2)
    y2 = min(fits.height, y2)

    # viewport dimensions
    dimx = abs(x2 - x1 + 1)
    dimy = abs(y2 - y1 + 1)

    if fits.compressed_pixels == Nothing && image
        # handle a 2D image
        println("2D image::viewport: $image")

        pixels = fits.pixels[x1:x2, y1:y2]
        mask = fits.mask[x1:x2, y1:y2]

        native_size = dimx * dimy
        viewport_size = width * height

        if native_size > viewport_size
            # downsize the pixels & mask      
            scale = Float32(width) / Float32(dimx)
            println("re-sizing the viewport down to $(Int32(round(100.0*scale)))%")

            try
                pixels = Float32.(imresize(pixels, (width, height)))
                mask = Bool.(imresize(mask, (width, height), method = Constant())) # use Nearest-Neighbours for the mask
            catch e
                println(e)
            end
        end

        dims = size(pixels)
        view_width = dims[1]
        view_height = dims[2]

        resp = IOBuffer()

        write(resp, Int32(view_width))
        write(resp, Int32(view_height))

        # compress pixels with ZFP
        prec = ZFP_MEDIUM_PRECISION

        if quality == high
            prec = ZFP_HIGH_PRECISION
        elseif quality == medium
            prec = ZFP_MEDIUM_PRECISION
        elseif quality == low
            prec = ZFP_LOW_PRECISION
        end

        compressed_pixels = zfp_compress(pixels, precision = prec)
        write(resp, Int32(length(compressed_pixels)))
        write(resp, compressed_pixels)

        compressed_mask = lz4_hc_compress(collect(flatten(UInt8.(mask))))
        write(resp, Int32(length(compressed_mask)))
        write(resp, compressed_mask)

        return (resp, Nothing)
    else
        # handle ras distributed Futures
        println("3D cube::viewport: $image")

        beam = eval(Meta.parse(uppercase(req["beam"])))
        intensity = eval(Meta.parse(req["intensity"]))

        # calculate the centre and squared radius
        cx = abs(x1 + x2) / 2
        cy = abs(y1 + y2) / 2
        r = min(abs(x2 - x1) / 2, abs(y2 - y1) / 2)
        r2 = r * r

        local pixels , mask

        if image
            pixels = zeros(Float32, dimx, dimy)
            mask = map(!isnan, pixels)
        end

        spectrum = zeros(Float32, frame_length)

        results = RemoteChannel(() -> Channel{Tuple}(32))

        results_task = @async while true
            try
                thread_pixels, thread_mask, thread_spectrum = take!(results)

                if thread_pixels != Nothing && thread_mask != Nothing
                    pixels .+= thread_pixels
                    mask .&= thread_mask
                end
            catch e
                println("results task completed")
                break
            end
        end

        @everywhere function calculateViewportSpectrum(
            datasetid,
            first_channel,
            last_channel,
            x1,
            x2,
            y1,
            y2,
            bImage,
            idx,
            queue,
        )
            put!(queue, (Nothing, Nothing, Nothing))
        end

        # for each Future in ras find the corresponding worker
        # launch jobs on each worker, pass the channel indices
        ras = []
        for job in fits.compressed_pixels
            idx = findall(fits.indices[job.where])
            println(job, "\twhere:", job.where, "::", idx)

            append!(
                ras,
                @spawnat job.where calculateViewportSpectrum(
                    fits.datasetid,
                    first_frame,
                    last_frame,
                    x1,
                    x2,
                    y1,
                    y2,
                    image,
                    idx,
                    results,
                )
            )
        end

        println("ras: ", ras)

        @time wait.(ras)

        close(results)
        wait(results_task)

        return (Nothing, Nothing)
    end

end