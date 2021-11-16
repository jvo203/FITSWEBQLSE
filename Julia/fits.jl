import Base.Iterators: flatten
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
using PhysicalConstants.CODATA2018: c_0;
using ThreadsX;

const MADV_WILLNEED = 3

include("classifier.jl")
# include("IPP.jl")
include("ISPC.jl")

const NBINS = 1024

@enum Quality low medium high
@everywhere @enum Intensity MEAN INTEGRATED
@everywhere @enum Beam CIRCLE SQUARE # "square" is a reserved Julia function

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

@everywhere struct VideoToneMapping
    flux::String
    dmin::Float32
    dmax::Float32
    median::Float32
    sensitivity::Float32
    slope::Float32
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

    # all-data statistics (needed by video streams)
    dmin::Float32
    dmax::Float32
    data_median::Float32
    data_mad::Float32
    data_mad₊::Float32
    data_mad₋::Float32
    video_ready::Bool

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
            -prevfloat(typemax(Float32)),
            prevfloat(typemax(Float32)),
            NaN32,
            NaN32,
            NaN32,
            NaN32,
            false,
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
            -prevfloat(typemax(Float32)),
            prevfloat(typemax(Float32)),
            NaN32,
            NaN32,
            NaN32,
            NaN32,
            false,
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

        serialize(io, fits.dmin)
        serialize(io, fits.dmax)
        serialize(io, fits.data_median)
        serialize(io, fits.data_mad)
        serialize(io, fits.data_mad₊)
        serialize(io, fits.data_mad₋)
        serialize(io, fits.video_ready)

        serialize(io, fits.has_header)
        serialize(io, fits.has_data)
        serialize(io, fits.has_error)
        # skipping fits.last_accessed

        if fits.depth == 1
            serialize(io, fits.progress)
        end

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

    fits.dmin = deserialize(io)
    fits.dmax = deserialize(io)
    fits.data_median = deserialize(io)
    fits.data_mad = deserialize(io)
    fits.data_mad₊ = deserialize(io)
    fits.data_mad₋ = deserialize(io)
    fits.video_ready = deserialize(io)

    fits.has_header = deserialize(io)
    fits.has_data = deserialize(io)
    fits.has_error = deserialize(io)
    # skipping fits.last_accessed

    if fits.depth == 1
        fits.progress = deserialize(io)
    end

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

function has_video(fits::FITSDataSet)::Bool
    has_video = false

    lock(fits.mutex)

    has_video = fits.video_ready

    unlock(fits.mutex)

    return has_video
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
    local crval3, cdelt3, crpix3

    header = fits.header

    # any errors will be propagated back and handled higher up
    crval3 = header["CRVAL3"]
    cdelt3 = header["CDELT3"]
    crpix3 = header["CRPIX3"]

    c = c_0.val # [m/s]
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

    local frame, frame_pixels, frame_mask
    local valid_pixels, valid_mask
    local frame_min, frame_max, frame_median
    local mean_spectrum, integrated_spectrum

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
                frame_pixels = reshape(read(hdu, :, :, frame, 1), (width, height))
            else
                frame_pixels = reshape(read(hdu, :, :, frame), (width, height))
            end

            frame_mask = invalidate_pixel.(frame_pixels, datamin, datamax, ignrval)

            # replace NaNs with 0.0
            frame_pixels[frame_mask] .= 0

            try
                zfp_compress_pixels(datasetid, frame, Float32.(frame_pixels), frame_mask)
            catch e
                println(e)
            end

            pixels .+= frame_pixels
            mask .&= frame_mask

            # pick out the valid values only
            valid_mask = .!frame_mask
            valid_pixels = @view frame_pixels[valid_mask]

            pixel_sum = sum(valid_pixels)
            pixel_count = length(valid_pixels)

            if pixel_count > 0
                frame_min, frame_max = ThreadsX.extrema(valid_pixels)
                frame_median = median(valid_pixels)
                mean_spectrum = pixel_sum / pixel_count
                integrated_spectrum = pixel_sum * cdelt3
            else
                # no mistake here, reverse the min/max values
                # so that global dmin/dmax can get correct values
                # in the face of all-NaN frames
                frame_min = prevfloat(typemax(Float32))
                frame_max = -prevfloat(typemax(Float32))
                frame_median = NaN32

                mean_spectrum = 0.0
                integrated_spectrum = 0.0
            end

            # convert to half-float (Float16)
            compressed_pixels = map(x -> Float16(x), frame_pixels)

            # insert back NaNs
            compressed_pixels[frame_mask] .= NaN16

            # store the data
            compressed_frames[frame] = compressed_pixels

            #=
            #  save the half-float (Float16) data
            cache_dir =
                ".cache" * Base.Filesystem.path_separator * datasetid
            filename =
                cache_dir *
                Base.Filesystem.path_separator *
                string(frame) *
                ".f16"

            io = open(filename, "w+")
            write(io, compressed_pixels)
            # serialize(io, compressed_pixels)
            close(io)
            =#

            # send back the reduced values
            put!(
                progress,
                (
                    frame,
                    frame_min,
                    frame_max,
                    frame_median,
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

            cache_dir = ".cache" * Base.Filesystem.path_separator * datasetid

            filename =
                cache_dir * Base.Filesystem.path_separator * string(myid()) * ".pixels"
            serialize(filename, pixels)

            filename = cache_dir * Base.Filesystem.path_separator * string(myid()) * ".mask"
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

function loadFITS(filepath::String, fits::FITSDataSet)

    if fits.datasetid == ""
        return
    end

    if Sys.iswindows()
        filepath = replace(filepath, "/" => "\\")
    end

    println("loading $filepath::$(fits.datasetid)")

    local f, width::Integer, height::Integer, depth::Integer
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

                    dmin, dmax = ThreadsX.extrema(valid_pixels)
                    println("dmin: $dmin, dmax: $dmax")

                    fits.dmin = dmin
                    fits.dmax = dmax
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
                medians = zeros(Float32, depth)

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

                        frame, min_val, max_val, med_val, mean_val, integrated_val, tid = take!(progress)

                        frame_min[frame] = Float32(min_val)
                        frame_max[frame] = Float32(max_val)
                        medians[frame] = Float32(med_val)
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

                # median of medians
                med_mask = map(!isnan, medians)
                valid_medians = medians[med_mask]

                # global median (approx.)
                data_median = median(valid_medians)

                println("dmin: $dmin, dmax: $dmax, approx. all-data median: $data_median")

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

                # finally estimate data_mad, data_madN, data_madP based on the all-data median
                local data_mad::Float32, data_count::Int64
                local data_mad₊::Float32, data_count₊::Int64
                local data_mad₋::Float32, data_count₋::Int64

                data_mad = 0.0
                data_count = 0

                data_mad₊ = 0.0
                data_count₊ = 0

                data_mad₋ = 0.0
                data_count₋ = 0

                results = RemoteChannel(() -> Channel{Tuple}(32))

                results_task = @async while true
                    try
                        thread_sum₊, thread_count₊, thread_sum₋, thread_count₋ =
                            take!(results)

                        data_mad₊ += thread_sum₊
                        data_count₊ += thread_count₊

                        data_mad₋ += thread_sum₋
                        data_count₋ += thread_count₋

                    catch e
                        println("global statistics task completed")
                        break
                    end
                end

                data_ras = [
                    @spawnat job.where calculateGlobalStatistics(
                        data_median,
                        fetch(job),
                        findall(indices[job.where]),
                        results,
                    )

                    for job in ras
                ]

                @time wait.(data_ras)

                close(results)
                wait(results_task)

                if data_count₊ + data_count₋ > 0
                    data_mad =
                        Float32(data_mad₊ + data_mad₋) / Float32(data_count₊ + data_count₋)
                end

                if data_count₊ > 0
                    data_mad₊ /= Float32(data_count₊)
                end

                if data_count₋ > 0
                    data_mad₋ /= Float32(data_count₋)
                end

                println("data_mad: $data_mad, data_mad₊: $data_mad₊, data_mad₋: $data_mad₋")

                fits.dmin = dmin
                fits.dmax = dmax
                fits.data_median = data_median
                fits.data_mad = data_mad
                fits.data_mad₊ = data_mad₊
                fits.data_mad₋ = data_mad₋

                lock(fits.mutex)
                fits.video_ready = true
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

# restore DArrays
@everywhere function preload_image(datasetid, global_pixels, global_mask)

    cache_dir = ".cache" * Base.Filesystem.path_separator * datasetid

    try
        filename = cache_dir * Base.Filesystem.path_separator * string(myid()) * ".pixels"
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

    # Remote Access Service
    ras = [@spawnat w preload_image(fits.datasetid, pixels, mask) for w in workers()]

    # wait for the pixels & mask to be restored
    bSuccess = all(fetch.(ras))

    fits.pixels = pixels
    fits.mask = mask

    # decompress the cube channels
    decompressData(fits)

    # adjust has_data and has_error
    lock(fits.mutex)
    fits.has_data = bSuccess
    fits.has_error = !bSuccess
    unlock(fits.mutex)

    # restore the cube channels
    # restoreData(fits)
end

@everywhere function load_f16_frames(datasetid, width, height, idx)
    compressed_frames = Dict{Int32,Matrix{Float16}}()
    # spinlock = Threads.SpinLock()

    # Threads.@threads
    for frame in idx
        try
            cache_dir = ".cache" * Base.Filesystem.path_separator * datasetid
            filename = cache_dir * Base.Filesystem.path_separator * string(frame) * ".f16"

            io = open(filename) # default is read-only
            compressed_pixels = Mmap.mmap(io, Matrix{Float16}, (width, height))
            Mmap.madvise!(compressed_pixels, MADV_WILLNEED)
            # compressed_pixels = deserialize(io)
            close(io)

            # touch the data by copying
            # ram_pixels = Array{Float16}(undef, width, height)
            # ram_pixels .= compressed_pixels

            # Threads.lock(spinlock)
            compressed_frames[frame] = compressed_pixels
            # Threads.unlock(spinlock)

            # println("restored frame #$frame")

        catch e
            println(e)
        end
    end

    return compressed_frames
end

function restoreData(fits::FITSDataSet)
    if (fits.datasetid == "") || (fits.depth <= 1)
        return
    end

    # Remote Access Service
    ras = [
        @spawnat w load_f16_frames(fits.datasetid, fits.width, fits.height, findall(value)) for (w, value) in fits.indices
    ]

    println("ras: ", ras)

    fits.compressed_pixels = ras

    @time wait.(ras)

    @everywhere function cache_frames(
        compressed_frames::Dict{Int32,Matrix{Float16}},
        queue::RemoteChannel{Channel{Tuple}},
    )

        for (idx, pixels) in compressed_frames
            try
                val = sum(pixels)
                put!(queue, (idx, val))
            catch e
                println("cache_frames: $e")
            end
        end

    end

    # finally preload frames by touching the mmapped data
    progress = RemoteChannel(() -> Channel{Tuple}(32))

    @async while true
        try
            idx, = take!(progress)
            println("cached frame #$idx")

            update_progress(fits, fits.depth)
        catch e
            println("caching data completed")
            break
        end
    end

    lock(fits.mutex)
    fits.progress = Threads.Atomic{Int}(0)
    unlock(fits.mutex)

    ras = [
        @spawnat job.where cache_frames(fetch(job), progress) for
        job in fits.compressed_pixels
    ]

    @time wait.(ras)
    close(progress)

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

function get_inner_dimensions(fits::FITSDataSet)
    local inner_width::Integer, inner_height::Integer

    inner_width = 0
    inner_height = 0

    # get the maximum common bounding box
    if isa(fits.mask, DArray)

        # go through the collated mask fetching inner dims from all workers
        @everywhere function get_inner_dimensions(global_mask::DArray)
            fits_dims = size(global_mask)

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

    return (inner_width, inner_height)
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

function getHistogram(fits::FITSDataSet, pixels, mask)
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

    println("getHistogram done")

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

    return (bins, tone_mapping)
end

function getImage(fits::FITSDataSet, width::Integer, height::Integer)
    local scale::Float32, pixels, mask
    local image_width::Integer, image_height::Integer
    local inner_width::Integer, inner_height::Integer

    inner_width = 0
    inner_height = 0
    bDownsize = false

    println("getImage::$(fits.datasetid)/($width)/($height)")

    # calculate scale, downsize when applicable
    inner_width, inner_height = get_inner_dimensions(fits)

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
    bins, tone_mapping = getHistogram(fits, pixels, mask)

    return (bins, tone_mapping, pixels, mask)
end

function getJSON(fits::FITSDataSet)
    local CD1_1, CD1_2, CD2_1, CD2_2
    local CRVAL1, CDELT1, CRPIX1, CUNIT1, CTYPE1
    local CRVAL2, CDELT2, CRPIX2, CUNIT2, CTYPE2
    local CRVAL3, CDELT3, CRPIX3, CUNIT3, CTYPE3
    local BMAJ, BMIN, BPA, BUNIT, BTYPE, SPECSYS
    local RESTFRQ, OBSRA, OBSDEC
    local OBJECT, DATEOBS, TIMESYS, LINE, FILTER

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

    c = c_0.val # [m/s]

    fRatio = frame_start / ref_freq
    v1 = (1.0 - fRatio^2) / (1.0 + fRatio^2) * c

    fRatio = frame_end / ref_freq
    v2 = (1.0 - fRatio^2) / (1.0 + fRatio^2) * c

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

    local first_frame, last_frame

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

    local first_frame, last_frame

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

function Einstein_velocity_addition(v1::Float64, v2::Float64)
    c = c_0.val # [m/s]

    return (v1 + v2) / (1.0 + v1 * v2 / c^2)
end

function Einstein_relative_velocity(f::Float64, f0::Float64, Δv::Float64)
    c = c_0.val # [m/s]

    fRatio = f / f0
    v = (1.0 - fRatio^2) / (1.0 + fRatio^2) * c

    return Einstein_velocity_addition(v, Δv)
end

function relativistic_rest_frequency(f::Float64, Δv::Float64)
    c = c_0.val # [m/s]

    β = Δv / c

    tmp = sqrt((1.0 + β) / (1.0 - β))

    return f * tmp
end

function get_frame2freq_vel(
    fits::FITSDataSet,
    frame::Integer,
    ref_freq::Float64,
    Δv::Float64,
    rest::Bool,
)
    header = fits.header

    # any errors will be propagated back and handled higher up
    crval3 = header["CRVAL3"]
    cdelt3 = header["CDELT3"]
    crpix3 = header["CRPIX3"]

    c = c_0.val / 1000.0 # [km/s]

    has_velocity = fits.has_velocity
    has_frequency = fits.has_frequency

    if ref_freq > 0.0
        has_frequency = true
    end

    if has_velocity && has_frequency
        v =
            crval3 * fits.frame_multiplier +
            cdelt3 * fits.frame_multiplier * (frame - crpix3) # [m/s]
        v /= 1000.0 # [km/s]

        f = ref_freq * sqrt((1.0 - v / c) / (1 + v / c)) # [Hz]
        f /= 1.0e9 # [GHz]

        if rest
            f = relativistic_rest_frequency(f, Δv)
        end

        return (f, v) # [GHz], [km/s]
    end

    val = crval3 * fits.frame_multiplier + cdelt3 * fits.frame_multiplier * (frame - crpix3)

    if has_frequency
        # find the corresponding velocity
        v = Einstein_relative_velocity(val, ref_freq, Δv)

        if rest
            val = relativistic_rest_frequency(val, Δv)
        end

        return (val / 1.0e9, v / 1000.0) # [GHz], [km/s]
    end

    if has_velocity
        # no frequency info, only velocity
        return (Nothing, val / 1000.0) # [km/s]
    end

    return (Nothing, Nothing)
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
    dx = req["dx"]

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
            println("re-sizing the viewport down to $(Int32(round(100.0 * scale)))%")

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
        intensity = eval(Meta.parse(uppercase(req["intensity"])))

        # calculate the centre and squared radius
        cx = abs(x1 + x2) >> 1
        cy = abs(y1 + y2) >> 1
        r = min(abs(x2 - x1) >> 1, abs(y2 - y1) >> 1)
        r2 = r * r

        local pixels, mask

        # by default no downsizing is needed
        bDownsize = false
        view_width = dimx
        view_height = dimy

        if image
            native_size = dimx * dimy
            viewport_size = width * height

            if native_size > viewport_size
                scale = Float32(width) / Float32(dimx)
                println("re-sizing the viewport down to $(Int32(round(100.0 * scale)))%")

                # downsize the pixels & mask
                bDownsize = true
                view_width = width
                view_height = height
            end

            pixels = zeros(Float32, view_width, view_height)
            mask = map(isnan, pixels)
        end

        spectrum = zeros(Float32, frame_length)

        results = RemoteChannel(() -> Channel{Tuple}(32))

        results_task = @async while true
            try
                thread_pixels, thread_mask, thread_spectrum = take!(results)

                if thread_pixels != Nothing && thread_mask != Nothing
                    pixels .+= thread_pixels
                    mask .|= thread_mask
                end

                if thread_spectrum != Nothing
                    for x in thread_spectrum
                        frame, val = x
                        spectrum[1+frame-first_frame] = val
                    end
                end

            catch e
                println("results task completed")
                break
            end
        end

        # for each Future in ras find the corresponding worker
        # launch jobs on each worker, pass the channel indices
        ras = [
            @spawnat job.where calculateViewportSpectrum(
                fetch(job),
                Int32(first_frame),
                Int32(last_frame),
                Int32(x1),
                Int32(x2),
                Int32(y1),
                Int32(y2),
                Int32(cx),
                Int32(cy),
                Int32(r2),
                beam,
                intensity,
                image,
                bDownsize,
                Int64(view_width),
                Int64(view_height),
                fits._cdelt3,
                findall(fits.indices[job.where]),
                results,
            )

            for job in fits.compressed_pixels
        ]

        println("ras: ", ras)

        @time wait.(ras)

        close(results)
        wait(results_task)

        # optionally downsample the spectrum
        if length(spectrum) > (dx >> 1)
            println("downsampling spectrum from $(length(spectrum)) to $(dx >> 1)")
            spectrum = imresize(spectrum, (dx >> 1,))
        end

        local image_resp, spec_resp

        if image
            image_resp = IOBuffer()

            write(image_resp, Int32(view_width))
            write(image_resp, Int32(view_height))

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
            write(image_resp, Int32(length(compressed_pixels)))
            write(image_resp, compressed_pixels)

            compressed_mask = lz4_hc_compress(collect(flatten(UInt8.(mask))))
            write(image_resp, Int32(length(compressed_mask)))
            write(image_resp, compressed_mask)
        else
            image_resp = Nothing
        end

        spec_resp = IOBuffer()

        # compress spectrum with ZFP
        prec = SPECTRUM_MEDIUM_PRECISION

        if image
            prec = SPECTRUM_HIGH_PRECISION
        end

        compressed_spectrum = zfp_compress(spectrum, precision = prec)

        write(spec_resp, Int32(length(spectrum)))
        write(spec_resp, compressed_spectrum)

        return (image_resp, spec_resp)
    end

end

function getImageSpectrum(fits::FITSDataSet, req::Dict{String,Any})
    if fits.depth < 1
        error("getImageSpectrum() only supports 3D cubes.")
    end

    local scale::Float32
    local inner_width::Integer, inner_height::Integer

    # use the entire FITS plane
    x1 = 1
    x2 = fits.width
    y1 = 1
    y2 = fits.height

    image = true
    width = round(Integer, req["width"])
    height = round(Integer, req["height"])
    dx = req["dx"]

    beam::Beam = SQUARE
    intensity = eval(Meta.parse(uppercase(req["intensity"])))

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

    # viewport dimensions
    dimx = fits.width
    dimy = fits.height

    # handle ras distributed Futures        

    # calculate the centre and squared radius
    cx = abs(x1 + x2) >> 1
    cy = abs(y1 + y2) >> 1
    r = min(abs(x2 - x1) >> 1, abs(y2 - y1) >> 1)
    r2 = r * r

    local pixels, mask

    # by default no downsizing is needed
    bDownsize = false
    view_width = dimx
    view_height = dimy

    # calculate scale, downsize when applicable
    inner_width, inner_height = get_inner_dimensions(fits)

    try
        scale = get_image_scale(width, height, inner_width, inner_height)
    catch e
        println(e)
        scale = 1.0
    end

    if scale < 1.0
        println("re-sizing the image down to $(Int32(round(100.0 * scale)))%")
        view_width = round(Integer, scale * fits.width)
        view_height = round(Integer, scale * fits.height)
        bDownsize = true
    end

    println("scale = $scale, image: $view_width x $view_height, bDownsize: $bDownsize")

    pixels = zeros(Float32, view_width, view_height)
    mask = map(isnan, pixels)

    spectrum = zeros(Float32, frame_length)

    results = RemoteChannel(() -> Channel{Tuple}(32))

    results_task = @async while true
        try
            thread_pixels, thread_mask, thread_spectrum = take!(results)

            if thread_pixels != Nothing && thread_mask != Nothing
                pixels .+= thread_pixels
                mask .|= thread_mask
            end

            if thread_spectrum != Nothing
                for x in thread_spectrum
                    frame, val = x
                    spectrum[1+frame-first_frame] = val
                end
            end

        catch e
            println("results task completed")
            break
        end
    end

    # for each Future in ras find the corresponding worker
    # launch jobs on each worker, pass the channel indices
    ras = [
        @spawnat job.where calculateViewportSpectrum(
            fetch(job),
            Int32(first_frame),
            Int32(last_frame),
            Int32(x1),
            Int32(x2),
            Int32(y1),
            Int32(y2),
            Int32(cx),
            Int32(cy),
            Int32(r2),
            beam,
            intensity,
            image,
            bDownsize,
            Int64(view_width),
            Int64(view_height),
            fits._cdelt3,
            findall(fits.indices[job.where]),
            results,
        )

        for job in fits.compressed_pixels
    ]

    println("ras: ", ras)

    @time wait.(ras)

    close(results)
    wait(results_task)

    # optionally downsample the spectrum
    if length(spectrum) > (dx >> 1)
        println("downsampling spectrum from $(length(spectrum)) to $(dx >> 1)")
        spectrum = imresize(spectrum, (dx >> 1,))
    end

    # next make a histogram
    bins, tone_mapping = getHistogram(fits, pixels, mask)

    # image response
    image_resp = IOBuffer()

    # first send the tone mapping
    flux = tone_mapping.flux

    # pad flux with spaces so that the length is a multiple of 4
    # this is needed for an array alignment in JavaScript
    len = 4 * (length(flux) ÷ 4 + 1)
    flux = lpad(flux, len, " ")

    write(image_resp, UInt32(length(flux)))
    write(image_resp, flux)
    write(image_resp, tone_mapping.pmin)
    write(image_resp, tone_mapping.pmax)
    write(image_resp, tone_mapping.med)
    write(image_resp, tone_mapping.sensitivity)
    write(image_resp, tone_mapping.ratio_sensitivity)
    write(image_resp, tone_mapping.white)
    write(image_resp, tone_mapping.black)

    # the histogram
    println("typeof(bins):", typeof(bins))
    write(image_resp, UInt32(length(bins)))
    write(image_resp, Int32.(bins))

    # next the image
    write(image_resp, UInt32(view_width))
    write(image_resp, UInt32(view_height))

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
    write(image_resp, UInt32(length(compressed_pixels)))
    write(image_resp, compressed_pixels)

    compressed_mask = lz4_hc_compress(collect(flatten(UInt8.(mask))))
    write(image_resp, UInt32(length(compressed_mask)))
    write(image_resp, compressed_mask)

    # spectrum response
    spec_resp = IOBuffer()

    # compress spectrum with ZFP        
    compressed_spectrum = zfp_compress(spectrum, precision = SPECTRUM_HIGH_PRECISION)

    write(spec_resp, UInt32(length(spectrum)))
    write(spec_resp, compressed_spectrum)

    return (image_resp, spec_resp)

end

function getSpectrum(fits::FITSDataSet, req::Dict{String,Any})
    if fits.depth < 1
        error("getSpectrum() only supports 3D cubes.")
    end

    header = fits.header

    local bunit

    try
        bunit = strip(header["BUNIT"])
    catch e
        bunit = ""
    end

    # use the entire FITS plane
    x1 = 1
    x2 = fits.width
    y1 = 1
    y2 = fits.height

    # disable viewport
    image = false

    frame_start = Float64(req["frame_start"])
    frame_end = Float64(req["frame_end"])

    Δv = Float64(req["deltaV"])
    rest = req["rest"]

    println("Δv: $Δv, rest: $rest")

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

    local beam::Beam

    try
        beam = eval(Meta.parse(uppercase(req["beam"])))
    catch _
        beam = SQUARE
    end

    intensity = eval(Meta.parse(uppercase(req["intensity"])))

    # calculate the centre and squared radius
    cx = abs(x1 + x2) >> 1
    cy = abs(y1 + y2) >> 1
    r = min(abs(x2 - x1) >> 1, abs(y2 - y1) >> 1)
    r2 = r * r

    spectrum = zeros(Float32, frame_length)

    results = RemoteChannel(() -> Channel{Tuple}(32))

    results_task = @async while true
        try
            _, _, thread_spectrum = take!(results)

            if thread_spectrum != Nothing
                for x in thread_spectrum
                    frame, val = x
                    spectrum[1+frame-first_frame] = val
                end
            end

        catch _
            println("results task completed")
            break
        end
    end

    # for each Future in ras find the corresponding worker
    # launch jobs on each worker, pass the channel indices
    ras = [
        @spawnat job.where calculateViewportSpectrum(
            fetch(job),
            Int32(first_frame),
            Int32(last_frame),
            Int32(x1),
            Int32(x2),
            Int32(y1),
            Int32(y2),
            Int32(cx),
            Int32(cy),
            Int32(r2),
            beam,
            intensity,
            false,
            false,
            Int64(dimx),
            Int64(dimy),
            fits._cdelt3,
            findall(fits.indices[job.where]),
            results,
        )

        for job in fits.compressed_pixels
    ]

    println("ras: ", ras)

    @time wait.(ras)

    close(results)
    wait(results_task)

    csv = IOBuffer()

    has_header = false

    intensity_column = "intensity [" * bunit

    if intensity == MEAN
        intensity_column = "mean " * intensity_column
    end

    if intensity == INTEGRATED
        intensity_column = "integrated " * intensity_column

        if fits.has_velocity
            intensity_column = intensity_column * "•km/s"
        end
    end

    intensity_column = intensity_column * "]"

    frequency_column = "frequency [GHz]"

    if rest
        frequency_column = "rest " * frequency_column
    end

    for (idx, val) in enumerate(spectrum)
        frame = first_frame + (idx - 1)

        # convert frame to frequency and/or velocity
        f, v = get_frame2freq_vel(fits, frame, ref_freq, Δv, rest)

        println("$idx\tchannel: $frame\tf: $f GHz\tv: $v km/s\tint.: $val")

        if f != Nothing && v != Nothing
            if !has_header
                write(
                    csv,
                    "\"channel\",\"$frequency_column\",\"velocity [km/s]\",\"$intensity_column\"\n",
                )
                has_header = true
            end

            write(csv, "$frame,$f,$v,$val\n")

            continue
        end

        if v != Nothing
            if !has_header
                write(csv, "\"channel\",\"velocity [km/s]\",\"$intensity_column\"\n")
                has_header = true
            end

            write(csv, "$frame,$v,$val\n")

            continue
        end

        if f != Nothing
            if !has_header
                write(csv, "\"channel\",\"$frequency_column\",\"$intensity_column\"\n")
                has_header = true
            end

            write(csv, "$frame,$f,$val\n")

            continue
        end
    end

    return csv
end

function alphaMask(x::Bool)::UInt8
    if x
        return UInt8(255)
    else
        return UInt8(0)
    end
end

function linear_tone_mapping(x::Float16, black::Float32, slope::Float32)::UInt8
    local pixel::Float32

    pixel = 255.0f0 / (1.0f0 + exp(-6.0f0 * (Float32(x) - black) * slope))

    return round(UInt8, clamp(pixel, 0.0f0, 255.0f0))
end

function logistic_tone_mapping(x::Float16, median::Float32, sensitivity::Float32)::UInt8
    local pixel::Float32

    pixel = 255.0f0 / (1.0f0 + exp(-6.0f0 * (Float32(x) - median) * sensitivity))

    return round(UInt8, clamp(pixel, 0.0f0, 255.0f0))
end

function ratio_tone_mapping(x::Float16, black::Float32, sensitivity::Float32)::UInt8
    local pixel::Float32

    pixel = 5.0f0 * (Float32(x) - black) * sensitivity

    if pixel > 0.0f0
        return round(UInt8, clamp(255.0f0 * pixel / (1.0f0 + pixel), 0.0f0, 255.0f0))
    else
        return UInt8(0)
    end
end

function square_tone_mapping(x::Float16, black::Float32, sensitivity::Float32)::UInt8
    local pixel::Float32

    pixel = (Float32(x) - black) * sensitivity

    if pixel > 0.0f0
        return round(UInt8, clamp(255.0f0 * pixel * pixel, 0.0f0, 255.0f0))
    else
        return UInt8(0)
    end
end

function legacy_tone_mapping(
    x::Float16,
    dmin::Float32,
    dmax::Float32,
    lmin::Float32,
    lmax::Float32,
)::UInt8
    local pixel::Float32

    pixel = 0.5f0 + (Float32(x) - dmin) / (dmax - dmin)

    if pixel > 0.0f0
        return round(
            UInt8,
            clamp(255.0f0 * (log(pixel) - lmin) / (lmax - lmin), 0.0f0, 255.0f0),
        )
    else
        return UInt8(0)
    end
end

function null_tone_mapping(x::Float16)::UInt8
    return UInt8(0)
end

function getVideoFrame(
    fits::FITSDataSet,
    frame_idx::Integer,
    flux::String,
    image_width::Integer,
    image_height::Integer,
    bDownsize::Bool,
    keyframe::Bool,
)
    local pixels, mask
    # local luma, alpha

    if fits.compressed_pixels == Nothing
        error("Uninitialised compressed pixels.")
    end

    # calculate white, black, sensitivity from the all-data histogram
    u = 7.5f0
    _median = fits.data_median
    _black = max(fits.dmin, (fits.data_median - u * fits.data_mad₋))
    _white = min(fits.dmax, (fits.data_median + u * fits.data_mad₊))
    _sensitivity = 1.0f0 / (_white - _black)
    _slope = 1.0f0 / (_white - _black)
    _dmin = fits.dmin
    _dmax = fits.dmax

    # video tone mapping
    tone =
        VideoToneMapping(flux, _dmin, _dmax, _median, _sensitivity, _slope, _white, _black)

    # target a specific worker instead of a "scatter-gun" approach
    for job in fits.compressed_pixels
        idx = fits.indices[job.where]

        if idx[frame_idx]
            pixels, mask, dims = @fetchfrom job.where fetchVideoFrame(
                fetch(job),
                frame_idx,
                tone,
                image_width,
                image_height,
                bDownsize,
                keyframe,
            )

            len = dims[1] * dims[2]

            # decompress and reconstruct (reshape) the pixels/mask arrays
            pixels = reshape(lz4_decompress(pixels, len), dims)
            mask = reshape(lz4_decompress(mask, len), dims)

            return (pixels, mask)
        end
    end

    return (nothing, nothing)

    #=
    # BitMatrix -> Array{Bool} -> Array{UInt8}
    # alpha_task = Threads.@spawn alpha = alphaMask.(mask)
    alpha = alphaMask.(mask)

    # convert Float16 pixels to UInt8 (apply tone mapping)
    # luma = Matrix{UInt8}(undef, size(pixels))

    if flux == "linear"
        luma = linear_tone_mapping.(pixels, _black, _slope)
    elseif flux == "logistic"
        luma = logistic_tone_mapping.(pixels, _median, _sensitivity)
    elseif flux == "ratio"
        luma = ratio_tone_mapping.(pixels, _black, _sensitivity)
    elseif flux == "square"
        luma = square_tone_mapping.(pixels, _black, _sensitivity)
    elseif flux == "legacy"
        luma = legacy_tone_mapping.(pixels, _dmin, _dmax, lmin, lmax)
    else
        luma = null_tone_mapping.(pixels)
    end
    =#

    # use Intel SPMD C: {pixels,alpha} -> {luma, alpha} ... ???
    # in some cases, where pixels <= 0.0 alpha needs to be set to 0
    # even if a pixel is not NaN

    # wait(alpha_task)

    # return (luma, alpha)
end

@everywhere function fetchVideoFrame(
    compressed_frames::Dict{Int32,Matrix{Float16}},
    frame::Integer,
    tone::VideoToneMapping,
    image_width::Integer,
    image_height::Integer,
    bDownsize::Bool,
    keyframe::Bool,
)
    local frame_pixels, pixels, mask
    local dstWidth, dstHeight

    try
        frame_pixels = compressed_frames[frame]
        # println("processing video frame $frame")
    catch e
        return
    end

    dims = size(frame_pixels)
    width = dims[1]
    height = dims[2]

    if bDownsize
        dstWidth = image_width
        dstHeight = image_height
    else
        dstWidth = width
        dstHeight = height
    end

    pixels = Matrix{UInt8}(undef, (dstWidth, dstHeight))
    mask = Matrix{UInt8}(undef, (dstWidth, dstHeight))

    src_stride = strides(frame_pixels)
    dst_stride = strides(pixels)

    if tone.flux == "linear"
        ccall(
            make_linear_video_frame_fptr,
            Cvoid,
            (
                Ptr{Float16},
                Cint,
                Cint,
                Cint,
                Ptr{UInt8},
                Ptr{UInt8},
                Cint,
                Cint,
                Cint,
                Cfloat,
                Cfloat,
            ),
            pointer(frame_pixels),
            width,
            height,
            src_stride[2],
            pointer(pixels),
            pointer(mask),
            dstWidth,
            dstHeight,
            dst_stride[2],
            tone.black,
            tone.slope,
        )
    elseif tone.flux == "logistic"
        ccall(
            make_logistic_video_frame_fptr,
            Cvoid,
            (
                Ptr{Float16},
                Cint,
                Cint,
                Cint,
                Ptr{UInt8},
                Ptr{UInt8},
                Cint,
                Cint,
                Cint,
                Cfloat,
                Cfloat,
            ),
            pointer(frame_pixels),
            width,
            height,
            src_stride[2],
            pointer(pixels),
            pointer(mask),
            dstWidth,
            dstHeight,
            dst_stride[2],
            tone.median,
            tone.sensitivity,
        )
    elseif tone.flux == "ratio"
        ccall(
            make_ratio_video_frame_fptr,
            Cvoid,
            (
                Ptr{Float16},
                Cint,
                Cint,
                Cint,
                Ptr{UInt8},
                Ptr{UInt8},
                Cint,
                Cint,
                Cint,
                Cfloat,
                Cfloat,
            ),
            pointer(frame_pixels),
            width,
            height,
            src_stride[2],
            pointer(pixels),
            pointer(mask),
            dstWidth,
            dstHeight,
            dst_stride[2],
            tone.black,
            tone.sensitivity,
        )
    elseif tone.flux == "square"
        ccall(
            make_square_video_frame_fptr,
            Cvoid,
            (
                Ptr{Float16},
                Cint,
                Cint,
                Cint,
                Ptr{UInt8},
                Ptr{UInt8},
                Cint,
                Cint,
                Cint,
                Cfloat,
                Cfloat,
            ),
            pointer(frame_pixels),
            width,
            height,
            src_stride[2],
            pointer(pixels),
            pointer(mask),
            dstWidth,
            dstHeight,
            dst_stride[2],
            tone.black,
            tone.sensitivity,
        )
    elseif tone.flux == "legacy"
        lmin = log(0.5f0)
        lmax = log(1.5f0)

        ccall(
            make_legacy_video_frame_fptr,
            Cvoid,
            (
                Ptr{Float16},
                Cint,
                Cint,
                Cint,
                Ptr{UInt8},
                Ptr{UInt8},
                Cint,
                Cint,
                Cint,
                Cfloat,
                Cfloat,
                Cfloat,
                Cfloat,
            ),
            pointer(frame_pixels),
            width,
            height,
            src_stride[2],
            pointer(pixels),
            pointer(mask),
            dstWidth,
            dstHeight,
            dst_stride[2],
            tone.dmin,
            tone.dmax,
            lmin,
            lmax,
        )
    else
        pixels .= 0
        mask .= 0
    end

    dims = size(pixels)
    pixels = lz4_compress(collect(flatten(pixels)))
    mask = lz4_compress(collect(flatten(mask)))

    return (pixels, mask, dims)

    # the code below will not execute
    #=
    if bDownsize
        try
            # tried using Threads.@spawn: imresize does not seem to be thread-safe
            local pixels_task

            # pixels_task = Threads.@spawn pixels =
            #    Float16.(resizeCubic32fC1R(Float32.(pixels), image_width, image_height))

            pixels = resizeNearest8uC1R(pixels, image_width, image_height)
            mask = resizeNearest8uC1R(mask, image_width, image_height)

            #=
            # mask_task = @spawnat :any 
            mask =
                round.(
                    UInt8,
                    clamp.(
                        imresize(mask, (image_width, image_height), method = Constant()),
                        0,
                        255,
                    ),
                ) # use Nearest-Neighbours for the mask
            # mask = Bool.(imresize(mask, (image_width, image_height), method = Constant()),) # use Nearest-Neighbours for the mask                          

            if keyframe
                # pixels_task = @spawnat :any 
                pixels =
                    round.(
                        UInt8,
                        clamp.(imresize(pixels, (image_width, image_height)), 0, 255),
                    )
            else
                # pixels_task = @spawnat :any 
                pixels =
                    round.(
                        UInt8,
                        clamp.(
                            imresize(
                                pixels,
                                (image_width, image_height),
                                method = Constant(),
                            ),
                            0,
                            255,
                        ),
                    )
            end
            =#

            # mask = fetch(mask_task)
            # pixels = fetch(pixels_task)

            # wait(pixels_task)
            # wait(mask_task)            
        catch e
            println(e)
            println(
                "imresize error: ",
                size(pixels),
                "-->($image_width,$image_height); ",
                typeof(pixels),
                ";",
                size(mask),
                "-->($image_width,$image_height); ",
                typeof(mask),
            )
            return
        end
    end

    # println(typeof(pixels), ";", typeof(mask), ";", size(pixels), ";", size(mask), "; bDownsize:", bDownsize)

    # LZ4-compress {pixels,mask} for a faster transmission from the remote worker
    # to the root process (network bandwidth savings)
    dims = size(pixels)
    pixels = lz4_compress(collect(flatten(pixels)))
    mask = lz4_compress(collect(flatten(mask)))

    return (pixels, mask, dims)
    =#
end

@everywhere function calculateViewportSpectrum(
    compressed_frames::Dict{Int32,Matrix{Float16}},
    first_frame::Int32,
    last_frame::Int32,
    x1::Int32,
    x2::Int32,
    y1::Int32,
    y2::Int32,
    cx::Int32,
    cy::Int32,
    r2::Int32,
    beam::Beam,
    intensity::Intensity,
    bImage::Bool,
    bDownsize::Bool,
    view_width::Int64,
    view_height::Int64,
    cdelt3::Float32,
    idx::Vector{Int64},
    queue::RemoteChannel{Channel{Tuple}},
)
    # println("#threads per worker: ", Threads.nthreads())

    local thread_pixels, thread_mask
    local view_pixels, view_mask

    # viewport dimensions
    dimx = abs(x2 - x1 + 1)
    dimy = abs(y2 - y1 + 1)

    # TO-DO: replace the mutex with per-thread arrays
    # then finally combine the arrays into the final spectrum
    # or better still a single spectrum the length of fits.depth filled with zeros initially
    # Tried it all: finally settled on a spin lock instead

    average = true

    if intensity == INTEGRATED
        average = false
    end

    spectrum = Array{Tuple{Int32,Float32},1}()
    spinlock = Threads.SpinLock()

    if bImage
        # thread_pixels = [zeros(Float32, dimx, dimy) for tid = 1:Threads.nthreads()]
        # thread_mask = [map(isnan, th_pix) for th_pix in thread_pixels]

        view_pixels = zeros(Float32, dimx, dimy)
        view_mask = map(isnan, view_pixels)
    end

    # set the initial capacity to avoid reallocations
    depth = last_frame - first_frame + 1
    sizehint!(spectrum, depth)

    Threads.@threads for frame in idx
        # @threads macro does not cope with a Dict nor SparseVector object iterator ...
        # not even with "findnz(SparseVector)"

        if frame < first_frame || frame > last_frame
            continue
        end

        try
            Threads.lock(spinlock)
            pixels = compressed_frames[frame]
            Threads.unlock(spinlock)

            # viewport = @view pixels[x1:x2, y1:y2]
            # mask = map(!isnan, viewport)
            # val = sum(Float32.(viewport[mask])) * cdelt3

            if bImage
                viewport = pixels[x1:x2, y1:y2]
                mask = map(isnan, viewport)

                # replace NaNs with 0.0
                viewport[mask] .= 0.0

                # thread_pixels[tid] .+= viewport
                # thread_mask[tid] .|= .!mask

                Threads.lock(spinlock)
                view_pixels .+= viewport
                view_mask .|= .!mask
                Threads.unlock(spinlock)
            end

            val = Float32(0.0)

            stride = strides(pixels)

            if beam == CIRCLE
                val = ccall(
                    radial_spec_fptr,
                    Cfloat,
                    (
                        Ptr{Float16},
                        UInt32,
                        Int32,
                        Int32,
                        Int32,
                        Int32,
                        Int32,
                        Int32,
                        Int32,
                        Bool,
                        Float32,
                    ),
                    pixels,
                    stride[2],
                    x1 - 1,
                    x2,
                    y1 - 1,
                    y2,
                    cx,
                    cy,
                    r2,
                    average,
                    cdelt3,
                )
            elseif beam == SQUARE
                val = ccall(
                    square_spec_fptr,
                    Cfloat,
                    (Ptr{Float16}, UInt32, Int32, Int32, Int32, Int32, Bool, Float32),
                    pixels,
                    stride[2],
                    x1 - 1,
                    x2,
                    y1 - 1,
                    y2,
                    average,
                    cdelt3,
                )
            end

            Threads.lock(spinlock)
            push!(spectrum, (frame, val))
            Threads.unlock(spinlock)

            # println(Threads.threadid(), "::", frame, ", val = ", val, ", val2 = ", val2)
        catch e
            # println(e)
            @error "calculateViewportSpectrum" exception = (e, catch_backtrace())
        end

        GC.safepoint()
    end

    if bImage
        # combine the pixels/mask from each thread
        #= 
        pixels = zeros(Float32, dimx, dimy)
        mask = map(isnan, pixels)

        for th_pixels in thread_pixels
            pixels .+= th_pixels
        end

        for th_mask in thread_mask
            mask .|= th_mask
        end =#

        if bDownsize
            try
                view_pixels = Float32.(imresize(view_pixels, (view_width, view_height)),)
                view_mask =
                    Bool.(
                        imresize(view_mask, (view_width, view_height), method = Constant()),
                    ) # use Nearest-Neighbours for the mask
            catch e
                # println(e)
                println(
                    "imresize error: ",
                    size(view_pixels),
                    "-->($view_width,$view_height); ",
                    typeof(view_pixels),
                    ";",
                    typeof(view_mask),
                )
            end
        end

        put!(queue, (view_pixels, view_mask, spectrum))
    else
        put!(queue, (Nothing, Nothing, spectrum))
    end
end

@everywhere function calculateGlobalStatistics(
    data_median::Float32,
    compressed_frames::Dict{Int32,Matrix{Float16}},
    idx::Vector{Int64},
    queue::RemoteChannel{Channel{Tuple}},
)
    println("#threads per worker: ", Threads.nthreads())

    spinlock = Threads.SpinLock()

    local sum₊::Float32, count₊::Int64
    local sum₋::Float32, count₋::Int64

    sum₊ = 0.0
    count₊ = 0

    sum₋ = 0.0
    count₋ = 0

    Threads.@threads for frame in idx
        try
            Threads.lock(spinlock)
            pixels = compressed_frames[frame]
            Threads.unlock(spinlock)

            mask = map(!isnan, pixels)
            valid_pixels = @view pixels[mask]

            pixelsN = filter(x -> x < data_median, valid_pixels)
            pixelsP = filter(x -> x > data_median, valid_pixels)

            countN = length(pixelsN)
            countP = length(pixelsP)

            sumN = sum(map(x -> abs(x - data_median), pixelsN))
            sumP = sum(map(x -> abs(x - data_median), pixelsP))

            Threads.lock(spinlock)
            begin
                sum₊ += sumP
                count₊ += countP

                sum₋ += sumN
                count₋ += countN
            end
            Threads.unlock(spinlock)

        catch e
            # println(e)
            @error "calculateGlobalStatistics" exception = (e, catch_backtrace())
        end
    end

    put!(queue, (sum₊, count₊, sum₋, count₋))
end

function zfp_compress_pixels(datasetid, frame, pixels, mask)
    cache_dir = ".cache" * Base.Filesystem.path_separator * datasetid
    filename = cache_dir * Base.Filesystem.path_separator * string(frame)

    compressed_pixels = zfp_compress(pixels, precision = 14)
    compressed_mask = lz4_hc_compress(collect(flatten(UInt8.(mask))))

    serialize(filename * ".zfp", compressed_pixels)
    serialize(filename * ".lz4", compressed_mask)
end

@everywhere function load_zfp_frames(
    datasetid,
    width,
    height,
    idx,
    queue::RemoteChannel{Channel{Int32}},
)
    compressed_frames = Dict{Int32,Matrix{Float16}}()
    frames = Channel{Tuple}(32)

    frames_task = @async while true
        try
            frame, frame_pixels = take!(frames)
            compressed_frames[frame] = frame_pixels
        catch e
            if isa(e, InvalidStateException) && e.state == :closed
                break
            else
                println(e)
            end
        end
    end

    Threads.@threads for frame in idx
        try
            cache_dir = ".cache" * Base.Filesystem.path_separator * datasetid
            filename = cache_dir * Base.Filesystem.path_separator * string(frame)

            compressed_pixels = deserialize(filename * ".zfp")
            compressed_mask = deserialize(filename * ".lz4")

            # decompress the data and convert into the right format                
            frame_mask = reshape(
                Bool.(lz4_decompress(compressed_mask, width * height)),
                (width, height),
            )
            frame_pixels = Float16.(zfp_decompress(compressed_pixels))

            # insert back NaNs
            frame_pixels[frame_mask] .= NaN16

            put!(frames, (frame, frame_pixels))

            # println("decompressed frame #$frame")

            put!(queue, frame)
        catch e
            println(e)
        end

        # allow garbage collection to run
        GC.safepoint()
    end

    close(frames)
    wait(frames_task)

    return compressed_frames
end

function decompressData(fits::FITSDataSet)
    if (fits.datasetid == "") || (fits.depth <= 1)
        return
    end

    lock(fits.mutex)
    fits.progress = Threads.Atomic{Int}(0)
    unlock(fits.mutex)

    progress = RemoteChannel(() -> Channel{Int32}(32))

    @async while true
        try
            idx = take!(progress)
            println("decompressed frame #$idx")

            update_progress(fits, fits.depth)
        catch e
            println("decompressing data completed")
            break
        end
    end

    # Remote Access Service
    ras = [
        @spawnat w load_zfp_frames(
            fits.datasetid,
            fits.width,
            fits.height,
            findall(value),
            progress,
        ) for (w, value) in fits.indices
    ]

    println("ras: ", ras)

    fits.compressed_pixels = ras

    @time wait.(ras)
    close(progress)
end