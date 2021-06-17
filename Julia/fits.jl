using Dates;
using DistributedArrays;
using FITSIO;
using SharedArrays;

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

    # pixels, spectrum
    image::Any
    spectrum::Any

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

        # println(fits.header)
        # process_header(fits)

        # read a 2D image
        if depth == 1
            println("reading a $width X $height 2D image")

            try

                @time fits.image = reshape(read(hdu), (width, height))
                println("FITS image dimensions: ", size(fits.image))

                lock(fits.mutex)
                fits.has_data = true
                unlock(fits.mutex)

                update_progress(fits, 1)
            catch e
                println("Error reading pixels: $e")
            end

        else
            println(
                "reading a $width X $height X $depth 3D data cube using $(length(workers())) parallel worker(s)",
            )

            try
                # idx = SharedArray{Int}(depth)
                pixels = zeros(Float32, width, height)
                spectrum = zeros(Float32, depth)

                # TO-DO: reducing pixels, using cdelt3
                # TO-DO: integrated_spectrum, mean_spectrum

                jobs = RemoteChannel(() -> Channel{Int}(32))
                progress = RemoteChannel(() -> Channel{Tuple}(32))

                # fill-in the jobs queue
                @async for i = 1:depth
                    put!(jobs, i)

                    # close the channel after the last value has been sent
                    if i == depth
                        close(jobs)
                    end
                end

                # process the incoming results in the background
                progress_task = @async while true
                    try
                        frame, val = take!(progress)
                        spectrum[frame] = Float32(val)
                        update_progress(fits, depth)
                        # println("reading frame #$frame::$val; done")
                    catch e
                        println("progress task completed")
                        break
                    end
                end

                @everywhere function load_fits_frame(
                    jobs,
                    progress,
                    path,
                    width,
                    height,
                    hdu_id,
                )

                    local frame

                    proc_pixels = zeros(Float32, width, height)

                    try

                        fits_file = FITS(path)

                        while true
                            frame = take!(jobs)

                            pixels = reshape(
                                read(fits_file[hdu_id], :, :, frame, 1),
                                (width, height),
                            )

                            proc_pixels += pixels

                            val = sum(pixels)

                            put!(progress, (frame, val))

                            # println("processing frame #$frame")
                        end

                    catch e
                    # println("task $(myid)/$frame::error: $e")
                    finally
                        # send the (pixels, mask) to the results queue

                        println("loading FITS cube finished")
                    end

                end

                #spawn remote jobs
                @time @sync for w in workers()
                    @spawnat w load_fits_frame(
                        jobs,
                        progress,
                        filepath,
                        width,
                        height,
                        hdu_id,
                    )
                end

                close(progress)

                wait(progress_task)

                fits.image = pixels
                fits.spectrum = spectrum

                println("pixels:", size(pixels))
                # println("spectrum:", fits.spectrum)

                lock(fits.mutex)
                fits.has_data = true
                unlock(fits.mutex)

            catch e
                println("distributed computing error: $e")
            end

            try
                # ras = [@spawnat w 10 for w in workers()]
                # fits_files = DArray(ras)
                # println("fits_handles:", fits_files)
                # println([@fetchfrom w localindices(fits_files) for w in workers()])        

                # ras = [@spawnat w rand(2, 2) for w in workers()[1:4]]
                # ras = reshape(ras, (2, 2))
                # D = DArray(ras)
                # println([@fetchfrom p DistributedArrays.localindices(D) for p in workers()])

                # println("DArray:", D)

            catch e
                println("DArray error: $e")
            end

            println("reading depth($depth) > 1 is not fully implemented yet.")
        end

        break
    end

    update_timestamp(fits)

end
