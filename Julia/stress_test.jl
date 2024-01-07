using Base.Threads
using CodecLz4
using Dates
using Distributed
using FHist, Statistics, Plots
using HTTP
using JSON
using UUIDs
using WebSockets

function get_dataset_url(host, port, id)
    return "http://" * host * ":" * port * "/fitswebql/FITSWebQL.html?db=alma&table=cube&datasetId=" * id
end

function poll_progress(host, port, id)
    strURL = "http://" * host * ":" * port * "/fitswebql/progress/" * id

    resp = HTTP.get(strURL)
    # println(resp)

    if resp.status == 200
        return JSON.parse(String(resp.body))["progress"]
    else
        return nothing
    end
end

function get_dataset(host, port, id)
    url = get_dataset_url(host, port, id)
    return HTTP.get(url)
end

function fetch_image_spectrum(host, port, id)
    width = 800
    height = 600
    strURL = "http://" * host * ":" * port * "/fitswebql/image_spectrum?datasetId=" * id * "&width=" * string(width) * "&height=" * string(height) * "&fetch_data=true"

    resp = HTTP.get(strURL)
    println("$id::fetch_image_spectrum status: ", resp.status)

    if resp.status != 200
        return nothing
    end

    # print the length and type of the received data
    println("$id: length of the received data: ", length(resp.body), " bytes, type: ", typeof(resp.body))

    if length(resp.body) == 0
        return nothing
    end

    data = resp.body

    # parse the received binary data Vector{UInt8}    
    offset = 1

    # read the string length as UInt32
    str_length = reinterpret(UInt32, data[offset:offset+3])[1]
    offset += sizeof(UInt32) + str_length

    # skip the first 7 Float32 numbers
    offset += 7 * sizeof(Float32)

    # read the img_width and img_height as UInt32
    img_width = reinterpret(UInt32, data[offset:offset+3])[1]
    img_height = reinterpret(UInt32, data[offset+4:offset+7])[1]
    offset += 2 * sizeof(UInt32)

    println("$id: img_width: ", img_width, ", img_height: ", img_height)

    # read the pixels length as UInt32
    pixels_length = reinterpret(UInt32, data[offset:offset+3])[1]
    offset += sizeof(UInt32) + pixels_length

    # read the mask length as UInt32
    mask_length = reinterpret(UInt32, data[offset:offset+3])[1]
    offset += sizeof(UInt32) + mask_length

    println("$id: pixels_length: ", pixels_length, ", mask_length: ", mask_length)

    # read the json length as UInt32
    json_length = reinterpret(UInt32, data[offset:offset+3])[1]
    offset += sizeof(UInt32)

    # read the buffer length as UInt32
    buffer_length = reinterpret(UInt32, data[offset:offset+3])[1]
    offset += sizeof(UInt32)

    # read the JSON string encoded in UTF-8
    json_buf = data[offset:offset+buffer_length-1]
    offset += buffer_length

    # decompress the LZ4 encoded JSON string    
    json_str = String(lz4_decompress(json_buf, json_length))

    println("$id: json_length: ", json_length, ", json_str: ", json_str)

    # parse the JSON string
    json = JSON.parse(json_str)

    # read the FITS header length as UInt32
    fits_header_length = reinterpret(UInt32, data[offset:offset+3])[1]
    offset += sizeof(UInt32)

    # read the buffer length as UInt32
    buffer_length = reinterpret(UInt32, data[offset:offset+3])[1]
    offset += sizeof(UInt32)

    # read the FITS header string encoded with LZ4
    fits_header_buf = data[offset:offset+buffer_length-1]
    offset += buffer_length

    # decompress the LZ4 encoded FITS header string
    header = String(lz4_decompress(fits_header_buf, fits_header_length))
    #println("$id: fits_header_length: ", fits_header_length)

    return (json, header)
end

function test(host, port, id, stat)
    resp = get_dataset(host, port, id)

    # check the HTTP response code
    if resp.status != 200
        println(resp)
        return
    end

    # repeatedly poll for progress
    while true
        progress = poll_progress(host, port, id)

        if isnothing(progress)
            println("\nno progress")
            break
        end

        println("datasetid: ", id, ", progress: ", Int(floor(progress)), "%")

        # throw a DomainError if the progress is over 100% (should not happen, I want to catch any logical bugs, network problems, etc.)
        if progress > 100
            println("\nanomalous progress detected: $(progress)!")
            throw(DomainError(progress, "anomalous progress detected"))
        end

        if progress == 100
            break
        else
            sleep(1)
        end

    end

    # fetch the image spectrum
    json, _ = fetch_image_spectrum(host, port, id)

    println("datasetid: ", id, ", json: ", json)
    #println("datasetid: ", id, ", header: ", header)

    # get width and height from the JSON string
    fits_width = json["width"]
    fits_height = json["height"]
    fits_depth = json["depth"]
    println("datasetid: ", id, ", FITS width: ", fits_width, ", height: ", fits_height, ", depth: ", fits_depth)

    # get the CRVAL3, CRPIX3, CDELT3, and NAXIS3 values from the JSON string
    CRVAL3 = json["CRVAL3"]
    CRPIX3 = json["CRPIX3"]
    CDELT3 = json["CDELT3"]
    RESTFRQ = json["RESTFRQ"]
    println("datasetid: ", id, ", FITS crval3: ", CRVAL3, ", crpix3: ", CRPIX3, ", cdelt3: ", CDELT3, ", restfrq: ", RESTFRQ)

    val1 = CRVAL3 + CDELT3 * (1 - CRPIX3)
    val2 = CRVAL3 + CDELT3 * (fits_depth - CRPIX3)

    data_band_lo = min(val1, val2)
    data_band_hi = max(val1, val2)
    println("datasetid: ", id, ", data_band_lo: ", data_band_lo, ", data_band_hi: ", data_band_hi)

    # make a unique uuidv4 session id
    session_id = UUIDs.uuid4()

    # increment the port number by 1
    ws_port = string(parse(Int, port) + 1)

    wsURL = "ws://" * host * ":" * string(ws_port) * "/fitswebql/websocket/" * id * "/" * string(session_id)
    println("datasetid: ", id, ", wsURL: ", wsURL)

    base = Dates.value(now())

    # next open a WebSocket client connection
    WebSockets.open(wsURL) do ws
        running = true

        @async while running
            data, success = readguarded(ws)

            # print the type and length of the received data
            len = length(data)

            if success
                # check if Vector{UInt8} can be converted to String
                try
                    str = String(copy(data))
                    println(stderr, "[$id::WS] received '", ascii(str), "'")
                catch _
                    offset = 1
                    timestamp = reinterpret(Float32, data[offset:offset+3])[1]
                    offset += sizeof(Float32)
                    recv_seq_id = reinterpret(UInt32, data[offset:offset+3])[1]
                    offset += sizeof(UInt32)
                    msg_type = reinterpret(UInt32, data[offset:offset+3])[1]

                    ts = Dates.value(now()) - base
                    latency = ts - timestamp
                    put!(stat, latency)

                    println(stderr, "[$id::WS] received $len bytes, timestamp: $timestamp, recv_seq_id: $recv_seq_id, type: $msg_type, latency: $latency [ms]")
                end
            else
                println(stderr, "[$id::WS] closed")
                break
            end
        end

        @async while running
            try
                # make a timestamp as a single floating-point number
                timestamp = Dates.value(now())

                # send a heartbeat message
                msg = "[heartbeat] " * string(timestamp)
                success = writeguarded(ws, msg)

                if !success
                    break
                end

                sleep(1)
            catch _
                break
            end
        end

        fps = 30
        video_fps = 5

        # a real-time image spectrum loop
        counter = 0
        @async while running
            # make a timestamp as a single floating-point number
            timestamp = Dates.value(now()) - base

            # a random region
            x1, x2 = rand(1:fits_width), rand(1:fits_width)
            y1, y2 = rand(1:fits_height), rand(1:fits_height)

            # image: true 10%, false 90%
            image = rand() < 0.1 ? true : false

            # beam: "circle" or "square"
            beam = rand() < 0.5 ? "circle" : "square"

            # intensity: "mean" or "integrated"
            intensity = rand() < 0.5 ? "mean" : "integrated"

            # quality: "low", "medium", or "high"
            quality = rand() < 0.33 ? "low" : (rand() < 0.5 ? "medium" : "high")

            # zoomed_size: random between 50 and 200
            zoomed_size = rand(50:200)

            # make a JSON message
            msg = JSON.json(Dict("type" => "realtime_image_spectrum",
                "dx" => 800,
                "width" => zoomed_size,
                "height" => zoomed_size,
                "timestamp" => timestamp,
                "x1" => x1,
                "x2" => x2,
                "y1" => y1,
                "y2" => y2,
                "image" => image,
                "beam" => beam,
                "intensity" => intensity,
                "quality" => quality,
                "frame_start" => data_band_lo,
                "frame_end" => data_band_hi,
                "ref_freq" => RESTFRQ,
                "seq_id" => counter))

            # send a realtime image spectrum message            
            success = writeguarded(ws, msg)

            if !success
                break
            end

            # a fixed frames per second
            sleep(1 / fps)

            counter = counter + 1
        end

        # a video loop
        @async begin
            seq_id = 0

            # make a timestamp as a single floating-point number
            timestamp = Dates.value(now()) - base

            msg = JSON.json(Dict("type" => "init_video",
                "width" => 800,
                "height" => 600,
                "view" => "tile",
                "fps" => video_fps,
                "timestamp" => timestamp,
                "frame" => data_band_lo,
                "seq_id" => seq_id,
                "bitrate" => 1000,
                "flux" => "legacy",
                "ref_freq" => RESTFRQ))

            # send a message
            success = writeguarded(ws, msg)

            if success
                println("video loop started.")
            end

            while running
                # loop through fits_depth
                for i in 1:fits_depth
                    seq_id = seq_id + 1
                    frame = CRVAL3 + CDELT3 * (i - CRPIX3)

                    # make a timestamp as a single floating-point number
                    timestamp = Dates.value(now()) - base

                    # make a JSON message
                    msg = JSON.json(Dict("type" => "video",
                        "frame" => frame,
                        "key" => false,
                        "fill" => 255,
                        "ref_freq" => RESTFRQ,
                        "fps" => video_fps,
                        "seq_id" => seq_id,
                        "bitrate" => 1000,
                        "timestamp" => timestamp))

                    # send a message
                    success = writeguarded(ws, msg)

                    if !success
                        break
                    end

                    # a fixed frames per second
                    sleep(1 / video_fps)
                end
            end

            msg = JSON.json(Dict("type" => "end_video"))

            # send a message
            success = writeguarded(ws, msg)

            if success
                println("video loop ended.")
            end
        end

        # sleep for X hours
        sleep(1 * 3600) # was 12 hours
        # sleep(10) # testing
        running = false

        # send a close message
        println("[$id::WS] closing...")
        writeguarded(ws, "[close]")
    end
end

responses::Int64 = 0
total_time::Float64 = 0.0
stat = RemoteChannel(() -> Channel{Float64}(32))
hist = Hist1D(Float64, bins=0.0:10.0:2000.0, overflow=true)

stat_task = @async while true
    global responses, total_time

    try
        response_time = take!(stat)
        total_time += response_time
        responses += 1
        push!(hist, response_time)
    catch e
        if isa(e, InvalidStateException) && e.state == :closed
            println("statistics task completed, #responses: ", responses, ", total_time: ", total_time, " [ms]")

            if responses > 0
                println("average response time: ", total_time / responses, " [ms]")
                println("histogram: ", hist)
                print("mean: ", mean(hist), ", std: ", std(hist), ", median: ", median(hist))
                plot(hist; size=(600, 500), legend=:topright)
                savefig(homedir() * "/histogram.pdf")
            end

            break
        else
            println(e)
        end
    end
end

host = "capricorn"
port = "8080"
datasets = ["ALMA01047077", "ALMA01018218", "ALMA01003454", "ALMA01575449", "ALMA01015786", "ALMA01084695"]

# a dry run to warm up (pre-compile) Julia functions
# test(host, port, datasets[1], stat)

jobs = [Threads.@spawn test(host, port, dataset, stat) for dataset in datasets]
wait.(jobs)

close(stat)
wait(stat_task)

println("stress-test completed.")