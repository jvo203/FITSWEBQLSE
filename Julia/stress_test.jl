using Base.Threads
using CodecLz4
using Dates
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

function test(host, port, id)
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

    # next open a WebSocket client connection
    WebSockets.open(wsURL) do ws
        @async while true
            data, success = readguarded(ws)

            # print the type and length of the received data
            len = length(data)

            if success
                # check if Vector{UInt8} can be converted to String
                try
                    str = String(data)
                    println(stderr, "[$id::WS] received: ", ascii(str))
                catch _
                    println(stderr, "[$id::WS] received binary data $len bytes")
                end
            else
                println(stderr, "[$id::WS] closed")
                break
            end
        end

        @async while true
            # make a timestamp as a single floating-point number
            timestamp = Dates.value(now())

            # send a heartbeat message
            msg = "[heartbeat] " * string(timestamp)
            success = writeguarded(ws, msg)

            if !success
                break
            end

            sleep(1)
        end

        # a real-time image spectrum loop
        counter = 0
        @async while true
            # make a timestamp as a single floating-point number
            timestamp = Dates.value(now())

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

            # assume fps of 10
            sleep(1 / 10)

            counter = counter + 1
        end

        # a video loop
        @async begin
            println("video loop started.")

            sleep(1)

            println("video loop ended.")
        end

        # sleep for 12 hours
        # sleep(12 * 3600)
        sleep(5) # testing

        # send a close message        
        writeguarded(ws, "[close]")
        sleep(10)
    end
end

host = "capricorn"
port = "8080"
datasets = ["ALMA01047077", "ALMA01018218", "ALMA01003454", "ALMA01575449", "ALMA01015786", "ALMA01084695"]

# a dry run to warm up (pre-compile) Julia functions
test(host, port, datasets[1])

#jobs = [@spawn test(host, port, dataset) for dataset in datasets]
#wait.(jobs)

println("stress-test completed.")