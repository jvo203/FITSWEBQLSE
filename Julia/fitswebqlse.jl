import Base.Iterators: flatten
using CodecBzip2;
using CodecLz4;
using Distributed;
using HTTP;
using JSON;
using Printf;
using Sockets;
using SQLite;
using WebSockets;
using ZfpCompression;

const LOCAL_VERSION = true
const PRODUCTION = false

const VERSION_MAJOR = 5
const VERSION_MINOR = 0
const VERSION_SUB = 0
const SERVER_STRING =
    "FITSWEBQLSE v" *
    string(VERSION_MAJOR) *
    "." *
    string(VERSION_MINOR) *
    "." *
    string(VERSION_SUB)

const WASM_VERSION = "21.07.XX.X"
const VERSION_STRING = "SV2021-07-XX.X-ALPHA"

const ZFP_HIGH_PRECISION = 16
const ZFP_MEDIUM_PRECISION = 11
const ZFP_LOW_PRECISION = 8

const SPECTRUM_HIGH_PRECISION = 24
const SPECTRUM_MEDIUM_PRECISION = 16

const HT_DOCS = "htdocs"
const HTTP_PORT = 8080
const WS_PORT = HTTP_PORT + 1

@everywhere include("fits.jl")

# a global list of FITS objects
FITS_OBJECTS = Dict{String,FITSDataSet}()
FITS_LOCK = ReentrantLock()

function serveFile(path::String)
    # strip out a question mark (if there is any)
    pos = findlast("?", path)

    if !isnothing(pos)
        path = SubString(path, 1:(pos[1]-1))
    end

    # cache a response
    headers = ["Cache-Control" => "public, max-age=86400"]

    # add mime types
    if endswith(path, ".htm") || endswith(path, ".html")
        push!(headers, "Content-Type" => "text/html")
    end

    if endswith(path, ".txt")
        push!(headers, "Content-Type" => "text/plain")
    end

    if endswith(path, ".css")
        push!(headers, "Content-Type" => "text/css")
    end

    if endswith(path, ".js")
        push!(headers, "Content-Type" => "application/javascript")
    end

    if endswith(path, ".wasm")
        push!(headers, "Content-Type" => "application/wasm")
    end

    if endswith(path, ".pdf")
        push!(headers, "Content-Type" => "application/pdf")
    end

    if endswith(path, ".ico")
        push!(headers, "Content-Type" => "image/x-icon")
    end

    if endswith(path, ".png")
        push!(headers, "Content-Type" => "image/png")
    end

    if endswith(path, ".gif")
        push!(headers, "Content-Type" => "image/gif")
    end

    if endswith(path, ".webp")
        push!(headers, "Content-Type" => "image/webp")
    end

    if endswith(path, ".svg")
        push!(headers, "Content-Type" => "image/svg+xml")
    end

    if endswith(path, ".jpeg") || endswith(path, ".jpg")
        push!(headers, "Content-Type" => "image/jpeg")
    end

    if endswith(path, ".mp4")
        push!(headers, "Content-Type" => "video/mp4")
    end

    try
        return isfile(path) ? HTTP.Response(200, headers; body = read(path)) :
               HTTP.Response(404, "$path Not Found.")
    catch e
        return HTTP.Response(404, "Error: $e")
    end
end

function serveDirectory(request::HTTP.Request)
    headers = ["Content-Type" => "application/json"]

    params = HTTP.queryparams(HTTP.URI(request.target))

    dir = ""

    try
        dir = params["dir"]

        # on Windows remove the root slash
        if Sys.iswindows() && length(dir) > 0
            dir = lstrip(dir, '/')
        end

        if dir == ""
            dir = homedir()
        end
    catch e
        # if they keyword is not found fall back on a home directory
        dir = homedir()
    end

    # append a slash so that on Windows "C:" becomes "C:/"
    if dir == "C:"
        dir = dir * "/"
    end

    println("Scanning $dir ...")

    resp = chop(JSON.json(Dict("location" => dir)), tail = 1) * ", \"contents\":["

    elements = false

    try
        foreach(readdir(dir)) do f
            filename = lowercase(f)

            if !startswith(filename, ".")

                path = dir * Base.Filesystem.path_separator * f

                info = stat(path)

                if isdir(path)
                    dict = Dict(
                        "type" => "dir",
                        "name" => f,
                        "last_modified" => Libc.strftime(info.mtime),
                    )

                    resp *= JSON.json(dict) * ","
                    elements = true
                end

                if isfile(path)

                    # filter the filenames
                    if endswith(filename, ".fits") || endswith(filename, ".fits.gz")

                        dict = Dict(
                            "type" => "file",
                            "size" => info.size,
                            "name" => f,
                            "last_modified" => Libc.strftime(info.mtime),
                        )

                        resp *= JSON.json(dict) * ","
                        elements = true
                    end
                end

            end
        end
    catch e
    end

    if elements
        resp = chop(resp, tail = 1) * "]}"
    else
        resp *= "]}"
    end

    try
        return HTTP.Response(200, headers; body = resp)
    catch e
        return HTTP.Response(404, "Error: $e")
    end
end

function serveROOT(request::HTTP.Request)
    # @show request
    # @show request.method
    # @show HTTP.header(request, "Content-Type")
    # @show HTTP.payload(request)
    @show request.target

    # prevent a simple directory traversal
    if occursin("../", request.target) || occursin("..\\", request.target)
        return HTTP.Response(404, "Not Found")
    end

    path = HT_DOCS * HTTP.unescapeuri(request.target)

    if request.target == "/"
        if LOCAL_VERSION
            path *= "local_j.html"
        else
            path *= "test.html"
        end
    end

    return serveFile(path)
end

# a recursive function (very elegant)
function get_dataset(prefix::String, params, datasets, idx::Integer)
    try
        push!(datasets, params[prefix*string(idx)])
        get_dataset(prefix, params, datasets, idx + 1)
    catch e
        # no more datasets, stop recursion
        return
    end
end

function serveHeartBeat(request::HTTP.Request)
    timestamp = HTTP.URIs.splitpath(HTTP.unescapeuri(request.target))[3]

    try
        return HTTP.Response(200, timestamp)
    catch e
        return HTTP.Response(404, "Error: $e")
    end
end

function serveProgress(request::HTTP.Request)
    datasetid = ""

    try
        datasetid = HTTP.URIs.splitpath(HTTP.unescapeuri(request.target))[3]
    catch e
    end

    fits_object = get_dataset(datasetid, FITS_OBJECTS, FITS_LOCK)

    if fits_object.datasetid == ""
        return HTTP.Response(404, "Not Found")
    end

    if has_error(fits_object)
        return HTTP.Response(500, "Internal Server Error")
    end

    # get the progress tuple
    progress, elapsed = get_progress(fits_object)

    # form a JSON response
    resp = IOBuffer()
    write(resp, "{\"progress\" : $progress, \"elapsed\" : $elapsed}")

    headers = ["Content-Type" => "application/json"]

    try
        return HTTP.Response(200, headers; body = take!(resp))
    catch e
        return HTTP.Response(404, "Error: $e")
    end
end


function streamMolecules(http::HTTP.Stream)
    global splat_db

    request::HTTP.Request = http.message
    request.body = read(http)
    closeread(http)

    params = HTTP.queryparams(HTTP.URI(request.target))
    # println(params)

    datasetid = ""
    freq_start::Float64 = 0.0
    freq_end::Float64 = 0.0

    try
        datasetid = params["datasetId"]
        freq_start = parse(Float64, params["freq_start"])
        freq_end = parse(Float64, params["freq_end"])
    catch e
        println(e)
        HTTP.setstatus(http, 404)
        startwrite(http)
        write(http, "Not Found")
        closewrite(http)
        return nothing
    end

    if freq_start == 0.0 || freq_end == 0.0
        # get the frequency range from the dataset

        fits_object = get_dataset(datasetid, FITS_OBJECTS, FITS_LOCK)

        if fits_object.datasetid == ""
            HTTP.setstatus(http, 404)
            startwrite(http)
            write(http, "Not Found")
            closewrite(http)
            return nothing
        end

        if has_error(fits_object)
            HTTP.setstatus(http, 500)
            startwrite(http)
            write(http, "Internal Server Error")
            closewrite(http)
            return nothing
        end

        if !has_header(fits_object)
            HTTP.setstatus(http, 202)
            startwrite(http)
            write(http, "Accepted")
            closewrite(http)
            return nothing
        end

        try
            freq_start, freq_end = get_frequency_range(fits_object)
        catch e
            println("streamMolecules::$e")

            HTTP.setstatus(http, 404)
            startwrite(http)
            write(http, "Not Found")
            closewrite(http)
            return nothing
        end
    end

    println("get_molecules::$datasetid; [$freq_start, $freq_end] [GHz]")

    # fetch the molecules from Splatalogue
    strSQL = "SELECT * FROM lines WHERE frequency>=$freq_start AND frequency<=$freq_end;"

    has_molecules = false
    resp = IOBuffer()
    write(resp, "{\"molecules\" : [")

    try
        for row in SQLite.DBInterface.execute(splat_db, strSQL)
            has_molecules = true
            json = JSON.json(row)

            write(resp, json)
            write(resp, ",")
        end
    catch e
        println("streamMolecules::$e")

        HTTP.setstatus(http, 404)
        startwrite(http)
        write(http, "Not Found")
        closewrite(http)
        return nothing
    end

    json = String(take!(resp))

    if !has_molecules
        json = "{\"molecules\" : []}"
    else
        # remove the last character (comma) from json, end an array
        json = chop(json, tail = 1) * "]}"
    end

    # compress with bzip2 (more efficient than LZ4HC)
    compressed = transcode(Bzip2Compressor, json)
    println(
        "SPECTRAL LINES JSON length: $(length(json)); bzip2-compressed: $(length(compressed))",
    )

    # cache a response
    HTTP.setheader(http, "Cache-Control" => "public, max-age=86400")

    # sending binary data
    HTTP.setheader(http, "Content-Type" => "application/octet-stream")

    HTTP.setstatus(http, 200)
    startwrite(http)
    write(http, compressed)
    closewrite(http)

    return nothing
end

function streamImageSpectrum(http::HTTP.Stream)
    request::HTTP.Request = http.message
    request.body = read(http)
    closeread(http)

    params = HTTP.queryparams(HTTP.URI(request.target))
    # println(params)

    datasetid = ""
    quality::Quality = medium
    width::Integer = 0
    height::Integer = 0
    fetch_data::Bool = false

    try
        datasetid = params["datasetId"]
        width = round(Integer, parse(Float64, params["width"]))
        height = round(Integer, parse(Float64, params["height"]))
    catch e
        println(e)
        HTTP.setstatus(http, 404)
        startwrite(http)
        write(http, "Not Found")
        closewrite(http)
        return nothing
    end

    try
        quality = eval(Meta.parse(params["quality"]))
    catch e
    end

    try
        fetch_data = parse(Bool, params["fetch_data"])
    catch e
    end

    HTTP.setheader(http, "Cache-Control" => "no-cache")
    HTTP.setheader(http, "Cache-Control" => "no-store")
    HTTP.setheader(http, "Pragma" => "no-cache")
    HTTP.setheader(http, "Content-Type" => "application/octet-stream")

    fits_object = get_dataset(datasetid, FITS_OBJECTS, FITS_LOCK)

    if fits_object.datasetid == "" || width <= 0 || height <= 0
        HTTP.setstatus(http, 404)
        startwrite(http)
        write(http, "Not Found")
        closewrite(http)
        return nothing
    end

    if has_error(fits_object)
        HTTP.setstatus(http, 500)
        startwrite(http)
        write(http, "Internal Server Error")
        closewrite(http)
        return nothing
    end

    if !has_data(fits_object)
        HTTP.setstatus(http, 202)
        startwrite(http)
        write(http, "Accepted")
        closewrite(http)
        return nothing
    end

    try
        local image_task

        # get the JSON description
        json_task = @async getJSON(fits_object)

        if fits_object.depth > 0

            # downsize a 2D image and/or handle a distributed 3D cube
            image_task = @async getImage(fits_object, width, height)

        else

            HTTP.setstatus(http, 501)
            startwrite(http)
            write(http, "Not Implemented")
            closewrite(http)
            return nothing

        end

        # by default chop cuts the last character only; replace it with ','
        json = chop(fetch(json_task)) * ","
        histogram, tone_mapping, pixels, mask = fetch(image_task)

        # chop the first '{' character only
        json = json * chop(JSON.json("histogram" => histogram), head = 1, tail = 0)

        println(tone_mapping)

        HTTP.setstatus(http, 200)
        startwrite(http)

        # first send the tone mapping
        write(http, Int32(length(tone_mapping.flux)))
        write(http, tone_mapping.flux)
        write(http, tone_mapping.pmin)
        write(http, tone_mapping.pmax)
        write(http, tone_mapping.med)
        write(http, tone_mapping.sensitivity)
        write(http, tone_mapping.ratio_sensitivity)
        write(http, tone_mapping.white)
        write(http, tone_mapping.black)

        # next the image
        dims = size(pixels)
        img_width = dims[1]
        img_height = dims[2]

        write(http, Int32(img_width))
        write(http, Int32(img_height))

        # compress pixels with ZFP
        prec = ZFP_MEDIUM_PRECISION

        if quality == high
            prec = ZFP_HIGH_PRECISION
        elseif quality == medium
            prec = ZFP_MEDIUM_PRECISION
        elseif quality == low
            prec = ZFP_LOW_PRECISION
        end

        println("pixels type: ", typeof(pixels))

        compressed_pixels = zfp_compress(pixels, precision = prec)
        write(http, Int32(length(compressed_pixels)))
        write(http, compressed_pixels)

        compressed_mask = lz4_hc_compress(collect(flatten(UInt8.(mask))))
        write(http, Int32(length(compressed_mask)))
        write(http, compressed_mask)

        if fetch_data
            # JSON
            json_len = length(json)
            compressed_json = lz4_hc_compress(Vector{UInt8}(json))
            compressed_len = length(compressed_json)

            write(http, Int32(json_len))
            write(http, Int32(compressed_len))
            write(http, compressed_json)

            # FITS HEADER
            header = fits_object.headerStr
            header_len = length(header)
            compressed_header = lz4_hc_compress(Vector{UInt8}(header))
            compressed_len = length(compressed_header)

            write(http, Int32(header_len))
            write(http, Int32(compressed_len))
            write(http, compressed_header)

            if fits_object.mean_spectrum != Nothing
                compressed_spectrum = zfp_compress(
                    fits_object.mean_spectrum,
                    precision = SPECTRUM_HIGH_PRECISION,
                )

                write(http, Int32(length(fits_object.mean_spectrum)))
                write(http, Int32(length(compressed_spectrum)))
                write(http, compressed_spectrum)
            end

            if fits_object.integrated_spectrum != Nothing
                compressed_spectrum = zfp_compress(
                    fits_object.integrated_spectrum,
                    precision = SPECTRUM_HIGH_PRECISION,
                )

                write(http, Int32(length(fits_object.integrated_spectrum)))
                write(http, Int32(length(compressed_spectrum)))
                write(http, compressed_spectrum)
            end
        end

        closewrite(http)
        return nothing

    catch e
        println(e)
        HTTP.setstatus(http, 404)
        startwrite(http)
        write(http, e)
        closewrite(http)
        return nothing
    end

    return nothing
end

function serveImageSpectrum(request::HTTP.Request)
    params = HTTP.queryparams(HTTP.URI(request.target))
    # println(params)

    datasetid = ""
    quality::Quality = medium
    width::Integer = 0
    height::Integer = 0
    fetch_data::Bool = false

    try
        datasetid = params["datasetId"]
        width = round(Integer, parse(Float64, params["width"]))
        height = round(Integer, parse(Float64, params["height"]))
    catch e
        println(e)
        return HTTP.Response(404, "Not Found")
    end

    try
        quality = eval(Meta.parse(params["quality"]))
    catch e
    end

    try
        fetch_data = parse(Bool, params["fetch_data"])
    catch e
    end

    fits_object = get_dataset(datasetid, FITS_OBJECTS, FITS_LOCK)

    if fits_object.datasetid == "" || width <= 0 || height <= 0
        return HTTP.Response(404, "Not Found")
    end

    if has_error(fits_object)
        return HTTP.Response(500, "Internal Server Error")
    end

    if !has_data(fits_object)
        return HTTP.Response(202, "Accepted")
    end

    try
        local image_task

        # get the JSON description
        json_task = @async getJSON(fits_object)

        if fits_object.depth > 1
            # handle a distributed 3D cube
            image_task = @async getImage(fits_object, width, height, quality, fetch_data)
        else
            # downsize a 2D image

            return HTTP.Response(501, "Not Implemented")
        end

        # by default chop cuts the last character only; replace it with ','
        json = chop(fetch(json_task)) * ","
        histogram, pixels, mask = fetch(image_task)

        # chop the first '{' character only
        json = json * chop(JSON.json("histogram" => histogram), head = 1, tail = 0)
        println(json)

        return HTTP.Response(501, "Not Implemented")

    catch e
        println(e)
        return HTTP.Response(404, "Error: $e")
    end
end

function serveFITS(request::HTTP.Request)
    root_path = HTTP.URIs.splitpath(request.target)[1]

    params = HTTP.queryparams(HTTP.URI(request.target))

    println("root path: \"$root_path\"")
    # println(params)

    has_fits = true
    is_composite = false

    dir = ""
    datasets = []
    ext = ""

    try
        ext = params["ext"]
    catch e
    end

    try
        dir = params["dir"]
    catch e
    end

    try
        if params["view"] == "composite"
            is_composite = true
        end
    catch e
    end

    try
        push!(datasets, params["filename"])
    catch e
        # try multiple filenames (recursion)
        get_dataset("filename", params, datasets, 1)
    end

    foreach(datasets) do f
        has_fits = has_fits && dataset_exists(f, FITS_OBJECTS, FITS_LOCK)
    end

    if !has_fits
        foreach(datasets) do f
            if !dataset_exists(f, FITS_OBJECTS, FITS_LOCK)

                # try to restore data from cache
                try
                    println("restoring $f")

                    fits_object = deserialize_fits(f)

                    if fits_object.depth > 1
                        lock(fits_object.mutex)
                        fits_object.has_data = false
                        unlock(fits_object.mutex)

                        @async restoreImage(fits_object)

                        @async restoreData(fits_object)
                    end

                    insert_dataset(fits_object, FITS_OBJECTS, FITS_LOCK)
                catch e
                    println("cannot restore $f from cache::$e")

                    fits_object = FITSDataSet(f)
                    insert_dataset(fits_object, FITS_OBJECTS, FITS_LOCK)

                    # leave the slash as before, even in Windows
                    filepath = dir * "/" * f * "." * ext
                    @async loadFITS(filepath, fits_object)
                end
            end
        end
    end

    resp = IOBuffer()

    write(resp, "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\">\n")
    write(
        resp,
        "<link href=\"https://fonts.googleapis.com/css?family=Inconsolata\" rel=\"stylesheet\"/>\n",
    )
    write(
        resp,
        "<link href=\"https://fonts.googleapis.com/css?family=Material+Icons\" rel=\"stylesheet\"/>\n",
    )
    write(resp, "<script src=\"https://d3js.org/d3.v5.min.js\"></script>\n")
    write(
        resp,
        "<script src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/reconnecting-websocket.min.js\"></script>\n",
    )
    write(
        resp,
        "<script src=\"//cdnjs.cloudflare.com/ajax/libs/numeral.js/2.0.6/numeral.min.js\"></script>\n",
    )
    write(
        resp,
        "<script src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/ra_dec_conversion.min.js\"></script>\n",
    )
    write(
        resp,
        "<script src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/sylvester.min.js\"></script>\n",
    )
    write(
        resp,
        "<script src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/shortcut.min.js\"></script>\n",
    )
    write(
        resp,
        "<script src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/colourmaps.min.js\"></script>\n",
    )
    write(
        resp,
        "<script src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/lz4.min.js\"></script>\n",
    )
    write(
        resp,
        "<script src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/marchingsquares-isocontours.min.js\"></script>\n",
    )
    write(
        resp,
        "<script src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/marchingsquares-isobands.min.js\"></script>\n",
    )
    # Bzip2 decoder
    write(resp, "<script src=\"bzip2.js\"></script>\n")

    # WebAssembly
    write(resp, "<script src=\"client.", WASM_VERSION, ".js\"></script>\n")
    write(
        resp,
        "<script>\n",
        "Module.ready\n",
        "\t.then(status => console.log(status))\n",
        "\t.catch(e => console.error(e));\n",
        "</script>\n",
    )

    # Bootstrap v3.4.1
    write(
        resp,
        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, user-scalable=no, minimum-scale=1, maximum-scale=1\">\n",
    )
    write(
        resp,
        "<link rel=\"stylesheet\" href=\"https://stackpath.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css\" integrity=\"sha384-HSMxcRTRxnN+Bdg0JdbxYKrThecOKuH5zCYotlSAcp1+c8xmyTe9GYg1l9a69psu\" crossorigin=\"anonymous\">",
    )
    write(
        resp,
        "<script src=\"https://code.jquery.com/jquery-1.12.4.min.js\" integrity=\"sha384-nvAa0+6Qg9clwYCGGPpDQLVpLNn0fRaROjHqs13t4Ggj3Ez50XnGQqc/r8MhnRDZ\" crossorigin=\"anonymous\"></script>",
    )
    write(
        resp,
        "<script src=\"https://stackpath.bootstrapcdn.com/bootstrap/3.4.1/js/bootstrap.min.js\" integrity=\"sha384-aJ21OjlMXNL5UyIl/XNwTMqvzeRMZH2w8c5cRVpzpU8Y5bApTppSuUkhZXN0VxHd\" crossorigin=\"anonymous\"></script>",
    )

    # GLSL vertex shader
    write(resp, "<script id=\"vertex-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/vertex-shader.vert"))
    write(resp, "</script>\n")

    write(resp, "<script id=\"legend-vertex-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/legend-vertex-shader.vert"))
    write(resp, "</script>\n")

    # GLSL fragment shaders
    write(resp, "<script id=\"common-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/common-shader.frag"))
    write(resp, "</script>\n")

    write(resp, "<script id=\"legend-common-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/legend-common-shader.frag"))
    write(resp, "</script>\n")

    # tone mappings
    write(resp, "<script id=\"ratio-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/ratio-shader.frag"))
    write(resp, "</script>\n")

    write(resp, "<script id=\"logistic-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/logistic-shader.frag"))
    write(resp, "</script>\n")

    write(resp, "<script id=\"square-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/square-shader.frag"))
    write(resp, "</script>\n")

    write(resp, "<script id=\"legacy-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/legacy-shader.frag"))
    write(resp, "</script>\n")

    write(resp, "<script id=\"linear-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/linear-shader.frag"))
    write(resp, "</script>\n")

    # colourmaps
    write(resp, "<script id=\"greyscale-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/greyscale-shader.frag"))
    write(resp, "</script>\n")

    write(resp, "<script id=\"negative-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/negative-shader.frag"))
    write(resp, "</script>\n")

    write(resp, "<script id=\"amber-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/amber-shader.frag"))
    write(resp, "</script>\n")

    write(resp, "<script id=\"red-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/red-shader.frag"))
    write(resp, "</script>\n")

    write(resp, "<script id=\"green-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/green-shader.frag"))
    write(resp, "</script>\n")

    write(resp, "<script id=\"blue-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/blue-shader.frag"))
    write(resp, "</script>\n")

    write(resp, "<script id=\"hot-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/hot-shader.frag"))
    write(resp, "</script>\n")

    write(resp, "<script id=\"rainbow-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/rainbow-shader.frag"))
    write(resp, "</script>\n")

    write(resp, "<script id=\"parula-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/parula-shader.frag"))
    write(resp, "</script>\n")

    write(resp, "<script id=\"inferno-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/inferno-shader.frag"))
    write(resp, "</script>\n")

    write(resp, "<script id=\"magma-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/magma-shader.frag"))
    write(resp, "</script>\n")

    write(resp, "<script id=\"plasma-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/plasma-shader.frag"))
    write(resp, "</script>\n")

    write(resp, "<script id=\"viridis-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/viridis-shader.frag"))
    write(resp, "</script>\n")

    write(resp, "<script id=\"cubehelix-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/cubehelix-shader.frag"))
    write(resp, "</script>\n")

    write(resp, "<script id=\"jet-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/jet-shader.frag"))
    write(resp, "</script>\n")

    write(resp, "<script id=\"haxby-shader\" type=\"x-shader/x-vertex\">\n")
    write(resp, read(HT_DOCS * "/fitswebql/haxby-shader.frag"))
    write(resp, "</script>\n")

    # FITSWebQL main JavaScript + CSS
    write(resp, "<script src=\"fitswebqlse.js?", VERSION_STRING, "\"></script>\n")
    write(
        resp,
        "<link rel=\"stylesheet\" href=\"fitswebqlse.css?",
        VERSION_STRING,
        "\"/>\n",
    )

    # HTML content
    va_count = length(datasets)
    write(resp, "<title>FITSWEBQLSE</title></head><body>\n")
    write(resp, "<div id='votable' style='width: 0; height: 0;' data-va_count='$va_count' ")

    if va_count == 1
        datasetid = datasets[1]
        write(resp, "data-datasetId='$datasetid' ")
    else
        for i = 1:va_count
            datasetid = datasets[i]
            write(resp, "data-datasetId$i='$datasetid' ")
        end

        if is_composite && va_count <= 3
            write(resp, "data-composite='1' ")
        end
    end

    if !LOCAL_VERSION
        write(resp, "data-root-path='$root_path/' ")
    else
        write(resp, "data-root-path='/' ")
    end

    write(
        resp,
        " data-server-version='",
        VERSION_STRING,
        "' data-server-string='",
        SERVER_STRING,
    )

    if LOCAL_VERSION
        write(resp, "' data-server-mode='LOCAL")
    else
        write(resp, "' data-server-mode='SERVER")
    end

    has_fits_str = has_fits ? "1" : "0"
    write(resp, "' data-has-fits='$has_fits_str'></div>\n")

    if PRODUCTION
        write(resp, "<script>var WS_SOCKET = 'wss://';</script>\n")
    else
        write(resp, "<script>var WS_SOCKET = 'ws://';</script>\n")
    end

    write(resp, "<script>var WS_PORT = $WS_PORT;</script>\n")

    # the page entry point
    write(
        resp,
        "<script>",
        "const golden_ratio = 1.6180339887;",
        "var ALMAWS = null ;",
        "var wsVideo = null ;",
        "var wsConn = null;",
        "var firstTime = true;",
        "var has_image = false;",
        "var PROGRESS_VARIABLE = 0.0;",
        "var PROGRESS_INFO = '' ;",
        "var RESTFRQ = 0.0;",
        "var USER_SELFRQ = 0.0;",
        "var USER_DELTAV = 0.0;",
        "var ROOT_PATH = '/fitswebql/';",
        "var idleResize = -1;",
        "window.onresize = resizeMe;",
        "window.onbeforeunload = function() {",
        "    if (wsConn != null)",
        "    {",
        "        for (let i = 0; i < va_count; i++)",
        "            wsConn[i].close();",
        "    }",
        "",
        "          if (wsVideo != null)",
        "             wsVideo.close();",
        "    };",
        "mainRenderer(); </script>\n",
    )

    write(resp, "</body></html>")

    try
        return HTTP.Response(200, take!(resp))
    catch e
        return HTTP.Response(404, "Error: $e")
    end
end

const FITSWEBQL_ROUTER = HTTP.Router()

HTTP.@register(FITSWEBQL_ROUTER, "GET", "/", serveROOT)
HTTP.@register(FITSWEBQL_ROUTER, "GET", "/get_directory", serveDirectory)
HTTP.@register(FITSWEBQL_ROUTER, "GET", "/*/FITSWebQL.html", serveFITS)
HTTP.@register(FITSWEBQL_ROUTER, "POST", "/*/heartbeat/*", serveHeartBeat)
HTTP.@register(FITSWEBQL_ROUTER, "POST", "/*/progress/*", serveProgress)
HTTP.@register(
    FITSWEBQL_ROUTER,
    "GET",
    "/*/image_spectrum/",
    HTTP.StreamHandlerFunction(streamImageSpectrum)
)
HTTP.@register(
    FITSWEBQL_ROUTER,
    "GET",
    "/*/get_molecules/",
    HTTP.StreamHandlerFunction(streamMolecules)
)

# open a Splatalogue database
splat_db = SQLite.DB("splatalogue_v3.db")

println("WELCOME TO $SERVER_STRING (Supercomputer Edition)")
println("Point your browser to http://localhost:$HTTP_PORT")
println("Press CTRL+C to exit.")

# Sockets.localhost or Sockets.IPv4(0)
host = Sockets.IPv4(0)

function ws_coroutine(ws, datasetid, ids)
    @info "Started websocket coroutine for $datasetid" ws

    while isopen(ws)
        data, = readguarded(ws)
        s = String(data)

        if s == ""
            writeguarded(ws, "Exiting")
            break
        end

        # ping back heartbeat messages
        if occursin("[heartbeat]", s)
            @info "[ws] heartbeat"
            writeguarded(ws, s)
            continue
        end

        @info "Received: $s"
    end

    @info "$datasetid will now close " ws

end

function ws_gatekeeper(req, ws)
    orig = WebSockets.origin(req)
    target = HTTP.unescapeuri(req.target)

    @info "\nOrigin: $orig   Target: $target   subprotocol: $(subprotocol(req))"

    # check if there is a <datasetid> present in <target>
    pos = findlast("/", target)

    if !isnothing(pos)
        targets = SubString(target, pos[1] + 1)
        ids = split(targets, ";")
        datasetid = ids[1]

        @info "\n[ws] datasetid $datasetid"

        ws_coroutine(ws, datasetid, ids)
    else
        @info "[ws] Missing datasetid"
    end

end

ws_handle(req) = SERVER_STRING |> WebSockets.Response

const ws_server = WebSockets.ServerWS(ws_handle, ws_gatekeeper)

@async WebSockets.with_logger(WebSocketLogger()) do
    WebSockets.serve(ws_server, host, WS_PORT)
end

HTTP.serve(FITSWEBQL_ROUTER, host, UInt16(HTTP_PORT))
