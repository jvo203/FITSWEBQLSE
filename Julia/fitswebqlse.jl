import Base.Iterators: flatten
using ArgParse;
using CodecBzip2;
using CodecLz4;
using ConfParser;
using Distributed;
using CFITSIO;
using HTTP;
using JSON;
using LibPQ, Tables;
using LogRoller;
using Printf;
using Sockets;
using SQLite;
using WebSockets;
using x265_jll;
using ZfpCompression;

# needed by the x265 encoder
mutable struct x265_picture
    pts::Clong
    dts::Clong
    userData::Ptr{Cvoid}
    planeR::Ptr{Cuchar}
    planeG::Ptr{Cuchar}
    planeB::Ptr{Cuchar}
    strideR::Cint
    strideG::Cint
    strideB::Cint
    bitDepth::Cint
end

mutable struct x265_nal
    type::Cint
    sizeBytes::Cint
    # payload::Ptr{Cuchar}
    payload::Ptr{UInt8}
end

x265_nal(nal::Ptr, idx::Integer) = unsafe_load(Ptr{x265_nal}(nal), idx)
x265_picture(picture::Ptr) = unsafe_load(Ptr{x265_picture}(picture))

function x265_apiver()
    @static if Sys.isapple()
        parts = split(x265_jll.get_libx265_path(), ".")
        return parts[length(parts)-1]
    end

    @static if Sys.islinux()
        parts = split(readlink(x265_jll.get_libx265_path()), ".")
        return last(parts)
    end

    @static if Sys.iswindows()
        error("Not implemented: don't know how to access a shared lib on Windows")
    end
end

# the encoder_open function call uses the x265 API version
const encoder_open = "x265_encoder_open_" * x265_apiver()
# end of x265

isbuffered(c::Channel) = c.sz_max == 0 ? false : true

function check_channel_state(c::Channel)
    if !isopen(c)
        excp = c.excp
        excp !== nothing && throw(excp)
        throw(closed_exception())
    end
end

function replace!(c::Channel{T}, v) where {T}
    check_channel_state(c)
    v = convert(T, v)
    return isbuffered(c) ? replace_one(c, v) : put!(c, v)
end

function replace_one(c::Channel, v)
    lock(c)
    try
        while length(c.data) == c.sz_max
            check_channel_state(c)
            wait(c.cond_put)
        end

        if isempty(c.data)
            push!(c.data, v)
        else
            # replace the first element
            c.data[1] = v
        end

        # notify all, since some of the waiters may be on a "fetch" call.
        notify(c.cond_take, nothing, true, false)
    finally
        unlock(c)
    end
    return v
end

function add_machines(file)

    machines = readlines(file)

    for entry in machines
        local count, host

        v = split(entry, ":")

        host = v[1]

        if length(v) > 1
            count = parse(Int, v[2])
        else
            count = 1
        end

        println("host: $host, count: $count")

        if host == "localhost"
            println("adding $count local worker(s)")
            addprocs(count)
        else
            println("adding $count remote worker(s) on $host")
            addprocs([(host, count)])
        end
    end

    println("distributed workers: ", workers())
end

function parse_commandline()
    s = ArgParseSettings()

    @add_arg_table s begin
        "--config"
        help = "a configuration file."
        default = "config.ini"
        "--machines"
        help = "a machines file for setting up distributed workers in the reproducible (persistent) order. Entries: host[:count]. host == 'localhost' implies a local topology (WARNING: 'localhost' results in Julia errors at present, to be fixed)."
        default = ""
        "--port"
        help = "an HTTP listening port, defaults to 8080. WebSockets will use the next port (i.e. 8081). The port can also be specified in the .ini config file. Any config file will override this command-line argument."
        arg_type = Int
        default = 8080
    end

    return parse_args(s)
end

# parse command-line arguments (a config file, port numbers etc.)
parsed_args = parse_commandline()

# try adding distributed workers
try
    add_machines(parsed_args["machines"])
catch _
end

LOCAL_VERSION = true
PRODUCTION = false
TIMEOUT = 60 # [s]
LOGS = "LOGS"

DB_HOST = ""
DB_PORT = -1
DB_USER = ""
DB_PASSWORD = ""
DB_HOME = "/home"

FITS_HOME = ".cache"
@everywhere FITS_CACHE = ".cache"

# backup FITS download
const JVO_FITS_SERVER = "jvox.vo.nao.ac.jp"
const JVO_FITS_DB = "alma"

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

const WASM_VERSION = "21.09.XX.X"
const VERSION_STRING = "J/SV2022-01-XX.X-ALPHA"

const ZFP_HIGH_PRECISION = 16
const ZFP_MEDIUM_PRECISION = 11
const ZFP_LOW_PRECISION = 8

const SPECTRUM_HIGH_PRECISION = 24
const SPECTRUM_MEDIUM_PRECISION = 16

const FITS_CHUNK = 2880

# default config file
CONFIG_FILE = "config.ini"

const HT_DOCS = "htdocs"
HTTP_PORT = 8080
WS_PORT = HTTP_PORT + 1

@everywhere include("fits.jl")
include("kalman.jl")

# a global list of FITS objects
FITS_OBJECTS = Dict{String,FITSDataSet}()
FITS_LOCK = ReentrantLock()

function get_jvo_path(
    datasetid,
    host::String,
    port::Int64,
    user::String,
    password::String,
    db::String,
    table::String,
)
    local url::String, dataid::String, sql::String
    local conn

    url = "postgresql://" * user

    if password != ""
        url *= ":" * password
    end

    url *= "@" * host

    if port > 0
        url *= ":" * string(port)
    end

    url *= "/" * db

    try
        conn = LibPQ.Connection(url)
    catch _
        error("cannot connect to PostgreSQL")
    end

    # dataid: if db is alma append _00_00_00
    if db == "alma"
        dataid = datasetid * "_00_00_00"
    else
        dataid = datasetid
    end

    sql = "SELECT path FROM " * table * " WHERE data_id = '" * dataid * "';"
    println(sql)

    filepath = ""

    try
        result = execute(conn, sql)
        data = columntable(result)
        path = data[:path][1]

        pos = findfirst(".", table)

        if !isnothing(pos)
            filepath =
                DB_HOME *
                "/" *
                db *
                "/" *
                uppercase(SubString(table, 1:(pos[1]-1))) *
                "/" *
                path
        else
            if (db == "spcam") || (db == "moircs")
                filepath = DB_HOME * "/subaru/" * db * "/mosaic/" * path
            else
                filepath = DB_HOME * "/" * db * "/" * path
            end
        end

        println("filepath:", filepath)
    catch err
        println(err)
        close(conn)
        error("cannot read from PostgreSQL")
    end

    close(conn)

    # check if the file exists
    if isfile(filepath)
        return filepath
    else
        # otherwise throw an error    
        error("'$filepath' does not exist.")
    end
end

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

function gracefullyShutdown(request::HTTP.Request)
    @async exitFunc(true)

    return HTTP.Response(200, "Shutting down $(SERVER_STRING)")
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
    try
        timestamp = HTTP.URIs.splitpath(HTTP.unescapeuri(request.target))[3]
        return HTTP.Response(200, timestamp)
    catch e
        return HTTP.Response(404, "Error: $e")
    end
end

function serveProgress(request::HTTP.Request)
    global FITS_OBJECTS, FITS_LOCK

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

    update_timestamp(fits_object)

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
    global splat_db, FITS_OBJECTS, FITS_LOCK

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
            write(resp, json, ",")
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

function streamFITS(http::HTTP.Stream)
    global FITS_OBJECTS, FITS_LOCK

    request::HTTP.Request = http.message
    request.body = read(http)
    closeread(http)

    HTTP.setheader(http, "Cache-Control" => "no-cache")
    HTTP.setheader(http, "Cache-Control" => "no-store")
    HTTP.setheader(http, "Pragma" => "no-cache")

    params = HTTP.queryparams(HTTP.URI(request.target))
    println(params)

    datasetid = ""
    x1 = 0
    x2 = 0
    y1 = 0
    y2 = 0
    frame_start::Float64 = 0.0
    frame_end::Float64 = 0.0
    ref_freq::Float64 = 0.0

    try
        datasetid = params["datasetId"]
        x1 = round(Integer, parse(Int32, params["x1"]))
        x2 = round(Integer, parse(Int32, params["x2"]))
        y1 = round(Integer, parse(Int32, params["y1"]))
        y2 = round(Integer, parse(Int32, params["y2"]))
        frame_start = parse(Float64, params["frame_start"])
        frame_end = parse(Float64, params["frame_end"])
        ref_freq = parse(Float64, params["ref_freq"])
    catch e
        println(e)
        HTTP.setstatus(http, 404)
        startwrite(http)
        write(http, "Not Found")
        closewrite(http)
        return nothing
    end

    fits_object = get_dataset(datasetid, FITS_OBJECTS, FITS_LOCK)

    if fits_object.datasetid == "" || x1 <= 0 || x2 <= 0 || y1 <= 0 || y2 <= 0
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
        HTTP.setstatus(http, 404)
        startwrite(http)
        write(http, "Not Found")
        closewrite(http)
        return nothing
    end

    # sanity checks (X-Y-Z bounds)
    local first_frame, last_frame

    try
        first_frame, last_frame =
            get_spectrum_range(fits_object, frame_start, frame_end, ref_freq)
        frame_length = last_frame - first_frame + 1
        println(
            "[get_spectrum_range] :: [$first_frame, $last_frame] <$frame_length> ($(fits_object.depth))",
        )
    catch e
        println(e)
        HTTP.setstatus(http, 500)
        startwrite(http)
        write(http, "Internal Server Error")
        closewrite(http)
        return nothing
    end

    # impose ordering
    if x2 < x1
        x1, x2 = x2, x1
    end

    # impose ordering
    if y2 < y1
        y1, y2 = y2, y1
    end

    x1 = max(1, x1)
    y1 = max(1, y1)
    x2 = min(fits_object.width, x2)
    y2 = min(fits_object.height, y2)

    # cut-out dimensions
    dimx = abs(x2 - x1 + 1)
    dimy = abs(y2 - y1 + 1)

    println(
        "[streamFITS] :: x1=$x1, x2=$x2, y1=$y1, y2=$y2, first_frame=$first_frame, last_frame=$last_frame; dimx=$dimx, dimy=$dimy.",
    )

    try
        return streamFITS(http, fits_object, x1, x2, y1, y2, first_frame, last_frame)
    catch err
        println(err)

        HTTP.setstatus(http, 404)

        # undo any header changes done in 'streamFITS' below
        HTTP.setheader(http, "Content-Type" => "text/plain")
        HTTP.setheader(http, "Content-Transfer-Encoding" => "base64")
        HTTP.setheader(http, "Content-Disposition" => "inline")
        HTTP.setheader(http, "Accept-Ranges" => "none")

        startwrite(http)
        write(http, "$err")
        closewrite(http)
        return nothing
    end
end

function streamFITS(
    http::HTTP.Stream,
    fits::FITSDataSet,
    x1,
    x2,
    y1,
    y2,
    first_frame,
    last_frame,
)
    # any errors will be caught/handled higher up

    filter = "[" * string(x1) * ":" * string(x2) * "," * string(y1) * ":" * string(y2) * "," * string(first_frame) * ":" * string(last_frame) * "]"

    f = fits_open_file(fits.filepath * filter)

    num = fits_get_num_hdus(f)
    println("Number of HDUs in the file: ", num)

    naxes = 0
    width = 0
    height = 0
    depth = 1

    for hdu_id = 1:num
        hdu_type = fits_movabs_hdu(f, hdu_id)
        println(hdu_id, ") hdu_type = ", hdu_type)

        try
            naxes, = fits_read_keyword(f, "NAXIS")
            naxes = parse(Int, naxes)
        catch _
            continue
        end

        if naxes < 3
            # continue searching for the "right" HDU
            continue
        end

        # we have at least three dimensions
        try
            dims = fits_get_img_size(f)

            width = dims[1]
            height = dims[2]
            depth = dims[3]
        catch _
        end

        # stop the loop here
        break
    end

    if naxes < 3
        error("FITS data cube naxes should be >= 3.")
    end

    # get the header string
    headerStr = fits_hdr2str(f)
    padding = FITS_CHUNK - length(headerStr) % FITS_CHUNK

    println("FITS cut-out: $width x $height x $depth\theader length: ", length(headerStr), ", padding: $padding")

    if depth < 1
        error("FITS data cube depth should be >= 1.")
    end

    # HTTP.setheader(http, "Content-Type" => "application/octet-stream")
    disposition = "attachment; filename=" * replace(fits.datasetid, "/" => "_") * "-subregion.fits"
    HTTP.setheader(http, "Content-Type" => "application/force-download")
    HTTP.setheader(http, "Content-Disposition" => disposition)
    HTTP.setheader(http, "Content-Transfer-Encoding" => "binary")
    HTTP.setheader(http, "Accept-Ranges" => "bytes")

    HTTP.setstatus(http, 200)

    total_length = 0

    try
        startwrite(http)

        # first send the FITS header
        write(http, headerStr)
        total_length += length(headerStr)

        # pad a header with blanks (spaces)
        if padding > 0
            write(http, ' '^padding)
            total_length += padding
        end

        frame_pixels = Array{Float32}(undef, width, height)
        nelements = width * height

        # then the data
        for frame = 1:depth
            fpixel = [1, 1, frame, 1] # to cover all cases (NAXIS up to 4), use 4 values
            fits_read_pix(f, fpixel, nelements, frame_pixels)

            # convert to BIG-ENDIAN UInt8
            binary_pixels = collect(flatten(map(x -> reverse(reinterpret(UInt8, [x])), frame_pixels)))

            println("read FITS channel #$frame, pixels length: ", length(frame_pixels), ", type: ", typeof(frame_pixels))
            println("binary pixels length: ", length(binary_pixels))

            write(http, binary_pixels)
            total_length += length(binary_pixels)
        end

        # pad the output to a multiple of FITS_CHUNK
        padding = FITS_CHUNK - total_length % FITS_CHUNK

        if padding > 0
            write(http, '\0'^padding)
            total_length += padding
        end

        println("[streamFITS]::written $total_length bytes.")

    catch e
        println(e)
    finally
        close(f)
        closewrite(http)
    end

    return nothing
end

function streamImageSpectrum(http::HTTP.Stream)
    global FITS_OBJECTS, FITS_LOCK

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
    catch _
    end

    try
        fetch_data = parse(Bool, params["fetch_data"])
    catch _
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
        flux = tone_mapping.flux

        # pad flux with spaces so that the length is a multiple of 4
        # this is needed for an array alignment in JavaScript
        len = 4 * (length(flux) ÷ 4 + 1)
        flux = lpad(flux, len, " ")

        write(http, UInt32(length(flux)))
        write(http, flux)
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

function create_root_path(root_path)
    link = HT_DOCS * Base.Filesystem.path_separator * root_path

    if !isdir(link)
        target = "fitswebql"
        println("making a symbolic link $link --> $target")

        try
            symlink(target, link)
        catch err
            println(err)
        end

    end
end

function serveFITS(request::HTTP.Request)
    global FITS_OBJECTS, FITS_LOCK

    root_path = HTTP.URIs.splitpath(request.target)[1]

    params = HTTP.queryparams(HTTP.URI(request.target))

    println("root path: \"$root_path\"")
    # println(params)

    create_root_path(root_path)

    has_fits = true
    is_composite = false

    dir = ""
    datasets = []
    ext = ""
    pattern = ""

    # ALMA defaults
    db = "alma"
    table = "cube"

    try
        db = params["db"]
    catch e
    end

    try
        table = params["table"]
    catch e
    end

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

    if LOCAL_VERSION
        pattern = "filename"
    else
        pattern = "datasetId"
    end

    try
        push!(datasets, params[pattern])
    catch e
        # try multiple datasets (use recursion)
        get_dataset(pattern, params, datasets, 1)
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
                    finalizer(finale, fits_object)

                    if fits_object.depth > 1
                        fits_object.has_data[] = false

                        @async restoreImage(fits_object)
                        # @async restoreData(fits_object)
                    end

                    insert_dataset(fits_object, FITS_OBJECTS, FITS_LOCK)
                catch e
                    println("cannot restore $f from cache::$e")

                    fits_object = FITSDataSet(f)
                    finalizer(finale, fits_object)

                    # sane defaults
                    if occursin("hsc", db)
                        fits_object.flux = "ratio"
                    end

                    if occursin("fugin", table)
                        fits_object.flux = "logistic"
                    end

                    insert_dataset(fits_object, FITS_OBJECTS, FITS_LOCK)

                    local filepath = ""
                    local url = missing

                    if LOCAL_VERSION
                        # leave the slash as before, even in Windows
                        filepath = dir * "/" * f * "." * ext
                    else
                        # try the FITS home first                        
                        filepath = FITS_HOME * "/" * f * ".fits"

                        if !isfile(filepath)
                            # get the FITS path from PostgreSQL
                            try
                                filepath = get_jvo_path(
                                    f,
                                    DB_HOST,
                                    DB_PORT,
                                    DB_USER,
                                    DB_PASSWORD,
                                    db,
                                    table,
                                )
                            catch _
                                filepath = FITS_HOME * "/" * f * ".fits"

                                # the last throw of dice ...                            
                                url =
                                    "http://" *
                                    JVO_FITS_SERVER *
                                    ":8060/skynode/getDataForALMA.do?db=" *
                                    JVO_FITS_DB *
                                    "&table=cube&data_id=" *
                                    f *
                                    "_00_00_00"
                            end
                        end
                    end

                    @async loadFITS(fits_object, filepath, url) # @async or @spawn
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
    write(resp, "<script src=\"https://d3js.org/d3.v6.min.js\"></script>\n")
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

    # HTML5 FileSaver
    write(
        resp,
        "<script src=\"https://kit.fontawesome.com/8433b7dde2.js\" crossorigin=\"anonymous\"></script>\n",
    )
    write(resp, "<script src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/FileSaver.js\"></script>\n")

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

    # Bootstrap viewport
    write(
        resp,
        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, user-scalable=no, minimum-scale=1, maximum-scale=1\">\n",
    )

    # Bootstrap v4.1.3
    #=write(
        resp,
        "<link rel=\"stylesheet\" href=\"https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css\" integrity=\"sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO\" crossorigin=\"anonymous\">",
    )
    write(
        resp,
        "<script src=\"https://code.jquery.com/jquery-3.3.1.slim.min.js\" integrity=\"sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo\" crossorigin=\"anonymous\"></script>",
    )
    write(
        resp,
        "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js\" integrity=\"sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn689aCwoqbBJiSnjAK/l8WvCWPIPm49\" crossorigin=\"anonymous\"></script>",
    )
    write(
        resp,
        "<script src=\"https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js\" integrity=\"sha384-ChfqqxuZUCnJSK3+MXmPNIyE6ZbWh2IMqE241rYiqJxyMiZ6OW/JmZQ5stwEULTy\" crossorigin=\"anonymous\"></script>",
    )=#

    # Bootstrap v3.4.1
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
        write(resp, "data-root-path='/$root_path/' ")
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
HTTP.@register(FITSWEBQL_ROUTER, "GET", "/exit", gracefullyShutdown)
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
HTTP.@register(
    FITSWEBQL_ROUTER,
    "GET",
    "/*/get_fits/",
    HTTP.StreamHandlerFunction(streamFITS)
)

@everywhere function set_fits_cache(cache::String)
    global FITS_CACHE = cache
end

try
    global CONFIG_FILE = parsed_args["config"]
catch _
    println("A config file not supplied. Will try a default config.ini .")
end

# read the config file (if available)
try
    conf = ConfParse(CONFIG_FILE)
    parse_conf!(conf)

    try
        global HTTP_PORT = parse(Int64, retrieve(conf, "fitswebql", "port"))
        global WS_PORT = HTTP_PORT + 1
    catch _
        # cannot find the port, try the command-line arguments
        try
            global HTTP_PORT = parsed_args["port"]
            global WS_PORT = HTTP_PORT + 1
        catch _
        end
    end

    # [fitswebql]

    try
        global LOCAL_VERSION = parse(Bool, retrieve(conf, "fitswebql", "local"))
    catch _
    end

    try
        global PRODUCTION = parse(Bool, retrieve(conf, "fitswebql", "production"))
    catch _
    end

    try
        global TIMEOUT = parse(Int64, retrieve(conf, "fitswebql", "timeout"))
    catch _
    end

    try
        global FITS_HOME = retrieve(conf, "fitswebql", "home")
    catch _
    end

    try
        global LOGS = retrieve(conf, "fitswebql", "logs")
    catch _
    end

    try
        global FITS_CACHE = retrieve(conf, "fitswebql", "cache")

        # synchronise <FITS_CACHE> across the workers
        ras = [@spawnat w set_fits_cache(FITS_CACHE) for w in workers()]

        @time wait.(ras)
    catch _
    end

    # [postgresql]

    try
        global DB_HOST = retrieve(conf, "postgresql", "host")
    catch _
    end

    try
        global DB_PORT = parse(Int64, retrieve(conf, "postgresql", "port"))
    catch _
    end

    try
        global DB_USER = retrieve(conf, "postgresql", "user")
    catch _
    end

    try
        global DB_PASSWORD = retrieve(conf, "postgresql", "password")
    catch _
    end

    try
        global DB_HOME = retrieve(conf, "postgresql", "home")
    catch _
    end
catch _
    println("Cannot parse the config file $CONFIG_FILE .")
end

# open a Splatalogue database
const splat_db = SQLite.DB("splatalogue_v3.db")

println("$SERVER_STRING (Supercomputer Edition)")
println("DATASET TIMEOUT: $(TIMEOUT)s")
println("Point your browser to http://localhost:$HTTP_PORT")
println(
    "Press CTRL+C or send SIGINT to exit. Alternatively point your browser to http://localhost:$HTTP_PORT/exit",
)

# Sockets.localhost or Sockets.IPv4(0)
host = Sockets.IPv4(0)

function ws_coroutine(ws, ids)
    global FITS_OBJECTS, FITS_LOCK

    local scale::Float32, flux::String, fps::Integer, bitrate::Integer
    local last_video_seq::Integer, last_frame_idx::Integer
    local image_width::Integer, image_height::Integer, bDownsize::Bool

    # HEVC
    local param, encoder, picture, planeB, luma, alpha
    local filter::KalmanFilter, ts
    local tone::Union{Missing,VideoToneMapping}

    local video_mtx = ReentrantLock()

    param = C_NULL
    encoder = C_NULL
    picture = C_NULL
    tone = missing

    datasetid = String(ids[1])

    begin
        fits_object = get_dataset(datasetid, FITS_OBJECTS, FITS_LOCK)

        if fits_object.datasetid == "" || has_error(fits_object)
            @error "$datasetid not found, closing a websocket coroutine."
            writeguarded(ws, "[close]")
            return
        end
    end

    @info "Started a websocket coroutine for $datasetid" ws

    # an outgoing queue for messages to be sent
    outgoing = RemoteChannel(() -> Channel{Any}(32))
    sent_task = @async while true
        try
            msg = take!(outgoing)

            if typeof(msg) == IOBuffer
                msg = take!(msg)
            end

            if !writeguarded(ws, msg)
                break
            end
        catch e
            if isa(e, InvalidStateException) && e.state == :closed
                println("sent task completed")
                break
            else
                println(e)
            end
        end
    end

    viewport_requests = Channel{Dict{String,Any}}(32)

    realtime = @async while true
        try
            req = take!(viewport_requests)
            # println(datasetid, "::", req)

            fits_object = get_dataset(datasetid, FITS_OBJECTS, FITS_LOCK)

            if fits_object.datasetid == "" || has_error(fits_object)
                error("$datasetid not found.")
            end

            if !has_data(fits_object)
                error("$datasetid: no data found.")
            end

            elapsed =
                @elapsed viewport, spectrum = getViewportSpectrum(fits_object, req)
            elapsed *= 1000.0 # [ms]

            Threads.@spawn begin
                if viewport != Nothing
                    # send a viewport
                    println("[getViewportSpectrum] elapsed: $elapsed [ms]")

                    resp = IOBuffer()

                    # the header
                    write(resp, Float32(req["timestamp"]))
                    write(resp, Int32(req["seq_id"]))
                    write(resp, Int32(1)) # 0 - spectrum, 1 - viewport
                    write(resp, Float32(elapsed))

                    # the body
                    write(resp, take!(viewport))

                    put!(outgoing, resp)
                    #if !writeguarded(ws, take!(resp))
                    #    break
                    #end
                end

                if spectrum != Nothing
                    # send a spectrum
                    println("[getViewportSpectrum] elapsed: $elapsed [ms]")

                    resp = IOBuffer()

                    # the header
                    write(resp, Float32(req["timestamp"]))
                    write(resp, Int32(req["seq_id"]))
                    write(resp, Int32(0)) # 0 - spectrum, 1 - viewport
                    write(resp, Float32(elapsed))

                    # the body
                    write(resp, take!(spectrum))

                    put!(outgoing, resp)
                    #if !writeguarded(ws, take!(resp))
                    #    break
                    #end
                end
            end

            update_timestamp(fits_object)
        catch e
            if isa(e, InvalidStateException) && e.state == :closed
                println("real-time viewport task completed")
                break
            else
                println(e)
            end
        end
    end

    video_requests = Channel{Dict{String,Any}}(32)

    video = @async while true
        try
            req = take!(video_requests)

            fits_object = get_dataset(datasetid, FITS_OBJECTS, FITS_LOCK)

            if fits_object.datasetid == "" || has_error(fits_object)
                error("$datasetid not found.")
            end

            if !has_data(fits_object)
                error("$datasetid: no data found.")
            end

            if !has_video(fits_object)
                continue
            end

            keyframe = req["key"]

            # obtain a cube channel
            frame = Float64(req["frame"])

            ref_freq = 0.0 # by default ref_freq is missing
            try
                ref_freq = Float64(req["ref_freq"])
            catch e
            end

            try
                # lock(video_mtx)

                deltat = Float64(Dates.value(now() - ts)) # [ms]
                ts = now()

                # Kalman Filter tracking/prediction
                update(filter, frame, deltat)
                frame2 = predict(filter, frame, deltat)

                frame_idx, = get_spectrum_range(fits_object, frame, frame, ref_freq)
                frame_idx2, = get_spectrum_range(fits_object, frame2, frame2, ref_freq)

                println(
                    "deltat: $deltat [ms]; frame_idx: $frame_idx, predicted: $frame_idx2",
                )

                # use a predicted frame for non-keyframes
                if !keyframe
                    # disable Kalman Filter for now, deltat needs to be reduced
                    # frame_idx = frame_idx2
                end

                if !keyframe && (last_frame_idx == frame_idx)
                    println("skipping a repeat video frame")
                    continue
                else
                    last_frame_idx = frame_idx
                    println("video frame: $frame_idx; keyframe: $keyframe")
                end

                if ismissing(tone)
                    println("initialising video tone mapping")

                    if has_video(fits_object)
                        # calculate white, black, sensitivity from the all-data histogram
                        u = 7.5f0
                        _median = fits_object.data_median
                        _black = max(fits_object.dmin, (fits_object.data_median - u * fits_object.data_mad₋))
                        _white = min(fits_object.dmax, (fits_object.data_median + u * fits_object.data_mad₊))
                        _sensitivity = 1.0f0 / (_white - _black)
                        _slope = 1.0f0 / (_white - _black)
                        _dmin = fits_object.dmin
                        _dmax = fits_object.dmax

                        # video tone mapping
                        tone =
                            VideoToneMapping(flux, _dmin, _dmax, _median, _sensitivity, _slope, _white, _black)                        
                    else
                        error("video tone mapping has not been initialised.")
                    end
                else
                    println("video tone mapping:", tone)
                end

                # by this point the VideoToneMapping variable is valid                

                Threads.@spawn begin
                    # interpolate variable values into a thread
                    t_frame_idx = $frame_idx
                    t_tone = $tone
                    t_image_width = $image_width
                    t_image_height = $image_height
                    t_bDownsize = $bDownsize
                    t_keyframe = $keyframe

                    try
                        # get a video frame                        
                        elapsed = @elapsed luma, alpha = getVideoFrame(
                            fits_object,
                            t_frame_idx,
                            t_tone,
                            t_image_width,
                            t_image_height,
                            t_bDownsize,
                            t_keyframe,
                        )
                        elapsed *= 1000.0 # [ms]

                        #=println(
                            typeof(luma),
                            ";",
                            typeof(alpha),
                            ";",
                            size(luma),
                            ";",
                            size(alpha),
                            "; bDownsize:",
                            bDownsize,
                            "; elapsed: $elapsed [ms]",
                        )=#

                        lock(video_mtx)

                        if picture != C_NULL
                            # update the x265_picture structure                
                            picture_jl = x265_picture(picture)

                            picture_jl.planeR = pointer(luma)
                            picture_jl.strideR = strides(luma)[2]

                            picture_jl.planeG = pointer(alpha)
                            picture_jl.strideG = strides(alpha)[2]

                            # sync the Julia structure back to C
                            unsafe_store!(Ptr{x265_picture}(picture), picture_jl)

                            if encoder != C_NULL
                                # HEVC-encode the luminance and alpha channels
                                iNal = Ref{Cint}(0)
                                pNals = Ref{Ptr{Cvoid}}(C_NULL)

                                # iNal_jll value: iNal[] 

                                # an array of pointers
                                # local pNals_jll::Ptr{Ptr{Cvoid}} = pNals[]                        

                                # int x265_encoder_encode(x265_encoder *encoder, x265_nal **pp_nal, uint32_t *pi_nal, x265_picture *pic_in, x265_picture *pic_out);
                                # int ret = x265_encoder_encode(encoder, &pNals, &iNal, picture, NULL);

                                encoding = @elapsed stat = ccall(
                                    (:x265_encoder_encode, libx265),
                                    Cint,
                                    (
                                        Ptr{Cvoid},
                                        Ref{Ptr{Cvoid}},
                                        Ref{Cint},
                                        Ptr{Cvoid},
                                        Ptr{Cvoid},
                                    ),
                                    encoder,
                                    pNals,
                                    iNal,
                                    picture,
                                    C_NULL,
                                )
                                encoding *= 1000.0 # [ms]

                                println(
                                    "x265_encoder_encode::stat = $stat, iNal = ",
                                    iNal[],
                                    ", pNals($pNals): ",
                                    pNals[],
                                    "; elapsed: $encoding [ms]",
                                )

                                for idx = 1:iNal[]
                                    nal = x265_nal(pNals[], idx)
                                    # println("NAL #$idx: $nal")

                                    resp = IOBuffer()

                                    # the header
                                    write(resp, Float32(req["timestamp"]))
                                    write(resp, Int32(req["seq_id"]))
                                    write(resp, Int32(5)) # 5 - video frame
                                    write(resp, Float32(elapsed + encoding))

                                    # the body
                                    payload = Vector{UInt8}(undef, nal.sizeBytes)
                                    unsafe_copyto!(
                                        pointer(payload),
                                        nal.payload,
                                        nal.sizeBytes,
                                    )
                                    write(resp, payload)

                                    put!(outgoing, resp)
                                end
                            end
                        end

                    catch e
                        println("Inner error: ", e)
                    finally
                        if islocked(video_mtx)
                            unlock(video_mtx)
                        end
                    end

                    # do not use a finalizer in a Base.thread ...
                    #if islocked(video_mtx)
                    #    println("unlocking a locked video_mtx")
                    #    unlock(video_mtx)
                    #end
                end
            catch e
                println("Outer error: ", e)
            end

            update_timestamp(fits_object)
        catch e
            if isa(e, InvalidStateException) && e.state == :closed
                println("real-time video task completed")
                break
            else
                println(e)
            end
        end
    end

    while isopen(ws)
        data, = readguarded(ws)
        s = String(data)

        if s == ""
            break
        end

        # ping back heartbeat messages
        if occursin("[heartbeat]", s)
            # @info "[ws] heartbeat"

            fits_object = get_dataset(datasetid, FITS_OBJECTS, FITS_LOCK)

            if fits_object.datasetid == "" || has_error(fits_object)
                @error "$datasetid not found, closing a websocket coroutine."
                writeguarded(ws, "[close]")
                break
            end

            if has_error(fits_object)
                @error "$datasetid: an error detected, closing a websocket coroutine."
                writeguarded(ws, "[close]")
                break
            end

            update_timestamp(fits_object)

            try
                put!(outgoing, s)
            catch e
                println(e)
            finally
                continue
            end

            #=
            if writeguarded(ws, s)
                continue
            else
                break
            end
            =#
        end

        # @info "Received: $s"

        # convert the message into JSON
        try
            msg = JSON.parse(s)
            @info msg

            if msg["type"] == "image"
                # sub-region selection

                fits_object = get_dataset(datasetid, FITS_OBJECTS, FITS_LOCK)

                if fits_object.datasetid == "" || has_error(fits_object)
                    error("$datasetid not found.")
                end

                if !has_data(fits_object)
                    error("$datasetid: no data found.")
                end

                @time image, spectrum, video_tone_mapping = getImageSpectrum(fits_object, msg)

                if video_tone_mapping != Nothing
                    tone = video_tone_mapping
                end

                if image != Nothing
                    resp = IOBuffer()

                    # the header                    
                    write(resp, Float32(msg["timestamp"]))
                    write(resp, Int32(0))
                    write(resp, Int32(2)) # 2 - image + histogram

                    # the body
                    write(resp, take!(image))

                    put!(outgoing, resp)
                end

                if spectrum != Nothing
                    resp = IOBuffer()

                    # the header
                    write(resp, Float32(0.0))
                    write(resp, Int32(0))
                    write(resp, Int32(3)) # 3 - spectrum refresh

                    # the body
                    write(resp, take!(spectrum))

                    put!(outgoing, resp)
                end
            end

            if msg["type"] == "spectrum"
                # CSV spectrum export

                fits_object = get_dataset(datasetid, FITS_OBJECTS, FITS_LOCK)

                if fits_object.datasetid == "" || has_error(fits_object)
                    error("$datasetid not found.")
                end

                if !has_data(fits_object)
                    error("$datasetid: no data found.")
                end

                @time csv = take!(getSpectrum(fits_object, msg))

                # LZ4-compress csv
                compressed_csv = lz4_hc_compress(csv)
                println(
                    "csv length: ",
                    length(csv),
                    ", compressed csv length: ",
                    length(compressed_csv),
                )

                # send a WebSockets response
                resp = IOBuffer()

                # the header                    
                write(resp, Float32(msg["timestamp"]))
                write(resp, Int32(0))
                write(resp, Int32(6)) # 6 - spectrum csv

                # the body
                write(resp, UInt32(length(csv))) # pass the original length
                write(resp, compressed_csv)

                put!(outgoing, resp)
            end

            if msg["type"] == "realtime_image_spectrum"
                # replace!(viewport_requests, msg)
                push!(viewport_requests, msg) # there is too much lag
            end

            # init_video
            if msg["type"] == "init_video"
                width = round(Integer, msg["width"])
                height = round(Integer, msg["height"])
                flux = msg["flux"]
                last_video_seq = msg["seq_id"]
                last_frame_idx = -1
                bitrate = msg["bitrate"]
                fps = round(Integer, msg["fps"])

                fits_object = get_dataset(datasetid, FITS_OBJECTS, FITS_LOCK)

                if fits_object.datasetid == "" || has_error(fits_object)
                    error("$datasetid not found.")
                end

                if !has_data(fits_object)
                    error("$datasetid: no data found.")
                end

                # obtain an initial cube channel
                frame = Float64(msg["frame"])

                # calculate scale, downsize when applicable
                inner_width, inner_height = get_inner_dimensions(fits_object)

                try
                    scale = get_image_scale(width, height, inner_width, inner_height)
                catch e
                    println(e)
                    scale = 1.0
                end

                if scale < 1.0
                    image_width = round(Integer, scale * fits_object.width)
                    image_height = round(Integer, scale * fits_object.height)
                    bDownsize = true
                else
                    image_width = fits_object.width
                    image_height = fits_object.height
                    bDownsize = false
                end

                println(
                    "scale = $scale, image: $image_width x $image_height, bDownsize: $bDownsize",
                )

                dict = Dict(
                    "type" => "init_video",
                    "width" => image_width,
                    "height" => image_height,
                    "padded_width" => image_width,
                    "padded_height" => image_height,
                )

                resp = JSON.json(dict)

                put!(outgoing, resp)

                if true
                    #if writeguarded(ws, resp)
                    try
                        lock(video_mtx)

                        # initialize the Kalman Filter
                        filter = KalmanFilter(frame, true)
                        ts = now()

                        # upon success init the HEVC encoder
                        param = ccall((:x265_param_alloc, libx265), Ptr{Cvoid}, ())

                        if param == C_NULL
                            @error "NULL x265_param"
                            continue
                        end

                        # set default parameters
                        ccall(
                            (:x265_param_default_preset, libx265),
                            Cvoid,
                            (Ptr{Cvoid}, Cstring, Cstring),
                            param,
                            "superfast",
                            "zerolatency",
                        )

                        # extra parameters

                        # FPS
                        stat = ccall(
                            (:x265_param_parse, libx265),
                            Cint,
                            (Ptr{Cvoid}, Cstring, Cstring),
                            param,
                            "fps",
                            #string(fps),
                            "30",
                        )

                        if stat != 0
                            @error "Cannot set FPS"
                        end

                        # bRepeatHeaders = 1
                        stat = ccall(
                            (:x265_param_parse, libx265),
                            Cint,
                            (Ptr{Cvoid}, Cstring, Ptr{Cvoid}),
                            param,
                            "repeat-headers",
                            C_NULL,
                        )

                        if stat != 0
                            @error "Cannot set repeat-headers"
                        end

                        # internalCsp = X265_CSP_I444
                        stat = ccall(
                            (:x265_param_parse, libx265),
                            Cint,
                            (Ptr{Cvoid}, Cstring, Cstring),
                            param,
                            "input-csp",
                            "i444",
                        )

                        if stat != 0
                            @error "Cannot set input-csp"
                        end

                        # set video resolution
                        res = string(image_width) * "x" * string(image_height)
                        stat = ccall(
                            (:x265_param_parse, libx265),
                            Cint,
                            (Ptr{Cvoid}, Cstring, Cstring),
                            param,
                            "input-res",
                            res,
                        )

                        if stat != 0
                            @error "Cannot set input-res"
                        end

                        # set constant quality rate
                        crf = Integer(28)
                        stat = ccall(
                            (:x265_param_parse, libx265),
                            Cint,
                            (Ptr{Cvoid}, Cstring, Cstring),
                            param,
                            "crf",
                            string(crf),
                        )

                        if stat != 0
                            @error "Cannot set CRF"
                        end

                        # x265 encoder
                        encoder =
                            ccall((encoder_open, libx265), Ptr{Cvoid}, (Ptr{Cvoid},), param)
                        println("typeof(encoder): ", typeof(encoder), "; value: $encoder")

                        if encoder == C_NULL
                            @error "NULL x265_encoder"
                            continue
                        end

                        # x265 picture
                        picture = ccall((:x265_picture_alloc, libx265), Ptr{Cvoid}, ())
                        println("typeof(picture): ", typeof(picture), "; value: $picture")

                        if picture == C_NULL
                            @error "NULL x265_picture"
                            continue
                        end

                        ccall(
                            (:x265_picture_init, libx265),
                            Cvoid,
                            (Ptr{Cvoid}, Ptr{Cvoid}),
                            param,
                            picture,
                        )

                        planeB = fill(UInt8(128), image_width, image_height)

                        picture_jl = x265_picture(picture)
                        picture_jl.strideR = 0
                        picture_jl.strideG = 0
                        picture_jl.planeB = pointer(planeB)
                        picture_jl.strideB = strides(planeB)[2]
                        picture_jl.bitDepth = 8

                        # sync the Julia structure back to C
                        unsafe_store!(Ptr{x265_picture}(picture), picture_jl)
                    catch e
                        println(e)
                    finally
                        unlock(video_mtx)
                    end

                    continue
                else
                    break
                end
            end

            # end_video
            if msg["type"] == "end_video"
                # clean up x265
                try
                    lock(video_mtx)

                    if encoder ≠ C_NULL
                        # release the x265 encoder
                        ccall((:x265_encoder_close, libx265), Cvoid, (Ptr{Cvoid},), encoder)
                        encoder = C_NULL

                        @info "cleaned up the x265 encoder"
                    end

                    if picture ≠ C_NULL
                        # release the x265 picture structure
                        ccall((:x265_picture_free, libx265), Cvoid, (Ptr{Cvoid},), picture)
                        picture = C_NULL

                        @info "cleaned up the x265 picture"
                    end

                    if param ≠ C_NULL
                        # release the x265 parameters structure
                        ccall((:x265_param_free, libx265), Cvoid, (Ptr{Cvoid},), param)
                        param = C_NULL

                        @info "cleaned up x265 parameters"
                    end
                catch e
                    println(e)
                finally
                    unlock(video_mtx)
                end

                continue
            end

            # realtime streaming video frame requests
            if msg["type"] == "video"
                # replace!(video_requests, msg)
                push!(video_requests, msg)
            end
        catch e
            println("ws_coroutine::$e")
            # @error "ws_coroutine::" exception = (e, catch_backtrace())
        end
    end

    close(outgoing)
    wait(sent_task)

    close(viewport_requests)
    close(video_requests)

    wait(realtime)
    wait(video)

    # clean up x265
    if encoder ≠ C_NULL
        # release the x265 encoder
        ccall((:x265_encoder_close, libx265), Cvoid, (Ptr{Cvoid},), encoder)

        @info "cleaned up the x265 encoder"
    end

    if picture ≠ C_NULL
        # release the x265 picture structure
        ccall((:x265_picture_free, libx265), Cvoid, (Ptr{Cvoid},), picture)

        @info "cleaned up the x265 picture"
    end

    if param ≠ C_NULL
        # release the x265 parameters structure
        ccall((:x265_param_free, libx265), Cvoid, (Ptr{Cvoid},), param)

        @info "cleaned up x265 parameters"
    end

    @info "$datasetid will now close " ws

end

function ws_gatekeeper(req, ws)
    orig = WebSockets.origin(req)
    target = HTTP.unescapeuri(req.target)

    @info "\nOrigin: $orig   Target: $target   subprotocol: $(subprotocol(req))"

    # check if there is a '<sessionid>/<datasetid>' present in <target>
    pos = findlast("/", target)

    if !isnothing(pos)
        sessionid = SubString(target, pos[1] + 1)
        @info "\n[ws] sessionid $sessionid"

        target = SubString(target, 1, pos[1] - 1)
        pos = findlast("/", target)

        if !isnothing(pos)

            targets = SubString(target, pos[1] + 1)
            ids = split(targets, ";")

            @info "\n[ws] datasetid $(ids[1])"

            ws_coroutine(ws, ids)
        else
            @info "[ws] Missing datasetid"
        end
    else
        @info "[ws] Missing sessionid"
    end

end

ws_handle(req) = SERVER_STRING |> WebSockets.Response

const ws_server = WebSockets.ServerWS(ws_handle, ws_gatekeeper)

function remove_symlinks()
    # scan HT_DOCS for any symlinks and remove them

    foreach(readdir(HT_DOCS, join = true)) do f

        # is it a symbolic link ?
        if islink(f)
            # if so remove it
            try
                println("removing a symbolic link $f")
                rm(f)
            catch err
                println(err)
            end
        end

    end
end

function exitFunc(exception = false)
    global ws_server

    remove_symlinks()

    try
        println("WebSocket Server .out channel: ", string(take!(ws_server.out)))
        close(ws_server)
    catch e
        println(e)
    end

    # interrupt all distributed workers
    # interrupt()

    @info "FITSWEBQLSE shutdown."
    exit()

    # unreachable code
    # throwing an exception here seems to hang Julia
    if exception
        throw(InterruptException())
    end

    for w in workers()
        try
            println("removing a distributed worker #$w")
            rmprocs(w, waitfor = 10)
        catch e
            println(e)
        end
    end

end

if Base.isinteractive()
    Base.exit_on_sigint(false)
else
    Base.atexit(exitFunc)
end

# if !Base.isinteractive()
#    Base.atexit(exitFunc)
#    Base.exit_on_sigint(true)
#end

ws_log_file =
    RollingFileWriter(LOGS * Base.Filesystem.path_separator * "ws.log", 256 * 1024, 10)

@async WebSockets.with_logger(WebSocketLogger(ws_log_file)) do
    WebSockets.serve(ws_server, host, WS_PORT)
end

if TIMEOUT > 0
    # a garbage collection loop (dataset timeout)
    @async while true
        sleep(10)

        # purge datasets
        for (datasetid, fits) in FITS_OBJECTS
            elapsed = datetime2unix(now()) - fits.last_accessed[]

            if elapsed > TIMEOUT
                println("Purging a dataset '$datasetid' ...")

                lock(FITS_LOCK)

                try
                    # delete!(FITS_OBJECTS, datasetid)
                    fits_object = pop!(FITS_OBJECTS, datasetid)
                    println("Removed '$(fits_object.datasetid)' .")
                catch e
                    println("Failed to remove a dataset: $e")
                finally
                    unlock(FITS_LOCK)
                end

                # do not wait, trigger garbage collection *NOW*
                GC.gc()

                # yet another run to trigger finalizers ...
                GC.gc()
            end
        end
    end
end

# HTTP.serve(FITSWEBQL_ROUTER, host, UInt16(HTTP_PORT), on_shutdown = exitFunc)

try
    HTTP.serve(FITSWEBQL_ROUTER, host, UInt16(HTTP_PORT))
catch err
    warn(err)
    typeof(err) == InterruptException && rethrow(err)
finally
    exitFunc()
end