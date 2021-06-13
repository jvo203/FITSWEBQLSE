using Distributed;
using HTTP;
using JSON;
using Sockets;

const HT_DOCS = "htdocs"
const HTTP_PORT = 8080
const WS_PORT = HTTP_PORT + 1

println(default_worker_pool())

function serveFile(path::String)

    # TO-DO:
    # add mime types

    headers = ["Cache-Control" => "public, max-age=86400"]

    try
        return isfile(path) ? HTTP.Response(200, headers; body = read(path)) :
               HTTP.Response(404, "$path Not Found.")
    catch e
        return HTTP.Response(404, "Error: $e")
    end
end

function serveDirectory(request::HTTP.Request)
    @show request.target

    headers = ["Content-Type" => "application/json"]

    params = HTTP.queryparams(HTTP.URI(request.target))

    dir = ""

    try
        dir = params["dir"]
    catch e
        # if they keyword is not found fall back on a home directory
        dir = ENV["HOME"]
    end

    println("Scanning $dir ...")

    resp = chop(JSON.json(Dict("location" => dir)), tail = 1) * ", \"contents\":["

    elements = false

    foreach(readdir(dir)) do f
        filename = lowercase(f)

        if !startswith(filename, ".")

            path = dir * "/" * f

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
    if occursin("../", request.target)
        return HTTP.Response(404, "Not Found")
    end

    path = HT_DOCS * HTTP.unescapeuri(request.target)

    if request.target == "/"
        path *= "local_j.html"
    end

    return serveFile(path)
end

function serveFITS(request::HTTP.Request)
    @show request.target

    root_path = HTTP.URIs.splitpath(request.target)[1]

    params = HTTP.queryparams(HTTP.URI(request.target))

    println("root path: \"$root_path\"")
    println(params)

    try
        return HTTP.Response(200, "FITSWEBQLSE")
    catch e
        return HTTP.Response(404, "Error: $e")
    end
end

const FITSWEBQL_ROUTER = HTTP.Router()

HTTP.@register(FITSWEBQL_ROUTER, "GET", "/", serveROOT)
HTTP.@register(FITSWEBQL_ROUTER, "GET", "/get_directory", serveDirectory)
HTTP.@register(FITSWEBQL_ROUTER, "GET", "/*/FITSWebQL.html", serveFITS)

println("FITSWEBQL SE (Supercomputer Edition)")
println("Press CTRL+C to exit.")

# Sockets.localhost or Sockets.IPv4(0)
HTTP.serve(FITSWEBQL_ROUTER, Sockets.IPv4(0), UInt16(HTTP_PORT))
