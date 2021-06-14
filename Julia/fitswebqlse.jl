using Distributed;
using HTTP;
using JSON;
using Sockets;
using WebSockets;

const LOCAL_VERSION = true
const PRODUCTION = false

const VERSION_MAJOR = 5
const VERSION_MINOR = 0
const VERSION_SUB = 0
const SERVER_STRING = "FITSWEBQLSE v" * string(VERSION_MAJOR) * "." * string(VERSION_MINOR) * "." * string(VERSION_SUB)

const WASM_VERSION = "21.04.XX.X"
const VERSION_STRING = "SV2021-06-XX.X-ALPHA"

const HT_DOCS = "htdocs"
const HTTP_PORT = 8080
const WS_PORT = HTTP_PORT + 1

@everywhere include("fits.jl")

println(default_worker_pool())

function serveFile(path::String)
    # strip out a question mark (if there is any)
    pos = findlast("?", path)

    if !isnothing(pos)
        path = SubString(path, 1:(pos[1] - 1))
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
        return isfile(path) ? HTTP.Response(200, headers; body=read(path)) :
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
    catch e
        # if they keyword is not found fall back on a home directory
        dir = ENV["HOME"]
    end

    println("Scanning $dir ...")

    resp = chop(JSON.json(Dict("location" => dir)), tail=1) * ", \"contents\":["
        
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
        resp = chop(resp, tail=1) * "]}"
    else
        resp *= "]}"
    end

    try
        return HTTP.Response(200, headers; body=resp)
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

# a recursive function (very elegant)
function get_dataset(prefix::String, params, datasets, idx::Integer)
    try
        push!(datasets, params[prefix * string(idx)])
        get_dataset(prefix, params, datasets, idx + 1)
    catch e
        # no more datasets, stop recursion
        return
    end
end

function serveFITS(request::HTTP.Request)
    root_path = HTTP.URIs.splitpath(request.target)[1]
    
    params = HTTP.queryparams(HTTP.URI(request.target))

    println("root path: \"$root_path\"")
    println(params)

    has_fits = false
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

    if !has_fits
        foreach(datasets) do f
            filepath = dir * "/" * f * "." * ext
            @async loadFITS(filepath)
        end
    end
    
    resp = IOBuffer();
    
    write(resp, "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\">\n")
    write(resp, "<link href=\"https://fonts.googleapis.com/css?family=Inconsolata\" rel=\"stylesheet\"/>\n")
    write(resp, "<link href=\"https://fonts.googleapis.com/css?family=Material+Icons\" rel=\"stylesheet\"/>\n")
    write(resp, "<script src=\"https://d3js.org/d3.v5.min.js\"></script>\n")
    # write(resp, "<script src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/reconnecting-websocket.min.js\"></script>\n")
    write(resp, "<script src=\"//cdnjs.cloudflare.com/ajax/libs/numeral.js/2.0.6/numeral.min.js\"></script>\n")
    write(resp, "<script src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/ra_dec_conversion.min.js\"></script>\n")
    write(resp, "<script src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/sylvester.min.js\"></script>\n")
    write(resp, "<script src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/shortcut.min.js\"></script>\n")
    write(resp, "<script src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/colourmaps.min.js\"></script>\n")
    write(resp, "<script src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/lz4.min.js\"></script>\n")
    write(resp, "<script src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/marchingsquares-isocontours.min.js\"></script>\n")
    write(resp, "<script src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/marchingsquares-isobands.min.js\"></script>\n")

    # WebAssembly
    write(resp, "<script src=\"client.", WASM_VERSION, ".js\"></script>\n")
    write(resp, "<script>\n", "Module.ready\n", "\t.then(status => console.log(status))\n", "\t.catch(e => console.error(e));\n", "</script>\n")

    # Bootstrap v3.4.1
    write(resp, "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, user-scalable=no, minimum-scale=1, maximum-scale=1\">\n")
    write(resp, "<link rel=\"stylesheet\" href=\"https://stackpath.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css\" integrity=\"sha384-HSMxcRTRxnN+Bdg0JdbxYKrThecOKuH5zCYotlSAcp1+c8xmyTe9GYg1l9a69psu\" crossorigin=\"anonymous\">")
    write(resp, "<script src=\"https://code.jquery.com/jquery-1.12.4.min.js\" integrity=\"sha384-nvAa0+6Qg9clwYCGGPpDQLVpLNn0fRaROjHqs13t4Ggj3Ez50XnGQqc/r8MhnRDZ\" crossorigin=\"anonymous\"></script>")
    write(resp, "<script src=\"https://stackpath.bootstrapcdn.com/bootstrap/3.4.1/js/bootstrap.min.js\" integrity=\"sha384-aJ21OjlMXNL5UyIl/XNwTMqvzeRMZH2w8c5cRVpzpU8Y5bApTppSuUkhZXN0VxHd\" crossorigin=\"anonymous\"></script>")

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
    write(resp, "<link rel=\"stylesheet\" href=\"fitswebqlse.css?", VERSION_STRING, "\"/>\n")

    # HTML content
    va_count = length(datasets)
    write(resp, "<title>FITSWEBQLSE</title></head><body>\n")
    write(resp, "<div id='votable' style='width: 0; height: 0;' data-va_count='$va_count' ")
        
    if va_count == 1
        datasetid = datasets[1]
        write(resp, "data-datasetId='$datasetid' ")
    else
        for i in 1:va_count
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

    write(resp, " data-server-version='", VERSION_STRING, "' data-server-string='", SERVER_STRING)

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
    write(resp,
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
              "mainRenderer(); </script>\n")

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

println("WELCOME TO $SERVER_STRING (Supercomputer Edition)")
println("Point your browser to http://localhost:$HTTP_PORT")
println("Press CTRL+C to exit.")

host = Sockets.localhost 
# Sockets.IPv4(0)

    # @async HTTP.WebSockets.listen(host, UInt16(WS_PORT)) do ws
    # while !eof(ws)
    #    data = readavailable(ws)
    #    # println("[ws] $data")
    #    write(ws, data)
    # end
    # end

function gatekeeper(req, ws)
    orig = WebSockets.origin(req)
    @info "\nOrigin: $orig   Target: $(req.target)   subprotocol: $(subprotocol(req))"
    if occursin(LOCALIP, orig)
        coroutine(ws)
    elseif orig == ""
        @info "Non-browser clients don't send Origin. We liberally accept the update request in this case:" ws
        coroutine(ws)
    else
        @warn "Inacceptable request"
    end
end

handle(req) = replace(BAREHTML, "<body></body>" => BODY) |> WebSockets.Response

const ws_server = WebSockets.ServerWS(handle, gatekeeper)

@info "In browser > $host:$WS_PORT , F12> console > ws = new WebSocket(\"ws://$host:$WS_PORT\") "
@async WebSockets.with_logger(WebSocketLogger()) do
    WebSockets.serve(ws_server, host, WS_PORT)
end

# Sockets.localhost or Sockets.IPv4(0)
HTTP.serve(FITSWEBQL_ROUTER, host, UInt16(HTTP_PORT))

put!(serverWS.in, "close!")
