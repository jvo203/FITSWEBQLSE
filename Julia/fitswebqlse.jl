using Distributed;
using HTTP;
using JSON;
using Sockets;

const HT_DOCS = "../htdocs"
const HTTP_PORT = 8080
const WS_PORT = HTTP_PORT + 1

println(default_worker_pool())

function serveFile(path)

    # TO-DO:
    # add mime types

    try
        # return HTTP.Response(404, "serving $path")
        return isfile(path) ? HTTP.Response(200, read(path)) : HTTP.Response(404, "$path Not Found.")
    catch e
return HTTP.Response(404, "Error: $e")
    end
end

function serveDirectory(request::HTTP.Request)
    @show request.target

    params = HTTP.queryparams(HTTP.URI(request.target))
    dir = params["dir"]
    println("Scanning $dir ...")

    try
        return HTTP.Response(200, "WELCOME TO FITSWEBQL SE")
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

    path = HT_DOCS * HTTP.unescapeuri(request.target)

    if request.target == "/"
        path *= "local.html"
    end

    return serveFile(path)

    try
        return HTTP.Response("WELCOME TO FITSWEBQL SE")
    catch e
return HTTP.Response(404, "Error: $e")
    end
end

const FITSWEBQL_ROUTER = HTTP.Router()

HTTP.@register(FITSWEBQL_ROUTER, "GET", "/", serveROOT)
HTTP.@register(FITSWEBQL_ROUTER, "GET", "/get_directory", serveDirectory)

println("FITSWEBQL SE (Supercomputer Edition)")
println("Press CTRL+C to exit.")

# Sockets.localhost or Sockets.IPv4(0)
HTTP.serve(FITSWEBQL_ROUTER, Sockets.IPv4(0), UInt16(HTTP_PORT))
