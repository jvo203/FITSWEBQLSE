using Distributed;
using HTTP;
using Sockets;

println(default_worker_pool())

function serveROOT(request::HTTP.Request)
    # @show request
    # @show request.method
    # @show HTTP.header(request, "Content-Type")
    # @show HTTP.payload(request)
    @show request.target

    try
        return HTTP.Response("WELCOME TO FITSWEBQL SE")
    catch e
        return HTTP.Response(404, "Error: $e")
    end
end

const FITSWEBQL_ROUTER = HTTP.Router()

HTTP.@register(FITSWEBQL_ROUTER, "GET", "/", serveROOT)

# Sockets.localhost or Sockets.IPv4(0)
HTTP.serve(FITSWEBQL_ROUTER, Sockets.IPv4(0), UInt16(8080))
