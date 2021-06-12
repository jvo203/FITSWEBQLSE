using Distributed;
using HTTP;
using Sockets;

println(default_worker_pool())

function getROOT(request::HTTP.Request)
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

HTTP.@register(FITSWEBQL_ROUTER, "GET", "/", getROOT)

HTTP.serve(FITSWEBQL_ROUTER, Sockets.localhost, 8080) #IPv4(0)