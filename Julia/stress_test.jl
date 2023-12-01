using Base.Threads
using HTTP
using JSON
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
    fetch_image_spectrum(host, port, id)
end

host = "capricorn"
port = "8080"
datasets = ["ALMA01047077", "ALMA01018218", "ALMA01003454", "ALMA01575449", "ALMA01015786", "ALMA01084695"]

jobs = [@spawn test(host, port, dataset) for dataset in datasets]
wait.(jobs)

println("stress-test completed.")