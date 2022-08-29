using HTTP
using JSON
using LibPQ, Tables
using ProgressMeter

function connect_db(db_name)
    user = String(UInt8.([106])) * String(UInt8.([118])) * String(UInt8.([111]))
    password = user * String(UInt8.([33]))
    host = "jvof"

    url = "postgresql://" * user

    if password != ""
        url *= ":" * password
    end

    url *= "@" * host
    url *= "/" * db_name

    return LibPQ.Connection(url)
end

function get_fits_total(conn, threshold)
    # threshold is given in GB
    strSQL = "select sum(file_size) from cube where binf1=1 and binf2=1 and binf3=1 and binf4=1 and file_size>=$(threshold)*1024*1024*1024.;"

    res = execute(conn, strSQL)
    data = columntable(res)

    # convert the result to GB and round
    return round(data[1][1] / (1024^3))
end

function get_large_datasets(conn, threshold)
    # threshold is given in GB
    strSQL = "select dataset_id, file_size from cube where binf1=1 and binf2=1 and binf3=1 and binf4=1 and file_size>=$(threshold)*1024*1024*1024. order by file_size desc;"

    res = execute(conn, strSQL)
    data = columntable(res)

    return data
end

function poll_progress(datasetid)
    strURL = "http://grid80:8080/fitswebql/progress/" * datasetid

    resp = HTTP.get(strURL)
    # println(resp)

    if resp.status == 200
        return JSON.parse(String(resp.body))["progress"]
    else
        return nothing
    end
end

function get_dataset_url(datasetid)    
    return "http://grid80:8080/fitswebql/FITSWebQL.html?db=alma&table=cube&datasetId=" * datasetid
end

function preload_dataset(datasetid)
    local progress, strURL

    strURL = get_dataset_url(datasetid)

    # access the FITSWEBQLSE
    resp = HTTP.get(strURL)

    # check the HTTP response code
    if resp.status != 200
        println(resp)
        return
    end

    # wait until a dataset has been loaded
    p = Progress(100, 1)

    # repeatedly poll for progress
    while true
        progress = poll_progress(datasetid)

        if progress == nothing
            println("no progress")
            break
        end
        
        update!(p, Int(floor(progress)))

        # thow a DomainError if the progress is over 100% (should not happen, I want to catch any logical bugs, network problems, etc.)
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

    # then wait 30 seconds to allow for the 60s dataset timeout (avoid a RAM overload)
    # sleep(30) # or not ...
end

# conservative assumptions
utilisation = 0.70 # 70% of the disk space is used
compression = 3.5 # 3.5:1 compression ratio (really 3.53)
cache = 3.7 * 1024 # SSD cache size per node in GB
nodes = 3  # number of nodes in the cluster

conn = connect_db("alma")

threshold = 40 # GB
volume = get_fits_total(conn, threshold)
println("total volume of datasets > $(threshold) GB: $(volume) GB")

required = round(volume / compression / nodes)
println("required SSD cache space per node: $(required) GB, available: $(cache) GB, $(utilisation*100)% cache utilisation available cache: $(round(cache*utilisation)) GB")

datasets = get_large_datasets(conn, threshold)

ids = datasets[:dataset_id]
sizes = datasets[:file_size]

count = 1
total_count = length(ids) # number of datasets to preload

html = IOBuffer()
write(html, "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\">\n")
write(html, "<title>Preloaded datasets</title>\n</head>\n<body>\n")

# HTML h1
write(html, "<h1>Preloaded datasets</h1>\n")

# append HTML table header
write(html, "<table><tr><th>Dataset ID</th><th>Size</th><th>Cache Type</th></tr>\n")

for (datasetid, filesize) in zip(ids, sizes)
    global count
    local cache_type

    if count > 1
        break
    end

    println("#$count/$total_count :: $datasetid :: $(round(filesize / 1024^3,digits=1)) GB")
    preload_dataset(datasetid)
    count = count + 1

    # make HTML link
    link = get_dataset_url(datasetid)

    # set cache type
    if filesize / 1024^3 > threshold
        cache_type = "SSD"
    else
        cache_type = "HDD"
    end        

    # append HTML table row
    write(html, "<tr><td><a href=\"$link\">$datasetid</a></td><td>$(round(filesize / 1024^3,digits=1)) GB</td><td>$cache_type</td></tr>\n")
end

# end the HTML table
write(html, "</table>\n")

# end the HTML document
write(html, "</body>\n</html>\n")

# export the HTML table to a file
open("fitsDB.html", "w") do f
    write(f, String(take!(html)))
end

close(conn)