using HTTP
using JSON
using LibPQ, Tables
using ProgressMeter

function connect_db(db_name)
    user = String(UInt8.([106])) * String(UInt8.([118])) * String(UInt8.([111]))
    password = user * String(UInt8.([33]))
    # host = "jvof" # on zodiac
    host = "jvox.vo.nao.ac.jp" # on the cluster
    port = 5433

    url = "postgresql://" * user

    if password != ""
        url *= ":" * password
    end

    url *= "@" * host

    if port != 5432
        url *= ":" * string(port)
    end

    url *= "/" * db_name

    return LibPQ.Connection(url)
end

function get_datasets(conn, threshold, start_date)
    # threshold is given in GB
    # find the datasets with the file_size >= threshold AND regist_date >= starting date
    strSQL = "select dataset_id, file_size, path, regist_date from cube where binf1=1 and binf2=1 and binf3=1 and binf4=1 and file_size>=$(threshold)*1024*1024*1024. and regist_date>='$(start_date)' order by file_size desc;"

    res = execute(conn, strSQL)
    data = columntable(res)

    return data
end

function copy_dataset(datasetid, file_size, path)
    src = "/home/alma/" * path
    dst = "/mnt/fits/files/" * datasetid * ".fits"

    # check if the src file exists
    if !isfile(src)
        println("The source file $(src) does not exist. Skipping.")
        return false
    end

    # get the src filesize
    src_filesize = filesize(src)

    if src_filesize != file_size
        println("The source file $(src) has a different size than the database. Skipping.")
        return false
    end

    println("Copying dataset $(datasetid) with size $(round(file_size / 1024^3,digits=1)) GB from $(src) to $(dst)")

    # check if the dst file already exists
    if isfile(dst)
        # first check the file size
        dst_filesize = filesize(dst)

        if dst_filesize == src_filesize
            println("The destination file $(dst) already exists. Skipping.")
            return true
        end
    end

    # make a 256KB chunk
    chunk = 256 * 1024

    p = Progress(file_size, 1, "Copying...")   # minimum update interval: 1 second

    # copy the source file in chunks
    open(src, "r") do src_file
        open(dst, "w") do dst_file
            while !eof(src_file)
                write(dst_file, (read(src_file, chunk)))
                update!(p, position(src_file))
            end
        end
    end

    return true
end

function poll_progress(datasetid)
    strURL = "http://grid00:8080/fitswebql/progress/" * datasetid

    resp = HTTP.get(strURL)
    # println(resp)

    if resp.status == 200
        return JSON.parse(String(resp.body))["progress"]
    else
        return nothing
    end
end

function get_dataset_url(datasetid)
    return "http://grid00:8080/fitswebql/FITSWebQL.html?db=alma&table=cube&datasetId=" * datasetid
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
    p = Progress(100, 1, "Loading...")

    # repeatedly poll for progress
    while true
        progress = poll_progress(datasetid)

        if isnothing(progress)
            println("\nno progress")
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
    sleep(31) # or not ...
end

conn = connect_db("alma")

threshold = 20 #GB
large_threshold = 40 #GB
start_date = "2023-02-01"

# get the start_date from the command line argument (if there is one)
if length(ARGS) > 0
    start_date = ARGS[1]
end

println("Starting date: $(start_date)")

res = get_datasets(conn, threshold, start_date)

ids = res[:dataset_id]
sizes = res[:file_size]
paths = res[:path]
dates = res[:regist_date]

total_count = length(ids) # number of datasets to preload

println("Total number of datasets to preload: $(total_count)")

# first copy then preload (somehow it helps to even out glusterfs load balancing)
count = 1
for (datasetid, file_size, path, date) in zip(ids, sizes, paths, dates)
    global count

    println("\n<1st pass> #$count/$total_count :: $datasetid :: $(round(file_size / 1024^3,digits=1)) GB, registration date: $date")

    # only copy for file_size >= large_threshold
    if file_size >= large_threshold * 1024^3
        # copy should be enabled for large datasets only 
        # otherwise we will run out of disk space
        println("\tCOPY: #$count/$total_count :: $datasetid :: $(round(file_size / 1024^3,digits=1)) GB")
        copy_dataset(datasetid, file_size, path)
    else
        # preload
        println("\tPRELOAD: #$count/$total_count :: $datasetid :: $(round(file_size / 1024^3,digits=1)) GB")
        preload_dataset(datasetid)
    end

    # increment the index
    count = count + 1
end

# sleep for 5 minutes to allow for the glusterfs to settle down
println("Sleeping for 5 minutes to allow for the glusterfs to settle down...")
sleep(5 * 60)

# another pass, this time preloading large datasets only (hopefully the glusterfs should be ready)
count = 1
for (datasetid, file_size, path, date) in zip(ids, sizes, paths, dates)
    global count

    println("\n<2nd pass> #$count/$total_count :: $datasetid :: $(round(file_size / 1024^3,digits=1)) GB, registration date: $date")

    if file_size >= large_threshold * 1024^3
        # preload
        println("\tPRELOAD: #$count/$total_count :: $datasetid :: $(round(file_size / 1024^3,digits=1)) GB")
        preload_dataset(datasetid)
    end

    # increment the index
    count = count + 1
end

close(conn)
