using LibPQ, Tables

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

function verify_dataset(datasetid, file_size, path)
    src = "/home/alma/" * path

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

    # try to read the first 80 bytes
    open(src) do io
        buf = read(io, 80)
        println("Read first 80 bytes of dataset $(datasetid): $(String(buf))")
    end

    println(
        "Verified dataset $(datasetid) with size $(round(file_size / 1024^3,digits=1)) GB @ $(src)",
    )

end

conn = connect_db("alma")

threshold = 20 #GB
large_threshold = 40 #GB
start_date = "2000-01-01"

res = get_datasets(conn, threshold, start_date)

ids = res[:dataset_id]
sizes = res[:file_size]
paths = res[:path]
dates = res[:regist_date]

total_count = length(ids) # number of datasets to check

println("Total number of datasets to check: $(total_count)")

# verify if the datasets are can be accessed
count = 1
for (datasetid, file_size, path, date) in zip(ids, sizes, paths, dates)
    global count

    println(
        "\nVerifying #$count/$total_count :: $datasetid :: $(round(file_size / 1024^3,digits=1)) GB, registration date: $date",
    )

    verify_dataset(datasetid, file_size, path)

    # increment the index
    count += 1
end

close(conn)