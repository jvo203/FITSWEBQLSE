using LibPQ, Tables

function connect_db(db_name, host)
    println("LibPQ: connecting to $host")

    user = String(UInt8.([106])) * String(UInt8.([118])) * String(UInt8.([111]))
    password = user * String(UInt8.([33]))

    url = "postgresql://" * user

    if password != ""
        url *= ":" * password
    end

    url *= "@" * host
    url *= "/" * db_name

    return LibPQ.Connection(url)
end

function get_datasets(conn, threshold, start_date)
    # threshold is given in GB
    # find the datasets with the file_size >= threshold AND regist_date >= starting date
    strSQL = "select dataset_id, file_size, path, regist_date from cube where binf1=1 and binf2=1 and binf3=1 and binf4=1 and file_size>=$(threshold)*1024*1024*1024. and regist_date>='$(start_date)' order by regist_date asc;"

    println("LibPQ: executing query '$strSQL'")

    res = execute(conn, strSQL)
    data = columntable(res)

    return data
end

threshold = 20 #GB
start_date = "2023-09-01"

# host = "jvof"
host = "jvox.vo.nao.ac.jp"

# optional: get the start_date and a host from the command line argument (if there is one)
if length(ARGS) > 0
    start_date = ARGS[1]
end

if length(ARGS) > 1
    host = ARGS[2]
end

conn = connect_db("alma", host)
res = get_datasets(conn, threshold, start_date)

ids = res[:dataset_id]
sizes = res[:file_size]
paths = res[:path]
dates = res[:regist_date]

count = 1
total_count = length(ids) # number of datasets to check

println("Checking $(total_count) datasets")

for (datasetid, file_size, path, date) in zip(ids, sizes, paths, dates)
    global count

    println("\t#$count/$total_count :: $datasetid :: $(round(file_size / 1024^3,digits=1)) GB, registration date: $date")

    # increment the index
    count = count + 1
end

close(conn)