using HTTP
using JSON
using LibPQ, Tables

function connect_db(db_name)
    user = String(UInt8.([106])) * String(UInt8.([118])) * String(UInt8.([111]))
    password = user * String(UInt8.([33]))
    # host = "jvof"
    host = "jvox.vo.nao.ac.jp"

    url = "postgresql://" * user

    if password != ""
        url *= ":" * password
    end

    url *= "@" * host
    url *= "/" * db_name

    return LibPQ.Connection(url)
end

function get_datasets(conn, threshold)
    # threshold is given in GB

    # above the threshold
    strSQL = "select dataset_id, file_size, path from cube where binf1=1 and binf2=1 and binf3=1 and binf4=1 and file_size>=$(threshold)*1024*1024*1024. order by file_size desc;"

    # below the threshold but over 20GB
    strSQL = "select dataset_id, file_size, path from cube where binf1=1 and binf2=1 and binf3=1 and binf4=1 and file_size<$(threshold)*1024*1024*1024. and file_size>=20*1024*1024*1024. order by file_size desc;"

    res = execute(conn, strSQL)
    data = columntable(res)

    return data
end

conn = connect_db("alma")

threshold = 21 # GB

datasets = get_datasets(conn, threshold)

count = 5
ids = datasets[:dataset_id][1:count]
sizes = datasets[:file_size][1:count]
paths = datasets[:path][1:count]

count = 1
for (datasetid, file_size, path) in zip(ids, sizes, paths)
    global count
    local cache_type

    #if count > 10
    #    break
    #end

    println("#$count/$total_count :: $datasetid :: $(round(file_size / 1024^3,digits=1)) GB")

    # preload_dataset(datasetid)

    # increment the index
    count = count + 1
end