using LibPQ, Tables
using ProgressMeter

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

    res = execute(conn, strSQL)
    data = columntable(res)

    return data
end

function check_dataset(datasetid, path)
    src = "/home/alma/" * path

    if !isfile(src)
        # append to the error file
        open("error.txt", "a") do f
            write(f, "$(datasetid) :: $(src)\n")
        end
    end
end

conn = connect_db("alma")
datasets = get_datasets(conn, 0)

ids = datasets[:dataset_id]
paths = datasets[:path]

count = 1
total_count = length(ids) # number of datasets to check

println("Checking $(total_count) datasets")

p = Progress(total_count, 1, "Checking...")

for (datasetid, path) in zip(ids, paths)
    global p, count

    # println("Checking dataset $(count) of $(total_count)")

    # check if the dataset exists
    check_dataset(datasetid, path)

    update!(p, count)

    # increment the index
    count = count + 1
end

close(conn)