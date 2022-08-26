using LibPQ, Tables;

function connect_db(db_name)
    host = "jvof"
    user = SubString(host, 1, 3)
    password = user * String(UInt8.([33]))    

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
    return round(data[1][1]/(1024^3))
end

# conservative assumptions
utilisation = 0.70 # 70% of the disk space is used
compression = 3.5 # 3.5:1 compression ratio
cache = 3.7 * 1024 # SSD cache size per node in GB
nodes = 3  # number of nodes in the cluster

conn = connect_db("alma")

threshold = 40 # GB
volume = get_fits_total(conn, threshold)
println("total volume of datasets > $(threshold) GB: $(volume) GB")

required = round(volume  / compression / nodes)
println("required disk space per node: $(required) GB, available: $(cache) GB, $(utilisation*100)% cache utilisation available cache: $(round(cache*utilisation)) GB")

close(conn)