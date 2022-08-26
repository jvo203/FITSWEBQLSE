using LibPQ, Tables;

function connect_db(db_name)
    user = "jvo"    
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
    return round(data[1][1]/(1024^3))
end

conn = connect_db("alma")

dataSize = get_fits_total(conn, 150)
println("total data size: $(dataSize) GB")

close(conn)