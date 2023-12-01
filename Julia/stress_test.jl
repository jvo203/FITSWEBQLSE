using HTTP
using JSON
using WebSockets

function get_dataset_url(host, port, id)
    return "http://" * host * ":" * port * "/fitswebql/FITSWebQL.html?db=alma&table=cube&datasetId=" * id
end

datasets = ["ALMA01047077", "ALMA01018218", "ALMA01003454", "ALMA01575449", "ALMA01015786", "ALMA01084695"]

host = "localhost"
port = "8080"