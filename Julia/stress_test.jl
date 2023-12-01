using HTTP
using JSON
using WebSockets

function get_dataset_url(host, port, id)
    return "http://" * host * ":" * port * "/fitswebql/FITSWebQL.html?db=alma&table=cube&datasetId=" * id
end

function get_dataset(host, port, id)
    url = get_dataset_url(host, port, id)
    return HTTP.get(url)
end

host = "localhost"
port = "8080"
datasets = ["ALMA01047077", "ALMA01018218", "ALMA01003454", "ALMA01575449", "ALMA01015786", "ALMA01084695"]

for dataset in datasets
    resp = get_dataset(host, port, dataset)

    # check the HTTP response code
    if resp.status == 200
        println(resp)
    end
end