# using Base:splat
using JSON
using SQLite

splat_db = SQLite.DB("../splatalogue_v3.db")
SQLite.tables(splat_db)

freq_start = 688.2213591803346
freq_end = 689.3824263880999

strSQL = "SELECT * FROM lines WHERE frequency>=$freq_start AND frequency<=$freq_end;"
println(strSQL)

has_molecules = false
resp = IOBuffer()
write(resp, "{\"molecules\" : [")

for row in SQLite.DBInterface.execute(splat_db, strSQL)
    global has_molecules = true
    json = JSON.json(row)

    write(resp, json)
    write(resp, ",")
end

json = String(take!(resp))

if !has_molecules
    json = "{\"molecules\" : []}"
else
    # remove the last character (comma) from json
    json = chop(json, tail=1) * "]}"
end

println(json)