using Base:splat
using SQLite

splat_db = SQLite.DB("../splatalogue_v3.db")
SQLite.tables(splat_db)

freq_start = 688.2213591803346
freq_end = 689.3824263880999

strSQL = "SELECT * FROM lines WHERE frequency>=$freq_start AND frequency<=$freq_end;"
println(strSQL)

res = SQLite.DBInterface.execute(splat_db, strSQL)

for row in res
    println(row)
end