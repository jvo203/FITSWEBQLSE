using Base:splat
using SQLite

splat_db = SQLite.DB("../splatalogue_v3.db")

SQLite.tables(splat_db)