import sqlite3

conn = sqlite3.connect("asd.db")
c = conn.cursor()

strSQL = "DROP TABLE IF EXISTS spectra ;"

try:
    c.execute(strSQL)
except sqlite3.OperationalError as err:
    print(err, ":", strSQL)
