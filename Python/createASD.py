import sqlite3
import hashlib
import urllib.request

conn = sqlite3.connect("asd.db")
c = conn.cursor()

strSQL = "DROP TABLE IF EXISTS spectra ;"

try:
    c.execute(strSQL)
except sqlite3.OperationalError as err:
    print(err, ":", strSQL)

strSQL = "CREATE TABLE lines (Ion TEXT, Observed REAL, Ritz REAL, relative_intensity REAL, hash TEXT UNIQUE) ;"

try:
    c.execute(strSQL)
except sqlite3.OperationalError as err:
    print(err, ":", strSQL)

server = "https://www.nist.gov/pml/atomic-spectra-database"

# finalize_db()
conn.close()
