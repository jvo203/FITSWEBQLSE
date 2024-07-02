import sqlite3
import hashlib
import urllib2
import time
from BeautifulSoup import BeautifulSoup

conn = sqlite3.connect('splatalogue.db')
c = conn.cursor()

strSQL = "DROP TABLE IF EXISTS lines ;"

try:    
    c.execute(strSQL)
except sqlite3.OperationalError as err:
    print err, ":", strSQL
    
strSQL = "CREATE TABLE lines (species TEXT, name TEXT, frequency REAL, qn TEXT, cdms_intensity REAL, lovas_intensity REAL, E_L REAL, linelist TEXT, hash TEXT UNIQUE) ;"

try:    
    c.execute(strSQL)
except sqlite3.OperationalError as err:
    print err, ":", strSQL

def parse_results(res):
    for row in res.findAll('tr', style=True):
        data = row.findAll('td')

        if len(data) == 10:
            #print data

            link = data[1].find('a')
            species = "".join([str(x) for x in link.contents])
            name = data[2].text

            try:
                frequency = float(data[3].text.split()[0])
            except ValueError:
                frequency = 0

            try:
                measured = float(data[4].text.split()[0])
            except ValueError:
                measured = 0
                
            qn = data[5].text

            try:
                cdms_intensity = float(data[6].text)
                cdms_intensity = data[6].text
            except ValueError:
                cdms_intensity = 'NULL'

            try:
                lovas_intensity = float(data[7].text)
                lovas_intensity = data[7].text
            except ValueError:
                lovas_intensity = 'NULL'

            try:
                E_L = float(data[8].text)
                E_L = data[8].text
            except ValueError:
                E_L = 'NULL'
                
            linelist = data[9].text

            #print species, '|', name, '|', frequency, '|', measured, '|', qn, '|', cdms_intensity, '|', lovas_intensity, '|', E_L, '|', linelist

            strHash = species + name + str(frequency) + str(measured) + qn + cdms_intensity + lovas_intensity + E_L + linelist
            hash_object = hashlib.md5(strHash.encode())
            hash = hash_object.hexdigest()
            
            strSQL = "INSERT INTO lines VALUES('" + species.replace("'", "''") + "','" + name.replace("'", "''") + "'," + str(max(frequency,measured)) + ",'" + qn.replace("'", "''") + "'," + cdms_intensity + "," + lovas_intensity + "," + E_L + ",'" + linelist.replace("'", "''") + "','" + hash.replace("'", "''") + "') ;"            

            try:
                c.execute(strSQL)
            except sqlite3.OperationalError as err:
                print err, ":", strSQL
            except sqlite3.IntegrityError as err:
                print err, ":", strSQL
                
    conn.commit()        

def fetch_lines(url):
    success = False

    while not success:
        try:
            response = urllib2.urlopen(url)
            success = True
        except urllib2.URLError as err:
            print err
            time.sleep(10)
    
    page = BeautifulSoup(response.read())
    results = page.body.find('table', attrs={'class' : 'results'})

    parse_results(results)
    
    div = page.body.find('div')        
    for link in div.findAll('a', href=True):
        if "Next" in link.text:
            #print link['href']
            fetch_lines(server + link['href'])

def finalize_db():
    #############################################################
    strSQL = "CREATE TABLE new_lines (species TEXT, name TEXT, frequency REAL, qn TEXT, cdms_intensity REAL, lovas_intensity REAL, E_L REAL, linelist TEXT) ;"

    try:    
        c.execute(strSQL)
    except sqlite3.OperationalError as err:
        print err, ":", strSQL

    #############################################################
    strSQL = "INSERT INTO new_lines SELECT species, name, frequency, qn, cdms_intensity, lovas_intensity, E_L, linelist FROM lines;"

    try:    
        c.execute(strSQL)
    except sqlite3.OperationalError as err:
        print err, ":", strSQL

    #############################################################
    strSQL = "DROP TABLE IF EXISTS lines ;"

    try:    
        c.execute(strSQL)
    except sqlite3.OperationalError as err:
        print err, ":", strSQL

    #############################################################
    strSQL = "ALTER TABLE new_lines RENAME TO lines ;"

    try:    
        c.execute(strSQL)
    except sqlite3.OperationalError as err:
        print err, ":", strSQL

    #############################################################
    strSQL = "CREATE INDEX freq_idx ON lines (frequency) ;"

    try:
        c.execute(strSQL)
    except sqlite3.OperationalError as err:
        print err, ":", strSQL

    #############################################################
    strSQL = "vacuum ;"

    try:
        c.execute(strSQL)
    except sqlite3.OperationalError as err:
        print err, ":", strSQL

error = 1e-4
fmin = 232.563385 - error
fmax = 232.797516 + error

server = "http://www.cv.nrao.edu/php/splat/"

#full frequency range
f1 = 1
f2 = 951

for f in range (f1,f2):
    fmin = f - error
    fmax = f + 1

    url = server + "c.php?chemical_name=&calcIn=&data_version=v3.0&from=" + str(fmin) + "&to=" + str(fmax) + "&frequency_units=GHz&energy_range_from=&energy_range_to=&lill=on&submit=Search&displayJPL=displayJPL&displayCDMS=displayCDMS&displayLovas=displayLovas&displaySLAIM=displaySLAIM&displayToyaMA=displayToyaMA&displayOSU=displayOSU&displayRecomb=displayRecomb&displayLisa=displayLisa&displayRFI=displayRFI&ls1=ls1&ls5=ls5&el1=el1"
    #print url

    print "frequency range:", fmin, fmax, "GHz"
    
    fetch_lines(url)

finalize_db()
conn.close()
