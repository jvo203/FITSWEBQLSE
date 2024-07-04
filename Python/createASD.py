import sqlite3
import hashlib
import urllib.request
import urllib.error
import time
import io
import re
import pandas as pd
from bs4 import BeautifulSoup

conn = sqlite3.connect("asd.db")
c = conn.cursor()

strSQL = "DROP TABLE IF EXISTS lines ;"

try:
    c.execute(strSQL)
except sqlite3.OperationalError as err:
    print(err, ":", strSQL)

strSQL = "DROP TABLE IF EXISTS new_lines ;"

try:
    c.execute(strSQL)
except sqlite3.OperationalError as err:
    print(err, ":", strSQL)

strSQL = "CREATE TABLE lines (element TEXT, sp_num INTEGER, obs_wl REAL, ritz_wl REAL, intens REAL, hash TEXT UNIQUE) ;"

try:
    c.execute(strSQL)
except sqlite3.OperationalError as err:
    print(err, ":", strSQL)


def finalize_db():
    #############################################################
    strSQL = "CREATE TABLE new_lines (element TEXT, sp_num INTEGER, obs_wl REAL, ritz_wl REAL, intens REAL) ;"

    try:
        c.execute(strSQL)
    except sqlite3.OperationalError as err:
        print(err, ":", strSQL)

    #############################################################
    strSQL = "INSERT INTO new_lines SELECT element, sp_num, obs_wl, ritz_wl, intens FROM lines;"

    try:
        c.execute(strSQL)
    except sqlite3.OperationalError as err:
        print(err, ":", strSQL)

    #############################################################
    strSQL = "DROP TABLE IF EXISTS lines ;"

    try:
        c.execute(strSQL)
    except sqlite3.OperationalError as err:
        print(err, ":", strSQL)

    #############################################################
    strSQL = "ALTER TABLE new_lines RENAME TO lines ;"

    try:
        c.execute(strSQL)
    except sqlite3.OperationalError as err:
        print(err, ":", strSQL)

    #############################################################
    strSQL = "CREATE INDEX obs_idx ON lines (obs_wl) ;"

    try:
        c.execute(strSQL)
    except sqlite3.OperationalError as err:
        print(err, ":", strSQL)

    #############################################################
    strSQL = "CREATE INDEX ritz_idx ON lines (ritz_wl) ;"

    try:
        c.execute(strSQL)
    except sqlite3.OperationalError as err:
        print(err, ":", strSQL)

    conn.commit()

    #############################################################
    strSQL = "vacuum ;"

    try:
        c.execute(strSQL)
    except sqlite3.OperationalError as err:
        print(err, ":", strSQL)


def fetch_lines(url):
    print(url)

    success = False

    while not success:
        try:
            response = urllib.request.urlopen(url)
            success = True
        except urllib.error.URLError as err:
            print(err)
            time.sleep(10)

    soup = BeautifulSoup(response.read())
    html_data = soup.get_text()
    html_data = html_data.replace('"', "")
    data = io.StringIO(html_data)
    # print(html_data)

    data_frame = pd.read_csv(data, sep="\t").drop("Unnamed: 18", axis=1)
    print(data_frame)

    # clean intensities
    data_frame["intens"] = data_frame["intens"].apply(
        lambda item: re.sub("[^0-9]", "", str(item))
    )
    data_frame = data_frame[data_frame["intens"] != ""]
    data_frame["intens"] = pd.to_numeric(data_frame["intens"])

    # line threshold
    value = 10**3
    # strength line gA > 10**8 etc.
    data_frame = data_frame[data_frame["intens"] > value]

    # filter NaN values
    column = "obs_wl_air(A)"
    # remove '+' characters
    data_frame[column] = data_frame[column].apply(
        lambda item: re.sub("[^0-9.]", "", str(item))
    )
    data_frame = data_frame[data_frame[column] != ""]

    column = "ritz_wl_air(A)"
    # remove '+' characters
    data_frame[column] = data_frame[column].apply(
        lambda item: re.sub("[^0-9.]", "", str(item))
    )
    data_frame = data_frame[data_frame[column] != ""]

    # filter sp
    # data_frame = data_frame[data_frame["sp_num"].isin(sp_num)]

    # reset index
    data_frame.reset_index(inplace=True, drop=True)

    print(data_frame)

    for index, row in data_frame.iterrows():
        element = row["element"]
        sp_num = row["sp_num"]
        obs_wl = row["obs_wl_air(A)"]
        ritz_wl = row["ritz_wl_air(A)"]
        intens = row["intens"]

        strHash = element + str(sp_num) + str(obs_wl) + str(ritz_wl) + str(intens)
        hash_object = hashlib.md5(strHash.encode())
        hash = hash_object.hexdigest()

        strSQL = (
            "INSERT INTO lines VALUES('"
            + element.replace("'", "''")
            + "',"
            + str(sp_num)
            + ","
            + str(obs_wl)
            + ","
            + str(ritz_wl)
            + ","
            + str(intens)
            + ",'"
            + hash.replace("'", "''")
            + "') ;"
        )

        # print(strSQL)

        try:
            c.execute(strSQL)
        except sqlite3.OperationalError as err:
            print(err, ":", strSQL)
        except sqlite3.IntegrityError as err:
            print(err, ":", strSQL)

    conn.commit()


web_page = "https://www.nist.gov/pml/atomic-spectra-database"
server = "https://physics.nist.gov/cgi-bin/ASD/"

# CSV
# https://physics.nist.gov/cgi-bin/ASD/lines1.pl?spectra=&output_type=0&low_w=1000&upp_w=1100&unit=0&de=0&plot_out=0&I_scale_type=1&format=2&line_out=3&remove_js=on&en_unit=0&output=0&bibrefs=1&page_size=15&show_obs_wl=1&show_calc_wl=1&order_out=0&max_low_enrg=&show_av=2&max_upp_enrg=&tsb_value=0&min_str=&A_out=0&intens_out=on&max_str=&allowed_out=1&forbid_out=1&min_accur=&min_intens=&conf_out=on&term_out=on&enrg_out=on&J_out=on&submit=Retrieve+Data

# TAB
# https://physics.nist.gov/cgi-bin/ASD/lines1.pl?spectra=&output_type=0&low_w=1000&upp_w=1100&unit=0&de=0&plot_out=0&I_scale_type=1&format=3&line_out=3&remove_js=on&en_unit=0&output=0&bibrefs=1&page_size=15&show_obs_wl=1&show_calc_wl=1&order_out=0&max_low_enrg=&show_av=2&max_upp_enrg=&tsb_value=0&min_str=&A_out=0&intens_out=on&max_str=&allowed_out=1&forbid_out=1&min_accur=&min_intens=&conf_out=on&term_out=on&enrg_out=on&J_out=on&submit=Retrieve+Data

# HDS wavelength range
w1 = 3000  # Angstrom
w2 = 10000  # Angstrom
delta = 100  # Angstrom

for w in range(w1, w2, delta):
    wmin = w
    wmax = w + delta

    url = (
        server
        + "lines1.pl?spectra=&output_type=0&low_w="
        + str(wmin)
        + "&upp_w="
        + str(wmax)
        + "&unit=0&de=0&plot_out=0&I_scale_type=1&format=3&line_out=3&remove_js=on&en_unit=0&output=0&bibrefs=1&page_size=15&show_obs_wl=1&show_calc_wl=1&order_out=0&max_low_enrg=&show_av=2&max_upp_enrg=&tsb_value=0&min_str=&A_out=0&intens_out=on&max_str=&allowed_out=1&forbid_out=1&min_accur=&min_intens=&conf_out=on&term_out=on&enrg_out=on&J_out=on&submit=Retrieve+Data"
    )

    fetch_lines(url)

# wmin = 1000
# wmax = 1001

# url = (
#    server
#    + "lines1.pl?spectra=&output_type=0&low_w="
#    + str(wmin)
#    + "&upp_w="
#    + str(wmax)
#    + "&unit=0&de=0&plot_out=0&I_scale_type=1&format=3&line_out=3&remove_js=on&en_unit=0&output=0&bibrefs=1&page_size=15&show_obs_wl=1&show_calc_wl=1&order_out=0&max_low_enrg=&show_av=2&max_upp_enrg=&tsb_value=0&min_str=&A_out=0&intens_out=on&max_str=&allowed_out=1&forbid_out=1&min_accur=&min_intens=&conf_out=on&term_out=on&enrg_out=on&J_out=on&submit=Retrieve+Data"
# )

# fetch_lines(url)

finalize_db()
conn.close()
