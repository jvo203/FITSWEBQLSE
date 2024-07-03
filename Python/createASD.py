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

strSQL = "CREATE TABLE lines (element TEXT, sp_num INTEGER, obs_wl_vac REAL, ritz_wl_vac REAL, intens REAL, hash TEXT UNIQUE) ;"

try:
    c.execute(strSQL)
except sqlite3.OperationalError as err:
    print(err, ":", strSQL)


def fetch_lines(url):
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

    data_frame = pd.read_csv(data, sep="\t")
    # .drop("Unnamed: 20", axis=1)
    print(data_frame)

    # clean intensities
    data_frame["intens"] = data_frame["intens"].apply(
        lambda item: re.sub("[^0-9]", "", str(item))
    )
    data_frame = data_frame[data_frame["intens"] != ""]
    data_frame["intens"] = pd.to_numeric(data_frame["intens"])

    # line threshold
    value = 10**2
    # strength line gA > 10**8 etc.
    data_frame = data_frame[data_frame["intens"] > value]

    # filter NaN values
    column = "ritz_wl_vac(A)"
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


web_page = "https://www.nist.gov/pml/atomic-spectra-database"
server = "https://physics.nist.gov/cgi-bin/ASD/"

# CSV
# https://physics.nist.gov/cgi-bin/ASD/lines1.pl?spectra=&output_type=0&low_w=1000&upp_w=1100&unit=0&de=0&plot_out=0&I_scale_type=1&format=2&line_out=3&remove_js=on&en_unit=0&output=0&bibrefs=1&page_size=15&show_obs_wl=1&show_calc_wl=1&order_out=0&max_low_enrg=&show_av=2&max_upp_enrg=&tsb_value=0&min_str=&A_out=0&intens_out=on&max_str=&allowed_out=1&forbid_out=1&min_accur=&min_intens=&conf_out=on&term_out=on&enrg_out=on&J_out=on&submit=Retrieve+Data

# TAB
# https://physics.nist.gov/cgi-bin/ASD/lines1.pl?spectra=&output_type=0&low_w=1000&upp_w=1100&unit=0&de=0&plot_out=0&I_scale_type=1&format=3&line_out=3&remove_js=on&en_unit=0&output=0&bibrefs=1&page_size=15&show_obs_wl=1&show_calc_wl=1&order_out=0&max_low_enrg=&show_av=2&max_upp_enrg=&tsb_value=0&min_str=&A_out=0&intens_out=on&max_str=&allowed_out=1&forbid_out=1&min_accur=&min_intens=&conf_out=on&term_out=on&enrg_out=on&J_out=on&submit=Retrieve+Data

wmin = 1000
wmax = 1001

url = (
    server
    + "lines1.pl?spectra=&output_type=0&low_w="
    + str(wmin)
    + "&upp_w="
    + str(wmax)
    + "&unit=0&de=0&plot_out=0&I_scale_type=1&format=3&line_out=3&remove_js=on&en_unit=0&output=0&bibrefs=1&page_size=15&show_obs_wl=1&show_calc_wl=1&order_out=0&max_low_enrg=&show_av=2&max_upp_enrg=&tsb_value=0&min_str=&A_out=0&intens_out=on&max_str=&allowed_out=1&forbid_out=1&min_accur=&min_intens=&conf_out=on&term_out=on&enrg_out=on&J_out=on&submit=Retrieve+Data"
)

fetch_lines(url)

# finalize_db()
conn.close()
