use core::str;
use std::fs::File;
use std::io::BufReader;

use fitsrs::fits::Fits;
use wcs::{ImgXY, LonLat, WCS};

pub fn test_wcs(str: &str, x: f64, y: f64, ra: f64, dec: f64) {
    let fits_file = File::open(str).unwrap();
    let mut reader = BufReader::new(fits_file);
    let Fits { hdu } = Fits::from_reader(&mut reader).unwrap();

    let header = hdu.get_header();
    println!("Header: {:?}", header);

    let wcs = WCS::new(&header).unwrap();
}

fn main() {
    println!("Comparing the WCS functions with WCSLIB / WCSTools.");

    test_wcs(
        "/Users/chris/Downloads/SVS13_13CO.clean.image.pbcor.fits",
        905.0,
        880.0,
        52.2656215,
        31.2677022,
    ); // NG file

    test_wcs(
        "/Users/chris/Downloads/ALMA01018218.fits",
        856.49056,
        438.4528,
        261.2105354,
        -34.2435452,
    ); // OK file
}
