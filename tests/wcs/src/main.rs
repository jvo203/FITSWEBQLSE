use core::str;
use std::fs::File;
use std::io::BufReader;

use fitsrs::fits::Fits;
use wcs::{ImgXY, LonLat, WCS};

pub fn test_wcs(str: &str) {
    let fits_file = File::open(str).unwrap();
    let mut reader = BufReader::new(fits_file);
    let Fits { hdu } = Fits::from_reader(&mut reader).unwrap();
    let header = hdu.get_header();
    let wcs = WCS::new(&header).unwrap();
}

fn main() {
    println!("Comparing the WCS functions with WCSLIB / WCSTools.");
}
