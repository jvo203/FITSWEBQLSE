use core::str;
use std::fs::File;
use std::io::BufReader;

use fitsrs::fits::Fits;
use wcs::{ImgXY, LonLat, WCS};

#[derive(Debug)]
pub struct Sky {
    pub lon: f64,
    pub lat: f64,
}

#[derive(Debug)]
pub struct Pix {
    pub x: f64,
    pub y: f64,
}

pub fn pix2world(wcs: &WCS, x: f64, y: f64) -> Sky {
    let xy = ImgXY::new(x - 1.0, y - 1.0);

    match wcs.unproj_lonlat(&xy) {
        Some(lonlat) => {
            // the lonlat seems to be in radians, convert it to degrees
            let sky = Sky {
                lon: lonlat.lon().to_degrees(),
                lat: lonlat.lat().to_degrees(),
            };

            println!("[wcs::pix2world] {:?} [deg]", sky);

            sky
        }
        None => {
            // return NaN values
            let sky = Sky {
                lon: std::f64::NAN,
                lat: std::f64::NAN,
            };

            sky
        }
    }
}

pub fn world2pix(wcs: &WCS, lon: f64, lat: f64) -> Pix {
    let lonlat = LonLat::new(lon.to_radians(), lat.to_radians());

    match wcs.proj_lonlat(&lonlat) {
        Some(xy) => {
            let pix = Pix {
                x: xy.x() + 1.0,
                y: xy.y() + 1.0,
            };

            println!("[wcs::world2pix] {:?} [pix]", pix);

            pix
        }
        None => {
            // return NaN values
            let pix = Pix {
                x: std::f64::NAN,
                y: std::f64::NAN,
            };

            pix
        }
    }
}

pub fn test_wcs(str: &str, x: f64, y: f64, ra: f64, dec: f64) {
    println!("Testing WCS functions with file: {}", str);

    let fits_file = File::open(str).unwrap();
    let mut reader = BufReader::new(fits_file);
    let Fits { hdu } = Fits::from_reader(&mut reader).unwrap();

    let header = hdu.get_header();
    // println!("Header: {:?}", header);

    let wcs = WCS::new(&header).unwrap();

    let world = pix2world(&wcs, x, y);
    println!("Expected: {:?}", Sky { lon: ra, lat: dec });
    println!("Result: {:?}", world);

    let pix = world2pix(&wcs, ra, dec);
    println!("Expected: {:?}", Pix { x, y });
    println!("Result: {:?}", pix);
}

fn main() {
    println!("Comparing the WCS functions with WCSLIB / WCSTools.");

    // passing the ra, dec obtained from SAO ds9
    test_wcs(
        "/Users/chris/Downloads/SVS13_13CO.clean.image.pbcor.fits",
        905.0,
        880.0,
        52.2656215,
        31.2677022,
    ); // NG file

    println!("");

    // passing the ra, dec obtained from SAO ds9
    test_wcs(
        "/Users/chris/Downloads/ALMA01018218.fits",
        856.49056,
        438.4528,
        261.2105354,
        -34.2435452,
    ); // OK file
}
