extern crate cfg_if;
extern crate wasm_bindgen;

mod utils;

use cfg_if::cfg_if;
use wasm_bindgen::prelude::*;

use std::collections::HashMap;
use std::sync::{Arc, RwLock};

extern crate wcs;
extern crate web_sys;

use wcs::{ImgXY, LonLat, WCS};

#[macro_use]
extern crate lazy_static;

// WCS structures
lazy_static! {
    static ref DATASETS: Arc<RwLock<HashMap<i32, Arc<RwLock<Box<WCS>>>>>> =
        Arc::new(RwLock::new(HashMap::new()));
}

// A macro to provide `println!(..)`-style syntax for `console.log` logging.
macro_rules! log {
    ( $( $t:tt )* ) => {
        web_sys::console::log_1(&format!( $( $t )* ).into());
    }
}

cfg_if! {
    // When the `wee_alloc` feature is enabled, use `wee_alloc` as the global allocator.
    if #[cfg(feature = "wee_alloc")] {
        extern crate wee_alloc;
        #[global_allocator]
        static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
    }
}

#[wasm_bindgen]
pub fn init_wcs(index: i32, s: &str) {
    utils::set_panic_hook();

    // create a new BufReader from s
    /*let mut bytes = BufReader::new(s.as_bytes());

    let mut num_bytes_read: u64 = 0;
    let mut card_80_bytes_buf = [0u8; 80];

    // parse the header
    let header: Header<Image> =
        Header::parse(&mut bytes, &mut num_bytes_read, &mut card_80_bytes_buf).unwrap();
    log!("[rwcs::init_wcs] {:?}", header);*/

    // create a new WCS object from the header
    let wcs = WCS::new(s).unwrap();

    DATASETS
        .write()
        .unwrap()
        .insert(index, Arc::new(RwLock::new(Box::new(wcs))));

    log!(
        "[rwcs::init_wcs] WCS object created and inserted into DATASETS @ index: {}",
        index
    );
}

#[wasm_bindgen]
#[derive(Debug)]
pub struct Sky {
    pub lon: f64,
    pub lat: f64,
}

#[wasm_bindgen]
#[derive(Debug)]
pub struct Pix {
    pub x: f64,
    pub y: f64,
}

#[wasm_bindgen]
pub fn pix2lonlat(index: i32, x: f64, y: f64) -> Sky {
    let binding = DATASETS.read().unwrap();
    let wcs = binding.get(&index).unwrap().read().unwrap();

    let xy = ImgXY::new(x - 1.0, y - 1.0);
    match wcs.unproj_lonlat(&xy) {
        Some(lonlat) => {
            // the lonlat seems to be in radians, convert it to degrees
            let sky = Sky {
                lon: lonlat.lon().to_degrees(),
                lat: lonlat.lat().to_degrees(),
            };

            log!("[rwcs::pix2lonlat] {:?} [deg]", sky);

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

#[wasm_bindgen]
pub fn lonlat2pix(index: i32, lon: f64, lat: f64) -> Pix {
    let binding = DATASETS.read().unwrap();
    let wcs = binding.get(&index).unwrap().read().unwrap();

    let lonlat = LonLat::new(lon.to_radians(), lat.to_radians());
    match wcs.proj_lonlat(&lonlat) {
        Some(xy) => {
            let pix = Pix {
                x: xy.x() + 1.0,
                y: xy.y() + 1.0,
            };

            log!("[rwcs::lonlat2pix] {:?} [pix]", pix);

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
