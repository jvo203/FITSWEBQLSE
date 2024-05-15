extern crate cfg_if;
extern crate wasm_bindgen;

mod utils;

use cfg_if::cfg_if;
use wasm_bindgen::prelude::*;

use std::collections::HashMap;
use std::io::BufReader;
use std::sync::{Arc, RwLock};

extern crate fitsrs;
extern crate wcs;
extern crate web_sys;

use wcs::{ImgXY, WCS};

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

use fitsrs::hdu::header::extension::image::Image;
use fitsrs::hdu::header::Header;

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
    let mut bytes = BufReader::new(s.as_bytes());

    let mut num_bytes_read: u64 = 0;
    let mut card_80_bytes_buf = [0u8; 80];

    // parse the header
    let header: Header<Image> =
        Header::parse(&mut bytes, &mut num_bytes_read, &mut card_80_bytes_buf).unwrap();
    log!("[rwcs::init_wcs] {:?}", header);

    // create a new WCS object from the header
    let wcs = WCS::new(&header).unwrap();

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
pub fn pix2lonlat(index: i32, x: f64, y: f64) {
    let binding = DATASETS.read().unwrap();
    let wcs = binding.get(&index).unwrap().read().unwrap();

    let xy = ImgXY::new(x - 1.0, y - 1.0);
    let lonlat = wcs.unproj_lonlat(&xy).unwrap();

    log!("[rwcs::pix2sky] {:?}", lonlat);
}
