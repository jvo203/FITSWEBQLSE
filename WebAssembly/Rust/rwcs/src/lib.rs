extern crate cfg_if;
extern crate wasm_bindgen;

mod utils;

use cfg_if::cfg_if;
use wasm_bindgen::prelude::*;

extern crate fitsrs;
extern crate wcs;
extern crate web_sys;

// A macro to provide `println!(..)`-style syntax for `console.log` logging.
macro_rules! log {
    ( $( $t:tt )* ) => {
        web_sys::console::log_1(&format!( $( $t )* ).into());
    }
}

use fitsrs::hdu::header::{extension::image::Image, Header};

cfg_if! {
    // When the `wee_alloc` feature is enabled, use `wee_alloc` as the global allocator.
    if #[cfg(feature = "wee_alloc")] {
        extern crate wee_alloc;
        #[global_allocator]
        static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
    }
}

#[wasm_bindgen]
pub fn init_wcs(index: i32, s: &str, n_key_rec: i32, va_count: i32) {
    log!(
        "index: {}, #records: {}, str: {}, #datasets: {}",
        index,
        n_key_rec,
        s,
        va_count
    );

    // parse a new header from the string s
    // let header = Header::<R>::parse(s).unwrap();
}
