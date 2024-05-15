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

use fitsrs::hdu::header::extension::image::Image;
use fitsrs::hdu::header::Header;

use std::collections::HashMap;
use std::io::BufReader;

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
    utils::set_panic_hook();

    log!(
        "index: {}, #records: {}, str: {}, #datasets: {}",
        index,
        n_key_rec,
        s,
        va_count
    );

    // create a new BufReader from s
    let mut bytes = BufReader::new(s.as_bytes());

    let mut num_bytes_read: u64 = 0;
    let mut card_80_bytes_buf = [0u8; 80];

    // parse the header
    let header: Header<Image> =
        Header::parse(&mut bytes, &mut num_bytes_read, &mut card_80_bytes_buf).unwrap();
    log!("Header: {:?}", header);
}
