extern crate cfg_if;
extern crate wasm_bindgen;

mod utils;

use cfg_if::cfg_if;
use wasm_bindgen::prelude::*;

cfg_if! {
    // When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
    // allocator.
    if #[cfg(feature = "wee_alloc")] {
        extern crate wee_alloc;
        #[global_allocator]
        static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
    }
}

#[wasm_bindgen]
pub fn init_wcs(index: i32, s: &str, n_key_rec: i32, va_count: i32) {
    // print the string s to the console
    println!(
        "index: {}, #records: {}, str: {}, #datasets: {}",
        index, n_key_rec, s, va_count
    );
}
