mod utils;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn init_wcs(s: &str, nkeyrec: i32) {
    // print the string s to the console
    println!("no. keywords: {}, str: {}", nkeyrec, s);
}
