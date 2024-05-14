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

use fitsrs::card::Card;
use fitsrs::card::Value;
use fitsrs::error::Error;
use fitsrs::fits::Fits;
use fitsrs::hdu::header::extension::image::Image;
use fitsrs::hdu::header::extension::Xtension;
use fitsrs::hdu::header::Header;
use fitsrs::hdu::primary::consume_next_card;
use fitsrs::hdu::{header, HDU};
use std::collections::HashMap;
use std::io::Read;

use std::io::BufReader;

cfg_if! {
    // When the `wee_alloc` feature is enabled, use `wee_alloc` as the global allocator.
    if #[cfg(feature = "wee_alloc")] {
        extern crate wee_alloc;
        #[global_allocator]
        static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
    }
}

fn parse_generic_card(card: &[u8; 80]) -> Result<Option<Card>, Error> {
    let kw = &card[..8];
    let value = match kw {
        b"END     " => None,
        b"HISTORY " => {
            let v = Value::String(String::from_utf8_lossy(&card[8..]).to_string());
            Some(v)
        }
        _ => {
            let str = String::from_utf8_lossy(&card[8..]);
            // Take until the comment beginning with '/'
            let str = str.split('/').next().unwrap();
            let v = if str.is_empty() {
                // empty value (just a comment present)
                Value::Undefined
            } else {
                // not empty value, there is at least one character
                // check if it is an =
                let str = if str.chars().next().unwrap() == '=' {
                    &str[1..]
                } else {
                    &str
                };

                // remove the ' ' before and after
                let str = str.trim();

                if str.is_empty() {
                    Value::Undefined
                } else if let Ok(val) = str.parse::<f64>() {
                    Value::Float(val)
                } else if let Ok(val) = str.parse::<i64>() {
                    Value::Integer(val)
                } else if str == "T" {
                    Value::Logical(true)
                } else if str == "F" {
                    Value::Logical(false)
                } else {
                    // Last case check for a string
                    let inside_str = str.split('\'').collect::<Vec<_>>();

                    if inside_str.len() >= 2 {
                        // This is a true string because it is nested inside simple quotes
                        Value::String(inside_str[1].to_string())
                    } else {
                        // This is not a string but we did not attempt to parse it
                        // so we store it as a string value
                        Value::String(str.to_string())
                    }
                }
            };

            Some(v)
        }
    };

    if let Some(value) = value {
        // 1. Init the fixed keyword slice
        let mut owned_kw: [u8; 8] = [0; 8];
        // 2. Copy from slice
        owned_kw.copy_from_slice(kw);

        Ok(Some(Card::new(owned_kw, value)))
    } else {
        Ok(None)
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

    // parse a new header from the string s
    // let header = Header::<R>::parse(s).unwrap();

    // turn the string into a mutable Vector of bytes
    // let mut bytes = s.as_bytes().to_vec();

    // create a new BufReader from s
    let mut bytes = BufReader::new(s.as_bytes());

    //let Fits { hdu } = Fits::from_reader(&mut bytes).unwrap();
    //log!("Fits file: {:?}", fits);

    let mut num_bytes_read: u64 = 0;
    let mut card_80_bytes_buf = [0u8; 80];

    /*let hdu = HDU::<BufReader<&[u8]>, Image>::new(
        &mut bytes,
        &mut num_bytes_read,
        &mut card_80_bytes_buf,
    )
    .unwrap();

    let header = hdu.get_header();
    log!("Header: {:?}", header);*/

    let header: Header<Image> =
        Header::parse(&mut bytes, &mut num_bytes_read, &mut card_80_bytes_buf).unwrap();
    log!("Header: {:?}", header);

    /* Consume mandatory keywords */
    /*let xtension: Image =
        Xtension::parse(&mut bytes, &mut num_bytes_read, &mut card_80_bytes_buf).unwrap();

    log!("Xtension: {:?}", xtension);*/
}
