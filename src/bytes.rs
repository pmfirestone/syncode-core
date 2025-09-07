// src/bytes.rs
/*!
Language models generally use byte level vocabularies, but Huggingface
distributes these as Unicode characters according to a mapping between bytes
and code points. In order to correctly constrain at the level of bytes, we need
to convert these characters back into the bytes we represent; this module
contains functions for doing this.
*/

use std::collections::HashMap;

use regex_automata::util::lazy::Lazy;

/// Converts bytes to unicode characters.
/// See https://github.com/openai/gpt-2/blob/master/src/encoder.py#L9
fn char_bytes() -> HashMap<char, u8> {
    let mut bs: Vec<u8> = vec![];
    bs.extend(b'!'..=b'~');
    bs.extend(b'\xA1'..=b'\xAC');
    bs.extend(b'\xAE'..=b'\xFF');

    let mut cs: Vec<u32> = bs.iter().map(|i| *i as u32).collect();
    let mut n = 0;

    for b in 0..=255u8 {
        if !bs.contains(&b) {
            bs.push(b);
            cs.push(u32::pow(2, 8) + n);
            n += 1;
        }
    }

    // Safety: cs contains all values from bs (between 0 and 255),
    // and some values of value 2⁸ + n, where n is between 0 and 255. This is between 255 and 512.
    // Both ranges are valid UTF-32 values (which is fully saturated until 0xD000)
    bs.into_iter()
        .zip(cs)
        .map(|(f, t)| (unsafe { std::char::from_u32_unchecked(t) }, f))
        .collect()
}
const CHAR_BYTES_MAP: Lazy<HashMap<char, u8>> = Lazy::new(char_bytes);

/// Convert a string of unicode characters back to the bytes they represent.
///
/// WARNING: This is surprisingly slow.
pub fn restore_bytes(input: &String) -> Vec<u8> {
    input.chars().map(|c| CHAR_BYTES_MAP[&c]).collect()
}
