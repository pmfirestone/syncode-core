// use syncode_core::mask::dfa_mask_store;
use std::collections::HashMap;
use tokenizers::Tokenizer;

fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let tokenizer = Tokenizer::from_pretrained("Qwen/Qwen3-235B-A22B", None)?;
    let vocab = tokenizer.get_vocab(false);
    let tokens: Vec<&String> = vocab.keys().collect();
    println!("{:#?}", restore_bytes(tokens[32]));
    //    let mask_store = dfa_mask_store();
    Ok(())
}

/// Converts bytes to unicode characters.
/// See https://github.com/openai/gpt-2/blob/master/src/encoder.py#L9
pub fn char_bytes() -> HashMap<char, u8> {
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

/// Convert a string of unicode characters back to the bytes they represent.
fn restore_bytes(input: &String) -> Vec<u8> {
    let char_bytes_map = char_bytes();
    input.chars().map(|c| char_bytes_map[&c]).collect()
}
