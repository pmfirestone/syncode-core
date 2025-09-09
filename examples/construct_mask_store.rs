// use syncode_core::mask::dfa_mask_store;
use rayon::prelude::*;
use syncode_core::bytes::restore_bytes;
use tokenizers::Tokenizer;

fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let tokenizer = Tokenizer::from_pretrained("Qwen/Qwen3-235B-A22B", None)?;
    let vocab = tokenizer.get_vocab(false);
    let tokens: Vec<&String> = vocab.keys().collect();
    let byte_tokens: Vec<Vec<u8>> = tokens.par_iter().map(|t| restore_bytes(t)).collect();
    println!("{:#?}", byte_tokens[32]);
    // let mask_store = dfa_mask_store();
    Ok(())
}
