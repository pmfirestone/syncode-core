use std::collections::HashSet;
// use syncode_core::mask::dfa_mask_store;
use rayon::prelude::*;
use std::fs;
use syncode_core::bytes::restore_bytes;
use syncode_core::grammar::EBNFParser;
use syncode_core::mask::dfa_mask_store;
use syncode_core::types::{Lexer, Parser};
use tokenizers::Tokenizer;

fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let tokenizer = Tokenizer::from_pretrained("Qwen/Qwen3-235B-A22B", None)?;
    let vocab = tokenizer.get_vocab(false);
    let tokens: Vec<&String> = vocab.keys().collect();
    let byte_tokens: Vec<Vec<u8>> = tokens.par_iter().map(|t| restore_bytes(t)).collect();

    let Ok(grammar) = EBNFParser::new(
        &fs::read_to_string("./grammars/json.lark").unwrap(),
        "start",
    )
    .parse() else {
        panic!()
    };

    let parser = Parser::new(&grammar.clone());
    let Ok(lexer) = Lexer::new(grammar.terminals.clone()) else {
        panic!()
    };

    // println!("{:#?}", byte_tokens[32]);
    let mask_store = dfa_mask_store(&grammar.terminals, byte_tokens, &parser, &lexer, 2);
    // println!("{:#?}", mask_store);
    Ok(())
}
