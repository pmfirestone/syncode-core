use rayon::prelude::*;
use serde_json::{de, ser};
use std::collections::HashSet;
use std::fs;
use syncode_core::bytes::restore_bytes;
use syncode_core::grammar::EBNFParser;
use syncode_core::mask::{dfa_mask_store, grammar_mask};
use syncode_core::types::{Lexer, Parser};
use tokenizers::Tokenizer;

fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let model_id = "Qwen/Qwen3-235B-A22B";
    let language = "json";
    let grammar_file = format!("./grammars/{language}.lark");
    let tokenizer = Tokenizer::from_pretrained(model_id, None)?;
    let vocab = tokenizer.get_vocab(false);
    let tokens: Vec<&String> = vocab.keys().collect();
    let byte_tokens: Vec<Vec<u8>> = tokens.par_iter().map(|t| restore_bytes(t)).collect();

    let Ok(grammar) = EBNFParser::new(&fs::read_to_string(grammar_file).unwrap(), "start").parse()
    else {
        panic!()
    };

    let parser = Parser::new(&grammar.clone());
    let Ok(lexer) = Lexer::new(grammar.terminals.clone()) else {
        panic!()
    };

    // println!("{:#?}", byte_tokens[32]);
    let mask_store = dfa_mask_store(&grammar.terminals, &byte_tokens, &parser, &lexer, 2);

    let mut cache = fs::File::open("./cache/{model_id}/{language}.json")?;

    // ser::to_writer(cache, &mask_store);

    let candidate = r#"{
  "basics": {
    "name": "Preston Firestone",
    "label": "Programmer",
    "image": "",
    "email": "pf8@illinois.edu",
    "phone": "+1 (224) 688-2924","#;

    let tokens = tokenizer.encode(candidate, false);

    let Ok((terminals, remainder)) = lexer.lex(candidate.as_bytes()) else {
        panic!()
    };

    let Ok(accept_sequences) = parser.parse(terminals, remainder.clone()) else {
        panic!()
    };

    let mask = grammar_mask(&accept_sequences, &remainder, mask_store, &byte_tokens);

    println!("{:#?}", mask);
    Ok(())
}
