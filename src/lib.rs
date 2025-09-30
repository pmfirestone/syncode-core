// src/lib.rs
//! Constrain is a library for probablistic grammar-guided generation.
//!
//! For an example of use see the file `examples/construct_mask_store.rs`. See
//! individual crates for further documentation.
#![doc = include_str!("../README.md")]
#![feature(test)]

use bytes::restore_bytes;
use grammar::EBNFParser;
use mask::{DFAMaskStore, dfa_mask_store};
use std::fs;
use tokenizers::Tokenizer;
use types::Parser;

pub mod bytes;
pub mod dfa;
pub mod generate;
pub mod grammar;
pub mod lexer;
pub mod mask;
pub mod parser;
pub mod production;
pub mod table;
pub mod terminal;
pub mod token;
pub mod types;

pub fn mask_store(model_id: &str, grammar_file: &str) -> DFAMaskStore {
    let tokenizer = Tokenizer::from_pretrained(model_id, None).unwrap();
    let mut byte_tokens: Vec<Vec<u8>> = Vec::new();

    // Make sure the tokens are in index order.
    for (idx, _) in [0..tokenizer.get_vocab_size(false)].iter().enumerate() {
        byte_tokens.push(restore_bytes(&tokenizer.id_to_token(idx as u32).unwrap()));
    }

    let Ok(grammar) = EBNFParser::new(&fs::read_to_string(grammar_file).unwrap(), "start").parse()
    else {
        panic!()
    };

    let Ok(parser) = Parser::new(&grammar.clone()) else {
        panic!()
    };

    dfa_mask_store(&grammar.terminals, &byte_tokens, &parser, 2)
}
