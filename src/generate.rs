// src/generate.rs
//! Masked constrained generation.

use bstr::ByteSlice;
use candle_core::{Device, Tensor};
use candle_transformers::generation::LogitsProcessor;
use candle_transformers::models::gemma3::Model;
use tokenizers::Tokenizer;

use crate::{
    bytes::restore_bytes,
    grammar::Grammar,
    lexer::Lexer,
    mask::{DFAMaskStore, grammar_mask},
    parser::Parser,
};

/// Generate a sequence constrained by a given grammar.
///
/// This is algorithm 3 from the SynCode paper.
pub fn masked_generate(
    model: &mut Model,
    tokenizer: &Tokenizer,
    input_prompt: &[u8],
    n_max: usize,
    decode: &mut LogitsProcessor,
    parser: &Parser,
    lexer: &Lexer,
    grammar: &Grammar,
    mask_store: &DFAMaskStore,
) -> String {
    let Ok(encoding) = tokenizer.encode(input_prompt.to_str_lossy(), true) else {
        panic!()
    };

    let mut tokens_so_far = encoding.get_ids().to_vec();

    let byte_tokens: Vec<Vec<u8>> = tokenizer
        .get_vocab(false)
        .into_iter()
        .map(|t| restore_bytes(&t.0))
        .collect();

    for _ in [0..n_max] {
        let Ok(tokens) = Tensor::new(tokens_so_far.clone(), &Device::Cpu) else {
            panic!()
        };

        let Ok(logits) = model.forward(&tokens, 0) else {
            panic!()
        };

        let Ok(sequence_so_far) = tokenizer.decode(&tokens_so_far, false) else {
            panic!()
        };

        let Ok((terminals, remainder)) = lexer.lex(sequence_so_far.as_bytes()) else {
            panic!()
        };

        let Ok(accept_sequences) = parser.parse(&terminals, &remainder) else {
            panic!()
        };

        let mask = grammar_mask(
            &accept_sequences,
            &remainder,
            mask_store,
            &byte_tokens,
            grammar,
        );

        let tensor_mask = Tensor::new(
            mask[..]
                .iter()
                .map(|t| if *t { 1 } else { 0 })
                .collect::<Vec<u8>>(),
            &Device::Cpu,
        );

        let Ok(masked) = logits * tensor_mask else {
            panic!()
        };

        let Ok(new_token) = decode.sample(&masked) else {
            panic!()
        };

        tokens_so_far.push(new_token);
    }

    let Ok(output) = tokenizer.decode(&tokens_so_far, true) else {
        panic!();
    };
    output
}
