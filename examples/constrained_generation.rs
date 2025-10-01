// examples/constrained_generation.rs
//! Generate a constrained output.

// use candle_core::IntDType;
// use std::fs;
// use syncode_core::{
//     bytes::restore_bytes, grammar::EBNFParser, lexer::Lexer, mask::grammar_mask, mask_store,
//     parser::Parser,
// };
// use tokenizers::{Tokenizer, tokenizer};

// fn main() {
//     let model_id = "Qwen/Qwen3-4B-Thinking-2507";
//     let grammar_file = "./grammars/json.lark";

//     let store = mask_store(model_id, grammar_file);

//     // Harness to avoid generating with the model.
//     let sample = r#"{"menu": {
//   "id": "file",
//   "value": "File",
//   "popup": {
//     "menuitem": [
//       {"value": "New", "onclick": "CreateNewDoc()"},
//       {"value": "Open", "onclick": "OpenDoc()"},
//       {"value": "Close", "onclick": "CloseDoc()"}
//     ]
//   }
// }}"#;

//     let tokenizer = Tokenizer::from_pretrained(model_id, None).unwrap();

//     let vocab = tokenizer.get_vocab(false);
//     let tokens: Vec<&String> = vocab.keys().collect();
//     let byte_tokens: Vec<Vec<u8>> = tokens.into_iter().map(|t| restore_bytes(t)).collect();

//     let Ok(encoding) = tokenizer.encode(sample, false) else {
//         panic!()
//     };

//     let Ok(grammar) = EBNFParser::new(&fs::read_to_string(grammar_file).unwrap(), "start").parse()
//     else {
//         panic!()
//     };

//     let Ok(lexer) = Lexer::new(&grammar.terminals, &grammar.ignore_terminals) else {
//         panic!()
//     };

//     let Ok(parser) = Parser::new(&grammar) else {
//         panic!()
//     };

//     let tokens_ids = encoding.get_ids();

//     for (idx, token) in tokens_ids.iter().enumerate() {
//         let mut sequence_so_far: Vec<u8> = Vec::new();

//         for token in &encoding.get_tokens()[..idx] {
//             sequence_so_far.extend(restore_bytes(token));
//         }

//         let Ok((terminals, remainder)) = lexer.lex(&sequence_so_far[..]) else {
//             panic!()
//         };

//         let Ok(accept_sequences) = parser.parse(&terminals, &remainder) else {
//             panic!()
//         };

//         println!("{:#?}", accept_sequences);

//         let mask = grammar_mask(
//             &accept_sequences,
//             &remainder,
//             &store,
//             &byte_tokens,
//             &grammar,
//         );

//         // assert!(
//         //     mask[token.as_usize()],
//         println!(
//             "Mask value: {}\nToken: {} {}\nIter: {}",
//             mask[token.as_usize()],
//             tokenizer.decode(&[*token], false).unwrap(),
//             token,
//             idx
//         );
//     }
// }
