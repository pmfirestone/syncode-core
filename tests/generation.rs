#![feature(test)]

extern crate test;
use rayon::prelude::*;
use std::fs;
use syncode_core::bytes::restore_bytes;
use syncode_core::grammar::EBNFParser;
use syncode_core::grammar::Grammar;
use syncode_core::lexer::Lexer;
use syncode_core::mask::{dfa_mask_store, grammar_mask};
use syncode_core::mask_store;
use syncode_core::parser::Parser;
use syncode_core::production::Production;
use syncode_core::terminal::Terminal;
use tokenizers::Tokenizer;

#[bench]
fn build_mask_store_json(b: &mut test::Bencher) {
    let model_id = "Qwen/Qwen3-235B-A22B";
    let language = "json";
    let grammar_file = format!("./grammars/{language}.lark");
    let tokenizer = Tokenizer::from_pretrained(model_id, None).unwrap();
    let vocab = tokenizer.get_vocab(false);
    let tokens: Vec<&String> = vocab.keys().collect();
    let byte_tokens: Vec<Vec<u8>> = tokens.into_par_iter().map(|t| restore_bytes(t)).collect();

    let Ok(grammar) = EBNFParser::new(&fs::read_to_string(grammar_file).unwrap(), "json").parse()
    else {
        panic!()
    };

    let Ok(parser) = Parser::new(&grammar.clone()) else {
        panic!()
    };

    // dfa_mask_store(&grammar.terminals, &byte_tokens, &parser, 2);
    b.iter(|| dfa_mask_store(&grammar.terminals, &byte_tokens, &parser, 2));
}

#[test]
fn generate_json() {
    let model_id = "Qwen/Qwen3-4B-Thinking-2507";
    let grammar_file = "./grammars/json.lark";

    let store = mask_store(model_id, grammar_file);

    // Harness to avoid generating with the model.
    let sample = r#"{"menu": {
  "id": "file",
  "value": "File",
  "popup": {
    "menuitem": [
      {"value": "New", "onclick": "CreateNewDoc()"},
      {"value": "Open", "onclick": "OpenDoc()"},
      {"value": "Close", "onclick": "CloseDoc()"}
    ]
  }
}}"#;

    let tokenizer = Tokenizer::from_pretrained(model_id, None).unwrap();

    let vocab = tokenizer.get_vocab(false);
    let tokens: Vec<&String> = vocab.keys().collect();
    let byte_tokens: Vec<Vec<u8>> = tokens.into_iter().map(|t| restore_bytes(t)).collect();

    let Ok(encoding) = tokenizer.encode(sample, false) else {
        panic!()
    };

    let Ok(grammar) = EBNFParser::new(&fs::read_to_string(grammar_file).unwrap(), "json").parse()
    else {
        panic!()
    };

    let Ok(lexer) = Lexer::new(&grammar.terminals, &grammar.ignore_terminals) else {
        panic!()
    };

    let Ok(parser) = Parser::new(&grammar) else {
        panic!()
    };

    let tokens_ids = encoding.get_ids();

    for (idx, token) in tokens_ids.iter().enumerate() {
        let mut sequence_so_far: Vec<u8> = Vec::new();

        for token in &encoding.get_tokens()[..idx] {
            sequence_so_far.extend(restore_bytes(token));
        }

        let Ok((terminals, remainder)) = lexer.lex(&sequence_so_far[..]) else {
            panic!()
        };

        let Ok(accept_sequences) = parser.parse(&terminals, &remainder) else {
            panic!()
        };

        println!("{:#?}", accept_sequences);

        let mask = grammar_mask(
            &accept_sequences,
            &remainder,
            &store,
            &byte_tokens,
            &grammar,
        );

        assert!(
            mask[*token as usize],
            "Mask value: {}\nToken: {} {}\nIter: {}",
            mask[*token as usize],
            tokenizer.decode(&[*token], false).unwrap(),
            token,
            idx
        );
    }
}

#[test]
fn test_calc() {
    let Ok(grammar) =
        EBNFParser::new(&fs::read_to_string("grammars/calc.lark").unwrap(), "start").parse()
    else {
        panic!()
    };

    // eprintln!("grammar: {:#?}", grammar);

    let model_vocabulary: Vec<Vec<u8>> = vec![
        b"1".into(),
        b"a".into(),
        b"+".into(),
        b" ".into(),
        b"1.".into(),
        b".1".into(),
    ];

    let Ok(parser) = Parser::new(&grammar) else {
        panic!()
    };

    // eprintln!("action_table: {:#?}", parser.action_table);
    // eprintln!("goto_table: {:#?}", parser.goto_table);

    let Ok(lexer) = Lexer::new(&grammar.terminals, &grammar.ignore_terminals) else {
        panic!()
    };

    let mask_store = dfa_mask_store(&grammar.terminals, &model_vocabulary, &parser, 2);

    let prefix = b"1 + 1.";

    let Ok((tokens, remainder)) = lexer.lex(prefix) else {
        panic!()
    };

    let Ok(accept_sequences) = parser.parse(&tokens, &remainder) else {
        panic!()
    };

    let mask = grammar_mask(
        &accept_sequences,
        &remainder,
        &mask_store,
        &model_vocabulary,
        &grammar,
    );

    assert_eq!(mask, vec![true, false, false, true, false, true]);
}

#[test]
fn test_dfa_mask_store() {
    let model_vocabulary: Vec<Vec<u8>> = vec![
        "_prime():".as_bytes().into(),
        b"\xE2\x88".into(),
        "'''".as_bytes().into(),
        " hi".as_bytes().into(),
        "indeed".as_bytes().into(),
        "n0pe".as_bytes().into(),
    ];
    let lexical_terminals = vec![
        Terminal::new("L_PAREN", r"\(", 0),
        Terminal::new("R_PAREN", r"\)", 0),
        Terminal::new("IDENTIFIER", r"[a-zA-Z_]*", 0),
        Terminal::new("$", "", 0),
    ];
    let candidate_string = b"is";
    let terminal = Terminal::new("IDENTIFIER", r"[a-zA-Z_]*", 0);
    let grammar = Grammar {
        symbol_set: vec![
            "L_PAREN".to_string(),
            "R_PAREN".to_string(),
            "IDENTIFIER".to_string(),
            "$".to_string(),
        ],
        terminals: lexical_terminals.clone(),
        start_symbol: "start".to_string(),
        productions: vec![Production::new(
            "start",
            vec!["IDENTIFIER", "L_PAREN", "R_PAREN", "start"].into(),
        )],
        ignore_terminals: vec![],
    };
    let Ok(parser) = Parser::new(&grammar) else {
        panic!()
    };
    // println!("{:#?}", parser);
    let Ok(lexer) = Lexer::new(&grammar.terminals, &grammar.ignore_terminals) else {
        panic!()
    };
    let store = dfa_mask_store(&lexical_terminals, &model_vocabulary, &parser, 2);
    let starting_state = terminal.advance(terminal.start_state(), candidate_string);
    // println!("{:#?}", store);
    assert_eq!(
        store
            .get(&(
                terminal.name.to_string(),
                starting_state,
                vec![Terminal::new("L_PAREN", r"\(", 0).name.to_string(),]
            ))
            .unwrap(),
        &vec![true, false, false, false, true, false],
    );
}
