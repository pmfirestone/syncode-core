use bstr::ByteSlice;
// tests/generation.rs
use syncode_core::grammar::EBNFParser;
use syncode_core::mask::{dfa_mask_store, grammar_mask};
use syncode_core::types::*;

use std::fs;

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
    ];

    let Ok(parser) = Parser::new(&grammar) else {
        panic!()
    };

    // eprintln!("action_table: {:#?}", parser.action_table);

    let Ok(lexer) = Lexer::new(&grammar.terminals) else {
        panic!()
    };

    let mask_store = dfa_mask_store(&grammar.terminals, &model_vocabulary, &parser, 2);

    let prefix = b"1+1";

    let Ok((tokens, remainder)) = lexer.lex(prefix) else {
        panic!()
    };

    // eprintln!("remainder: {}", remainder.value.as_bstr());
    // eprintln!("tokens: {:#?}", tokens);

    let Ok(accept_sequences) = parser.parse(tokens, remainder.clone()) else {
        panic!()
    };

    // eprintln!("{:#?}", accept_sequences);

    let mask = grammar_mask(
        &accept_sequences,
        &remainder,
        mask_store,
        &model_vocabulary,
        &grammar,
    );

    assert_eq!(mask, vec![true, false, false, true, true]);
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
        productions: vec![Production {
            lhs: "start".to_string(),
            rhs: vec![
                "IDENTIFIER".to_string(),
                "L_PAREN".to_string(),
                "R_PAREN".to_string(),
                "start".to_string(),
            ],
        }],
        ignore_terminals: vec![],
    };
    let Ok(parser) = Parser::new(&grammar) else {
        panic!()
    };
    // println!("{:#?}", parser);
    let Ok(lexer) = Lexer::new(&grammar.terminals) else {
        panic!()
    };
    let store = dfa_mask_store(&lexical_terminals, &model_vocabulary, &parser, 2);
    let starting_state = terminal.advance(terminal.start_state(), candidate_string);
    // println!("{:#?}", store);
    assert_eq!(
        store
            .get(&(
                terminal.name,
                starting_state,
                vec![
                    Terminal::new("L_PAREN", r"\(", 0).name,
                    Terminal::new("R_PAREN", r"\)", 0).name,
                ]
            ))
            .unwrap(),
        &vec![true, false, false, false, true, false],
    );
}
