#![feature(test)]
extern crate test;
use std::collections::HashSet;
use std::fs;
use syncode_core::grammar::EBNFParser;
use syncode_core::{lexer::Lexer, parser::Parser};

#[bench]
fn build_json_parser(b: &mut test::Bencher) {
    let Ok(grammar) =
        EBNFParser::new(&fs::read_to_string("grammars/json.lark").unwrap(), "json").parse()
    else {
        panic!()
    };

    b.iter(|| Parser::new(&grammar));
}

#[bench]
fn parse_json(b: &mut test::Bencher) {
    let text = fs::read_to_string("tests/large-file.json").unwrap();
    let grammar = EBNFParser::new(&fs::read_to_string("grammars/json.lark").unwrap(), "json")
        .parse()
        .unwrap();

    let parser = Parser::new(&grammar).unwrap();

    let lexer = Lexer::new(&grammar.terminals, &grammar.ignore_terminals).unwrap();
    let (tokens, remainder) = lexer.lex(text.as_bytes()).unwrap();

    b.iter(|| parser.parse(&tokens, &remainder));
}

// #[bench]
// fn build_go_parser(b: &mut test::Bencher) {
//     let Ok(grammar) =
//         EBNFParser::new(&fs::read_to_string("grammars/go.lark").unwrap(), "start").parse()
//     else {
//         panic!()
//     };

//     b.iter(|| Parser::new(&grammar));
// }

#[test]
fn parse_simple_grammar() {
    let Ok(grammar) = EBNFParser::new("s: c c\nc: \"C\" c | \"D\"", "s").parse() else {
        panic!()
    };

    let Ok(parser) = Parser::new(&grammar) else {
        panic!()
    };

    let Ok(lexer) = Lexer::new(&grammar.terminals, &grammar.ignore_terminals) else {
        panic!();
    };

    let Ok((tokens, remainder)) = lexer.lex(b"CC") else {
        panic!()
    };

    let Ok(accept_sequences) = parser.parse(&tokens, &remainder) else {
        panic!()
    };

    assert_eq!(
        HashSet::from([
            vec!["__ANONYMOUS_LITERAL_D_2".to_string()],
            vec![
                "__ANONYMOUS_LITERAL_C_1".to_string(),
                "__ANONYMOUS_LITERAL_D_2".to_string()
            ],
            vec!["__ANONYMOUS_LITERAL_C_1".to_string()],
            vec![
                "__ANONYMOUS_LITERAL_C_1".to_string(),
                "__ANONYMOUS_LITERAL_C_1".to_string()
            ]
        ]),
        accept_sequences
    );
}
