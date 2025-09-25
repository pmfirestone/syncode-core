use std::collections::HashSet;
use syncode_core::grammar::EBNFParser;
use syncode_core::types::{Lexer, Parser};

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

    let Ok(accept_sequences) = parser.parse(tokens, remainder) else {
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

#[test]
fn parse_json() {
    use std::fs;

    let Ok(grammar) = &EBNFParser::new(
        &fs::read_to_string("./grammars/json.lark").unwrap(),
        "start",
    )
    .parse() else {
        panic!()
    };

    // eprintln!("Grammar: {:#?}", grammar);

    let Ok(parser) = Parser::new(&grammar) else {
        panic!()
    };

    let Ok(lexer) = Lexer::new(&grammar.terminals, &grammar.ignore_terminals) else {
        panic!()
    };

    // eprintln!("{:#?}", grammar.terminals);

    // eprintln!("Action table: {:#?}", parser.action_table);
    // eprintln!("Goto table: {:#?}", parser.goto_table);

    let Ok((tokens, remainder)) = lexer.lex(
        r##"{
  "basics": {
    "name": "Preston Firestone",
    "label": "Programmer",
    "image": "",
    "email": "pf8@illinois.edu",
    "phone": "+1 (224) 688-2924",
    "summary": "Master's Student in Computer Science","##
            .as_bytes(),
    ) else {
        panic!()
    };

    // eprintln!("{:#?}", tokens);
    // eprintln!("{:#?}", remainder);

    let Ok(accept_sequences) = parser.parse(tokens, remainder) else {
        panic!()
    };

    eprintln!("{:#?}", accept_sequences);
}
