// src/parser.rs
//! The parser for SynCode. Takes in a lexed sequence of tokens and determines
//! the accept sequences that could follow.

use std::collections::HashSet;
use std::fmt;

use crate::table::tables;
use crate::types::*;

// Rust RFC 1733 introduces this syntax as a way to alias bounds, which would
// make this module much more readable. Unfortunately, as of 2025-05-09, the
// behavior is not yet stable. See
// https://github.com/rust-lang/rfcs/blob/master/text/1733-trait-alias.md.

impl Parser {
    /// Construct a new parser from a grammar.
    pub fn new(grammar: &Grammar) -> Parser {
        let Ok(lexer) = Lexer::new(grammar.terminals.clone(), HashSet::new()) else {
            panic!("Could not construct lexer.")
        };
        let (action_table, goto_table) = tables(grammar.clone());
        Parser {
            lexer,
            action_table,
            goto_table,
            start_state: 0,
            token_index: 0,
        }
    }

    /// Return all the terminals that could come after this one, regardless of
    /// the state the parser is in.
    pub fn next_terminals(&self, terminal: &String) -> Vec<String> {
        let states_that_accept_this_terminal: Vec<usize> = self
            .action_table
            .keys()
            .filter(|key| key.1 == *terminal)
            .map(|key| key.0)
            .collect();
        let mut terminals_that_could_follow_this_one: Vec<String> = Vec::new();
        for state in states_that_accept_this_terminal {
            // Suppose we've accepted this terminal.
            let Ok(next_state) = self.next(terminal, vec![state]) else {
                panic!("Bloody Nora, you've made a real dog's breakfast out of this one.")
            };
            terminals_that_could_follow_this_one.extend(self.follow(&next_state))
        }
        terminals_that_could_follow_this_one
    }

    /// Return the terminals that the parser will accept in the current state.
    pub fn follow(&self, state_stack: &[usize]) -> Vec<String> {
        self.action_table
            .keys()
            .filter(|key| key.0 == *state_stack.last().unwrap())
            .map(|key| key.1.clone())
            .collect()
    }

    /// Feed a token to the parser and process it according to the LR(1) algorithm.
    ///
    /// The inner loop of the LR parsing algorithm.
    pub fn next(
        &self,
        terminal: &String,
        state_stack: Vec<usize>,
    ) -> Result<Vec<usize>, ParserError> {
        // This implementation is verbose because of the error handling
        // involved. Perhaps there's a way to make it more streamlined by
        // consolidating the error-managing boiler plate.
        let mut state_stack = state_stack;

        loop {
            // Get the current state.
            let Some(state) = state_stack.last() else {
                return Err(ParserError::StackUnderflow);
            };

            // Get the action for this state and terminal.
            let Some(action) = self.action_table.get(&(*state, terminal.clone())) else {
                return Err(ParserError::InvalidState(*state));
            };

            // eprintln!("Current state: {:?}, Token: {:?}", state, token);
            // eprintln!("Action: {:?}", action);
            // eprintln!("Transitions: {:?}", states.get(&state));

            // Dispatch on action types.
            match action {
                Action::Shift(next_state) => {
                    // Just push next state on shift.
                    state_stack.push(*next_state);
                    return Ok(state_stack); // Not yet accepted.
                }

                Action::Reduce(rule) => {
                    // On a reduce, pop states according to the rule expansion length.
                    let size = rule.rhs.len();

                    if size > 0 {
                        // Pop the appropriate number of states.
                        for _ in 0..size {
                            if state_stack.pop().is_none() {
                                return Err(ParserError::StackUnderflow);
                            }
                        }
                    }

                    // Look up the current state.
                    let Some(current_state) = state_stack.last() else {
                        return Err(ParserError::StackUnderflow);
                    };

                    // Get the next state for this state and nonterminal.
                    let Some(next_state) = self.goto_table.get(&(*current_state, rule.lhs.clone()))
                    else {
                        return Err(ParserError::InvalidState(*current_state));
                    };

                    // Make this the new current state.
                    state_stack.push(*next_state);
                }

                Action::Accept => {
                    // We're probably never going to reach this case, and there
                    // isn't really anything for us to do if we do.
                    return Ok(state_stack);
                }

                _ => {
                    // Anything else is an Error action.
                    return Err(ParserError::SyntaxError(format!(
                        "Parser error at token: {}",
                        terminal,
                    )));
                }
            }
        }
    }

    /// Parse tokens without building a tree, just producing the accept
    /// sequences.
    ///
    /// Take in the tokens successfully lexed so far and the remainder, which
    /// is either the last token, if the end of that token coincides with the
    /// end of the string generated so far, or the part of the string after the
    /// end of the last token successfully lexed.
    ///
    /// This procedure is like Algorithm 4 from the paper, except that it takes
    /// in the tokens and remainder and returns only the accept sequences.
    pub fn parse(
        &self,
        tokens: Vec<Token>,
        remainder: Token,
    ) -> Result<HashSet<Vec<String>>, ParserError> {
        let mut a0: Vec<String> = Vec::new();
        let mut a1: Vec<String> = Vec::new();

        let last_token = tokens[tokens.len() - 1].clone();
        let mut state_stack = vec![self.start_state];

        for token in &tokens[..] {
            let terminal = token.clone().terminal.unwrap().name.clone();
            // FIXME: There must be a less horrid way to do this.
            let Ok(new_state_stack) = self.next(&terminal, state_stack) else {
                break;
            };
            state_stack = new_state_stack;
            a0 = a1;
            a1 = self.follow(&state_stack);
        }

        // There are two cases for accept sequences. See section 4.5 of the
        // paper and Algorithm 4, lines 15-21.
        let mut accept_sequences: HashSet<Vec<String>> = HashSet::new();
        if last_token == remainder {
            // Case 1: the remainder is the last lexical token.
            let remainder_type = remainder.clone().terminal.unwrap().name;
            for terminal in a1 {
                accept_sequences.insert(vec![remainder_type.clone(), terminal]);
            }
            for terminal in a0 {
                accept_sequences.insert(vec![terminal]);
            }
        } else {
            // Case 2: the remainder is some unparsed nonsense.
            for terminal in a1 {
                accept_sequences.insert(vec![terminal]);
            }
        }
        Ok(accept_sequences)
    }
}

// Error types for the parser
#[derive(Debug, Clone)]
pub enum ParserError {
    UnexpectedToken {
        token: Token,
        expected: Vec<Terminal>,
        state_index: usize,
    },
    UnexpectedEof,
    LexerError,
    StackUnderflow,
    EmptyStack,
    InvalidState(usize),
    InvalidAction(String),
    /// We got a terminal that doesn't work in this state.
    InvalidTerminal(
        /// Actual.
        Terminal,
        /// Expected.
        Vec<Terminal>,
    ),
    InvalidToken,
    SyntaxError(String),
    ConfigError(String),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserError::UnexpectedToken {
                token,
                expected,
                state_index: _,
            } => {
                write!(
                    f,
                    "Unexpected token '{:?}' (type: {}) at line {}, column {}. Expected one of: {:?}",
                    token.value,
                    token.terminal.clone().unwrap().name,
                    token.line,
                    token.column,
                    expected
                )
            }
            ParserError::UnexpectedEof => {
                write!(f, "Unexpected end of input")
            }
            ParserError::LexerError => {
                write!(f, "Lexer error",)
            }
            ParserError::StackUnderflow => {
                write!(f, "Parser stack underflow")
            }
            ParserError::EmptyStack => {
                write!(f, "Parser stack is empty")
            }
            ParserError::InvalidState(msg) => {
                write!(f, "Invalid parser state: {}", msg)
            }
            ParserError::InvalidAction(msg) => {
                write!(f, "Invalid parser action: {}", msg)
            }
            ParserError::SyntaxError(msg) => {
                write!(f, "Syntax error: {}", msg)
            }
            ParserError::ConfigError(msg) => {
                write!(f, "Parser configuration error: {}", msg)
            }
            ParserError::InvalidTerminal(actual, _expected) => {
                // FIXME: Implement fmt for Vec<Terminal.>
                write!(
                    f,
                    "Unexpected terminal. Got: {}.",
                    actual // , expected
                )
            }
            ParserError::InvalidToken => {
                write!(
                    f,
                    "Received a token without a terminal that wasn't the remainder."
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use super::*;
    use crate::grammar::EBNFParser;
    // Terminal definitions to be used throughout tests. Commented out ones may
    // come in handy in future tests but are commented to avoid dead code warnings.
    fn word() -> Terminal {
        Terminal::new("WORD", r"[a-zA-Z_]\w*", 2)
    }

    // fn string() -> Terminal {
    //     Terminal::new("STRING", r#"("""[^"]*"""|'''[^']*''')"#, 2)
    // }

    fn space() -> Terminal {
        Terminal::new("SPACE", "\\s+", 0)
    }

    // fn equals() -> Terminal {
    //     Terminal::new("EQUALS", "=", 1)
    // }

    // fn dot() -> Terminal {
    //     Terminal::new("DOT", r"\.", 1)
    // }

    fn dec_number() -> Terminal {
        Terminal::new("DEC_NUMBER", r"0|[1-9]\d*", 1)
    }

    // fn oct_number() -> Terminal {
    //     Terminal::new("OCT_NUMBER", r"(?i)0o[0-7]+", 1)
    // }

    // fn bin_number() -> Terminal {
    //     Terminal::new("BIN_NUMBER", r"(?i)0b[0-1]+", 1)
    // }

    // fn hex_number() -> Terminal {
    //     Terminal::new("HEX_NUMBER", r"(?i)0x[\da-f]+", 1)
    // }

    // fn float_number() -> Terminal {
    //     Terminal::new(
    //         "FLOAT_NUMBER",
    //         r"((\d+\.\d*|\.\d+)(e[-+]?\d+)?|\d+(e[-+]?\d+))",
    //         1,
    //     )
    // }

    // fn semicolon() -> Terminal {
    //     Terminal::new("SEMICOLON", ";", 0)
    // }

    // fn newline() -> Terminal {
    //     Terminal::new("NEWLINE", r"\n", 1)
    // }

    fn star() -> Terminal {
        Terminal::new("STAR", r"\*", 1)
    }

    fn plus() -> Terminal {
        Terminal::new("PLUS", r"\+", 1)
    }

    /// A convenience terminal representing the end of the input.
    fn eof() -> Terminal {
        Terminal::new("$", "", 0)
    }

    // Mega-simple grammar courtesy of https://en.wikipedia.org/wiki/LR_parser.
    fn calc_rules() -> Vec<Production> {
        vec![
            Production {
                lhs: "goal".into(),
                rhs: vec!["sums".into(), "$".into()],
            },
            Production {
                lhs: "sums".into(),
                rhs: vec!["sums".into(), "PLUS".into(), "products".into()],
            },
            Production {
                lhs: "sums".into(),
                rhs: vec!["products".into()],
            },
            Production {
                lhs: "products".into(),
                rhs: vec!["products".into(), "STAR".into(), "value".into()],
            },
            Production {
                lhs: "products".into(),
                rhs: vec!["value".into()],
            },
            Production {
                lhs: "value".into(),
                rhs: vec!["DEC_NUMBER".into()],
            },
            Production {
                lhs: "value".into(),
                rhs: vec!["WORD".into()],
            },
        ]
    }

    fn calc_action_table() -> ActionTable {
        let rules = calc_rules();
        HashMap::from([
            ((0, "DEC_NUMBER".into()), Action::Shift(8)),
            ((0, "WORD".into()), Action::Shift(9)),
            ((1, "PLUS".into()), Action::Shift(2)),
            ((1, "$".into()), Action::Accept),
            ((2, "DEC_NUMBER".into()), Action::Shift(8)),
            ((2, "WORD".into()), Action::Shift(9)),
            ((3, "STAR".into()), Action::Shift(5)),
            ((3, "PLUS".into()), Action::Reduce(rules[1].clone())),
            ((4, "STAR".into()), Action::Shift(5)),
            ((4, "PLUS".into()), Action::Reduce(rules[2].clone())),
            ((4, "$".into()), Action::Reduce(rules[2].clone())),
            ((5, "DEC_NUMBER".into()), Action::Shift(8)),
            ((5, "WORD".into()), Action::Shift(9)),
            ((6, "STAR".into()), Action::Reduce(rules[3].clone())),
            ((6, "PLUS".into()), Action::Reduce(rules[3].clone())),
            ((6, "$".into()), Action::Reduce(rules[3].clone())),
            ((7, "STAR".into()), Action::Reduce(rules[4].clone())),
            ((7, "PLUS".into()), Action::Reduce(rules[4].clone())),
            ((7, "$".into()), Action::Reduce(rules[4].clone())),
            ((8, "STAR".into()), Action::Reduce(rules[5].clone())),
            ((8, "PLUS".into()), Action::Reduce(rules[5].clone())),
            ((8, "$".into()), Action::Reduce(rules[5].clone())),
            ((9, "STAR".into()), Action::Reduce(rules[6].clone())),
            ((9, "PLUS".into()), Action::Reduce(rules[6].clone())),
            ((9, "$".into()), Action::Reduce(rules[6].clone())),
        ])
    }

    fn calc_goto_table() -> GotoTable {
        HashMap::from([
            ((0, "sums".into()), 1),
            ((0, "products".into()), 4),
            ((0, "value".into()), 7),
            ((2, "products".into()), 3),
            ((2, "value".into()), 7),
            ((5, "value".into()), 6),
        ])
    }

    fn calc_parser() -> Parser {
        let action_table = calc_action_table();
        let goto_table = calc_goto_table();

        let Ok(lexer) = Lexer::new(
            vec![word(), star(), dec_number(), plus(), space()],
            HashSet::from([space()]),
        ) else {
            panic!()
        };

        Parser {
            lexer,
            action_table,
            goto_table,
            start_state: 0,
            token_index: 0,
        }
    }

    #[test]
    fn calc_grammar_step_through_states() {
        let parser = calc_parser();
        let terminals_to_parse: Vec<String> = vec![
            "WORD".into(),
            "STAR".into(),
            "DEC_NUMBER".into(),
            "PLUS".into(),
            "DEC_NUMBER".into(),
        ];
        let state_stack: Vec<usize> = vec![parser.start_state];

        let Ok(state_stack) = parser.next(&terminals_to_parse[0], state_stack) else {
            panic!()
        };
        assert_eq!(9, *state_stack.last().unwrap());

        let Ok(state_stack) = parser.next(&terminals_to_parse[1], state_stack) else {
            panic!()
        };
        assert_eq!(5, *state_stack.last().unwrap());

        let Ok(state_stack) = parser.next(&terminals_to_parse[2], state_stack) else {
            panic!()
        };
        assert_eq!(8, *state_stack.last().unwrap());

        let Ok(state_stack) = parser.next(&terminals_to_parse[3], state_stack) else {
            panic!()
        };
        assert_eq!(2, *state_stack.last().unwrap());

        let Ok(state_stack) = parser.next(&terminals_to_parse[4], state_stack) else {
            panic!()
        };
        assert_eq!(8, *state_stack.last().unwrap());
    }

    #[test]
    fn end_to_end_parse() {
        let parser = calc_parser();

        let input = "A * 2 + 1".as_bytes();

        let Ok((tokens, remainder)) = parser.lexer.lex(input) else {
            panic!()
        };

        let Ok(accept_sequences) = parser.parse(tokens, remainder.clone()) else {
            panic!()
        };

        assert_eq!(
            Token {
                value: "1".as_bytes().into(),
                terminal: Some(dec_number()),
                start_pos: 8,
                end_pos: 9,
                line: 1,
                end_line: 1,
                column: 9,
                end_column: 10
            },
            remainder
        );
        // It's not clear to me exactly what the semantics are here
        // w.r.t. whitespace and other ignored terminals. Also, how do we deal
        // with the possibilty that the remainder could change lexical type,
        // even to types that aren't permitted? As it is, the algorithm allows
        // a DEC_NUMBER to change to a WORD, even though that isn't actually
        // possible in the grammar: semantically, what this says is that
        // instead of being a DEC_NUMBER, the last lexical token could have
        // been a WORD, which is technically true. Nevertheless, we already
        // have enough information to know that the last token couldn't
        // possibly become a WORD and could only continue to be a DEC_NUMBER.
        assert_eq!(
            HashSet::from([
                vec!["DEC_NUMBER".into(), "STAR".into()],
                vec!["DEC_NUMBER".into(), "PLUS".into()],
                vec!["DEC_NUMBER".into(), "$".into()],
                vec!["WORD".into()],
                vec!["DEC_NUMBER".into()]
            ]),
            accept_sequences
        );
    }

    #[test]
    fn terminals_after_this_one() {
        let grammar = Grammar {
            symbol_set: vec![
                "start".to_string(),
                "L_PAREN".to_string(),
                "R_PAREN".to_string(),
                "IDENTIFIER".to_string(),
            ],
            terminals: vec![
                Terminal::new("L_PAREN", r"\(", 0),
                Terminal::new("R_PAREN", r"\)", 0),
                Terminal::new("IDENTIFIER", r"[a-zA-Z_]*", 0),
            ],
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
        };
        let parser = Parser::new(&grammar);
        assert_eq!(
            parser.next_terminals(&"L_PAREN".to_string()),
            vec!["R_PAREN".to_string()]
        );
    }

    #[test]
    fn parse_simple_grammar() {
        let grammar = EBNFParser::new("s: c c\nc: \"C\" c | \"D\"", "s").parse();
        let parser = Parser::new(&grammar);
        eprintln!("{:#?}", parser.action_table);
        let Ok((tokens, remainder)) = parser.lexer.lex(b"CC") else {
            panic!()
        };
        eprintln!("{:#?}", parser.parse(tokens, remainder));
    }

    #[test]
    fn parse_json() {
        use crate::grammar::EBNFParser;
        use std::fs;

        let parser = Parser::new(
            &EBNFParser::new(
                &fs::read_to_string("./grammars/json.lark").unwrap(),
                "?start",
            )
            .parse(),
        );

        //       eprintln!(
        //           "{:#?}",
        //           parser.parse(
        //               r#"{
        // "basics": {
        //   "name": "Preston Firestone",
        //   "label": "Programmer",
        //   "image": "",
        //   "email": "pf8@illinois.edu",
        //   "phone": "+1 (224) 688-2924",
        //   "summary": "Master's Student in Computer Science",
        //   "location": {
        //     "address": "2064 W Hutchinson St APT 1",
        //     "postalCode": "60618",
        //     "city": "Chicago",
        //     "countryCode": "USA",
        //     "region": "Illinois"
        //   },"#
        //               .as_bytes()
        //           )
        //       );
    }
}
