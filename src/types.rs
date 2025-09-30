// src/types.rs
//! Core types used throughout SynCode.

use regex_automata::{
    Anchored,
    dfa::{Automaton, dense},
    util::primitives::StateID,
    util::start::Config,
};

use serde::{Deserialize, Serialize};

use std::cmp::PartialEq;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::Hash;
use std::{cell::LazyCell, rc::Rc, sync::Arc};

/// A lexical token, what the lexer breaks the input into.
#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    /// The content of the token.
    pub value: Rc<[u8]>,
    /// The type of terminal that this is in the grammar. None if this token
    /// couldn't be lexed, which can happen in the case that this is the
    /// unlexable remainder.
    pub terminal: Option<Terminal>,
    /// Where in the input the token begins.
    pub start_pos: usize,
    /// Where in the input the token ends.
    pub end_pos: usize,
    /// The line of the input the token begins on.
    pub line: usize,
    /// The line of the input the token ends on.
    pub end_line: usize,
    /// The column of the input the token begins on.
    pub column: usize,
    /// The column of the input the token ends on.
    pub end_column: usize,
}


/// A non-terminal of the grammar.
#[derive(Clone)]
pub struct NonTerminal {
    /// The name of this nonterminal.
    pub name: String,
}

/// A single production of the grammar.
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Production {
    /// The left hand side of the production.
    pub lhs: Arc<String>,
    /// The right hand side of the production.
    pub rhs: Arc<Vec<String>>,
    // The priority of this production. Used to resolve conflicts when constructing the table.
    // pub priority: i32,
}

impl Production {
    /// Convenience constructor for tests.
    pub fn new(lhs: &str, rhs: Vec<&str>) -> Production {
        Production {
            lhs: lhs.to_string().into(),
            rhs: rhs
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .into(),
        }
    }
}

/// A context-free grammar.
///
/// For now, distinguish between terminals and nonterminals by capitalization:
/// nonterminals are lowercase and terminals are capitalized. This is highly
/// limited: for one, not all characters have a case (languages such as Arabic,
/// Hebrew, Chinese, Japanese, Korean lack case in their writing systems); for
/// another, it imposes a silly formality on the user.
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Grammar {
    /// The set of symbols that are active in this grammar.
    pub symbol_set: Vec<String>,
    /// The set of terminals that are in this grammar.
    pub terminals: Vec<Terminal>,
    /// The original start symbol, not the augmented one we've added.
    pub start_symbol: String,
    /// The productions that make up this grammar, including the start_production.
    pub productions: Vec<Production>,
    /// The termnals that the lexer should ignore. FIXME: It's an aberration
    /// that this is here, because this field is not part of the abstract idea
    /// of what a grammar is.
    pub ignore_terminals: Vec<String>,
}

/// An item of the item set for LR parsing.
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Item {
    /// The production that this item contains.
    pub production: Production,
    /// The position of the dot in the result. Invariant: must be in [0, result.len()].
    pub dot: usize,
    /// The look ahead terminal.
    pub lookahead: String,
}

/// Action enum for LR parsing.
#[derive(Clone, Debug, PartialEq)]
pub enum Action {
    /// Consume a terminal from input, going to the indicated state.
    Shift(usize),
    /// Reduce the symbols on the stack according to the production.
    Reduce(Production),
    /// Accept the input.
    Accept,
    /// Fail to accept the input.
    Error,
}

/// An action table is a map from a (state_id, terminal) pair to an action.
pub type ActionTable = HashMap<(usize, String), Action>;

/// A goto table is a map from a (state_id, nonterminal) pair to a state_id.
pub type GotoTable = HashMap<(usize, String), usize>;

/// The Parser with its tables.
///
/// We do not include the state stack as part of the parser struct, since it is
/// easier by far to handle this struct as an immutable value and keep the
/// stack as an argument that is passed in and out for each call.
#[derive(Clone, Debug)]
pub struct Parser {
    /// The action table.
    pub action_table: ActionTable,
    /// The goto table.
    pub goto_table: GotoTable,
    /// The index of the state to start at.
    pub start_state: usize,
    /// The grammar this parser parses, for future reference.
    pub grammar: Grammar,
}


impl Grammar {
    /// Find the terminal of this name.
    pub fn terminal_from_name(&self, name: &String) -> Option<Terminal> {
        for terminal in &self.terminals {
            if terminal.name == *name {
                return Some(terminal.clone());
            }
        }
        None
    }
}

