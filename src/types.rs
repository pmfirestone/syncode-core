// src/types.rs
//! Core types used throughout SynCode.

use regex_automata::{
    Anchored,
    dfa::{Automaton, dense},
    util::primitives::StateID,
    util::start::Config,
};

use std::cmp::PartialEq;
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::{cell::LazyCell, rc::Rc};

/// A symbol of the grammar is either a terminal or a nonterminal.
///
/// Each is identified by a unique number.
// #[derive(Clone, Debug, Hash, Eq, PartialEq)]
// pub enum Symbol {
//     Terminal(usize),
//     NonTerminal(usize),
// }

// /// Convenience type bindings to make the definitions of grammars more readable.
// type Terminal = usize;
// type NonTerminal = usize;

// /// A grammar has four components.
// #[derive(Clone, Debug, Hash, Eq, PartialEq)]
// pub struct Grammar {
//     /// The set of terminals that are in this grammar.
//     pub terminals: Vec<Terminal>,
//     /// The set of nonterminals that are active in this grammar.
//     pub nonterminals: Vec<NonTerminal>,
//     /// The first production; this one is the augmented one added to the grammar.
//     pub start_nonterminal: NonTerminal,
//     /// The productions that make up this grammar, including the start_production.
//     pub productions: Vec<(NonTerminal, Vec<Symbol>)>,
// }

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

/// A terminal of the grammar.
#[derive(Clone)]
pub struct Terminal {
    /// The name of this terminal in the grammar.
    pub name: String,
    /// The regex describing this terminal.
    pub pattern: String,
    /// The DFA that matches this terminal.
    pub dfa: Box<dense::DFA<Vec<u32>>>,
    /// This terminal's priority in lexing.
    pub priority: i32,
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
    pub lhs: String,
    /// The right hand side of the production.
    pub rhs: Vec<String>,
    // The priority of this production. Used to resolve conflicts when constructing the table.
    // pub priority: i32,
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
    /// The set of terminals that are in this grammar. Each terminal is itself
    /// a production of the grammar, because the grammar will be put in with
    /// regexes on the right-hand side of the definitions, including the
    /// definitions of terminals, which can be in terms of other terminals or
    /// regexes.
    pub terminals: Vec<Production>,
    /// The original start symbol, not the augmented one we've added.
    pub start_symbol: String,
    /// The productions that make up this grammar, including the start_production.
    pub productions: Vec<Production>,
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

// Implementations.
impl Terminal {
    /// Construct a new terminal.
    pub fn new(name: &str, pattern: &str, priority: i32) -> Self {
        let Ok(dfa) = dense::DFA::new(pattern) else {
            panic!(
                "While constructing the terminal {name}, could not build a DFA from the pattern {pattern}"
            )
        };
        Terminal {
            name: name.into(),
            pattern: pattern.into(),
            dfa: dfa.into(),
            priority,
        }
    }

    /// Get the initial state for this terminal's DFA.
    pub fn start_state(&self) -> StateID {
        let Ok(start_state) = self.dfa.start_state(&Config::new().anchored(Anchored::Yes)) else {
            panic!();
        };
        start_state
    }

    /// Return the state that this terminal's DFA ends up in after consuming these bytes.
    pub fn advance(&self, mut state: StateID, string: &[u8]) -> StateID {
        for &b in string {
            state = self.dfa.next_state(state, b);
        }
        state
    }
}

impl fmt::Display for Terminal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Terminal({}, {}, {})",
            self.name, self.pattern, self.priority
        )
    }
}

impl fmt::Debug for Terminal {
    /// We don't care about the DFA for the purpose of printing.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Terminal")
            .field("name", &self.name)
            .field("pattern", &self.pattern)
            .field("priority", &self.priority)
            .finish()
    }
}

impl Hash for Terminal {
    /// We don't care about the DFA for the purpose of hashing.
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.pattern.hash(state);
        self.priority.hash(state);
    }
}

impl PartialEq for Terminal {
    /// We don't care about the DFA for the purpose of equality comparison.
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.pattern == other.pattern && self.priority == other.priority
    }
}

impl Eq for Terminal {}

/// A convenience terminal representing the empty string.
pub const EPSILON: LazyCell<Terminal> = LazyCell::new(|| Terminal::new("EPSILON", "", 0));
