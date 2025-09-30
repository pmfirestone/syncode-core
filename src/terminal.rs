use regex_automata::{
    Anchored,
    dfa::{Automaton, dense},
    util::primitives::StateID,
    util::start::Config,
};
use std::cell::LazyCell;
use std::fmt;
use std::hash::Hash;
use std::sync::Arc;
/// A terminal of the grammar.
#[derive(Clone)]
pub struct Terminal {
    /// The name of this terminal in the grammar.
    pub name: String,
    /// The regex describing this terminal.
    pub pattern: String,
    /// The DFA that matches this terminal.
    pub dfa: Arc<dense::DFA<Vec<u32>>>,
    /// This terminal's priority in lexing.
    pub priority: i32,
}

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
