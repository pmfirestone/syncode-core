use crate::terminal::Terminal;
use std::cell::LazyCell;
use std::sync::Arc;

/// A lexical token, what the lexer breaks the input into.
#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    /// The content of the token.
    pub value: Arc<[u8]>,
    /// The type of terminal that this is in the grammar. None if this token
    /// couldn't be lexed, which can happen in the case that this is the
    /// unlexable remainder.
    pub terminal: Option<Terminal>,
    /// Where in the input the token begins.
    pub start_pos: usize,
    /// Where in the input the token ends.
    pub end_pos: usize,
}

/// A special empty token for convenience.
pub const EMPTY_TOKEN: LazyCell<Token> = LazyCell::new(|| Token {
    value: [].into(),
    terminal: None,
    start_pos: 0,
    end_pos: 0,
});
