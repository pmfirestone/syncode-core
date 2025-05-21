// src/lib.rs
/*!
[SynCode](https://arxiv.org/abs/2403.01632) is a library for constrained
generation. It forces an LLM to generate sequences that satisfy a given LR
grammar.
!*/
pub mod mask;
pub mod parser;
pub mod lexer;
pub mod dfa;
pub mod grammar;
pub mod table;
pub mod types;
