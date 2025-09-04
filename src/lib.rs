// src/lib.rs
/*!
[SynCode](https://arxiv.org/abs/2403.01632) is a library for constrained
generation. It forces an LLM to generate sequences that satisfy a given LR
grammar.

The interface presented to the user has two parts: a constructor and a masker.

The constructor takes in a model vocabulary and a context-free grammar and
returns a masker struct.

That struct has a single public function that takes in an array of bytes, the
model's generated output so far, and returns a mask, a boolean array, over the
model's vocabulary.

Applying this mask to the model's vocabulary should, provided it was
constructed with sufficient lookahead, prevent the model from ever going
outside of the grammar.

!*/
// pub mod desugar;
// pub mod dfa;
pub mod grammar;
// pub mod lexer;
// pub mod mask;
// pub mod parser;
// pub mod table;
pub mod types;
