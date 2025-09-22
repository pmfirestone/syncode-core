// src/lib.rs
//! A library for constrained generation using LLMs.
//!
//! We make far more of the crate public than is reasonable to facilitate
//! development and to document the internals of the crate.
//!
//! For an example of use see the file `examples/construct_mask_store.rs`. See
//! individual crates for further documentation.
//!
#![doc = include_str!("../README.md")]

pub mod bytes;
pub mod dfa;
pub mod grammar;
pub mod lexer;
pub mod mask;
pub mod parser;
pub mod table;
pub mod types;
