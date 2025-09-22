// src/mask.rd
//! The mask store for SynCode, implementing the basic algorithm from the paper.

use crate::dfa::all_dfa_states;
use crate::types::*;
use core::iter::Iterator;
use rayon::prelude::*;
use regex_automata::dfa::dense;
use regex_automata::{Anchored, dfa::Automaton, util::primitives::StateID, util::start::Config};
use std::collections::HashSet;
use std::iter::zip;
use std::{collections::HashMap, vec::Vec};

/// DFA mask store stores, for each possible position we could be at in
/// matching each terminal, and for each possible accept sequence that could
/// follow, the binary mask over the vocabulary that indicates the possible
/// acceptable continuations.
///
/// We represent the key in the implementation by a `String`
/// (which is a proxy for a DFA), a
/// `[regex_automata::util::primitives::StateId]`, which indicates where we are
/// in the accepting of that terminal, and a vector of `String`s representing
/// the accept sequence in question.
///
/// > For an integer α, the DFA mask store Mα is a function defined as Mα :
/// > QΩ × Γα → {0, 1}|V |, where QΩ = ⋃ τ ∈Γ Qτ represents the set of all DFA
/// > states and Γα is a set of α-length terminal sequences. Then Mα(q, Λ) = m is
/// > a binary mask such that t ∈ set(m) if dmatch(t, q, Λ).
type DFAMaskStore = HashMap<(String, StateID, Vec<String>), Vec<bool>>;

/// Compute whether the string could match a sequence of terminals starting at a certain state in the first DFA.
///
/// Given a DFA D(Q, Σ, δ, q0, F ), a string w ∈ Σ∗, a DFA state q ∈ Q and any sequence of terminals Λ = {τf +1, τf +2 . . . τf +d}, dmatch(w, q, Λ) = true, if either of the following conditions hold:
/// 1. δ∗(w, q) ∈ live(Q) or
/// 2. ∃w1 ∈ Σ∗, w2 ∈ Σ+ such that w1.w2 = w, δ∗(w1, q) ∈ F and Λ = {} or
/// 3. ∃w1 ∈ Σ∗, w2 ∈ Σ∗ such that w1.w2 = w, δ∗(w1, q) ∈ F, and dmatch(w2, qτf +10 , {τf +2 . . . τf +d}) = true where qτf +10 is the start state corresponding to the DFA for τf +1.
pub fn dmatch(
    input: &[u8],
    dfa: &dense::DFA<Vec<u32>>,
    starting_state: &StateID,
    accept_sequence: &[Terminal],
) -> bool {
    // We'll need to access the starting state later, but we also need
    // something we can mutate as we feed things into the DFA.
    let mut state: StateID = *starting_state;

    // Case 1: the DFA, starting at this state, consumes the entire input and is still alive.
    for &b in input {
        state = dfa.next_state(state, b);
    }
    // Neither dead nor quit means we could match in the future and so are live.
    if !(dfa.is_dead_state(state) || dfa.is_quit_state(state)) {
        return true;
    }

    // Case 2: The DFA consumes a prefix of the string, leaves a non-zero
    // suffix, and there is no sequence of terminals to follow. Assume that
    // grammars respect the maximum munch principle, so w1 is the maximal
    // matching prefix.
    state = *starting_state; // Reset to initial state.
    let mut index_reached: usize = 0;
    for (i, &b) in input.iter().enumerate() {
        state = dfa.next_state(state, b);
        if dfa.is_dead_state(state) | dfa.is_quit_state(state) {
            // We've failed to match, so stop feeding tokens in.
            break;
        }

        if dfa.is_match_state(state) {
            // We haven't yet failed to match, so keep count of how far
            // we've gotten in the input.
            index_reached = i;
        }
    }

    if index_reached > 0 && accept_sequence.is_empty() {
        // We matched at least some bytes and have no more terminals
        // to check.
        return true;
    }

    // Case 3: A prefix of the string is successfully consumed by the DFA, and
    // dmatch is true starting at the next member of sequence_of_terminals.
    state = *starting_state;
    for (i, &b) in input.iter().enumerate() {
        state = dfa.next_state(state, b);

        if !dfa.is_dead_state(state) {
            // Keep munching as long as we're alive.
            continue;
        }

        if dfa.is_dead_state(state) && i == 0 {
            // We failed on the first character, so give up.
            break;
        }

        // We've consumed at least one byte, the DFA is now dead, and there
        // are more terminals to check.
        if i > 0 && dfa.is_dead_state(state) && !accept_sequence.is_empty() {
            let new_dfa = &accept_sequence[0].dfa;

            return dmatch(
                &input[i - 1..],
                &new_dfa.clone(),
                &new_dfa
                    .start_state(&Config::new().anchored(Anchored::Yes))
                    .unwrap(),
                &accept_sequence[1..],
            );
        }
    }

    // None of the previous cases succeeded, so dmatch is false.
    false
}

/// Compute the mask for a given DFA state, accept sequence, and vocabulary.
///
/// Mα(q, Λ) = m is a binary mask such that t ∈ set(m) if dmatch(t, q, Λ),
/// where t is a string (token in the LLM's vocabulary), q is a DFA state, and
/// Λ is an accept sequence.
pub fn dfa_mask(
    dfa: &dense::DFA<Vec<u32>>,
    starting_state: &StateID,
    accept_sequence: &Vec<Terminal>,
    vocabulary: &Vec<Vec<u8>>,
) -> Vec<bool> {
    let mut mask: Vec<bool> = Vec::with_capacity(vocabulary.len());
    for token in vocabulary {
        mask.push(dmatch(token, dfa, starting_state, accept_sequence));
    }
    mask
}

/// Compute the grammar mask store.
///
/// The mask store is constructed offline by enumerating all DFA states QΩ,
/// considering all possible terminals in Γ, and all tokens in V. The DFA mask
/// store depends on the set of terminals Γ and the model’s vocabulary V. As a
/// result, a unique mask store is created for each grammar and tokenizer
/// combination, and to enhance efficiency, we cache and reuse this table for
/// future inferences.
pub fn dfa_mask_store(
    lexical_terminals: &Vec<Terminal>,
    model_vocabulary: &Vec<Vec<u8>>,
    parser: &Parser,
    _length_of_terminal_sequences: usize,
) -> DFAMaskStore {
    let all_states = all_dfa_states(lexical_terminals);
    let mut store: DFAMaskStore = HashMap::new();
    for (terminal, state_id) in &all_states {
        // For now, hard-code the lookahead of two terminals.
        // let next_terminals = parser.next_terminals(&terminal.name);
        for next_terminal in lexical_terminals {
            // let after_next_terminals = parser.next_terminals(next_terminal);
            for after_next_terminal in lexical_terminals {
                let accept_sequence_names =
                    vec![next_terminal.name.clone(), after_next_terminal.name.clone()];
                let accept_sequence_terminals = vec![
                    parser
                        .grammar
                        .terminal_from_name(&next_terminal.name)
                        .unwrap(),
                    parser
                        .grammar
                        .terminal_from_name(&after_next_terminal.name)
                        .unwrap(),
                ];
                let dfa = &terminal.dfa;
                store.insert(
                    (terminal.name.clone(), *state_id, accept_sequence_names),
                    dfa_mask(dfa, state_id, &accept_sequence_terminals, &model_vocabulary),
                );
            }
        }
    }
    store
}

/// Compute the mask for a given accept sequence and remainder.
///
/// Implement algorithm 2 from the paper.
pub fn grammar_mask(
    accept_sequences: &HashSet<Vec<String>>,
    remainder: &Token,
    mask_store: DFAMaskStore,
    model_vocabulary: &Vec<Vec<u8>>,
    grammar: &Grammar,
) -> Vec<bool> {
    let mut mask: Vec<bool> = vec![false; model_vocabulary.len()];
    for accept_sequence in accept_sequences {
        let first_terminal = grammar.terminal_from_name(&accept_sequence[0]).unwrap();
        let start_state = first_terminal.start_state();
        let end_state = first_terminal.advance(start_state, &remainder.value);
        // Check whether the DFA ended up in a live state.
        if !(first_terminal.dfa.is_dead_state(end_state)
            || first_terminal.dfa.is_quit_state(end_state))
        {
            // Take the union of the mask we've computed so far and the mask from the store.
            for (i, (cur, new)) in zip(
                mask.clone(),
                // Get the relevant mask out of the store.
                mask_store
                    .get(&(
                        first_terminal.name,
                        end_state,
                        accept_sequence[1..].to_vec(),
                    ))
                    .unwrap(),
            )
            .enumerate()
            {
                mask[i] = cur || *new;
            }
        }
    }
    mask
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dmatch_unittests() {
        let tests: Vec<(&[u8], Terminal, &[u8], Vec<Terminal>, bool)> = vec![
            (
                &[0xE2, 0x88],
                Terminal::new("FORALL", r"∀", 0),
                b"",
                vec![Terminal::new("FORALL", r"∀", 0)],
                true,
            ),
            (
                b"abbacde",
                Terminal::new("AB", "[ab]*", 0),
                b"ab",
                vec![Terminal::new("c", "c", 0)],
                true,
            ),
            (
                b"abcab",
                Terminal::new("A_OR_B", r"[ab]+", 0),
                b"",
                vec![Terminal::new("A_OR_B", r"[ab]+", 0)],
                false,
            ),
            // Illustrative example from page 12 of the paper.
            (
                b"_prime():",
                Terminal::new("IDENTIFIER", r"[a-zA-Z_]*", 0),
                b"is",
                vec![
                    Terminal::new("L_PAREN", r"\(", 0),
                    Terminal::new("R_PAREN", r"\)", 0),
                ],
                true,
            ),
            // False in strict mode, true in overapproximation mode (grammar mask).
            (
                b"abbacdd",
                Terminal::new("", r"[ab]*", 0),
                b"",
                vec![],
                true,
            ),
            (b"abba", Terminal::new("", r"[ab]*cd", 0), b"", vec![], true),
            (
                b"3not an id",
                Terminal::new("IDENTIFIER", r"[a-zA-Z_]*", 0),
                b"",
                vec![
                    Terminal::new("L_PAREN", r"\(", 0),
                    Terminal::new("R_PAREN", r"\)", 0),
                ],
                false,
            ),
            (
                b"indeed",
                Terminal::new("IDENTIFIER", r"[a-zA-Z_]*", 0),
                b"",
                vec![
                    Terminal::new("L_PAREN", r"\(", 0),
                    Terminal::new("R_PAREN", r"\)", 0),
                ],
                true,
            ),
        ];

        for (candidate_string, terminal, start_string, accept_sequence, succeeds) in tests {
            assert_eq!(
                dmatch(
                    candidate_string,
                    &terminal.dfa,
                    &terminal.advance(terminal.start_state(), start_string),
                    &accept_sequence
                ),
                succeeds
            );
        }
    }

    #[test]
    fn test_dfa_mask_name() {
        // Illustrative example from page 13 of the paper.
        let vocabulary = vec![
            "_prime():".as_bytes().into(),
            ":#".as_bytes().into(),
            "¡".as_bytes().into(),
            " hi".as_bytes().into(),
            "indeed".as_bytes().into(),
            "n0pe".as_bytes().into(),
        ];
        let terminal_sequence = vec![
            Terminal::new("L_PAREN", r"\(", 0),
            Terminal::new("R_PAREN", r"\)", 0),
        ];
        let terminal = Terminal::new("IDENTIFIER", r"[a-zA-Z_]*", 0);
        let starting_state = terminal.advance(terminal.start_state(), b"is");
        assert_eq!(
            dfa_mask(
                &terminal.dfa,
                &starting_state,
                &terminal_sequence,
                &vocabulary
            ),
            vec![true, false, false, false, true, false]
        );
    }

    #[test]
    fn test_dfa_mask_store() {
        let model_vocabulary: Vec<Vec<u8>> = vec![
            "_prime():".as_bytes().into(),
            [0xE2, 0x88].into(),
            "'''".as_bytes().into(),
            " hi".as_bytes().into(),
            "indeed".as_bytes().into(),
            "n0pe".as_bytes().into(),
        ];
        let lexical_terminals = vec![
            Terminal::new("L_PAREN", r"\(", 0),
            Terminal::new("R_PAREN", r"\)", 0),
            Terminal::new("IDENTIFIER", r"[a-zA-Z_]*", 0),
            Terminal::new("$", "", 0),
        ];
        let candidate_string = b"is";
        let terminal = Terminal::new("IDENTIFIER", r"[a-zA-Z_]*", 0);
        let grammar = Grammar {
            symbol_set: vec![
                "L_PAREN".to_string(),
                "R_PAREN".to_string(),
                "IDENTIFIER".to_string(),
                "$".to_string(),
            ],
            terminals: lexical_terminals.clone(),
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
        // println!("{:#?}", parser);
        let Ok(lexer) = Lexer::new(grammar.terminals) else {
            panic!()
        };
        let store = dfa_mask_store(&lexical_terminals, &model_vocabulary, &parser, 2);
        let starting_state = terminal.advance(terminal.start_state(), candidate_string);
        // println!("{:#?}", store);
        assert_eq!(
            store
                .get(&(
                    terminal.name,
                    starting_state,
                    vec![
                        Terminal::new("L_PAREN", r"\(", 0).name,
                        Terminal::new("R_PAREN", r"\)", 0).name,
                    ]
                ))
                .unwrap(),
            &vec![true, false, false, false, true, false],
        );
    }
}
