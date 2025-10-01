// src/grammar.rs
//! A context-free grammar and associated functions.

use crate::production::Production;
use crate::terminal::Terminal;

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

impl Grammar {
    /// Check whether a given symbol is a terminal in this grammar.
    fn is_terminal(&self, symbol: &String) -> bool {
        // eprintln!("symbol: {symbol}");
        for terminal in &self.terminals {
            // eprintln!("{terminal}");
            if *symbol == *terminal.name {
                return true;
            }
        }
        return false;
    }

    /// A variable is nullable if it can derive the empty string.
    fn is_nullable(&self, variable: &String) -> bool {
        // Implementation based on the algorithm from Hopcraft, Motwani, and
        // Ullman (2007) "Introduction to Automata Theory, Languages, and
        // Computation", sec. 7.1.3.
        for production in self.productions.iter().filter(|p| *p.lhs == *variable) {
            // If A -> "" is a production of G, then A is nullable.
            if *production.rhs == vec![""] {
                return true;
            }
        }

        // If there is a production B -> C1C2..Ck where each Ci is nullable,
        // then B is nullable. Note that each Ci must be a variable to be
        // nullable, so we only have to consider productions with all variable
        // bodies.
        for production in self.productions.iter().filter(|p| {
            *p.lhs == *variable
                && p.rhs
                    .iter()
                    .map(|s| !self.is_terminal(s))
                    .collect::<Vec<bool>>()
                    .iter()
                    .fold(true, |a, b| a && *b)
        }) {
            if production
                .rhs
                .iter()
                .map(|s| s != variable && self.is_nullable(s))
                .fold(true, |a, b| a && b)
            {
                return true;
            }
        }

        return false;
    }

    /// Remove all epsilon-productions from the grammar.
    fn eliminate_epsilon_productions(&self) -> Grammar {
        // Implementation based on the algorithm from Hopcraft, Motwani, and
        // Ullman (2007) "Introduction to Automata Theory, Languages, and
        // Computation", sec. 7.1.3.
        let mut productions: Vec<Production> = Vec::new();

        // For each production A -> X1X2..Xk of P, where k >= 1 suppose that m
        // of the k Xi's are nullable symbols. The new grammar G' will have m
        // versions of this production where the nullable Xi's in all possible
        // combinations are present or absent.
        for production in self.productions.iter() {
            if *production.rhs == vec![""] {
                // If a production of the form A -> "" is in P, we do not place
                // this production in P'.
                continue;
            }

            // Count the number of nullable symbols.
            let mut nullables = 0;
            for variable in production.rhs.iter() {
                if self.is_nullable(variable) {
                    nullables += 1;
                }
            }
            let total_variants = 2_usize.pow(nullables);

            let mut new_righthand_sides: Vec<Vec<&str>> = Vec::with_capacity(total_variants);

            for _ in [0..total_variants] {
                new_righthand_sides.push(vec![]);
            }

            for symbol in production.rhs.iter() {
                let old_righthand_sides = new_righthand_sides.clone();
                for new_rhs in &mut new_righthand_sides {
                    new_rhs.push(symbol);
                }
                if self.is_nullable(symbol) {
                    // If the symbol is nullable, save the version of the
                    // right-hand sides without this symbol appended to them.
                    new_righthand_sides.extend(old_righthand_sides);
                }
            }

            if nullables as usize == production.rhs.len() {
                // If m = k i.e. all symbols are nullable, then we do not include
                // the case where all Xi's are absent.
                new_righthand_sides = new_righthand_sides
                    .into_iter()
                    .filter(|v| v.len() > 0)
                    .collect();
            }

            for new_rhs in new_righthand_sides {
                let new_production = Production::new(&production.lhs, new_rhs);
                if !productions.contains(&new_production) {
                    // Avoid redundant productions.
                    productions.push(new_production);
                }
            }
        }

        Grammar {
            symbol_set: self.symbol_set.clone(),
            terminals: self.terminals.clone(),
            start_symbol: self.start_symbol.clone(),
            ignore_terminals: self.ignore_terminals.clone(),
            productions,
        }
    }

    /// Return the set of unit pairs in the grammar.
    fn unit_pairs(&self) -> Vec<(String, String)> {
        // Implementation based on the algorithm from Hopcraft, Motwani, and
        // Ullman (2007) "Introduction to Automata Theory, Languages, and
        // Computation", sec. 7.1.4.
        let mut unit_pairs: Vec<(String, String)> = Vec::new();

        for symbol in self.symbol_set.iter() {
            // (A, A) is a unit pair for any variable A. That is, A -*> A by
            // zero steps.
            if !self.is_terminal(&symbol) {
                unit_pairs.push((symbol.clone(), symbol.clone()));
            }
        }

        loop {
            let mut new_unit_pairs = vec![];

            for (a, b) in &unit_pairs {
                // Suppose we have determined that (a, b) is a unit pair.
                for production in &self.productions {
                    if *production.lhs == **b
                        && production.rhs.len() == 1
                        && !self.is_terminal(&production.rhs[0])
                    {
                        if !unit_pairs.contains(&(a.clone(), production.rhs[0].clone())) {
                            // There exists a unit production such that B -> C
                            // and we haven't already added this unit pair.
                            new_unit_pairs.push((a.clone(), production.rhs[0].clone()));
                        }
                    }
                }
            }

            if new_unit_pairs.is_empty() {
                // Nothing more to add.
                break;
            } else {
                unit_pairs.extend(new_unit_pairs);
            }
        }

        return unit_pairs;
    }

    /// Eliminate all the unit productions of the grammar (productions where
    /// A->B and A and B are nonterminals).
    fn eliminate_unit_productions(&self) -> Grammar {
        // Implementation based on the algorithm from Hopcraft, Motwani, and
        // Ullman (2007) "Introduction to Automata Theory, Languages, and
        // Computation", sec. 7.1.4.
        let mut productions: Vec<Production> = Vec::new();

        for (a, b) in self.unit_pairs() {
            // For each unit pair,
            for production in self
                .productions
                .iter()
                .filter(|p| *p.lhs == b && (p.rhs.len() > 1 || self.is_terminal(&p.rhs[0])))
            {
                // add to P' all the productions A->a, where B->a is a nonunit
                // production in P.
                let new_production = Production {
                    lhs: a.clone().into(),
                    rhs: production.rhs.clone(),
                };
                if !productions.contains(&new_production) {
                    productions.push(new_production);
                }
            }
        }

        Grammar {
            symbol_set: self.symbol_set.clone(),
            terminals: self.terminals.clone(),
            start_symbol: self.start_symbol.clone(),
            ignore_terminals: self.ignore_terminals.clone(),
            productions,
        }
    }
}

#[cfg(test)]
mod test {
    use std::fs;

    use crate::ebnf::EBNFParser;

    use super::*;
    extern crate test;

    fn expr_grammar() -> Grammar {
        Grammar {
            symbol_set: vec![
                "a".into(),
                "b".into(),
                "0".into(),
                "1".into(),
                "I".into(),
                "E".into(),
                "F".into(),
                "T".into(),
                "+".into(),
                "(".into(),
                ")".into(),
                "*".into(),
            ],
            terminals: vec![
                Terminal::new("a", "a", 0),
                Terminal::new("b", "b", 0),
                Terminal::new("0", "0", 0),
                Terminal::new("1", "1", 0),
                Terminal::new("+", r"\+", 0),
                Terminal::new("(", r"\(", 0),
                Terminal::new(")", r"\)", 0),
                Terminal::new("*", r"\*", 0),
            ],
            productions: vec![
                Production::new("I", vec!["a"]),
                Production::new("I", vec!["b"]),
                Production::new("I", vec!["I", "a"]),
                Production::new("I", vec!["I", "b"]),
                Production::new("I", vec!["I", "0"]),
                Production::new("I", vec!["I", "1"]),
                Production::new("F", vec!["I"]),
                Production::new("F", vec!["(", "E", ")"]),
                Production::new("T", vec!["F"]),
                Production::new("T", vec!["T", "*", "F"]),
                Production::new("E", vec!["T"]),
                Production::new("E", vec!["E", "+", "T"]),
            ],
            start_symbol: "E".into(),
            ignore_terminals: vec![],
        }
    }

    #[test]
    fn is_nullable() {
        let grammar = Grammar {
            symbol_set: vec![
                "T".into(),
                "S".into(),
                "A".into(),
                "B".into(),
                "a".into(),
                "b".into(),
                "".into(),
            ],
            terminals: vec![Terminal::new("a", "a", 0), Terminal::new("b", "b", 0)],
            productions: vec![
                Production::new("S", vec!["a", "T", "b"]),
                Production::new("T", vec!["A", "B"]),
                Production::new("A", vec!["a", "A", "A"]),
                Production::new("A", vec![""]),
                Production::new("B", vec!["b", "B", "B"]),
                Production::new("B", vec![""]),
            ],
            start_symbol: "S".into(),
            ignore_terminals: vec![],
        };

        assert!(grammar.is_nullable(&"A".into()));
        assert!(grammar.is_nullable(&"B".into()));
        assert!(grammar.is_nullable(&"T".into()));
        assert!(!grammar.is_nullable(&"S".into()));
    }

    #[test]
    fn eliminate_epsilon_productions() {
        let grammar = Grammar {
            symbol_set: vec![
                "S".into(),
                "A".into(),
                "B".into(),
                "a".into(),
                "b".into(),
                "".into(),
            ],
            terminals: vec![Terminal::new("a", "a", 0), Terminal::new("b", "b", 0)],
            productions: vec![
                Production::new("S", vec!["A", "B"]),
                Production::new("A", vec!["a", "A", "A"]),
                Production::new("A", vec![""]),
                Production::new("B", vec!["b", "B", "B"]),
                Production::new("B", vec![""]),
            ],
            start_symbol: "S".into(),
            ignore_terminals: vec![],
        };
        let grammar_prime = grammar.eliminate_epsilon_productions();

        assert_eq!(
            grammar_prime,
            Grammar {
                symbol_set: vec![
                    "S".into(),
                    "A".into(),
                    "B".into(),
                    "a".into(),
                    "b".into(),
                    "".into(),
                ],
                terminals: vec![Terminal::new("a", "a", 0), Terminal::new("b", "b", 0)],
                productions: vec![
                    Production::new("S", vec!["A", "B"]),
                    Production::new("S", vec!["B"]),
                    Production::new("S", vec!["A"]),
                    Production::new("A", vec!["a", "A", "A"]),
                    Production::new("A", vec!["a", "A"]),
                    Production::new("A", vec!["a"]),
                    Production::new("B", vec!["b", "B", "B"]),
                    Production::new("B", vec!["b", "B"]),
                    Production::new("B", vec!["b"]),
                ],
                start_symbol: "S".into(),
                ignore_terminals: vec![],
            }
        );
    }

    #[test]
    fn unit_pairs() {
        let grammar = expr_grammar();

        assert_eq!(
            vec![
                ("I".into(), "I".into(),),
                ("E".into(), "E".into(),),
                ("F".into(), "F".into(),),
                ("T".into(), "T".into(),),
                ("E".into(), "T".into(),),
                ("F".into(), "I".into(),),
                ("T".into(), "F".into(),),
                ("E".into(), "F".into(),),
                ("T".into(), "I".into(),),
                ("E".into(), "I".into(),),
            ],
            grammar.unit_pairs()
        );
    }

    #[test]
    fn eliminate_unit_productions() {
        // Use hashset because we don't care about order of elements, only
        // contents.
        use std::collections::HashSet;
        let mut results = HashSet::new();
        results.extend(expr_grammar().eliminate_unit_productions().productions);

        assert_eq!(
            HashSet::from([
                Production::new("E", vec!["E", "+", "T"]),
                Production::new("E", vec!["T", "*", "F"]),
                Production::new("E", vec!["(", "E", ")"]),
                Production::new("E", vec!["a"]),
                Production::new("E", vec!["b"]),
                Production::new("E", vec!["I", "a"]),
                Production::new("E", vec!["I", "b"]),
                Production::new("E", vec!["I", "0"]),
                Production::new("E", vec!["I", "1"]),
                Production::new("T", vec!["T", "*", "F"]),
                Production::new("T", vec!["(", "E", ")"]),
                Production::new("T", vec!["a"]),
                Production::new("T", vec!["b"]),
                Production::new("T", vec!["I", "a"]),
                Production::new("T", vec!["I", "b"]),
                Production::new("T", vec!["I", "0"]),
                Production::new("T", vec!["I", "1"]),
                Production::new("F", vec!["(", "E", ")"]),
                Production::new("F", vec!["a"]),
                Production::new("F", vec!["b"]),
                Production::new("F", vec!["I", "a"]),
                Production::new("F", vec!["I", "b"]),
                Production::new("F", vec!["I", "0"]),
                Production::new("F", vec!["I", "1"]),
                Production::new("I", vec!["a"]),
                Production::new("I", vec!["b"]),
                Production::new("I", vec!["I", "a"]),
                Production::new("I", vec!["I", "b"]),
                Production::new("I", vec!["I", "0"]),
                Production::new("I", vec!["I", "1"]),
            ]),
            results
        );
    }

    #[bench]
    fn eliminate_epsilon_productions_json(b: &mut test::Bencher) {
        let grammar = EBNFParser::new(
            &fs::read_to_string("grammars/json_sugar.lark").unwrap(),
            "json",
        )
        .parse()
        .unwrap();

        b.iter(|| grammar.eliminate_epsilon_productions());
    }
}
