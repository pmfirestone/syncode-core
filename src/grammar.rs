// src/grammar.rs
/*!
Parse [Lark's EBNF](https://lark-parser.readthedocs.io/en/stable/grammar.html)
and turn it into a `[crate::types::Grammar]` object.

The comments of this file make liberal use of text from Lark's grammar and
documentation, without explicit attribution.

Limitations:
We do not currently implement the following Lark behaviors (pull requests welcome!):
- Directives (the statements beginning with %), except %ignore, which we do implement.
- Priorities for either rules or terminals.
- Templates that will be expanded by the preprocessor.
- Square brackets for maybes. These are replaced by parentheses followed by a question mark.
- Magic intial characters for identifiers (underscore, question mark, and exclaimation point).
- Aliases for productions.
- Case insensitive string literals.

We provide versions of grammars where these features have been removed or inlined as applicable, as well as a formal specification of the subset of the grammar we implement.

The algorithm, at a high level, behaves as follows:

1. Scan the entire input once to identify all the terminals and non-terminals,
assigning them an index in the grammar. We assign maps between their names and
their index, and for terminals we also construct a vector of their respective
DFAs that can be indexed by the terminals' indices.

2. Parse the input, constructing "sugary" productions that contain grouping, choice, and quantifying operations, in addition to terminals and nonterminals.

3. "Desugar" the productions by repeatedly expanding groupings, choices, and quantifiers until the production yields only terminals and nonterminals.

4. Convert the members of the productions into their numerical indices rather than their string names and construct the Grammar struct that this module ultimately returns.
!*/
use std::collections::HashMap;

use crate::types::*;
use std::rc::Rc;
use regex::Regex;
use regex_automata::{dfa::dense::DFA, util::lazy::Lazy};

/// Track the current state of parsing.
///
/// Only one of these structs will exist throughout the life of the program,
/// and it will be mutated a lot.
struct EBNFParser {
    /// The current position in the input.
    cur_pos: usize,
    /// The index of the next terminal.
    next_terminal_idx: usize,
    /// The index of the next nonterminal.
    next_nonterminal_idx: usize,
    /// The map from terminals to indices.
    terminal_to_idx: HashMap<Box<str>, usize>,
    /// The map from nonterminals to indices.
    nonterminal_to_idx: HashMap<Box<str>, usize>,
    /// The map from terminal indices to DFAs.
    idx_to_dfa: HashMap<usize, Rc<DFA<Vec<u32>>>>,
    /// The name of the starting rule in the grammar.
    starting_rule_name: Box<str>,
    /// The ebnf grammar that we are currently parsing.
    input_string: Box<str>,
    /// The grammar that we will eventually return.
    grammar: Grammar,
    /// The current line.
    cur_line: usize,
    /// The current column.
    cur_column: usize,
    /// The name of the rule we are currently parsing (i.e. the nonterminal on
    /// the production's left-hand side).
    cur_rule_name: Box<str>,
    /// The name of the token we are currently parsing. "" if none.
    cur_token_name: Box<str>,
    /// The right-hand side of the rule or token we are currently parsing.
    cur_rhs: Vec<Symbol>,
}

/// The terminals of the grammar description language.

const STRING: &str = r#"\".*?(?<!\)(\\)*?\"i?"#;
const NL: &str = r"(\r?\n)+\s*";

// Anchor these to the beginning of the string, because we will be using the regex
// to pop the terminal off the beginning of the input.
const REGEXP: &str = r"/(?!/)(\/|\\|[^/])*?/[imslux]*";
const REGEXP_RE: Lazy<Regex> = Lazy::new(|| Regex::new(REGEXP).unwrap());
const OP: &str = r"^[+*]|[?](?![a-z])";
const OP_RE: Lazy<Regex> = Lazy::new(|| Regex::new(OP).unwrap());
const VBAR: &str = r"^((\r?\n)+\s*)?\|";
const VBAR_RE: Lazy<Regex> = Lazy::new(|| Regex::new(VBAR).unwrap());
const NUMBER: &str = r"^(+|-)?[0-9]+";
const NUMBER_RE: Lazy<Regex> = Lazy::new(|| Regex::new(NUMBER).unwrap());
const RULE: &str = r"^[a-z][_a-z0-9]*";
const RULE_RE: Lazy<Regex> = Lazy::new(|| Regex::new(RULE).unwrap());
const TOKEN: &str = r"^[A-Z][_A-Z0-9]*";
const TOKEN_RE: Lazy<Regex> = Lazy::new(|| Regex::new(TOKEN).unwrap());

/// A production with all the quantifiers and groupings and so on.
struct SugaryProduction {
    /// The nonterminal that generates this production.
    lhs: String,
    /// The symbols on the right hand side of the production.
    rhs: Vec<String>
}

impl EBNFParser {
    fn new(input_string: &str, starting_rule_name: &str) -> Self {
        EBNFParser {
            cur_pos: 0,
	    next_terminal_idx: 1,
	    next_nonterminal_idx: 1,
	    terminal_to_idx: HashMap::new(),
	    nonterminal_to_idx: HashMap::new(),
	    idx_to_dfa: HashMap::new(),
            starting_rule_name: starting_rule_name.into(),
            input_string: input_string.into(),
            grammar: Grammar {
                productions: vec![],
                terminals: vec![],
                nonterminals: vec![],
		start_nonterminal: 1
            },
            cur_line: 1,
            cur_column: 1,
            cur_rule_name: "".into(),
            cur_token_name: "".into(),
            cur_rhs: vec![],
        }
    }
    /// Parse input_string into a Grammar.
    ///
    /// The grammar will be "augmented", which means that the start rule will
    /// always be a production whose right-hand side is a single non-terminal. This
    /// is necessary for the algorithm in `[crate::table]`, which assumes this
    /// characteristic.
    fn parse(mut self) -> Grammar {
        // First pass: build index of nonterminals and terminals.
        while self.cur_pos <= self.input_string.len() {
            // Advance past whitespace and comments.
            self.consume_space();
            // item: rule | token
            if RULE_RE.is_match_at(&self.input_string, self.cur_pos) {
		self.parse_rule();
            } else if TOKEN_RE.is_match_at(&self.input_string, self.cur_pos) {
		self.parse_token();
            } else {
                self.report_parse_error("Invalid item.");
            }
        }
        self.grammar
    }

    /// Parse a rule.
    ///
    /// `rule: RULE priority? ":" expansions`
    ///
    /// Note that the params that are present in the syntax have already been
    /// expanded by the time this procedure is called.
    fn parse_rule(&mut self) {
        let rule_match = RULE_RE.find_at(&self.input_string, self.cur_pos).unwrap();

        self.cur_rule_name = rule_match.as_str().into();
	self.nonterminal_to_idx.insert(self.cur_rule_name.clone(), self.next_nonterminal_idx);
        self.consume(rule_match.len());

        self.consume_space();

        if self.peek(0) == ':' {
            self.consume(1);
        } else {
            self.report_parse_error("Expected ':'.");
        }

        self.parse_expansions();

        // self.cur_rule contains the lhs of the production.
        // self.cur_rhs contains the rhs of the production.

        // We are no longer parsing a rule.
        self.cur_rule_name = "".into();
	self.next_nonterminal_idx += 1;
    }

    /// Parse a token.
    ///
    /// `token: TOKEN priority? ":" expansions`
    ///
    /// Note that the params that are present in the syntax have already been
    /// expanded by the time this procedure is called.
    fn parse_token(&mut self) {
        let token_match = TOKEN_RE.find_at(&self.input_string, self.cur_pos).unwrap();

        self.cur_token_name = token_match.as_str().into();
        self.consume(token_match.len());

        if self.peek(0) == ':' {
            self.consume(1);
        } else {
            self.report_parse_error("Expected ':'.");
        }

        self.parse_expansions();

        // self.cur_rule contains the lhs of the production.
        // self.cur_rhs contains the rhs of the production.

        // We are no longer parsing a token.
        self.cur_token_name = "".into();
    }

    /// Parse a statement of the grammar.
    ///
    /// ```
    /// statement: "%ignore" expansions                    -> ignore
    ///          | "%declare" name+                        -> declare
    /// ```
    ///
    /// Note that %import, %override, and %extend directives will have already
    /// been expanded by the preprocessor by the time this procedure is called.
    fn parse_statement(&mut self) {
        match &self.input_string[self.cur_pos..self.cur_pos + 3] {
            // 3 characters are sufficient to disambiguate.
            "%ig" => {
                self.consume("%ignore".len());
//                self.parse_ignore()
            }
            _ => self.report_parse_error("Invalid statement."),
        }
    }


    /// Parse the expansions of the rule.
    ///
    /// `?expansions: alias (_VBAR alias)*`
    ///
    /// From this point on, we are on the right side of the rule. All of the
    /// procedures from here on down will, eventually, append to the right-hand
    /// side under construction.
    fn parse_expansions(&mut self) {
	self.parse_expression();
	self.consume_space();
        while VBAR_RE.is_match_at(&self.input_string, self.cur_pos) {
	    self.consume(1); // Eat the |.
	    self.consume_space();
            self.parse_expression();
	    self.consume_space();
        }
    }

    /// Parse an expression.
    ///
    /// `expr: atom (OP | "~" NUMBER (".." NUMBER))?`
    ///
    /// This level of the grammar separates the quantifiers from the thing they
    /// quantify.
    fn parse_expression(&mut self) {
        self.parse_atom();
        self.consume_space();
        if OP_RE.is_match_at(&self.input_string, self.cur_pos) {
	    
        } else if self.peek(0) == '~' {
            self.consume(1);
            self.consume_space();
            self.handle_range();
        }
    }

    /// Parse an atom.
    ///
    /// ```
    /// atom: "(" expansions ")"
    ///     | value
    /// ```
    ///
    /// An atom is the part of the expression before the quantifier. Note that
    /// square brackets will have been replaced with "(" expansions ")" "?"
    /// during preprocessing.
    fn parse_atom(&mut self) {
        if self.peek(0) == '(' {
            self.consume(1);
            self.parse_expansions();
            if self.peek(0) == ')' {
                self.consume(1);
            } else {
                self.report_parse_error("Expected ')'.");
            }
        } else {
            self.parse_value();
        }
    }

    /// Parse a value.
    ///
    /// ```
    /// value: STRING ".." STRING -> literal_range
    ///     | name
    ///     | (REGEXP | STRING) -> literal
    /// ```
    /// Note that template usages will already have been expanded during preprocessing.
    fn parse_value(&mut self) {
        match self.peek(0) {
            '"' => {
                // Could be a STRING or a STRING ".." STRING.  We need more
                // than one token of lookahead here, so go to a special
                // handler.
                self.handle_string_or_literal_range();
            }
            '/' => {
                self.parse_regex();
            }
            _ => {
                self.parse_name();
            }
        }
    }

    /// Parse a name.
    ///
    /// `name: RULE | TOKEN`
    fn parse_name(&mut self) {
        if RULE_RE.is_match_at(&self.input_string, self.cur_pos) {
            let rule_match = RULE_RE.find_at(&self.input_string, self.cur_pos).unwrap();
            self.cur_rhs
                .push(Symbol::NonTerminal(rule_match.as_str().into()));
            self.consume(rule_match.len());
        } else if TOKEN_RE.is_match_at(&self.input_string, self.cur_pos) {
            let token_match = TOKEN_RE.find_at(&self.input_string, self.cur_pos).unwrap();
            // The tokens can appear in the input before they are defined. Options:
            // 1. Scan the input once for terminal definitions and again for nonterminals.
            //
            //    Pros: allows us to include the defined tokens (i.e. regexes)
            //    directly in the production definition when the production is
            //    defined.
            //
            //    Cons: forces us to scan the input twice, filtering for
            //    terminals the first time and nonterminals the second.
            //
            // 2. Represent terminals simply by their name, resolving the
            // definitions after the grammar is parsed.
            //
            //    Pros: requires a single scan of the input, makes it simple to
            //    define production results.
            //
            //    Cons: forces us to look up the definition of the terminal
            //    after parsing so that we can use it in the lexer and mask
            //    store.
            //
            // The basic question is how we represent terminals and
            // nonterminals internally, and how and whether the way we do so
            // differs depending on the module using them.
            self.cur_rhs
                .push(Symbol::Terminal(/* The terminal whose name is token_match.as_str() */));
            self.consume(token_match.len());
        } else {
            self.report_parse_error("Expected a RULE or a TOKEN.");
        }
    }

    /// Parse a regular expression.
    ///
    /// `REGEXP: /\/(?!\/)(\\\/|\\\\|[^\/])*?\/[imslux]*/`
    ///
    /// Let this procedure handle the opening and closing slashes.
    ///
    /// REGEXPs can appear in terminal definitions, in which case they are part
    /// of that terminal's final regex, or they can appear in rule expansions,
    /// in which case they are an anonymous terminal that is a literal symbol
    /// in the rule. Our parser treats all literals as terminals, so in either
    /// event we make a new terminal and put it in the rhs we are building.
    fn parse_regex(&mut self) {
        self.consume(1);
        let Some(re_match) = REGEXP_RE.find_at(&self.input_string, self.cur_pos) else {
            // Make the "else does not diverge" warning go away.
            return self.report_parse_error("Failed to parse regular expression.");
        };

        // Make the new terminal.
        self.cur_rhs.push(Symbol::Terminal(Terminal::new(
            re_match.as_str(),
            re_match.as_str(),
            0, // I guess the anonymous terminals are the default priority?
        )));

        self.consume(re_match.len());
        // Eat the final slash.
        self.consume(1);
    }

    // The quantifiers require their own special handling, because they create
    // changes to the grammar.
    // Options for handling:
    //
    // 1. Modify the grammar as a string before parsing it.
    //
    //    Pros: the resulting grammar can be easily parsed into the form needed
    //    for creating an LR table.
    //
    //    Cons: string manipulations can be hellacious and easily break the
    //    syntax of the grammar description language.
    //
    // 2. Modify the productions the grammar produces after parsing.
    //
    //    Pros: manipulating data structures is more reliable.
    //
    //    Cons: requires an intermediate representation for quantified
    //    productions before they are turned into the productions used for the
    //    grammar.
    //
    // The basic dichotomy is whether to handle the quantifiers before or after
    // parsing the grammar file.

    /// Handle the quantifiers ?, *, and +.
    ///
    /// `item?` - Zero or one instances of item (”maybe”).
    ///
    /// Expanded into an alternative between two results, one where the
    /// quantified item is present and one where it is absent.
    /// ```
    /// a b? c   ->   a c | a b c
    /// ```
    ///
    /// `item*` - Zero or more instances of item:
    /// Extracted into recursion.
    /// ```
    /// a: b*     ->   a: _b_tag
    ///                _b_tag: (_b_tag b)?
    /// ```
    ///
    /// `item+` - One or more instances of item.
    /// Extracted into recursion with one instance of the item prefaced to the top rule:
    /// ```
    /// a: b+     ->   a: b _b_tag
    ///                _b_tag: (_b_tag b)?
    /// ```
    ///
    /// These transformations must be applied repeatedly until the input
    /// contains no more quantifiers. Since the * and + quantifiers introduce
    /// new ? quantifiers, the *s and +s must be addressed first, then all the
    /// ?s must be resolved.
    fn handle_op(&mut self) {}

    /// Handle a range of repetitions of the annotated item.
    ///
    /// `item ~ n` - Exactly n instances of item.
    /// This can be expanded by simply repeating the item in-place:
    /// ```
    /// item ~ n    ->   item item item... // n times
    /// ```
    /// `item ~ n..m` - Between n to m instances of item.
    /// This requires the introduction of new options on this production:
    /// ```
    /// item ~ n..m ->   item item item... // n times, item? item? item?... m-n times.
    /// ```
    fn handle_range(&mut self) {}

    // There are a number of structures in the grammar that modify it. The
    // question for all of these is which of the following approaches to take:
    //
    // 1. Handle them in preprocessing, i.e. as a manipulating to the input string before parsing
    //
    //    Pros: these special behaviors entirely disappear from all subsequent
    //    steps of evaluation, simplifying them.
    //
    //    Cons: string manipulations are difficult to do correctly, and these
    //    are relatively complicated behaviors.
    //
    // 2. Handle them during parsing as part of the grammar of the language.
    //
    //    Pros: they are all defined in the syntax of the language and can be
    //    parsed by the same algorithm we are already using.
    //
    //    Cons: it isn't clear at all how to implement the semantics of these
    //    structures as part of the generation of the grammar structure.
    //
    // 3. Manipulate the grammar after the fact to implement these behaviors.
    //
    //    Pros: avoids the question of how to implement while parsing.
    //
    //    Cons: some of these directives introduce things that themselves must
    //    be parsed, so parsing can't truly be complete until they are all
    //    effected in the grammar.

    /// Expand the templates in the grammar.
    ///
    /// Definition syntax:
    /// `my_template{param1, param2, ...}: <EBNF EXPRESSION>`
    /// Use syntax:
    /// `some_rule: my_template{arg1, arg2, ...}`
    /// Example:
    /// ```
    /// _separated{x, sep}: x (sep x)*  // Define a sequence of 'x sep x sep x ...'
    /// num_list: "[" _separated{NUMBER, ","} "]"   // Will match "[1, 2, 3]" etc.
    ///  ```
    /// This is a macro expander that modifies the input string in place.
    fn expand_templates(&mut self) {}

    /// Expand the imports in the grammar.
    ///
    /// When importing rules, all their dependencies will be imported into a
    /// namespace, to avoid collisions. It’s not possible to override their
    /// dependencies (e.g. like you would when inheriting a class).
    ///
    /// Syntax:
    ///    ```
    /// %import <module>.<TERMINAL>
    /// %import <module>.<rule>
    /// %import <module>.<TERMINAL> -> <NEWTERMINAL>
    /// %import <module>.<rule> -> <newrule>
    /// %import <module> (<TERM1>, <TERM2>, <rule1>, <rule2>)
    /// ```
    ///
    /// If the module path is absolute, [we] will attempt to load it from the
    /// built-in directory (which currently contains common.lark, python.lark,
    /// and unicode.lark).
    ///
    /// If the module path is relative, such as .path.to.file, [we] will
    /// attempt to load it from the current working directory. Grammars must
    /// have the .lark extension.
    ///
    /// The rule or terminal can be imported under another name with the -> syntax.
    fn expand_imports(&mut self) {}

    /// Expand override directives in the grammar.
    ///
    /// Override a rule or terminals, affecting all references to it, even in imported grammars.
    ///
    /// Useful for implementing an inheritance pattern when importing grammars.
    ///
    /// Example:
    /// ```
    /// %import my_grammar (start, number, NUMBER)
    /// // Add hex support to my_grammar
    /// %override number: NUMBER | /0x\w+/
    /// ````
    ///
    /// There is no requirement for a rule/terminal to come from another file,
    /// but that is probably the most common use case.
    fn expand_overrides(&mut self) {}

    /// Expand extend directives in the grammar.
    ///
    /// Extend the definition of a rule or terminal, e.g. add a new option on
    /// what it can match, like when separated with |.
    ///
    /// Useful for splitting up a definition of a complex rule with many
    /// different options over multiple files.
    ///
    /// Can also be used to implement a plugin system where a core grammar is extended by others.
    ///
    /// Example:
    /// ```
    /// %import my_grammar (start, NUMBER)
    ///
    /// // Add hex support to my_grammar
    /// %extend NUMBER: /0x\w+/
    /// ```
    ///
    /// There is no requirement for a rule/terminal to come from another file,
    /// but that is probably the most common use case.
    fn expand_extends(&mut self) {}

    /// Replace square brackets with ()?.
    ///
    /// `[item item ..]` is a maybe statement. It's the same as (item item
    /// ..)?, but when maybe_placeholders=True, Lark generates None if there is
    /// no match. since we don't care about this behavior (because we aren't
    /// generating an abstract syntax tree), we simplify parsing by replacing
    /// the brackets with parentheses and a question mark.
    fn replace_square_brackets(&mut self) {}

    /// Consume the specified number of characters, maintaining line and column number.
    fn consume(&mut self, count: usize) {
        for _ in 0..count {
            // Newline advances line count, except when "\n\r"? This logic is
            // verbatim from xgrammar.
            if self.peek(0) == '\n' || (self.peek(0) == '\r' && self.peek(1) != '\n') {
                self.cur_line += 1;
                self.cur_column = 1;
            } else {
                self.cur_column += 1;
            }
            self.cur_pos += 1;
        }
    }

    /// Peek the character delta ahead of the current one.
    fn peek(&self, delta: usize) -> char {
        // Evil because the Rust str and String abstractions are surprisingly
        // leaky, which makes them substantially harder to work with than in
        // other languages. This gets, strictly speaking, the (input_pos +
        // delta)th character in the input.
        self.input_string.chars().nth(self.cur_pos + delta).unwrap()
    }

    /// Consume the next whitespace and comment in the input.
    fn consume_space(&mut self) {
        while self.cur_pos < self.input_string.len()
            && (self.peek(0) == ' '
                || self.peek(0) == '\t'
                || self.peek(0) == '#'
                || self.peek(0) == '/' && self.peek(1) == '/')
        {
            if self.peek(0) == '#' || self.peek(0) == '/' && self.peek(1) == '/' {
                self.consume_to_end_of_line();
            }
        }
    }

    /// Consume to the end of the line.
    ///
    /// Useful for skipping aliases and comments.
    fn consume_to_end_of_line(&mut self) {
        while self.cur_pos < self.input_string.len() {
            if !(self.peek(0) == '\n' || (self.peek(0) == '\r' && self.peek(0) == '\n')) {
                self.consume(1);
            } else {
                break;
            }
        }
    }

    /// Report a parse error with the line and column number. This procedure
    /// will panic!() when called.
    fn report_parse_error(&self, message: &str) {
        panic!(
            "GBNF parse error at line {}, column {}: {message}",
            self.cur_line, self.cur_column
        );
    }
}
