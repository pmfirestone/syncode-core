// src/grammar.rs
/*!
Parse [Lark's EBNF](https://lark-parser.readthedocs.io/en/stable/grammar.html)
and turn it into a `[crate::types::Grammar]` object.

We attempt, as far as possible, to follow the grammar of the Lark
grammar-description language as specified in the file `lark.lark`. However,
where the documented features of the language differ from those specified in
the grammar, we follow the documentation. These deviations are documented in
the procedures where the implementation is affected; the user is directed to
Lark's grammar for guidance on the grammar of the language this parser
excepts. Whereever the behavior of this module differs from the documented
behavior of Lark, this is a bug in this module. In cases where the behavior of
the module is underspecified in the grammar, we follow the implementation of
Lark as closely as is reasonable.

FIXME:
- Broken parentheses with newlines.
- Populate remaining fields of grammar object besides `start_symbol` and `productions`.

TODO:
- Implement "maybe" blocks (square brackets).
- Implement directives.

There the the following limitations on the language this parser recognizes
compared to that of Lark itself. Pull requests implementing these would be
gladly accepted.

- Terminals must be either a regular expression or a string literal. Defining
  terminals in terms of other terminals using the ordinary constructs of the
  grammar description language is not supported.

- %ignore statements must be followed by either a regular expression or a
  %string literal. Grammar-level operations are not supported.

- %import statements are not supported.

- Lookaround is not supported in regular expressions.
!*/

use crate::types::*;
use regex::{Match, Regex};
use regex_automata::util::lazy::Lazy;

#[derive(PartialEq)]
enum Item {
    RULE,
    TOKEN,
    STATEMENT,
    NONE,
}

/// Track the current state of parsing.
///
/// Only one of these structs will exist throughout the life of the program,
/// and it will be mutated a lot.
pub struct EBNFParser {
    /// The current position in the input.
    cur_pos: usize,
    /// The ebnf grammar that we are currently parsing.
    input_string: String,
    /// The grammar that we will eventually return.
    grammar: Grammar,
    /// The current line.
    cur_line: usize,
    /// The current column.
    cur_column: usize,
    /// The names of the non-terminal we are defining; treated as a
    /// stack. Everytime we descend a layer in the grammar and have to generate
    /// intermediate non-terminals, we put them on this stack; then we pop on
    /// the way back up.
    name_stack: Vec<String>,
    /// The priority of the rule or token we are currently parsing. Defaults to 0.
    cur_priority: i32,
    /// Currently parsing a rule, token, or statement?
    cur_parsing: Item,
    /// A nonce value to use for generating unique new non-terminal names.
    nonce: usize,
    /// A temporary home for the terminals to ignore while we parse the grammar.
    ignore_terminals: Vec<String>,
}

/// The terminals of the grammar description language.
///
/// We will use these to check the beginning of the string, so they all begin
/// with a beginning-of-string anchor. This is because the Regex library does
/// not include the ability to check whether a match begins at a certain point
/// in the string, though we may have to implement this behavior ourself in the
/// future to enable lookaround matching, rather than simply using sring
/// slicing to force the behavior we want.
// Anchor these to the beginning of the string, because we will be using the regex
// to pop the terminal off the beginning of the input.

const STRING: &str = r#"^\"(?<content>.*?(\\)*?)\"(?<caseinvariant>i?)"#;
const STRING_RE: Lazy<Regex> = Lazy::new(|| Regex::new(STRING).unwrap());
const NL: &str = r"^(\r?\n)+\s*";
const NL_RE: Lazy<Regex> = Lazy::new(|| Regex::new(NL).unwrap());
const REGEXP: &str = r#"^\/(?<pattern>(\\\/|\\\\|[^\/])*?)\/(?<flags>[imslux]*)"#;
const REGEXP_RE: Lazy<Regex> = Lazy::new(|| Regex::new(REGEXP).unwrap());
const OP: &str = r"^[+\*\?]";
const OP_RE: Lazy<Regex> = Lazy::new(|| Regex::new(OP).unwrap());
const VBAR: &str = r"^((\r?\n)+\s*)?\|";
const VBAR_RE: Lazy<Regex> = Lazy::new(|| Regex::new(VBAR).unwrap());
const NUMBER: &str = r"^(+|-)?[0-9]+";
const NUMBER_RE: Lazy<Regex> = Lazy::new(|| Regex::new(NUMBER).unwrap());
const RULE: &str = r"^!?[_?]?(?<name>[a-z][_a-z0-9]*)";
const RULE_RE: Lazy<Regex> = Lazy::new(|| Regex::new(RULE).unwrap());
const TOKEN: &str = r"^_?[A-Z][_A-Z0-9]*";
const TOKEN_RE: Lazy<Regex> = Lazy::new(|| Regex::new(TOKEN).unwrap());

impl EBNFParser {
    pub fn new(input_string: &str, starting_rule_name: &str) -> Self {
        EBNFParser {
            cur_pos: 0,
            input_string: input_string.into(),
            grammar: Grammar {
                productions: vec![],
                terminals: vec![],
                symbol_set: vec![],
                start_symbol: starting_rule_name.into(),
                ignore_terminals: vec![],
            },
            cur_line: 1,
            cur_column: 1,
            name_stack: vec![],
            cur_priority: 0,
            cur_parsing: Item::RULE,
            nonce: 0,
            ignore_terminals: vec![],
        }
    }
    /// Parse input_string into a Grammar and a Lexer.
    ///
    /// The grammar will be "augmented", which means that the start rule will
    /// always be a production whose right-hand side is a single non-terminal. This
    /// is necessary for the algorithm in `[crate::table]`, which assumes this
    /// characteristic.
    pub fn parse(mut self) -> Result<Grammar, ()> {
        // Preprocessing.
        // self.replace_square_brackets();
        // self.expand_templates();
        // self.expand_imports();
        // self.expand_overrides();
        // self.expand_extends();
        // self.hoist_tokens();

        while self.cur_pos < self.input_string.len() {
            // Advance to next non-whitespace (or comment) in the input.
            self.consume_space();
            // item: rule | token
            if RULE_RE.is_match(&self.input_string[self.cur_pos..]) {
                self.parse_rule();
            } else if TOKEN_RE.is_match(&self.input_string[self.cur_pos..]) {
                self.parse_token();
            } else if self.peek(0) == Some('%') {
                self.parse_statement();
            } else {
                self.report_parse_error("Invalid item.");
            }
            self.consume_space();
        }

        // Ensure symbols are unique.
        self.grammar.symbol_set.sort();
        self.grammar.symbol_set.dedup();

        // Make ignore terminals ignored, if any. This is a non-trivial task.
        self.handle_ignore_terminals();

        // Include the special EOF symbol.
        self.grammar.symbol_set.push("$".into());
        self.grammar.terminals.push(Terminal::new("$", "", 0));

        Ok(self.grammar)
    }

    /// Parse a rule.
    ///
    /// `rule: RULE priority? ":" expansions`
    ///
    /// Note that the params that are present in the syntax have already been
    /// expanded by the time this procedure is called.
    fn parse_rule(&mut self) {
        // We know there's a match because this method is only called after
        // we've already determined that the next thing is a rule. The regex
        // RULE_RE removes the leading ? or !, if any, because we don't care
        // about it.
        let rule_match = RULE_RE
            .captures(&self.input_string[self.cur_pos..])
            .unwrap();
        // eprintln!("{:?}", rule_match);
        self.cur_parsing = Item::RULE;
        self.name_stack.push(rule_match["name"].into());
        self.consume(rule_match[0].len());

        self.consume_space();

        if self.peek(0) == Some('.') {
            self.parse_priority();
        }

        if self.peek(0) == Some(':') {
            self.consume(1);
        } else {
            self.report_parse_error("Expected ':'.");
        }

        self.parse_expansions();

        // We are no longer parsing a rule.
        self.name_stack.pop();
        self.cur_priority = 0;
        self.cur_parsing = Item::NONE;
    }

    /// Parse a token.
    ///
    /// `token: TOKEN priority? ":" REGEX | STRING | (STRING ".." STRING)`
    ///
    /// Note that the params that are present in the syntax have already been
    /// expanded by the time this procedure is called.
    fn parse_token(&mut self) {
        self.cur_parsing = Item::TOKEN;
        let token_match = TOKEN_RE.find(&self.input_string[self.cur_pos..]).unwrap();

        self.name_stack.push(token_match.as_str().into());
        self.consume(token_match.len());

        if self.peek(0) == Some('.') {
            self.parse_priority();
        }

        if self.peek(0) == Some(':') {
            self.consume(1);
        } else {
            self.report_parse_error("Expected ':'.");
        }

        self.consume_space();

        let value: String;

        match self.peek(0) {
            Some('"') => {
                // string
                value = self.parse_string();
            }
            Some('/') => {
                // regex
                value = self.parse_regex();
            }
            _ => {
                self.report_parse_error(
                    "Expected a regular expression, a string, or a literal range.",
                );
                // Make the compiler happy, because the above procedure never returns.
                return;
            }
        }

        self.grammar.terminals.push(Terminal::new(
            self.name_stack.last().unwrap(),
            &*value,
            self.cur_priority,
        ));

        // We are no longer parsing a token.
        self.name_stack.pop();
        self.cur_priority = 0;
        self.cur_parsing = Item::NONE;
    }

    /// Parse a statement of the grammar.
    ///
    /// ```
    /// statement: "%ignore" expansions                    -> ignore
    ///          | "%declare" name+                        -> declare
    /// ```
    ///
    /// Note that %import, %override, and %extend directives will have already
    /// been expanded by the preprocessor by ;the time this procedure is called.
    ///
    /// ```html
    /// %ignore <TERMINAL>
    /// ```
    ///
    /// Restricting the argument to %ignore to be a terminal is a deviation
    /// from the grammar as specified but not as implemented. We follow the
    /// documentation here, not the grammar specification.
    ///
    /// `%ignore` directives cannot be imported. Imported rules will abide by
    /// the `%ignore` directives declared in the main grammar.
    fn parse_statement(&mut self) {
        match &self.input_string[self.cur_pos..self.cur_pos + 3] {
            // 3 characters are sufficient to disambiguate.
            "%ig" => {
                self.consume("%ignore".len());
                self.consume_space();
                let Some(token) = TOKEN_RE.find(&self.input_string[self.cur_pos..]) else {
                    self.report_parse_error("Expected token after %ignore statement.");
                    return;
                };
                self.ignore_terminals.push(token.as_str().to_string());
                self.consume(token.as_str().len());
            }
            "%de" => {
                self.consume("%declare".len());
                // self.parse_declare()
            }
            _ => {
                self.report_parse_error("Invalid statement.");
            }
        }
    }

    /// Parse the priority.
    ///
    /// `priority: "." NUMBER`
    ///
    /// A priority can only be followed by a ":".
    fn parse_priority(&mut self) {
        if self.peek(0) == Some('.') {
            self.consume(1);
        } else {
            self.report_parse_error("Expected '.'.");
        }

        let Some(number_match) = NUMBER_RE.find(&self.input_string[self.cur_pos..]) else {
            // Add a return statement to make the compiler happy, even though
            // this call never returns.
            self.report_parse_error("Expected a number.");
            return;
        };

        // Parsing as an integer *should* never fail, since we've just matched
        // something that must be a valid number.
        self.cur_priority = number_match.as_str().parse::<i32>().unwrap();
        self.consume(number_match.len());
    }

    /// Parse the expansions of the rule.
    ///
    /// `?expansions: alias (_VBAR alias)*`
    ///
    /// Each `expansions` is turned into a non-terminal that's represented at
    /// the top level of the grammar by a number of productions.
    fn parse_expansions(&mut self) -> String {
        // self.consume_space();

        loop {
            let cur_alias = self.parse_alias();
            // eprintln!("cur_alias: {:?}", cur_alias);

            self.grammar.productions.push(Production {
                lhs: self.name_stack.last().unwrap().to_string(),
                rhs: cur_alias,
            });
            self.grammar
                .symbol_set
                .push(self.name_stack.last().unwrap().to_string());

            // If there's another alias, parse it.
            // self.consume_space();
            if VBAR_RE.is_match(&self.input_string[self.cur_pos..]) {
                // eprintln!("VBAR match");
                // Advance past the VBAR to the beginning of the next alias.
                self.consume_space();
                self.consume(1);
                continue;
            } else {
                // eprintln!("VBAR no match");
                break self.name_stack.last().unwrap().to_string();
            }
        }
    }

    /// Parse an alias.
    ///
    /// `?alias: expansion ["->" RULE]`
    fn parse_alias(&mut self) -> Vec<String> {
        // self.consume_space();
        let res = self.parse_expansion();
        // eprintln!("cur_expansion: {:#?}", res);
        // self.consume_to_end_of_line();
        if self.cur_pos < self.input_string.len() - 1
            && self.input_string[self.cur_pos..self.cur_pos + 2] == *"->"
        {
            // eprintln!("Got ->");
            // We don't actually care about aliases: they only matter to the
            // parse tree that Lark would build if it was using this
            // grammar. Just skip to the end of the line.
            self.consume_to_end_of_line();
        }
        res
    }

    /// Parse an expansion.
    ///
    /// `expansion: expr*`
    fn parse_expansion(&mut self) -> Vec<String> {
        let start_line = self.cur_line;
        let mut res = vec![];
        while self.cur_line == start_line && self.cur_pos < self.input_string.len() {
            self.consume_space();
            res.push(self.parse_expression());
            self.consume_space();
            if VBAR_RE.is_match(&self.input_string[self.cur_pos..])
                || self.peek(0) == Some(')')
                || self.peek(0) == Some(']')
                || (self.peek(0) == Some('-') && self.peek(1) == Some('>'))
            {
                // Break off whenever we reach something that's in the follow set of expr.
                break;
            }
            // eprintln!("line: {}", self.cur_line);
            // eprintln!("parse_expression: {:?}", res);
        }
        res
    }

    /// Parse an expression.
    ///
    /// `expr: atom (OP | "~" NUMBER (".." NUMBER))?`
    ///
    /// This level of the grammar separates the quantifiers from the thing they
    /// quantify.
    fn parse_expression(&mut self) -> String {
        let new_atom = self.parse_atom();
        // eprintln!("parse_expression: {}", new_atom);
        // eprintln!("cur_pos: {}", self.cur_pos);
        if OP_RE.is_match(&self.input_string[self.cur_pos..])
            && !(self.peek(0) == Some('?')
		 // Use a default value that will evaluate to false to avoid
		 // panicking in the case where the question mark is the last
		 // character of the input.
		 && self.peek(1).unwrap_or('!').is_ascii_lowercase())
        {
            // We don't get lookahead in this regex library, but the lark
            // grammar has the lookahead (?![a-z]) after the question mark
            // operator, which makes sure we don't treat the question mark at
            // the beginning of an identifier as a question mark
            // operator. Instead, we manually implement something like this lookahead.
            // TOOD: Switch to fancy-regex to support lookaround.
            // eprintln!("Match on OP_RE");
            let new_nonterm = self.new_nonterminal("expression");
            match self.peek(0).unwrap() {
                '*' => {
                    // Convert every repetition E* to a fresh non-terminal X
                    // and add X = $\epsilon$ | X E.
                    self.grammar.productions.push(Production {
                        lhs: new_nonterm.clone(),
                        rhs: vec!["".to_string()],
                    });
                    self.grammar.productions.push(Production {
                        lhs: new_nonterm.clone(),
                        rhs: vec![new_nonterm.clone(), new_atom],
                    });
                }
                '+' => {
                    // Convert every at-least-one repetition E+ to a fresh
                    // non-terminal X and add X = E | X E.
                    self.grammar.productions.push(Production {
                        lhs: new_nonterm.clone(),
                        rhs: vec![new_atom.clone()],
                    });
                    self.grammar.productions.push(Production {
                        lhs: new_nonterm.clone(),
                        rhs: vec![new_nonterm.clone(), new_atom.clone()],
                    });
                }
                '?' => {
                    // Convert every option E? to a fresh non-terminal X and
                    // add X = $\epsilon$ | E.
                    self.grammar.productions.push(Production {
                        lhs: new_nonterm.clone(),
                        rhs: vec!["".to_string()],
                    });
                    self.grammar.productions.push(Production {
                        lhs: new_nonterm.clone(),
                        rhs: vec![new_atom.clone()],
                    });
                }
                _ => { /* We should never reach this because we already matched above. */ }
            }
            self.consume(1);
            self.grammar.symbol_set.push(new_nonterm.clone());
            return new_nonterm;
            // TODO: Add support for range repeats.
            // } else if self.peek(0) == Some('~') {
            //     // The following is a range of some kind.
            //     self.consume(1);
            //     self.consume_space();
            //     // self.handle_range();
        } else {
            // eprintln!("No match on OP_RE");
            // An unquantified atom.
            new_atom
        }
    }

    /// Parse an atom.
    ///
    /// Return some text representing this atom.
    ///
    /// ```
    /// atom: "(" expansions ")"
    ///     | "[" expansions "]" -> maybe
    ///     | value
    /// ```
    fn parse_atom(&mut self) -> String {
        // eprintln!("Parsing atom: {}", self.cur_pos);
        if self.peek(0) == Some('(') {
            // Convert every group ( E ) to a fresh non-terminal X and add X = E.
            // eprintln!("In parentheses");
            self.consume(1);
            let new_nonterminal = self.new_nonterminal("group");
            self.name_stack.push(new_nonterminal);
            self.parse_expansions();
            if self.peek(0) == Some(')') {
                self.consume(1);
                return self.name_stack.pop().unwrap().clone();
            } else {
                self.report_parse_error("Expected ')'.")
            }
        } else if self.peek(0) == Some('[') {
            // `[item item ..]` is a maybe statement. It's the same as (item item..)?
            self.consume(1);
            let new_nonterminal = self.new_nonterminal("group");
            self.name_stack.push(new_nonterminal);
            self.parse_expansions();
            if self.peek(0) == Some(']') {
                self.consume(1);
                // Hack to get the caller (`parse_expression`) to engage the option logic.
                self.input_string.insert(self.cur_pos, '?');
                return self.name_stack.pop().unwrap().clone();
            } else {
                self.report_parse_error("Expected ']'.")
            }
        } else {
            self.parse_value()
        }
    }

    /// Parse a value, returning it.
    ///
    /// ```
    /// value: STRING ".." STRING -> literal_range
    ///     | name
    ///     | (REGEXP | STRING) -> literal
    /// ```
    /// Note that template usages will already have been expanded during preprocessing.
    fn parse_value(&mut self) -> String {
        match self.peek(0) {
            Some('"') => self.parse_string(),
            Some('/') => self.parse_regex(),
            _ => self.parse_name(),
        }
    }

    /// Parse a string (or range).
    ///
    /// If we're parsing a terminal, return a regex pattern matching the string
    /// (or range). If we're parsing a rule, create a new anonymous terminal
    /// and return its name.
    ///
    /// This procedure doesn't map neatly onto the grammar: we handle both
    /// literal strings and literal ranges by creating a new terminal.
    fn parse_string(&mut self) -> String {
        // TODO: Implement case invariance.
        let input_string = self.input_string.clone();
        // self.consume_space();

        let Some(matched_string) = STRING_RE.captures(&input_string[self.cur_pos..]) else {
            return self.report_parse_error("String ill-formed.");
        };

        // eprintln!("Matched string: {}", &matched_string["content"]);

        // Make sure we consume the number of characters in the string, not the
        // number of captures!
        self.consume(matched_string[0].len());

        let pattern: String;

        if self.peek(0) == Some('.') && self.peek(1) == Some('.') {
            // Literal range!
            self.consume(2);
            let Some(second_matched_string) = STRING_RE.captures(&input_string[self.cur_pos..])
            else {
                return self.report_parse_error("String ill-formed.");
            };

            // Construct new terminal.
            if !(matched_string["content"].len() == 1
                && second_matched_string["content"].len() == 1)
            {
                self.report_parse_error("The ends of literal ranges should be one character long.");
            }
            self.consume(second_matched_string[0].len());

            let first_char: char = matched_string["content"].chars().next().unwrap();
            let second_char: char = second_matched_string["content"].chars().next().unwrap();

            if !(first_char < second_char) {
                self.report_parse_error("Literal ranges should be in ascending order.");
            }

            pattern = format!(
                "[{first_char}-{second_char}]",
                // matched_string.name("caseinvariant").unwrap_or("").as_str()
            );
        } else {
            // Just a string alone.
            pattern = regex::escape(
                &matched_string["content"], // + matched_string.name("caseinvariant").unwrap_or("")
            );
        }

        match self.cur_parsing {
            Item::RULE => {
                let new_name = self.new_nonterminal("__ANONYMOUS_LITERAL");
                self.grammar.symbol_set.push(new_name.clone());
                self.grammar
                    .terminals
                    .push(Terminal::new(&new_name, &pattern, self.cur_priority));
                return new_name;
            }
            Item::TOKEN => {
                return pattern;
            }
            _ => self.report_parse_error("Unexpected string literal."),
        }
    }

    /// Parse a name.
    ///
    /// `name: RULE | TOKEN`
    fn parse_name(&mut self) -> String {
        let input_string = self.input_string.clone();
        if RULE_RE.is_match(&self.input_string[self.cur_pos..]) {
            let rule_match = RULE_RE.find(&input_string[self.cur_pos..]).unwrap();
            self.consume(rule_match.len());
            self.grammar
                .symbol_set
                .push(rule_match.as_str().to_string());
            return rule_match.as_str().into();
        } else if TOKEN_RE.is_match(&self.input_string[self.cur_pos..]) {
            let token_match = TOKEN_RE.find(&input_string[self.cur_pos..]).unwrap();
            self.consume(token_match.len());
            self.grammar
                .symbol_set
                .push(token_match.as_str().to_string());
            return token_match.as_str().into();
        } else {
            self.report_parse_error("Expected a RULE or a TOKEN.")
        }
    }

    /// Parse a regular expression, returning a string of the regular expression.
    ///
    /// `REGEXP: /\/(?!\/)(\\\/|\\\\|[^\/])*?\/[imslux]*/`
    ///
    /// Let this procedure handle the opening and closing slashes.
    ///
    /// REGEXPs can appear in terminal definitions, in which case they are part
    /// of that terminal's final regex, or they can appear in rule expansions,
    /// in which case they are an anonymous terminal that is a literal symbol
    /// in the rule. We make these anonymous terminals into terminals of the
    /// grammar and treat them as such.
    fn parse_regex(&mut self) -> String {
        let input_string = self.input_string.clone();
        let Some(re_match) = REGEXP_RE.captures(&input_string[self.cur_pos..]) else {
            self.report_parse_error("Failed to parse regular expression.");
            return "".into();
        };

        self.consume(re_match[0].len());
        // Make a new anonymous terminal.
        let pattern = re_match["pattern"].to_string();

        match self.cur_parsing {
            Item::RULE => {
                let new_name = self.new_nonterminal("__ANONYMOUS_LITERAL");
                self.grammar.symbol_set.push(new_name.clone());
                self.grammar
                    .terminals
                    .push(Terminal::new(&new_name, &pattern, self.cur_priority));
                return new_name;
            }
            Item::TOKEN => {
                return pattern;
            }
            _ => self.report_parse_error("Unexpected string literal."),
        }
    }

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
    /// ```
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

    /// Insert the ignore terminals into each production.
    ///
    /// We do this instead of passing the ignore terminals on to the lexer so
    /// that there aren't multiple kinds of terminals (lexer and parser) that
    /// we have to juggle when building a mask: the ignore information is
    /// already integrated directly into the grammar itself; the %ignore
    /// notation is just syntactic sugar for putting the named terminal at the
    /// beginning of each production and after each symbol in each production,
    /// except for productions that are already empty (to prevent infinite
    /// regression).
    fn handle_ignore_terminals(&mut self) {
        if !self.ignore_terminals.is_empty() {}
    }

    /// The symbols that "descend" from this one. A symbol descends
    /// from a symbol if it is on the right hand side of
    /// this symbol's production or a symbol that descends from it.
    fn descendents(&self, symbol: &String) -> Vec<String> {
        let mut descendents = vec![symbol.clone()];

        loop {
            // As long as there are new descendents to add.
            let old_descendents = descendents.clone();
            for symbol in &old_descendents {
                // Look for each production that begins with each current descendent.
                for production in &self.grammar.productions {
                    if production.lhs == **symbol {
                        for inner_symbol in &production.rhs {
                            if !descendents.contains(&inner_symbol) {
                                // Add the new descendents we haven't seen yet.
                                descendents.push(inner_symbol.clone());
                            }
                        }
                    }
                }
            }
            if descendents == old_descendents {
                break;
            }
        }
        descendents
    }

    /// Make the new nonterminal of the terminals to ignore.
    ///
    /// Return the name of the new nonterminal and the list of productions that
    /// result once we desugar the definition.
    ///
    /// This new nonterminal is defined as:
    /// ```
    /// new_nonterm: (ignore_terminal1 | ... | ignore_terminaln)*
    /// ```
    fn make_ignore_nonterminal(&mut self) -> (String, Vec<Production>) {
        let new_nonterminal = self.new_nonterminal("__IGNORE");
        let mut new_productions: Vec<Production> = vec![];
        for ignore in &self.ignore_terminals {
            new_productions.push(Production {
                lhs: new_nonterminal.clone(),
                rhs: vec![ignore.clone(), new_nonterminal.clone()],
            });
        }
        new_productions.push(Production {
            lhs: new_nonterminal.clone(),
            rhs: vec!["".to_string()],
        });
        (new_nonterminal, new_productions)
    }

    /// Insert a non-terminal between each nonterminal on the right-hand side of a production.
    fn insert_nonterminal(&self, production: Production, non_terminal: String) -> Production {
        let mut new_rhs = vec![non_terminal.clone()];
        for symbol in production.rhs {
            new_rhs.push(symbol);
            new_rhs.push(non_terminal.clone());
        }
        Production {
            lhs: production.lhs,
            rhs: new_rhs,
        }
    }

    /// Consume the specified number of characters, maintaining line and column number.
    fn consume(&mut self, count: usize) {
        for _ in 0..count {
            // Newline advances line count, except when "\r\n"? This logic is
            // verbatim from xgrammar.
            if self.peek(0) == Some('\n')
                || (self.peek(0) == Some('\r') && self.peek(1) != Some('\n'))
            {
                self.cur_line += 1;
                self.cur_column = 1;
            } else {
                self.cur_column += 1;
            }
            self.cur_pos += 1;
        }
        // eprintln!("{}", self.cur_pos);
    }

    /// Peek the character delta ahead of the current one.
    fn peek(&self, delta: usize) -> Option<char> {
        // Evil because the Rust str and String abstractions are surprisingly
        // leaky, which makes them substantially harder to work with than in
        // other languages. This gets, strictly speaking, the (input_pos +
        // delta)th character in the input.
        self.input_string.chars().nth(self.cur_pos + delta)
    }

    /// Consume the next whitespace and comment in the input.
    fn consume_space(&mut self) {
        while self.cur_pos < self.input_string.len()
            && (self.peek(0) == Some(' ')
                || self.peek(0) == Some('\n')
                || self.peek(0) == Some('\t')
                || self.peek(0) == Some('#')
                || self.peek(0) == Some('/') && self.peek(1) == Some('/'))
        {
            if self.peek(0) == Some('#') || self.peek(0) == Some('/') && self.peek(1) == Some('/') {
                self.consume_to_end_of_line();
            } else {
                self.consume(1);
            }
        }
    }

    /// Consume to the end of the line.
    ///
    /// Useful for skipping aliases and comments.
    fn consume_to_end_of_line(&mut self) {
        while self.cur_pos < self.input_string.len() {
            if !(self.peek(0) == Some('\n')
                && !(self.peek(0) == Some('\r') && self.peek(1) == Some('\n')))
            {
                self.consume(1);
            } else {
                break;
            }
        }
    }

    /// Make a new, unique nonterminal name.
    ///
    /// TODO: Make these more readable by using more semantic information from the grammar.
    fn new_nonterminal(&mut self, annotation: &str) -> String {
        self.nonce += 1;
        format!("{}_{}", annotation, self.nonce)
    }

    /// Report a parse error with the line and column number. This procedure
    /// will panic when called.
    fn report_parse_error(&self, message: &str) -> String {
        panic!(
            "EBNF parse error at line {}, column {}: {message}",
            self.cur_line, self.cur_column
        );
    }
}

mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn parse_simple_grammar() {
        let parser = EBNFParser::new("s: c c\n c: \"C\" c | \"D\"", "s");
        let Ok(grammar) = parser.parse() else {
            panic!()
        };
        let expected_grammar = Grammar {
            symbol_set: vec![
                "__ANONYMOUS_LITERAL_1".to_string(),
                "__ANONYMOUS_LITERAL_2".to_string(),
                "c".to_string(),
                "s".to_string(),
                "$".to_string(),
            ],
            terminals: vec![
                Terminal::new("__ANONYMOUS_LITERAL_1", "C", 0),
                Terminal::new("__ANONYMOUS_LITERAL_2", "D", 0),
                Terminal::new("$", "", 0),
            ],
            start_symbol: "s".to_string(),
            productions: vec![
                Production {
                    lhs: "s".to_string(),
                    rhs: vec!["c".to_string(), "c".to_string()],
                },
                Production {
                    lhs: "c".to_string(),
                    rhs: vec!["__ANONYMOUS_LITERAL_1".to_string(), "c".to_string()],
                },
                Production {
                    lhs: "c".to_string(),
                    rhs: vec!["__ANONYMOUS_LITERAL_2".to_string()],
                },
            ],
            ignore_terminals: vec![],
        };

        assert_eq!(grammar, expected_grammar);
    }

    #[test]
    fn parse_json_grammar() {
        let parser = EBNFParser::new(
            &fs::read_to_string("./grammars/json.lark").unwrap(),
            "start",
        );
        let grammar = parser.parse();
        println!("{:#?}", grammar.unwrap());
    }
}
