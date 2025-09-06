// src/grammar.rs
/*!
Parse [Lark's EBNF](https://lark-parser.readthedocs.io/en/stable/grammar.html)
and turn it into a `[crate::types::Grammar]` object.

FIXME:
- Broken parentheses with newlines.
- Populate remaining fields of grammar object besides `start_symbol` and `productions`.

TODO:
- Implement "maybe" blocks (square brackets).
- Implement directives.
!*/

use crate::types::*;
use regex::Regex;
use regex_automata::util::lazy::Lazy;

enum Item {
    RULE,
    TOKEN,
    STATEMENT,
}

/// Track the current state of parsing.
///
/// Only one of these structs will exist throughout the life of the program,
/// and it will be mutated a lot.
struct EBNFParser {
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
    /// The right-hand side of the rule or token we are currently parsing.
    cur_rhs: Vec<String>,
    /// Currently parsing a rule, token, or statement?
    cur_parsing: Item,
    /// A nonce value to use for generating unique new non-terminal names.
    nonce: usize,
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

const STRING: &str = r#"^\".*?(?<!\)(\\)*?\"i?"#;
const STRING_RE: Lazy<Regex> = Lazy::new(|| Regex::new(STRING).unwrap());
const NL: &str = r"^(\r?\n)+\s*";
const NL_RE: Lazy<Regex> = Lazy::new(|| Regex::new(NL).unwrap());
const REGEXP: &str = r"/(?!/)(\/|\\|[^/])*?/[imslux]*";
const REGEXP_RE: Lazy<Regex> = Lazy::new(|| Regex::new(REGEXP).unwrap());
const OP: &str = r"^[+\*]|[?][a-z]";
const OP_RE: Lazy<Regex> = Lazy::new(|| Regex::new(OP).unwrap());
const VBAR: &str = r"^((\r?\n)+\s*)?\|";
const VBAR_RE: Lazy<Regex> = Lazy::new(|| Regex::new(VBAR).unwrap());
const NUMBER: &str = r"^(+|-)?[0-9]+";
const NUMBER_RE: Lazy<Regex> = Lazy::new(|| Regex::new(NUMBER).unwrap());
const RULE: &str = r"^!?[_?]?[a-z][_a-z0-9]*";
const RULE_RE: Lazy<Regex> = Lazy::new(|| Regex::new(RULE).unwrap());
const TOKEN: &str = r"^_?[A-Z][_A-Z0-9]*";
const TOKEN_RE: Lazy<Regex> = Lazy::new(|| Regex::new(TOKEN).unwrap());

impl EBNFParser {
    fn new(input_string: &str, starting_rule_name: &str) -> Self {
        EBNFParser {
            cur_pos: 0,
            input_string: input_string.into(),
            grammar: Grammar {
                productions: vec![],
                terminals: vec![],
                symbol_set: vec![],
                start_symbol: starting_rule_name.into(),
            },
            cur_line: 1,
            cur_column: 1,
            name_stack: vec![],
            cur_priority: 0,
            cur_rhs: vec![],
            cur_parsing: Item::RULE,
            nonce: 0,
        }
    }
    /// Parse input_string into a Grammar.
    ///
    /// The grammar will be "augmented", which means that the start rule will
    /// always be a production whose right-hand side is a single non-terminal. This
    /// is necessary for the algorithm in `[crate::table]`, which assumes this
    /// characteristic.
    fn parse(mut self) -> Grammar {
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
        // We know there's a match because this method is only called after
        // we've already determined that the next thing is a rule.
        let rule_match = RULE_RE.find(&self.input_string[self.cur_pos..]).unwrap();
        // eprintln!("{:?}", rule_match);
        self.cur_parsing = Item::RULE;
        self.name_stack.push(rule_match.as_str().into());
        self.consume(rule_match.len());

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
        self.cur_rhs = vec![];
        self.cur_priority = 0;
    }

    /// Parse a token.
    ///
    /// `token: TOKEN priority? ":" expansions`
    ///
    /// Note that the params that are present in the syntax have already been
    /// expanded by the time this procedure is called.
    fn parse_token(&mut self) {
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

        self.parse_expansions();

        // self.cur_rule_name contains the lhs of the production.
        // self.cur_rhs contains the rhs of the production.
        // self.grammar.terminals.push(Production {
        //     lhs: self..clone(),
        //     rhs: self.cur_rhs.clone(),
        // });

        // We are no longer parsing a token.
        self.name_stack.pop();
        self.cur_priority = 0;
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
                // self.parse_ignore()
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
        self.consume_space();

        loop {
            let cur_alias = self.parse_alias();
            // eprintln!("cur_alias: {:?}", cur_alias);
            match self.cur_parsing {
                Item::RULE => {
                    // Push a new production to the set of productions.
                    self.grammar.productions.push(Production {
                        lhs: self.name_stack.last().unwrap().to_string(),
                        rhs: cur_alias,
                    });
                }
                Item::TOKEN => {
                    // Push a new terminal to the set of terminals.
                    //
                    // HELP: In Lark's EBNF, tokens do not have to be just a
                    // regex: they can contain arbitrary EBNF statements on
                    // their right-hand side. The remainder of SynCode's logic
                    // assumes that terminals are each a single regex, an
                    // requirement not enforced by Lark's syntax.

                    // self.grammar.terminals.push(Production {
                    //     lhs: self.name_stack.last().unwrap().to_string(),
                    //     rhs: cur_alias,
                    // });
                }
                _ => {
                    // Shouldn't reach this except in case of syntax error in input.
                }
            }

            // If there's another alias, parse it.
            self.consume_space();
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
        self.consume_space();
        let res = self.parse_expansion();
        self.consume_space();
        if self.cur_pos < self.input_string.len() - 1
            && self.input_string[self.cur_pos..self.cur_pos + 2] == *"->"
        {
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
            if VBAR_RE.is_match(&self.input_string[self.cur_pos..]) || self.peek(0) == Some(')') {
                // Break off if we reach a VBAR or a close parenthesis; this tells us that the whole alias is complete.
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
        if OP_RE.is_match(&self.input_string[self.cur_pos..]) {
            // eprintln!("Match on OP_RE");
            let new_nonterm = self.new_nonterminal("expression");
            match OP_RE
                .find(&self.input_string[self.cur_pos..])
                .unwrap()
                .as_str()
            {
                "*" => {
                    // Convert every repetition E* to a fresh non-terminal X
                    // and add X = $\epsilon$ | X E.
                    self.grammar.productions.push(Production {
                        lhs: new_nonterm.clone(),
                        rhs: vec![],
                    });
                    self.grammar.productions.push(Production {
                        lhs: new_nonterm.clone(),
                        rhs: vec![new_nonterm.clone(), new_atom],
                    });
                }
                "+" => {
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
                "?" => {
                    // Convert every option E? to a fresh non-terminal X and
                    // add X = $\epsilon$ | E.
                    self.grammar.productions.push(Production {
                        lhs: new_nonterm.clone(),
                        rhs: vec![],
                    });
                    self.grammar.productions.push(Production {
                        lhs: new_nonterm.clone(),
                        rhs: vec![new_atom.clone()],
                    });
                }
                _ => { /* We should never reach this because we already matched above. */ }
            }
            self.consume(1);
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
        } else {
            self.parse_value()
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
    fn parse_value(&mut self) -> String {
        match self.peek(0) {
            Some('"') => {
                // TODO: Implement literal ranges.
                let input_string = self.input_string.clone();
                let Some(matched_string) = STRING_RE.find(&input_string[self.cur_pos..]) else {
                    return self.report_parse_error("String never terminated.");
                };
                self.consume(matched_string.len());
                // Trim surounding quotation marks for return value.
                return matched_string.as_str()[1..matched_string.as_str().len()].to_string();
            }
            Some('/') => self.parse_regex(),
            _ => self.parse_name(),
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
            return rule_match.as_str().into();
        } else if TOKEN_RE.is_match(&self.input_string[self.cur_pos..]) {
            let token_match = TOKEN_RE.find(&input_string[self.cur_pos..]).unwrap();
            self.consume(token_match.len());
            return token_match.as_str().into();
        } else {
            self.report_parse_error("Expected a RULE or a TOKEN.")
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
    /// in the rule. We make these anonymous terminals into terminals of the
    /// grammar and treat them as such.
    fn parse_regex(&mut self) -> String {
        let input_string = self.input_string.clone();
        self.consume(1);
        let Some(re_match) = REGEXP_RE.find(&input_string[self.cur_pos..]) else {
            self.report_parse_error("Failed to parse regular expression.");
            return "".into();
        };
        self.consume(re_match.len());
        // Eat the final slash.
        self.consume(1);
        re_match.as_str().into()
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
    fn new_nonterminal(&mut self, annotation: &str) -> String {
        self.nonce += 1;
        format!("generated_{}_{}", annotation, self.nonce)
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

    #[test]
    fn successfully_terminates() {
        let parser = EBNFParser::new("start: (middle* | Y)\nmiddle: A+", "start");
        let grammar = parser.parse();
        println!("{:#?}", grammar);
    }
}
