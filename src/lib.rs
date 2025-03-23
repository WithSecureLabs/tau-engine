//! # Tau Engine
//!
//! This crate provides a library that tags documents by running and matching rules over them.
//! The engine makes use of a Pratt parser and a tree solver in order to evaluate the detection
//! logic of a rule against a document, if the outcome is true the document is considered tagged by
//! that rule.
//!
//!
//! ## Rules
//!
//! A rule is used to tag a document and is made up of three parts:
//! - `detection`: the logic used to evaluate a document.
//! - `true positives`: example documents that must evaluate to true for the given detection.
//! - `true negatives`: example documents that must evaluate to false for the given detection.
//!
//! The detection block is made up of a condition, and identifiers. This allows for simple but
//! expressive rules, below is a brief summary (see [Rules](Rule) for more):
//!
//! ### Identifiers
//!
//! Identifiers are used to help keep the condition concise and generally contain the core of the
//! matching logic. They consist of Key/Value pairs which allow for the extraction of data from the
//! document and the evaluate of its value. It should be noted that mappings are treated as
//! conjunctions, while sequences are treated as disjunctions.
//!
//! Identifiers make use of the following matching logic:
//! - `foobar`: an exact match of foobar
//! - `foobar*`: starts with foobar
//! - `*foobar`: ends with foobar
//! - `*foobar*`: contains foobar
//! - `?foobar`: regex foobar
//!
//! Any of the above can be made case insensitive with the `i` prefix, for example:
//! - `ifoobar`
//! - `ifoobar*`
//!
//! Escaping can be achieved with a combination of `'` and `"`.
//!
//! ### Condition
//!
//! The condition is just a boolean expression and supports the following:
//! - `and`: logical conjunction
//! - `or`: logical disjunction
//! - `==`: equality comparison
//! - `>`, `>=`, `<`, `<=`: numeric comparisons
//! - `not`: negate
//! - `all(i)`: make sequences behave as conjunctions
//! - `of(i, x)`: ensure a sequence has a minimum number of matches
//!
//!
//! ### Examples
//!
//! ```text
//! detection:
//!   A:
//!     foo.bar: foobar
//!
//!   condition: A
//!
//! true_positives:
//! - foo:
//!     bar: foobar
//!
//! true_negatives:
//! - foo:
//!     bar: foo
//! ```
//!
//! ## Documents
//!
//! A document is anything that can provide data to the engine in a meaningful way, usually through Key/Value
//! pairs, i.e: an event log, json object, yaml file, etc. Implementations are achieved with the
//! [`Document`](Document) trait.
//!
//! ## Solving
//!
//! This is an example of how you can tag a document against a provided rule:
//!
//! ```
//! use std::borrow::Cow;
//!
//! use tau_engine::{Document, Rule, Value};
//!
//! // Define a document.
//! struct Foo {
//!     foo: String,
//! }
//! impl Document for Foo {
//!     fn find(&self, key: &str) -> Option<Value<'_>> {
//!         match key {
//!             "foo" => Some(Value::String(Cow::Borrowed(&self.foo))),
//!             _ => None,
//!         }
//!     }
//! }
//!
//! // Write a rule.
//! let rule = r#"
//! detection:
//!   A:
//!     foo: foobar
//!   condition: A
//! true_positives:
//! - foo: foobar
//! true_negatives:
//! - foo: foo
//! "#;
//!
//! // Load and validate a rule.
//! let rule = Rule::from_str(rule).unwrap();
//! assert_eq!(rule.validate().unwrap(), true);
//!
//! // Create a document.
//! let foo = Foo {
//!     foo: "foobar".to_owned(),
//! };
//!
//! // Evalute the document with the rule.
//! assert_eq!(rule.matches(&foo), true);
//! ```
//!
//! ## Features
//!
//! The following are a list of features that can be enabled or disabled:
//! - **core**: Exposes some of Tau Engine's internals.
//! - **ignore_case**: Force the engine to always be case insensitive, this will ignore
//!     the `i` prefix and for that reason is not compatible with case sensitive rules.
//! - **json**: Enable serde json support, this will allow the tau-engine to solve on
//!     `serde_json::Value`.
//!
//!
//! ### JSON
//!
//! When JSON support is enabled for the tau-engine, the result is a solver that can now reason over
//! any document that can be deserialized into `serde_json::Value`.
//!
//! ```ignore
//! # use serde_json::json;
//! use tau_engine::{Document, Rule};
//!
//! // Write a rule.
//! let rule = r#"
//! detection:
//!   A:
//!     foo: foobar
//!   condition: A
//! true_positives:
//! - foo: foobar
//! true_negatives:
//! - foo: foo
//! "#;
//!
//! // Load and validate a rule.
//! let rule = Rule::from_str(rule).unwrap();
//! assert_eq!(rule.validate().unwrap(), true);
//!
//! // Create a document.
//! let foo = json!({
//!     "foo": "foobar",
//! });
//!
//! // Evalute the document with the rule.
//! assert_eq!(rule.matches(&foo), true);
//! ```

#![cfg_attr(feature = "benchmarks", feature(test))]

#[cfg_attr(test, macro_use)]
#[cfg(feature = "benchmarks")]
extern crate test;

pub use self::document::Document;
pub use self::error::{Error, Kind as ErrorKind};
pub use self::optimiser::Optimisations;
pub use self::rule::Rule;
pub use self::solver::solve;
pub use self::value::{Array, AsValue, Object, Value};

pub(crate) use error::Result;

mod document;
mod error;
mod identifier;
#[cfg(feature = "json")]
mod json;
mod optimiser;
mod parser;
mod rule;
mod solver;
mod tokeniser;
mod value;
mod yaml;

#[cfg(feature = "core")]
/// Exposes some of Tau Engine's internals.
pub mod core {
    /// Exposes some of Tau Engine's internal optimisations so that Expressions can be built by hand.
    pub mod optimiser {
        pub use crate::optimiser::*;
    }
    /// Exposes some of Tau Engine's internal parsing so that Expressions can be built by hand.
    pub mod parser {
        pub use crate::identifier::*;
        pub use crate::parser::*;
        pub use crate::tokeniser::*;
    }
    pub use crate::rule::Detection;

    use std::collections::HashMap;

    use crate::document::Document;
    use crate::parser::Expression;
    use crate::solver::SolverResult;

    lazy_static::lazy_static! {
        static ref IDENTIFIERS: HashMap<String, Expression> = HashMap::new();
    }

    /// Evaluates a `Document` with the provided expression.
    ///
    /// # Panics
    ///
    /// This method will panic if an invalid expression is provided
    pub fn solve(expression: &Expression, document: &dyn Document) -> bool {
        match super::solver::solve_expression(expression, &IDENTIFIERS, document) {
            SolverResult::True => true,
            SolverResult::False | SolverResult::Missing => false,
        }
    }

    /// Evaluates a `Document` with the provided expression, and identifiers.
    ///
    /// # Panics
    ///
    /// This method will panic if an invalid expression is provided
    pub fn solve_expression(
        expression: &Expression,
        identifiers: &HashMap<String, Expression>,
        document: &dyn Document,
    ) -> bool {
        match super::solver::solve_expression(expression, identifiers, document) {
            SolverResult::True => true,
            SolverResult::False | SolverResult::Missing => false,
        }
    }
}
