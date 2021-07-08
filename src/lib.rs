//! # Tau Engine
//!
//! This crate provides a library that tags documents through the evaluation of rules.
//! The engine makes use of a Pratt parser and a tree solver in order to match the contents of a
//! rule against a document.
//!
//!
//! ## Rules
//!
//! A rule is a collection of logic that is used to tag documents.
//! They are generally written in YAML and an example can be seen below
//!
//! ```text
//! detection:
//!     A:
//!         foo: bar
//!
//!     condition: A
//!
//! true_positives:
//! - foo: bar
//!
//! true_negatives:
//! - foo: baz
//! ```
//!
//! Where the detection contains the rule logic, the true_positives contain documents that should
//! evaluate true, and the true_negatives contain documents that should evaluate false.
//!
//!
//! ## Documents
//!
//! A document is the data that a rule is evaluating.
//! This is generically achieved through implementation of the ['Document'](document::Document) trait.
//! By implementing this trait on data types, the Tau Engine is then able to dynamically extract
//! values for evaluation.
//!
//! //```
//! //use std::borrow::Cow;
//! //use tau_engine::{Document, Value};
//!
//! //struct Foo {
//! //    pub foo: String,
//! //    pub bar: String,
//! //}
//!
//! //impl Document for Foo {
//! //    fn find(&self, key: &str) -> Option<Value<'_>> {
//! //        match key {
//! //            "foo" => Some(Value::Str(Cow::Borrowed(self.foo))),
//! //            "bar" => Some(Value::Str(Cow::Borrowed(self.bar))),
//! //            _ => None,
//! //        }
//! //    }
//! //}
//! //```
//!
//! //## Solving
//!
//! //```
//! //let rule = """
//! //    detection:
//! //        A:
//! //            foo: bar
//!
//! //        condition: A
//!
//! //    true_positives:
//! //    - foo: bar
//!
//! //    true_negatives:
//! //    - foo: baz
//! //""";
//! //```

#![cfg_attr(feature = "benchmarks", feature(test))]

#[cfg_attr(test, macro_use)]
#[cfg(feature = "benchmarks")]
extern crate test;

pub use self::document::Document;
pub use self::error::{Error, Kind as ErrorKind};
pub use self::rule::Rule;
pub use self::solver::solve;
pub use self::value::{Array, AsValue, Object, Value};

pub(crate) use error::Result;

mod document;
mod error;
mod identifier;
#[cfg(feature = "json")]
mod json;
mod parser;
mod rule;
mod solver;
mod tokeniser;
mod value;
mod yaml;
