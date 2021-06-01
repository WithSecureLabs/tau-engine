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
