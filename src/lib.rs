#![cfg_attr(feature = "benchmarks", feature(test))]

#[cfg_attr(test, macro_use)]
#[cfg(feature = "benchmarks")]
extern crate test;

pub use self::error::{Error, Kind as ErrorKind};
pub use self::value::{Array, AsValue, Object, Value};

pub(crate) use error::Result;

mod error;
mod identifier;
mod parser;
mod rule;
mod solver;
mod tokeniser;
mod value;
