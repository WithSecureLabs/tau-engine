#![cfg_attr(feature = "benchmarks", feature(test))]

#[cfg_attr(test, macro_use)]
#[cfg(feature = "benchmarks")]
extern crate test;

pub use self::error::{Error, Kind as ErrorKind};

pub(crate) use error::Result;

mod error;
mod parser;
mod rule;
mod tokeniser;
