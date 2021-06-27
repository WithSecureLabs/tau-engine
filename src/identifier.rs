use regex::{Regex, RegexBuilder};

#[derive(Clone, Debug)]
pub enum Pattern {
    Contains(String),
    Equal(i64),
    EndsWith(String),
    Exact(String),
    GreaterThan(i64),
    GreaterThanOrEqual(i64),
    LessThan(i64),
    LessThanOrEqual(i64),
    Regex(Regex),
    StartsWith(String),
}

#[derive(Clone, Debug)]
pub struct Identifier {
    pub ignore_case: bool,
    pub pattern: Pattern,
}

pub trait IdentifierParser {
    fn into_identifier(self) -> crate::Result<Identifier>;
}

impl IdentifierParser for String {
    fn into_identifier(self) -> crate::Result<Identifier> {
        let (insensitive, string) = if self.starts_with('i') {
            if cfg!(feature = "ignore_case") {
                (true, &self[..])
            } else {
                (true, &self[1..])
            }
        } else {
            if cfg!(feature = "ignore_case") {
                (true, &self[..])
            } else {
                (false, &self[..])
            }
        };
        let pattern = if string.starts_with('?') {
            Pattern::Regex(
                RegexBuilder::new(&string[1..])
                    .case_insensitive(insensitive)
                    .build()
                    .map_err(crate::error::parse_invalid_ident)?,
            )
        } else if string.starts_with(">=") {
            Pattern::GreaterThanOrEqual(
                string[2..]
                    .parse::<i64>()
                    .map_err(crate::error::parse_invalid_ident)?,
            )
        } else if string.starts_with('>') {
            Pattern::GreaterThan(
                string[1..]
                    .parse::<i64>()
                    .map_err(crate::error::parse_invalid_ident)?,
            )
        } else if string.starts_with("<=") {
            Pattern::LessThanOrEqual(
                string[2..]
                    .parse::<i64>()
                    .map_err(crate::error::parse_invalid_ident)?,
            )
        } else if string.starts_with('<') {
            Pattern::LessThan(
                string[1..]
                    .parse::<i64>()
                    .map_err(crate::error::parse_invalid_ident)?,
            )
        } else if string.starts_with('=') {
            Pattern::Equal(
                string[1..]
                    .parse::<i64>()
                    .map_err(crate::error::parse_invalid_ident)?,
            )
        } else if string.starts_with('*') && string.ends_with('*') {
            let s = if insensitive {
                string[1..string.len() - 1].to_lowercase()
            } else {
                string[1..string.len() - 1].to_string()
            };
            Pattern::Contains(s)
        } else if string.starts_with('*') {
            let s = if insensitive {
                string[1..].to_lowercase()
            } else {
                string[1..].to_string()
            };
            Pattern::EndsWith(s)
        } else if string.ends_with('*') {
            let s = if insensitive {
                string[..string.len() - 1].to_lowercase()
            } else {
                string[..string.len() - 1].to_string()
            };
            Pattern::StartsWith(s)
        } else if (string.starts_with('"') && string.ends_with('"'))
            || (string.starts_with('\'') && string.ends_with('\''))
        {
            let s = if insensitive {
                string[1..string.len() - 1].to_lowercase()
            } else {
                string[1..string.len() - 1].to_string()
            };
            Pattern::Exact(s)
        } else {
            let s = if insensitive {
                string.to_lowercase()
            } else {
                string.to_owned()
            };
            Pattern::Exact(s)
        };
        Ok(Identifier {
            ignore_case: insensitive,
            pattern,
        })
    }
}
