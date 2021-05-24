use regex::{Regex, RegexBuilder};

#[derive(Clone, Debug)]
pub enum Identifier {
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

pub trait IdentifierParser {
    fn into_identifier(self) -> crate::Result<Identifier>;
}

impl IdentifierParser for String {
    // TODO: Everything is forced lowercase atm, but this should be OPTIONAL...
    fn into_identifier(self) -> crate::Result<Identifier> {
        if self.starts_with('?') {
            Ok(Identifier::Regex(
                RegexBuilder::new(&self.as_str()[1..])
                    .case_insensitive(true)
                    .build()
                    .map_err(crate::error::parse_invalid_ident)?,
            ))
        } else if self.starts_with(">=") {
            Ok(Identifier::GreaterThanOrEqual(
                self.as_str()[2..]
                    .parse::<i64>()
                    .map_err(crate::error::parse_invalid_ident)?,
            ))
        } else if self.starts_with('>') {
            Ok(Identifier::GreaterThan(
                self.as_str()[1..]
                    .parse::<i64>()
                    .map_err(crate::error::parse_invalid_ident)?,
            ))
        } else if self.starts_with("<=") {
            Ok(Identifier::LessThanOrEqual(
                self.as_str()[2..]
                    .parse::<i64>()
                    .map_err(crate::error::parse_invalid_ident)?,
            ))
        } else if self.starts_with('<') {
            Ok(Identifier::LessThan(
                self.as_str()[1..]
                    .parse::<i64>()
                    .map_err(crate::error::parse_invalid_ident)?,
            ))
        } else if self.starts_with('=') {
            Ok(Identifier::Equal(
                self.as_str()[1..]
                    .parse::<i64>()
                    .map_err(crate::error::parse_invalid_ident)?,
            ))
        } else if self.starts_with('*') && self.ends_with('*') {
            Ok(Identifier::Contains(
                self.as_str()[1..self.len() - 1].to_lowercase(),
            ))
        } else if self.starts_with('*') {
            Ok(Identifier::EndsWith(self.as_str()[1..].to_lowercase()))
        } else if self.ends_with('*') {
            Ok(Identifier::StartsWith(
                self.as_str()[..self.len() - 1].to_lowercase(),
            ))
        } else if (self.starts_with('"') && self.ends_with('"'))
            || (self.starts_with('\'') && self.ends_with('\''))
        {
            Ok(Identifier::Exact(
                self.as_str()[1..self.len() - 1].to_lowercase(),
            ))
        } else {
            Ok(Identifier::Exact(self.to_lowercase()))
        }
    }
}
