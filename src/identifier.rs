use regex::{Regex, RegexBuilder};

// Identifier string matching patterns.
#[derive(Clone, Debug)]
pub enum Pattern {
    // `*foo*`
    Contains(String),
    // `=1`
    Equal(i64),
    // `*foo`
    EndsWith(String),
    // `foo`
    Exact(String),
    // `>1`
    GreaterThan(i64),
    // `>=1`
    GreaterThanOrEqual(i64),
    // `<1`
    LessThan(i64),
    // `<=1`
    LessThanOrEqual(i64),
    // `?foo`
    Regex(Regex),
    // `foo*`
    StartsWith(String),
}

// An identifier containing its pattern and case options
#[derive(Clone, Debug)]
pub struct Identifier {
    /// Whether the identifier is case insensitive.
    pub ignore_case: bool,
    /// The match pattern of the identifier.
    pub pattern: Pattern,
}

/// Parse data into an Identifier. This trait parses a Tau Engine identifier into an `Identifier`.
pub trait IdentifierParser {
    fn into_identifier(self) -> crate::Result<Identifier>;
}
impl IdentifierParser for String {
    fn into_identifier(self) -> crate::Result<Identifier> {
        let (insensitive, string) = if cfg!(feature = "ignore_case") {
            (true, &self[..])
        } else if let Some(s) = self.strip_prefix('i') {
            (true, s)
        } else {
            (false, &self[..])
        };
        let pattern = if let Some(s) = string.strip_prefix('?') {
            Pattern::Regex(
                RegexBuilder::new(s)
                    .case_insensitive(insensitive)
                    .build()
                    .map_err(crate::error::parse_invalid_ident)?,
            )
        } else if let Some(s) = string.strip_prefix(">=") {
            Pattern::GreaterThanOrEqual(
                s.parse::<i64>()
                    .map_err(crate::error::parse_invalid_ident)?,
            )
        } else if let Some(s) = string.strip_prefix('>') {
            Pattern::GreaterThan(
                s.parse::<i64>()
                    .map_err(crate::error::parse_invalid_ident)?,
            )
        } else if let Some(s) = string.strip_prefix("<=") {
            Pattern::LessThanOrEqual(
                s.parse::<i64>()
                    .map_err(crate::error::parse_invalid_ident)?,
            )
        } else if let Some(s) = string.strip_prefix('<') {
            Pattern::LessThan(
                s.parse::<i64>()
                    .map_err(crate::error::parse_invalid_ident)?,
            )
        } else if let Some(s) = string.strip_prefix('=') {
            Pattern::Equal(
                s.parse::<i64>()
                    .map_err(crate::error::parse_invalid_ident)?,
            )
        } else if string.starts_with('*') && string.ends_with('*') {
            let s = if insensitive {
                string[1..string.len() - 1].to_lowercase()
            } else {
                string[1..string.len() - 1].to_string()
            };
            Pattern::Contains(s)
        } else if let Some(s) = string.strip_prefix('*') {
            let s = if insensitive {
                s.to_lowercase()
            } else {
                s.to_string()
            };
            Pattern::EndsWith(s)
        } else if let Some(s) = string.strip_suffix('*') {
            let s = if insensitive {
                s.to_lowercase()
            } else {
                s.to_string()
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn contains() {
        let identifier = "*foo*".to_owned().into_identifier().unwrap();
        match identifier.pattern {
            Pattern::Contains(x) => {
                assert_eq!(x, "foo");
            }
            _ => panic!("unexpected pattern"),
        }
    }

    #[test]
    fn equal() {
        let identifier = "=1".to_owned().into_identifier().unwrap();
        match identifier.pattern {
            Pattern::Equal(x) => {
                assert_eq!(x, 1);
            }
            _ => panic!("unexpected pattern"),
        }
    }

    #[test]
    fn ends_with() {
        let identifier = "*foo".to_owned().into_identifier().unwrap();
        match identifier.pattern {
            Pattern::EndsWith(x) => {
                assert_eq!(x, "foo");
            }
            _ => panic!("unexpected pattern"),
        }
    }

    #[test]
    fn exact() {
        let identifier = "foo".to_owned().into_identifier().unwrap();
        match identifier.pattern {
            Pattern::Exact(x) => {
                assert_eq!(x, "foo");
            }
            _ => panic!("unexpected pattern"),
        }
    }

    #[test]
    fn greater_than() {
        let identifier = ">1".to_owned().into_identifier().unwrap();
        match identifier.pattern {
            Pattern::GreaterThan(x) => {
                assert_eq!(x, 1);
            }
            _ => panic!("unexpected pattern"),
        }
    }

    #[test]
    fn greater_than_or_equal() {
        let identifier = ">=1".to_owned().into_identifier().unwrap();
        match identifier.pattern {
            Pattern::GreaterThanOrEqual(x) => {
                assert_eq!(x, 1);
            }
            _ => panic!("unexpected pattern"),
        }
    }

    #[test]
    fn less_than() {
        let identifier = "<1".to_owned().into_identifier().unwrap();
        match identifier.pattern {
            Pattern::LessThan(x) => {
                assert_eq!(x, 1);
            }
            _ => panic!("unexpected pattern"),
        }
    }

    #[test]
    fn less_than_or_equal() {
        let identifier = "<=1".to_owned().into_identifier().unwrap();
        match identifier.pattern {
            Pattern::LessThanOrEqual(x) => {
                assert_eq!(x, 1);
            }
            _ => panic!("unexpected pattern"),
        }
    }

    #[test]
    fn regex() {
        let identifier = "?foo".to_owned().into_identifier().unwrap();
        match identifier.pattern {
            Pattern::Regex(x) => {
                assert_eq!(x.as_str(), "foo");
            }
            _ => panic!("unexpected pattern"),
        }
    }

    #[test]
    fn starts_with() {
        let identifier = "foo*".to_owned().into_identifier().unwrap();
        match identifier.pattern {
            Pattern::StartsWith(x) => {
                assert_eq!(x.as_str(), "foo");
            }
            _ => panic!("unexpected pattern"),
        }
    }
}
