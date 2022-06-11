use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

use tracing::debug;

/// Boolean symbols.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BoolSym {
    /// `&&`
    And,
    /// `==`
    Equal,
    /// `>`
    GreaterThan,
    /// `>=`
    GreaterThanOrEqual,
    /// `<`
    LessThan,
    /// `<=`
    LessThanOrEqual,
    /// `||`
    Or,
}
impl fmt::Display for BoolSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::And => write!(f, "&&"),
            Self::Equal => write!(f, "=="),
            Self::GreaterThan => write!(f, ">"),
            Self::GreaterThanOrEqual => write!(f, ">="),
            Self::LessThan => write!(f, "<"),
            Self::LessThanOrEqual => write!(f, "<="),
            Self::Or => write!(f, "||"),
        }
    }
}

/// Delimiting symbols.
#[derive(Clone, Debug, PartialEq)]
pub enum DelSym {
    /// `,`
    Comma,
    /// `(`
    LeftParenthesis,
    /// `)`
    RightParenthesis,
}

/// Modifier Symbols.
#[derive(Clone, Debug, PartialEq)]
pub enum ModSym {
    /// `int`
    Int,
    /// `not`
    Not,
    /// `str`
    Str,
}
impl fmt::Display for ModSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Not => write!(f, "not"),
            Self::Str => write!(f, "str"),
        }
    }
}
/// Miscellaneous Symbols.
#[derive(Clone, Debug, PartialEq)]
pub enum MiscSym {
    /// `not`
    Not,
}
impl fmt::Display for MiscSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Not => write!(f, "not"),
        }
    }
}

/// Match symbols.
#[derive(Clone, Debug, PartialEq)]
pub enum MatchSym {
    /// `all`
    All,
    /// `or`
    Of,
}

/// Tokens
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Delimiter(DelSym),
    Float(f64),
    Identifier(String),
    Integer(i64),
    Operator(BoolSym),
    Modifier(ModSym),
    Miscellaneous(MiscSym),
    Match(MatchSym),
}

impl Token {
    pub fn binding_power(&self) -> u8 {
        match *self {
            Token::Operator(ref s) => match *s {
                BoolSym::Or => 90,
                BoolSym::And => 80,
                BoolSym::Equal
                | BoolSym::GreaterThan
                | BoolSym::GreaterThanOrEqual
                | BoolSym::LessThan
                | BoolSym::LessThanOrEqual => 70,
            },
            Token::Miscellaneous(ref m) => match *m {
                MiscSym::Not => 95,
            },
            Token::Modifier(ref m) => match *m {
                ModSym::Int | ModSym::Not | ModSym::Str => 60,
            },
            Token::Match(ref s) => match *s {
                MatchSym::All | MatchSym::Of => 60,
            },
            Token::Delimiter(_) | Token::Float(_) | Token::Identifier(_) | Token::Integer(_) => 0,
        }
    }
}

/// Tokenise data into a collection of Tokens to then be used by the parser. This trait converts a
/// tau engine condition into collection of Tokens.
///
/// The condition string supports the following:
/// | Match | Description |
/// |---|---|
/// | '-', '0-9' | Integers |
/// | 'a-z', 'A-Z' | Keywords & Identifiers |
/// | ' ', '\x09'-'\x0d' | Whitespace |
/// | '=', '==', '>', '>=', '<', '<=' | Booleans |
/// | ',', '(', ')' | Miscellaneous |
///
/// Where keywords are:
/// - all
/// - and
/// - int
/// - not
/// - of
/// - or
/// - str
/// - string
pub trait Tokeniser {
    fn tokenise(&self) -> crate::Result<Vec<Token>>;
}
impl Tokeniser for String {
    fn tokenise(&self) -> crate::Result<Vec<Token>> {
        let mut it = self.chars().peekable();
        let mut tokens: Vec<Token> = vec![];
        while let Some(&c) = it.peek() {
            match c {
                '.' | '-' | '0'..='9' => {
                    // A number
                    let number: String = consume_while(&mut it, |a| a.is_numeric() || a == '.')
                        .into_iter()
                        .collect();
                    if number.contains('.') {
                        let float = number.parse().map_err(crate::error::token_invalid_num)?;
                        tokens.push(Token::Float(float));
                    } else {
                        let integer = number.parse().map_err(crate::error::token_invalid_num)?;
                        tokens.push(Token::Integer(integer));
                    }
                }
                'a'..='z' | 'A'..='Z' | '#' => {
                    if match_ahead(&mut it, "int(") {
                        tokens.push(Token::Modifier(ModSym::Int));
                        it.nth(2);
                    } else if match_ahead(&mut it, "string(") {
                        // NOTE: Deprecated
                        tokens.push(Token::Modifier(ModSym::Str));
                        it.nth(5);
                    } else if match_ahead(&mut it, "str(") {
                        tokens.push(Token::Modifier(ModSym::Str));
                        it.nth(2);
                    } else if match_ahead(&mut it, "and ") {
                        tokens.push(Token::Operator(BoolSym::And));
                        it.nth(2);
                    } else if match_ahead(&mut it, "or ") {
                        tokens.push(Token::Operator(BoolSym::Or));
                        it.nth(1);
                    } else if match_ahead(&mut it, "not ") {
                        tokens.push(Token::Miscellaneous(MiscSym::Not));
                        it.nth(2);
                    } else if match_ahead(&mut it, "not(") {
                        tokens.push(Token::Modifier(ModSym::Not));
                        it.nth(2);
                    } else if match_ahead(&mut it, "all(") {
                        tokens.push(Token::Match(MatchSym::All));
                        it.nth(2);
                    } else if match_ahead(&mut it, "of(") {
                        tokens.push(Token::Match(MatchSym::Of));
                        it.nth(1);
                    } else {
                        let identifier: String = consume_while(&mut it, |a| {
                            a.is_alphanumeric() || a == '_' || a == '.' || a == '#'
                        })
                        .into_iter()
                        .collect();
                        tokens.push(Token::Identifier(identifier));
                    }
                }
                ' ' | '\x09'..='\x0d' => {
                    it.next(); // no-op for whitespace
                }
                '=' => {
                    // "=="
                    let mut p = it.clone();
                    p.next();
                    if p.next().unwrap_or(' ') == '=' {
                        tokens.push(Token::Operator(BoolSym::Equal));
                        it.nth(1);
                    } else {
                        return Err(crate::error::token_invalid_char("expected '='"));
                    }
                }
                '<' => {
                    // "< | <="
                    let mut p = it.clone();
                    p.next();
                    if p.next().unwrap_or(' ') == '=' {
                        tokens.push(Token::Operator(BoolSym::LessThanOrEqual));
                        it.next();
                    } else {
                        tokens.push(Token::Operator(BoolSym::LessThan));
                    }
                    it.next();
                }
                '>' => {
                    // "> | >="
                    let mut p = it.clone();
                    p.next();
                    if p.next().unwrap_or(' ') == '=' {
                        tokens.push(Token::Operator(BoolSym::GreaterThanOrEqual));
                        it.next();
                    } else {
                        tokens.push(Token::Operator(BoolSym::GreaterThan));
                    }
                    it.next();
                }
                ',' => {
                    // ","
                    tokens.push(Token::Delimiter(DelSym::Comma));
                    it.next();
                }
                '(' => {
                    // "("
                    tokens.push(Token::Delimiter(DelSym::LeftParenthesis));
                    it.next();
                }
                ')' => {
                    // ")"
                    tokens.push(Token::Delimiter(DelSym::RightParenthesis));
                    it.next();
                }
                _ => {
                    return Err(crate::error::token_invalid_char(format!(
                        "unsupported character '{}'",
                        c
                    )));
                }
            }
        }
        debug!("tokenised '{}' into '{:?}'", self, tokens);

        Ok(tokens)
    }
}

// Helper functions
fn consume_while<F>(it: &mut Peekable<Chars<'_>>, condition: F) -> Vec<char>
where
    F: Fn(char) -> bool,
{
    let mut v: Vec<char> = vec![];
    while let Some(&ch) = it.peek() {
        if condition(ch) {
            it.next().unwrap();
            v.push(ch);
        } else {
            break;
        }
    }
    v
}

fn match_ahead(it: &mut Peekable<Chars<'_>>, value: &str) -> bool {
    let mut p = it.clone();
    for v in value.chars() {
        match p.next() {
            Some(c) if v != c => return false,
            Some(_) => {}
            None => return false,
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::error::{Kind, Token as Error};

    #[test]
    fn tokeniser_identifier() {
        let t = String::from("condition").tokenise().unwrap();
        assert_eq!(vec![Token::Identifier("condition".to_string())], t);
    }

    #[test]
    fn tokeniser_bool_and() {
        let t = String::from("a and b").tokenise().unwrap();
        assert_eq!(
            vec![
                Token::Identifier("a".to_string()),
                Token::Operator(BoolSym::And),
                Token::Identifier("b".to_string()),
            ],
            t
        );
    }

    #[test]
    fn tokeniser_bool_equal() {
        let t = String::from("a == b").tokenise().unwrap();
        assert_eq!(
            vec![
                Token::Identifier("a".to_string()),
                Token::Operator(BoolSym::Equal),
                Token::Identifier("b".to_string()),
            ],
            t
        );
    }

    #[test]
    fn tokeniser_bool_greater_than() {
        let t = String::from("a > b").tokenise().unwrap();
        assert_eq!(
            vec![
                Token::Identifier("a".to_string()),
                Token::Operator(BoolSym::GreaterThan),
                Token::Identifier("b".to_string()),
            ],
            t
        );
    }

    #[test]
    fn tokeniser_bool_greater_than_equal() {
        let t = String::from("a >= b").tokenise().unwrap();
        assert_eq!(
            vec![
                Token::Identifier("a".to_string()),
                Token::Operator(BoolSym::GreaterThanOrEqual),
                Token::Identifier("b".to_string()),
            ],
            t
        );
    }

    #[test]
    fn tokeniser_bool_less_than() {
        let t = String::from("a < b").tokenise().unwrap();
        assert_eq!(
            vec![
                Token::Identifier("a".to_string()),
                Token::Operator(BoolSym::LessThan),
                Token::Identifier("b".to_string()),
            ],
            t
        );
    }

    #[test]
    fn tokeniser_bool_less_than_equal() {
        let t = String::from("a <= b").tokenise().unwrap();
        assert_eq!(
            vec![
                Token::Identifier("a".to_string()),
                Token::Operator(BoolSym::LessThanOrEqual),
                Token::Identifier("b".to_string()),
            ],
            t
        );
    }

    #[test]
    fn tokeniser_bool_or() {
        let t = String::from("a or b").tokenise().unwrap();
        assert_eq!(
            vec![
                Token::Identifier("a".to_string()),
                Token::Operator(BoolSym::Or),
                Token::Identifier("b".to_string()),
            ],
            t
        );
    }

    #[test]
    fn tokeniser_misc_not() {
        let t = String::from("not a").tokenise().unwrap();
        assert_eq!(
            vec![
                Token::Miscellaneous(MiscSym::Not),
                Token::Identifier("a".to_string()),
            ],
            t
        );
    }

    #[test]
    fn tokeniser_mod_int() {
        let t = String::from("int(a)").tokenise().unwrap();
        assert_eq!(
            vec![
                Token::Modifier(ModSym::Int),
                Token::Delimiter(DelSym::LeftParenthesis),
                Token::Identifier("a".to_string()),
                Token::Delimiter(DelSym::RightParenthesis),
            ],
            t
        );
    }

    #[test]
    fn tokeniser_mod_not() {
        let t = String::from("not(a)").tokenise().unwrap();
        assert_eq!(
            vec![
                Token::Modifier(ModSym::Not),
                Token::Delimiter(DelSym::LeftParenthesis),
                Token::Identifier("a".to_string()),
                Token::Delimiter(DelSym::RightParenthesis),
            ],
            t
        );
    }

    #[test]
    fn tokeniser_mod_str() {
        let t = String::from("str(a)").tokenise().unwrap();
        assert_eq!(
            vec![
                Token::Modifier(ModSym::Str),
                Token::Delimiter(DelSym::LeftParenthesis),
                Token::Identifier("a".to_string()),
                Token::Delimiter(DelSym::RightParenthesis),
            ],
            t
        );
    }

    #[test]
    fn tokeniser_search_all() {
        let t = String::from("all(a)").tokenise().unwrap();
        assert_eq!(
            vec![
                Token::Match(MatchSym::All),
                Token::Delimiter(DelSym::LeftParenthesis),
                Token::Identifier("a".to_string()),
                Token::Delimiter(DelSym::RightParenthesis),
            ],
            t
        );
    }

    #[test]
    fn tokeniser_search_x_of() {
        let t = String::from("of(a, 2)").tokenise().unwrap();
        assert_eq!(
            vec![
                Token::Match(MatchSym::Of),
                Token::Delimiter(DelSym::LeftParenthesis),
                Token::Identifier("a".to_string()),
                Token::Delimiter(DelSym::Comma),
                Token::Integer(2),
                Token::Delimiter(DelSym::RightParenthesis),
            ],
            t
        );
    }

    #[test]
    fn tokeniser_expression() {
        let t = String::from("(foo and bar) or baz").tokenise().unwrap();
        assert_eq!(
            vec![
                Token::Delimiter(DelSym::LeftParenthesis),
                Token::Identifier("foo".to_string()),
                Token::Operator(BoolSym::And),
                Token::Identifier("bar".to_string()),
                Token::Delimiter(DelSym::RightParenthesis),
                Token::Operator(BoolSym::Or),
                Token::Identifier("baz".to_string()),
            ],
            t
        );
    }

    #[test]
    fn tokeniser_invalid_character() {
        let e = String::from("foo & bar").tokenise().err().unwrap();
        match e.kind() {
            Kind::Token(Error::InvalidCharacter) => {}
            _ => panic!("expected error"),
        }
    }
}
