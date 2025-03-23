use std::convert::TryFrom;
use std::fmt;
use std::iter::Iterator;
use std::iter::Peekable;

use aho_corasick::{AhoCorasick, AhoCorasickBuilder, AhoCorasickKind};
use regex::{Regex, RegexSet, RegexSetBuilder};
use serde_yaml::{Mapping, Value as Yaml};
use tracing::debug;

use crate::identifier::{Identifier, IdentifierParser, Pattern};
use crate::tokeniser::{BoolSym, DelSym, MatchSym, MiscSym, ModSym, Token, Tokeniser};

#[derive(Clone, Debug, PartialEq)]
pub enum MatchType {
    Contains(String),
    EndsWith(String),
    Exact(String),
    StartsWith(String),
}

impl MatchType {
    pub fn value(&self) -> &String {
        match self {
            Self::Contains(s) | Self::EndsWith(s) | Self::Exact(s) | Self::StartsWith(s) => s,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Match {
    All,
    Of(u64),
}

#[derive(Clone, Debug)]
pub enum Search {
    AhoCorasick(Box<AhoCorasick>, Vec<MatchType>, bool),
    Any,
    Contains(String),
    EndsWith(String),
    Exact(String),
    Regex(Regex, bool),
    RegexSet(RegexSet, bool),
    StartsWith(String),
}
impl fmt::Display for Search {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::AhoCorasick(_, t, i) => {
                write!(f, "{}aho_corasick({:?})", if *i { "i" } else { "" }, t)
            }
            Self::Any => write!(f, "any"),
            Self::Contains(s) => write!(f, "contains({})", s),
            Self::EndsWith(s) => write!(f, "ends_with({})", s),
            Self::Exact(s) => write!(f, "exact({})", s),
            Self::Regex(s, i) => write!(f, "{}regex({})", if *i { "i" } else { "" }, s),
            Self::RegexSet(s, i) => write!(
                f,
                "{}regex_set({:?})",
                if *i { "i" } else { "" },
                s.patterns()
            ),
            Self::StartsWith(s) => write!(f, "starts_with({})", s),
        }
    }
}
impl PartialEq for Search {
    fn eq(&self, other: &Search) -> bool {
        match (self, other) {
            (Search::Any, Search::Any) => true,
            (Search::AhoCorasick(_, m0, _), Search::AhoCorasick(_, m1, _)) => m0 == m1,
            (Search::Contains(s0), Search::Contains(s1)) => s0 == s1,
            (Search::EndsWith(s0), Search::EndsWith(s1)) => s0 == s1,
            (Search::Exact(s0), Search::Exact(s1)) => s0 == s1,
            (Search::Regex(r0, i0), Search::Regex(r1, i1)) => {
                r0.as_str() == r1.as_str() && i0 == i1
            }
            (Search::RegexSet(r0, i0), Search::RegexSet(r1, i1)) => {
                r0.patterns() == r1.patterns() && i0 == i1
            }
            (Search::StartsWith(s0), Search::StartsWith(s1)) => s0 == s1,
            (_, _) => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    BooleanGroup(BoolSym, Vec<Expression>),
    #[allow(clippy::enum_variant_names)]
    BooleanExpression(Box<Expression>, BoolSym, Box<Expression>),
    Boolean(bool),
    Cast(String, ModSym),
    Field(String),
    Float(f64),
    Identifier(String),
    Integer(i64),
    Match(Match, Box<Expression>),
    Matrix(Vec<String>, Vec<Vec<Option<Expression>>>),
    Negate(Box<Expression>),
    Nested(String, Box<Expression>),
    Null,
    Search(Search, String, bool),
}
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BooleanGroup(o, g) => write!(
                f,
                "group({} {})",
                o,
                g.iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::BooleanExpression(l, o, r) => write!(f, "expression({} {} {})", l, o, r),
            Self::Boolean(b) => write!(f, "bool({})", b),
            Self::Cast(s, t) => write!(f, "cast({}({}))", t, s),
            Self::Field(s) => write!(f, "field({})", s),
            Self::Float(n) => write!(f, "float({})", n),
            Self::Identifier(s) => write!(f, "identifier({})", s),
            Self::Integer(i) => write!(f, "int({})", i),
            Self::Match(Match::All, e) => {
                write!(f, "all({})", e)
            }
            Self::Match(Match::Of(i), e) => write!(f, "of({}, {})", e, i),
            Self::Matrix(c, m) => write!(
                f,
                "matrix([{}], [{}])",
                c.iter()
                    .map(|f| f.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                m.iter()
                    .map(|c| format!(
                        "[{}]",
                        c.iter()
                            .map(|e| e.as_ref().map(|s| s.to_string()).unwrap_or("".to_owned()))
                            .collect::<Vec<String>>()
                            .join(", ")
                    ))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Negate(e) => write!(f, "negate({})", e),
            Self::Nested(s, e) => write!(f, "nested({}, {})", s, e),
            Self::Null => write!(f, "null"),
            Self::Search(e, s, c) => write!(f, "search({}, {}, {})", s, e, c),
        }
    }
}
impl Expression {
    pub fn is_solvable(&self) -> bool {
        match self {
            Self::Boolean(_)
            | Self::Cast(_, _)
            | Self::Field(_)
            | Self::Float(_)
            | Self::Integer(_)
            | Self::Null => false,
            Self::BooleanGroup(_, _)
            | Self::BooleanExpression(_, _, _)
            | Self::Identifier(_)
            | Self::Match(_, _)
            | Self::Matrix(_, _)
            | Self::Negate(_)
            | Self::Nested(_, _)
            | Self::Search(_, _, _) => true,
        }
    }
}

// Pratt Parser used to parse the token stream
//
// Left-Denotation (LED) - how an operator consumes to the right with a left-context
// Null-Denotation (NUD) - how an operator consumes to the right with no left-context

pub(crate) fn parse(tokens: &[Token]) -> crate::Result<Expression> {
    let mut it = tokens.iter().peekable();
    let expression = parse_expr(&mut it, 0)?;
    if it.peek().is_some() {
        let remaining = it.collect::<Vec<&Token>>();
        return Err(crate::error::parse_invalid_expr(format!(
            "failed to parse the following tokens - '{:?}'",
            remaining
        )));
    }

    debug!("parsed '{:?}' into '{:?}'", tokens, expression);

    Ok(expression)
}

fn parse_expr<'a, I>(it: &mut Peekable<I>, right_binding_power: u8) -> crate::Result<Expression>
where
    I: Iterator<Item = &'a Token>,
{
    let mut left = parse_nud(it)?;
    while let Some(&next) = it.peek() {
        if right_binding_power >= next.binding_power() {
            break;
        }
        left = parse_led(left, it)?;
    }
    Ok(left)
}

fn parse_led<'a, I>(left: Expression, it: &mut Peekable<I>) -> crate::Result<Expression>
where
    I: Iterator<Item = &'a Token>,
{
    match it.next() {
        Some(t) => match *t {
            Token::Operator(ref s) => {
                let symbol = *s;
                let right = parse_expr(it, t.binding_power())?;
                // Handle special limited cases
                match symbol {
                    BoolSym::Equal => {
                        match left {
                            Expression::Boolean(_)
                            | Expression::Cast(_, _)
                            | Expression::Float(_)
                            | Expression::Integer(_) => {}
                            _ => {
                                return Err(crate::error::parse_led_preceding(format!(
                                    "encountered - '{:?}'",
                                    t
                                )));
                            }
                        }
                        match right {
                            Expression::Boolean(_)
                            | Expression::Cast(_, _)
                            | Expression::Float(_)
                            | Expression::Integer(_) => {}
                            _ => {
                                return Err(crate::error::parse_led_following(format!(
                                    "encountered - '{:?}'",
                                    t
                                )));
                            }
                        }
                        // Type enforcement
                        match (&left, &right) {
                            (
                                Expression::Cast(_, ModSym::Flt),
                                Expression::Cast(_, ModSym::Flt),
                            ) => {}
                            (
                                Expression::Cast(_, ModSym::Int),
                                Expression::Cast(_, ModSym::Int),
                            ) => {}
                            (
                                Expression::Cast(_, ModSym::Str),
                                Expression::Cast(_, ModSym::Str),
                            ) => {}
                            (Expression::Cast(_, ModSym::Flt), Expression::Float(_)) => {}
                            (Expression::Float(_), Expression::Cast(_, ModSym::Flt)) => {}
                            (Expression::Cast(_, ModSym::Int), Expression::Integer(_)) => {}
                            (Expression::Integer(_), Expression::Cast(_, ModSym::Int)) => {}
                            (_, _) => {
                                return Err(crate::error::parse_invalid_expr(format!(
                                    "encountered - '{:?}'",
                                    t
                                )));
                            }
                        }
                    }
                    BoolSym::GreaterThan
                    | BoolSym::GreaterThanOrEqual
                    | BoolSym::LessThan
                    | BoolSym::LessThanOrEqual => {
                        match left {
                            Expression::Cast(_, _)
                            | Expression::Float(_)
                            | Expression::Integer(_) => {}
                            _ => {
                                return Err(crate::error::parse_led_preceding(format!(
                                    "encountered - '{:?}'",
                                    t
                                )));
                            }
                        }
                        match right {
                            Expression::Cast(_, _)
                            | Expression::Float(_)
                            | Expression::Integer(_) => {}
                            _ => {
                                return Err(crate::error::parse_led_following(format!(
                                    "encountered - '{:?}'",
                                    t
                                )));
                            }
                        }
                        // Type enforcement
                        match (&left, &right) {
                            (
                                Expression::Cast(_, ModSym::Flt),
                                Expression::Cast(_, ModSym::Flt),
                            ) => {}
                            (
                                Expression::Cast(_, ModSym::Int),
                                Expression::Cast(_, ModSym::Int),
                            ) => {}
                            (Expression::Cast(_, ModSym::Flt), Expression::Float(_)) => {}
                            (Expression::Float(_), Expression::Cast(_, ModSym::Flt)) => {}
                            (Expression::Cast(_, ModSym::Int), Expression::Integer(_)) => {}
                            (Expression::Integer(_), Expression::Cast(_, ModSym::Int)) => {}
                            (_, _) => {
                                return Err(crate::error::parse_invalid_expr(format!(
                                    "encountered - '{:?}'",
                                    t
                                )));
                            }
                        }
                    }
                    _ => {}
                }
                Ok(Expression::BooleanExpression(
                    Box::new(left),
                    symbol,
                    Box::new(right),
                ))
            }
            Token::Delimiter(_)
            | Token::Float(_)
            | Token::Identifier(_)
            | Token::Integer(_)
            | Token::Miscellaneous(_)
            | Token::Modifier(_)
            | Token::Match(_) => Err(crate::error::parse_invalid_token(format!(
                "LED encountered - '{:?}'",
                t
            ))),
        },
        None => Err(crate::error::parse_invalid_token("LED expected token")),
    }
}

fn parse_nud<'a, I>(it: &mut Peekable<I>) -> crate::Result<Expression>
where
    I: Iterator<Item = &'a Token>,
{
    match it.next() {
        Some(t) => {
            match *t {
                Token::Delimiter(ref s) => match *s {
                    DelSym::LeftParenthesis => {
                        // Consume up to matching right parenthesis and parse that, we also discard
                        // the matching right parenthesis
                        let mut tokens: Vec<Token> = vec![];
                        let mut depth = 1;
                        for t in it.by_ref() {
                            if t == &Token::Delimiter(DelSym::LeftParenthesis) {
                                depth += 1;
                            } else if t == &Token::Delimiter(DelSym::RightParenthesis) {
                                depth -= 1;
                                if depth == 0 {
                                    break;
                                }
                            }
                            tokens.push(t.clone());
                        }
                        parse(&tokens)
                    }
                    DelSym::Comma | DelSym::RightParenthesis => Err(
                        crate::error::parse_invalid_token(format!("NUD encountered - '{:?}'", t)),
                    ),
                },
                Token::Float(ref n) => Ok(Expression::Float(*n)),
                Token::Identifier(ref n) => Ok(Expression::Identifier(n.to_string())),
                Token::Integer(ref n) => Ok(Expression::Integer(*n)),
                Token::Miscellaneous(ref m) => match *m {
                    MiscSym::Not => {
                        let right = parse_expr(it, t.binding_power())?;
                        match right {
                            Expression::BooleanGroup(_, _)
                            | Expression::BooleanExpression(_, _, _)
                            | Expression::Boolean(_)
                            | Expression::Identifier(_)
                            | Expression::Match(_, _)
                            | Expression::Negate(_)
                            | Expression::Nested(_, _)
                            | Expression::Search(_, _, _) => {}
                            _ => {
                                return Err(crate::error::parse_invalid_token(
                                    "NUD expected a negatable expression",
                                ));
                            }
                        }
                        Ok(Expression::Negate(Box::new(right)))
                    }
                },
                Token::Modifier(ref m) => match *m {
                    ModSym::Flt => {
                        // We expect Flt(column_identifier)
                        if let Some(t) = it.next() {
                            match *t {
                                Token::Delimiter(DelSym::LeftParenthesis) => {}
                                _ => {
                                    return Err(crate::error::parse_invalid_token(format!(
                                        "NUD expected left parenthesis - '{:?}'",
                                        t
                                    )));
                                }
                            }
                        } else {
                            return Err(crate::error::parse_invalid_token(
                                "NUD expected left parenthesis",
                            ));
                        }
                        let token = match it.next() {
                            Some(t) => t,
                            None => {
                                return Err(crate::error::parse_invalid_token(
                                    "NUD expected column identifier",
                                ));
                            }
                        };
                        if let Some(t) = it.next() {
                            match *t {
                                Token::Delimiter(DelSym::RightParenthesis) => {}
                                _ => {
                                    return Err(crate::error::parse_invalid_token(format!(
                                        "NUD expected right parenthesis - '{:?}'",
                                        t
                                    )));
                                }
                            }
                        } else {
                            return Err(crate::error::parse_invalid_token(
                                "NUD expected right parenthesis",
                            ));
                        }
                        match *token {
                            Token::Identifier(ref s) => {
                                Ok(Expression::Cast(s.to_string(), ModSym::Flt))
                            }
                            _ => Err(crate::error::parse_invalid_token(
                                "NUD expected column identifier",
                            )),
                        }
                    }
                    ModSym::Int => {
                        // We expect Int(column_identifier)
                        if let Some(t) = it.next() {
                            match *t {
                                Token::Delimiter(DelSym::LeftParenthesis) => {}
                                _ => {
                                    return Err(crate::error::parse_invalid_token(format!(
                                        "NUD expected left parenthesis - '{:?}'",
                                        t
                                    )));
                                }
                            }
                        } else {
                            return Err(crate::error::parse_invalid_token(
                                "NUD expected left parenthesis",
                            ));
                        }
                        let token = match it.next() {
                            Some(t) => t,
                            None => {
                                return Err(crate::error::parse_invalid_token(
                                    "NUD expected column identifier",
                                ));
                            }
                        };
                        if let Some(t) = it.next() {
                            match *t {
                                Token::Delimiter(DelSym::RightParenthesis) => {}
                                _ => {
                                    return Err(crate::error::parse_invalid_token(format!(
                                        "NUD expected right parenthesis - '{:?}'",
                                        t
                                    )));
                                }
                            }
                        } else {
                            return Err(crate::error::parse_invalid_token(
                                "NUD expected right parenthesis",
                            ));
                        }
                        match *token {
                            Token::Identifier(ref s) => {
                                Ok(Expression::Cast(s.to_string(), ModSym::Int))
                            }
                            _ => Err(crate::error::parse_invalid_token(
                                "NUD expected column identifier",
                            )),
                        }
                    }
                    ModSym::Not => {
                        // We expect Int(column_identifier)
                        if let Some(t) = it.next() {
                            match *t {
                                Token::Delimiter(DelSym::LeftParenthesis) => {}
                                _ => {
                                    return Err(crate::error::parse_invalid_token(format!(
                                        "NUD expected left parenthesis - '{:?}'",
                                        t
                                    )));
                                }
                            }
                        } else {
                            return Err(crate::error::parse_invalid_token(
                                "NUD expected left parenthesis",
                            ));
                        }
                        let token = match it.next() {
                            Some(t) => t,
                            None => {
                                return Err(crate::error::parse_invalid_token(
                                    "NUD expected column identifier",
                                ));
                            }
                        };
                        if let Some(t) = it.next() {
                            match *t {
                                Token::Delimiter(DelSym::RightParenthesis) => {}
                                _ => {
                                    return Err(crate::error::parse_invalid_token(format!(
                                        "NUD expected right parenthesis - '{:?}'",
                                        t
                                    )));
                                }
                            }
                        } else {
                            return Err(crate::error::parse_invalid_token(
                                "NUD expected right parenthesis",
                            ));
                        }
                        match *token {
                            Token::Identifier(ref s) => {
                                Ok(Expression::Cast(s.to_string(), ModSym::Not))
                            }
                            _ => Err(crate::error::parse_invalid_token(
                                "NUD expected column identifier",
                            )),
                        }
                    }
                    ModSym::Str => {
                        // We expect string(column_identifier)
                        if let Some(t) = it.next() {
                            match *t {
                                Token::Delimiter(DelSym::LeftParenthesis) => {}
                                _ => {
                                    return Err(crate::error::parse_invalid_token(format!(
                                        "NUD expected left parenthesis - '{:?}'",
                                        t
                                    )));
                                }
                            }
                        } else {
                            return Err(crate::error::parse_invalid_token(
                                "NUD expected left parenthesis",
                            ));
                        }
                        let token = match it.next() {
                            Some(t) => t,
                            None => {
                                return Err(crate::error::parse_invalid_token(
                                    "NUD expected column identifier",
                                ));
                            }
                        };
                        if let Some(t) = it.next() {
                            match *t {
                                Token::Delimiter(DelSym::RightParenthesis) => {}
                                _ => {
                                    return Err(crate::error::parse_invalid_token(format!(
                                        "NUD expected right parenthesis - '{:?}'",
                                        t
                                    )));
                                }
                            }
                        } else {
                            return Err(crate::error::parse_invalid_token(
                                "NUD expected right parenthesis",
                            ));
                        }
                        match *token {
                            Token::Identifier(ref s) => {
                                Ok(Expression::Cast(s.to_string(), ModSym::Str))
                            }
                            _ => Err(crate::error::parse_invalid_token(
                                "NUD expected column identifier",
                            )),
                        }
                    }
                },
                Token::Match(ref m) => match *m {
                    MatchSym::All => {
                        // We expect all(column_identifier)
                        if let Some(t) = it.next() {
                            match *t {
                                Token::Delimiter(DelSym::LeftParenthesis) => {}
                                _ => {
                                    return Err(crate::error::parse_invalid_token(format!(
                                        "NUD expected left parenthesis - '{:?}'",
                                        t
                                    )));
                                }
                            }
                        } else {
                            return Err(crate::error::parse_invalid_token(
                                "NUD expected left parenthesis",
                            ));
                        }
                        let token = match it.next() {
                            Some(t) => t,
                            None => {
                                return Err(crate::error::parse_invalid_token(
                                    "NUD expected column identifier",
                                ));
                            }
                        };
                        if let Some(t) = it.next() {
                            match *t {
                                Token::Delimiter(DelSym::RightParenthesis) => {}
                                _ => {
                                    return Err(crate::error::parse_invalid_token(format!(
                                        "NUD expected right parenthesis - '{:?}'",
                                        t
                                    )));
                                }
                            }
                        } else {
                            return Err(crate::error::parse_invalid_token(
                                "NUD expected right parenthesis",
                            ));
                        }
                        match *token {
                            Token::Identifier(ref s) => Ok(Expression::Match(
                                Match::All,
                                Box::new(Expression::Identifier(s.to_string())),
                            )),
                            _ => Err(crate::error::parse_invalid_token(
                                "NUD expected column identifier",
                            )),
                        }
                    }
                    MatchSym::Of => {
                        // We expect of(column_identifier, 1)
                        if let Some(t) = it.next() {
                            match *t {
                                Token::Delimiter(DelSym::LeftParenthesis) => {}
                                _ => {
                                    return Err(crate::error::parse_invalid_token(format!(
                                        "NUD expected left parenthesis - '{:?}'",
                                        t
                                    )));
                                }
                            }
                        } else {
                            return Err(crate::error::parse_invalid_token(
                                "NUD expected left parenthesis",
                            ));
                        }
                        let token = match it.next() {
                            Some(t) => t,
                            None => {
                                return Err(crate::error::parse_invalid_token(
                                    "NUD expected column identifier",
                                ));
                            }
                        };
                        if let Some(t) = it.next() {
                            match *t {
                                Token::Delimiter(DelSym::Comma) => {}
                                _ => {
                                    return Err(crate::error::parse_invalid_token(format!(
                                        "NUD expected comma - '{:?}'",
                                        t
                                    )));
                                }
                            }
                        } else {
                            return Err(crate::error::parse_invalid_token("NUD expected comma"));
                        }
                        let count = match it.next() {
                            Some(t) => match t {
                                Token::Integer(c) => match u64::try_from(*c) {
                                    Ok(u) => u,
                                    Err(_) => {
                                        return Err(crate::error::parse_invalid_token(format!(
                                            "NUD expected positive integer - '{:?}'",
                                            t
                                        )));
                                    }
                                },
                                _ => {
                                    return Err(crate::error::parse_invalid_token(format!(
                                        "NUD expected integer - '{:?}'",
                                        t
                                    )));
                                }
                            },
                            None => {
                                return Err(crate::error::parse_invalid_token(
                                    "NUD expected integer",
                                ));
                            }
                        };
                        if let Some(t) = it.next() {
                            match *t {
                                Token::Delimiter(DelSym::RightParenthesis) => {}
                                _ => {
                                    return Err(crate::error::parse_invalid_token(format!(
                                        "NUD expected right parenthesis - '{:?}'",
                                        t
                                    )));
                                }
                            }
                        } else {
                            return Err(crate::error::parse_invalid_token(
                                "NUD expected right parenthesis",
                            ));
                        }
                        match *token {
                            Token::Identifier(ref s) => Ok(Expression::Match(
                                Match::Of(count),
                                Box::new(Expression::Identifier(s.to_string())),
                            )),
                            _ => Err(crate::error::parse_invalid_token(
                                "NUD expected column identifier",
                            )),
                        }
                    }
                },
                Token::Operator(_) => Err(crate::error::parse_invalid_token(format!(
                    "NUD encountered - '{:?}'",
                    t
                ))),
            }
        }
        None => Err(crate::error::parse_invalid_token("NUD expected token")),
    }
}

pub fn parse_identifier(yaml: &Yaml) -> crate::Result<Expression> {
    match yaml {
        Yaml::Mapping(m) => parse_mapping(m),
        Yaml::Sequence(s) => {
            // We allow a sequence of maps only on the root
            let mut it = s.iter();
            match it.next() {
                Some(v) => match &v {
                    Yaml::Mapping(m) => {
                        let mut expressions = vec![parse_mapping(m)?];
                        for value in it {
                            // NOTE: A sequence can only be one type
                            if let Yaml::Mapping(mapping) = value {
                                expressions.push(parse_mapping(mapping)?);
                            } else {
                                return Err(crate::error::parse_invalid_ident(format!(
                                    "expected a sequence of mappings, encountered - {:?}",
                                    yaml
                                )));
                            }
                        }
                        Ok(Expression::BooleanGroup(BoolSym::Or, expressions))
                    }
                    _ => Err(crate::error::parse_invalid_ident(format!(
                        "expected a sequence of mappings, encountered - {:?}",
                        yaml
                    ))),
                },
                None => Err(crate::error::parse_invalid_ident(format!(
                    "expected a non empty sequence of mappings, encountered - {:?}",
                    yaml
                ))),
            }
        }
        _ => Err(crate::error::parse_invalid_ident(format!(
            "expected mapping or sequence, encountered - {:?}",
            yaml
        ))),
    }
}

// TODO: Extract common code and try to make this function a little bit more readable
fn parse_mapping(mapping: &Mapping) -> crate::Result<Expression> {
    let mut expressions = vec![];
    for (k, v) in mapping {
        let mut misc: Option<ModSym> = None;
        let (e, f) = match k {
            Yaml::String(s) => {
                // NOTE: Tokenise splits on whitespace, but this is undesired for keys, merge them
                // back together
                let mut identifier = vec![];
                let mut tokens = vec![];
                for token in s.tokenise()? {
                    match token {
                        Token::Identifier(s) => identifier.push(s),
                        _ => {
                            if !identifier.is_empty() {
                                tokens.push(Token::Identifier(identifier.join(" ")));
                                identifier.clear();
                            }
                            tokens.push(token);
                        }
                    }
                }
                if !identifier.is_empty() {
                    tokens.push(Token::Identifier(identifier.join(" ")));
                    identifier.clear();
                }
                let expr = parse(&tokens)?;
                let (e, s) = match expr {
                    Expression::Cast(f, s) => {
                        misc = Some(s.clone());
                        match s {
                            ModSym::Flt => (Expression::Cast(f.clone(), s), f),
                            ModSym::Int => (Expression::Cast(f.clone(), s), f),
                            ModSym::Not => (Expression::Field(f.clone()), f),
                            ModSym::Str => (Expression::Cast(f.clone(), s), f),
                        }
                    }
                    Expression::Identifier(s) => (Expression::Field(s.clone()), s),
                    Expression::Match(m, i) => {
                        if let Yaml::Sequence(_) = v {
                            match *i {
                                Expression::Identifier(s) => (
                                    Expression::Match(m, Box::new(Expression::Field(s.clone()))),
                                    s,
                                ),
                                _ => {
                                    return Err(crate::error::parse_invalid_ident(format!(
                                        "match condition mut contain a field, encountered - {:?}",
                                        k
                                    )));
                                }
                            }
                        } else {
                            return Err(crate::error::parse_invalid_ident(format!(
                                "match condition is only valid for sequences, encountered - {:?}",
                                k
                            )));
                        }
                    }
                    _ => {
                        return Err(crate::error::parse_invalid_ident(format!(
                            "mapping key must be a string or valid match condition, encountered - {:?}",
                            k
                        )));
                    }
                };
                (e, s)
            }
            _ => {
                return Err(crate::error::parse_invalid_ident(format!(
                    "mapping key must be a string, encountered - {:?}",
                    k
                )));
            }
        };
        let expression = match v {
            Yaml::Bool(b) => {
                if let Some(ModSym::Int) = misc {
                    Expression::BooleanExpression(
                        Box::new(e.clone()),
                        BoolSym::Equal,
                        Box::new(Expression::Integer(if *b { 1 } else { 0 })),
                    )
                } else if let Some(ModSym::Str) = misc {
                    Expression::Search(Search::Exact(b.to_string()), f.to_owned(), true)
                } else {
                    Expression::BooleanExpression(
                        Box::new(e.clone()),
                        BoolSym::Equal,
                        Box::new(Expression::Boolean(*b)),
                    )
                }
            }
            Yaml::Number(n) => {
                if let Some(i) = n.as_i64() {
                    if let Some(ModSym::Str) = misc {
                        Expression::Search(Search::Exact(i.to_string()), f.to_owned(), true)
                    } else {
                        Expression::BooleanExpression(
                            Box::new(e.clone()),
                            BoolSym::Equal,
                            Box::new(Expression::Integer(i)),
                        )
                    }
                } else if let Some(i) = n.as_f64() {
                    if let Some(ModSym::Int) = misc {
                        return Err(crate::error::parse_invalid_ident(format!(
                            "float cannot be cast into an integer, encountered - {:?}",
                            k
                        )));
                    } else if let Some(ModSym::Str) = misc {
                        Expression::Search(Search::Exact(i.to_string()), f.to_owned(), true)
                    } else {
                        Expression::BooleanExpression(
                            Box::new(e.clone()),
                            BoolSym::Equal,
                            Box::new(Expression::Float(i)),
                        )
                    }
                } else {
                    return Err(crate::error::parse_invalid_ident(format!(
                        "number must be a signed integer or float, encountered - {:?}",
                        k
                    )));
                }
            }
            Yaml::Null => Expression::BooleanExpression(
                Box::new(e.clone()),
                BoolSym::Equal,
                Box::new(Expression::Null),
            ),
            Yaml::String(s) => {
                let identifier = s.to_owned().into_identifier()?;
                let mut cast = false;
                if let Some(ref m) = misc {
                    if let ModSym::Str = m {
                        cast = true;
                    }
                    match &identifier.pattern {
                        Pattern::Any
                        | Pattern::Regex(_)
                        | Pattern::Contains(_)
                        | Pattern::EndsWith(_)
                        | Pattern::Exact(_)
                        | Pattern::StartsWith(_) => {
                            if let ModSym::Int = m {
                                return Err(crate::error::parse_invalid_ident(format!(
                                    "cannot cast string to integer, encountered - {:?}",
                                    k
                                )));
                            }
                        }
                        Pattern::Equal(_)
                        | Pattern::GreaterThan(_)
                        | Pattern::GreaterThanOrEqual(_)
                        | Pattern::LessThan(_)
                        | Pattern::LessThanOrEqual(_)
                        | Pattern::FEqual(_)
                        | Pattern::FGreaterThan(_)
                        | Pattern::FGreaterThanOrEqual(_)
                        | Pattern::FLessThan(_)
                        | Pattern::FLessThanOrEqual(_) => {
                            if let ModSym::Str = m {
                                return Err(crate::error::parse_invalid_ident(format!(
                                    "cannot cast integer to string, encountered - {:?}",
                                    k
                                )));
                            }
                        }
                    }
                }
                match identifier.pattern {
                    Pattern::Equal(i) => Expression::BooleanExpression(
                        Box::new(e.clone()),
                        BoolSym::Equal,
                        Box::new(Expression::Integer(i)),
                    ),
                    Pattern::GreaterThan(i) => Expression::BooleanExpression(
                        Box::new(e.clone()),
                        BoolSym::GreaterThan,
                        Box::new(Expression::Integer(i)),
                    ),
                    Pattern::GreaterThanOrEqual(i) => Expression::BooleanExpression(
                        Box::new(e.clone()),
                        BoolSym::GreaterThanOrEqual,
                        Box::new(Expression::Integer(i)),
                    ),
                    Pattern::LessThan(i) => Expression::BooleanExpression(
                        Box::new(e.clone()),
                        BoolSym::LessThan,
                        Box::new(Expression::Integer(i)),
                    ),
                    Pattern::LessThanOrEqual(i) => Expression::BooleanExpression(
                        Box::new(e.clone()),
                        BoolSym::LessThanOrEqual,
                        Box::new(Expression::Integer(i)),
                    ),
                    Pattern::FEqual(i) => Expression::BooleanExpression(
                        Box::new(e.clone()),
                        BoolSym::Equal,
                        Box::new(Expression::Float(i)),
                    ),
                    Pattern::FGreaterThan(i) => Expression::BooleanExpression(
                        Box::new(e.clone()),
                        BoolSym::GreaterThan,
                        Box::new(Expression::Float(i)),
                    ),
                    Pattern::FGreaterThanOrEqual(i) => Expression::BooleanExpression(
                        Box::new(e.clone()),
                        BoolSym::GreaterThanOrEqual,
                        Box::new(Expression::Float(i)),
                    ),
                    Pattern::FLessThan(i) => Expression::BooleanExpression(
                        Box::new(e.clone()),
                        BoolSym::LessThan,
                        Box::new(Expression::Float(i)),
                    ),
                    Pattern::FLessThanOrEqual(i) => Expression::BooleanExpression(
                        Box::new(e.clone()),
                        BoolSym::LessThanOrEqual,
                        Box::new(Expression::Float(i)),
                    ),
                    Pattern::Any => Expression::Search(Search::Any, f.to_owned(), cast),
                    Pattern::Regex(c) => Expression::Search(
                        Search::Regex(c, identifier.ignore_case),
                        f.to_owned(),
                        cast,
                    ),
                    Pattern::Contains(c) => Expression::Search(
                        if identifier.ignore_case {
                            Search::AhoCorasick(
                                Box::new(
                                    AhoCorasickBuilder::new()
                                        .ascii_case_insensitive(true)
                                        .kind(Some(AhoCorasickKind::DFA))
                                        .build(vec![c.clone()])
                                        .expect("failed to build dfa"),
                                ),
                                vec![MatchType::Contains(c)],
                                true,
                            )
                        } else {
                            Search::Contains(c)
                        },
                        f.to_owned(),
                        cast,
                    ),
                    Pattern::EndsWith(c) => Expression::Search(
                        if identifier.ignore_case {
                            Search::AhoCorasick(
                                Box::new(
                                    AhoCorasickBuilder::new()
                                        .ascii_case_insensitive(true)
                                        .kind(Some(AhoCorasickKind::DFA))
                                        .build(vec![c.clone()])
                                        .expect("failed to build dfa"),
                                ),
                                vec![MatchType::EndsWith(c)],
                                true,
                            )
                        } else {
                            Search::EndsWith(c)
                        },
                        f.to_owned(),
                        cast,
                    ),
                    Pattern::Exact(c) => Expression::Search(
                        if !c.is_empty() && identifier.ignore_case {
                            Search::AhoCorasick(
                                Box::new(
                                    AhoCorasickBuilder::new()
                                        .ascii_case_insensitive(true)
                                        .kind(Some(AhoCorasickKind::DFA))
                                        .build(vec![c.clone()])
                                        .expect("failed to build dfa"),
                                ),
                                vec![MatchType::Exact(c)],
                                true,
                            )
                        } else {
                            Search::Exact(c)
                        },
                        f.to_owned(),
                        cast,
                    ),
                    Pattern::StartsWith(c) => Expression::Search(
                        if identifier.ignore_case {
                            Search::AhoCorasick(
                                Box::new(
                                    AhoCorasickBuilder::new()
                                        .ascii_case_insensitive(true)
                                        .kind(Some(AhoCorasickKind::DFA))
                                        .build(vec![c.clone()])
                                        .expect("failed to build dfa"),
                                ),
                                vec![MatchType::StartsWith(c)],
                                true,
                            )
                        } else {
                            Search::StartsWith(c)
                        },
                        f.to_owned(),
                        cast,
                    ),
                }
            }
            Yaml::Mapping(m) => {
                if misc.is_some() {
                    return Err(crate::error::parse_invalid_ident(format!(
                        "nested mappings are not supported when casting or negating a field, encountered - {:?}",
                        k
                    )));
                }
                Expression::Nested(f.to_owned(), Box::new(parse_mapping(m)?))
            }
            Yaml::Sequence(s) => {
                // TODO: This block could probably be cleaned...
                // Now we need to be as fast as possible it turns out that builtin strings functions are
                // fastest when we only need to check a single condition, when we need to check more that
                // one AhoCorasick becomes the quicker, even though AC should be as fast as starts_with and
                // contains... We also want to order in terms of quickest on average:
                //
                //  1. ExactMatch
                //  2. StartsWith
                //  3. EndsWith
                //  4. Contains
                //  5. AhoCorasick
                //  6. Regex
                //
                //  And for the above use AhoCorasick when list is more than one for 2,3,4
                let mut exact: Vec<Identifier> = vec![];
                let mut starts_with: Vec<Identifier> = vec![];
                let mut ends_with: Vec<Identifier> = vec![];
                let mut contains: Vec<Identifier> = vec![];
                let mut regex: Vec<Identifier> = vec![];
                let mut rest: Vec<Expression> = vec![]; // NOTE: Don't care about speed of numbers atm

                let mut boolean = false;
                let mut cast = false;
                let mut mapping = false;
                let mut number = false;
                let mut string = false;

                let unmatched_e = if let Expression::Match(_, e) = &e {
                    *e.clone()
                } else {
                    e.clone()
                };

                for value in s {
                    let identifier = match value {
                        Yaml::Bool(b) => {
                            if let Some(ModSym::Int) = misc {
                                number = true;
                                rest.push(Expression::BooleanExpression(
                                    Box::new(unmatched_e.clone()),
                                    BoolSym::Equal,
                                    Box::new(Expression::Integer(if *b { 1 } else { 0 })),
                                ))
                            } else if let Some(ModSym::Str) = misc {
                                string = true;
                                exact.push(Identifier {
                                    ignore_case: false,
                                    pattern: Pattern::Exact(b.to_string()),
                                });
                            } else {
                                boolean = true;
                                rest.push(Expression::BooleanExpression(
                                    Box::new(e.clone()),
                                    BoolSym::Equal,
                                    Box::new(Expression::Boolean(*b)),
                                ))
                            }
                            continue;
                        }
                        Yaml::Null => {
                            rest.push(Expression::BooleanExpression(
                                Box::new(unmatched_e.clone()),
                                BoolSym::Equal,
                                Box::new(Expression::Null),
                            ));
                            continue;
                        }
                        Yaml::Number(n) => {
                            if let Some(i) = n.as_i64() {
                                if let Some(ModSym::Str) = misc {
                                    string = true;
                                    exact.push(Identifier {
                                        ignore_case: false,
                                        pattern: Pattern::Exact(i.to_string()),
                                    });
                                } else {
                                    number = true;
                                    rest.push(Expression::BooleanExpression(
                                        Box::new(e.clone()),
                                        BoolSym::Equal,
                                        Box::new(Expression::Integer(i)),
                                    ));
                                }
                                continue;
                            } else if let Some(i) = n.as_f64() {
                                if let Some(ModSym::Int) = misc {
                                    return Err(crate::error::parse_invalid_ident(format!(
                                        "float cannot be cast into an integer, encountered - {:?}",
                                        k
                                    )));
                                } else if let Some(ModSym::Str) = misc {
                                    string = true;
                                    exact.push(Identifier {
                                        ignore_case: false,
                                        pattern: Pattern::Exact(i.to_string()),
                                    });
                                } else {
                                    number = true;
                                    rest.push(Expression::BooleanExpression(
                                        Box::new(e.clone()),
                                        BoolSym::Equal,
                                        Box::new(Expression::Float(i)),
                                    ))
                                }
                                continue;
                            }
                            return Err(crate::error::parse_invalid_ident(format!(
                                "number must be a signed integer or float, encountered - {:?}",
                                k
                            )));
                        }
                        Yaml::String(s) => s.clone().into_identifier()?,

                        Yaml::Mapping(m) => {
                            if misc.is_some() {
                                return Err(crate::error::parse_invalid_ident(format!(
                                    "nested mappings are not supported when casting or negating a field, encountered - {:?}",
                                    k
                                )));
                            }
                            mapping = true;
                            // FIXME: We should be nesting at the end of the squence, currently we
                            // have to shake to remove this...
                            rest.push(Expression::Nested(
                                f.to_owned(),
                                Box::new(parse_mapping(m)?),
                            ));
                            continue;
                        }
                        _ => {
                            return Err(crate::error::parse_invalid_ident(format!(
                                "value must be a mapping or string, encountered - {:?}",
                                k
                            )));
                        }
                    };
                    if let Some(ref m) = misc {
                        if let ModSym::Str = m {
                            cast = true;
                        }
                        match &identifier.pattern {
                            Pattern::Any
                            | Pattern::Regex(_)
                            | Pattern::Contains(_)
                            | Pattern::EndsWith(_)
                            | Pattern::Exact(_)
                            | Pattern::StartsWith(_) => {
                                if let ModSym::Int = m {
                                    return Err(crate::error::parse_invalid_ident(format!(
                                        "cannot cast string to integer, encountered - {:?}",
                                        k
                                    )));
                                }
                            }
                            Pattern::Equal(_)
                            | Pattern::GreaterThan(_)
                            | Pattern::GreaterThanOrEqual(_)
                            | Pattern::LessThan(_)
                            | Pattern::LessThanOrEqual(_)
                            | Pattern::FEqual(_)
                            | Pattern::FGreaterThan(_)
                            | Pattern::FGreaterThanOrEqual(_)
                            | Pattern::FLessThan(_)
                            | Pattern::FLessThanOrEqual(_) => {
                                if let ModSym::Str = m {
                                    return Err(crate::error::parse_invalid_ident(format!(
                                        "cannot cast integer to string, encountered - {:?}",
                                        k
                                    )));
                                }
                            }
                        }
                    }
                    match identifier.pattern {
                        Pattern::Exact(_) => {
                            string = true;
                            exact.push(identifier)
                        }
                        Pattern::StartsWith(_) => {
                            string = true;
                            starts_with.push(identifier)
                        }
                        Pattern::EndsWith(_) => {
                            string = true;
                            ends_with.push(identifier)
                        }
                        Pattern::Contains(_) => {
                            string = true;
                            contains.push(identifier)
                        }
                        Pattern::Regex(_) => {
                            string = true;
                            regex.push(identifier)
                        }
                        Pattern::Any => {
                            string = true;
                            rest.push(Expression::Search(Search::Any, f.to_owned(), cast))
                        }
                        Pattern::Equal(i) => {
                            number = true;
                            rest.push(Expression::BooleanExpression(
                                Box::new(e.clone()),
                                BoolSym::Equal,
                                Box::new(Expression::Integer(i)),
                            ))
                        }
                        Pattern::GreaterThan(i) => {
                            number = true;
                            rest.push(Expression::BooleanExpression(
                                Box::new(e.clone()),
                                BoolSym::GreaterThan,
                                Box::new(Expression::Integer(i)),
                            ))
                        }
                        Pattern::GreaterThanOrEqual(i) => {
                            number = true;
                            rest.push(Expression::BooleanExpression(
                                Box::new(e.clone()),
                                BoolSym::GreaterThanOrEqual,
                                Box::new(Expression::Integer(i)),
                            ))
                        }
                        Pattern::LessThan(i) => {
                            number = true;
                            rest.push(Expression::BooleanExpression(
                                Box::new(e.clone()),
                                BoolSym::LessThan,
                                Box::new(Expression::Integer(i)),
                            ))
                        }
                        Pattern::LessThanOrEqual(i) => {
                            number = true;
                            rest.push(Expression::BooleanExpression(
                                Box::new(e.clone()),
                                BoolSym::LessThanOrEqual,
                                Box::new(Expression::Integer(i)),
                            ))
                        }
                        Pattern::FEqual(i) => {
                            number = true;
                            rest.push(Expression::BooleanExpression(
                                Box::new(e.clone()),
                                BoolSym::Equal,
                                Box::new(Expression::Float(i)),
                            ))
                        }
                        Pattern::FGreaterThan(i) => {
                            number = true;
                            rest.push(Expression::BooleanExpression(
                                Box::new(e.clone()),
                                BoolSym::GreaterThan,
                                Box::new(Expression::Float(i)),
                            ))
                        }
                        Pattern::FGreaterThanOrEqual(i) => {
                            number = true;
                            rest.push(Expression::BooleanExpression(
                                Box::new(e.clone()),
                                BoolSym::GreaterThanOrEqual,
                                Box::new(Expression::Float(i)),
                            ))
                        }
                        Pattern::FLessThan(i) => {
                            number = true;
                            rest.push(Expression::BooleanExpression(
                                Box::new(e.clone()),
                                BoolSym::LessThan,
                                Box::new(Expression::Float(i)),
                            ))
                        }
                        Pattern::FLessThanOrEqual(i) => {
                            number = true;
                            rest.push(Expression::BooleanExpression(
                                Box::new(e.clone()),
                                BoolSym::LessThanOrEqual,
                                Box::new(Expression::Float(i)),
                            ))
                        }
                    }
                }
                let mut multiple = false;
                let mut group: Vec<Expression> = vec![];
                let mut context: Vec<MatchType> = vec![];
                let mut needles: Vec<String> = vec![];
                let mut icontext: Vec<MatchType> = vec![];
                let mut ineedles: Vec<String> = vec![];
                let mut regex_set: Vec<Regex> = vec![];
                let mut iregex_set: Vec<Regex> = vec![];
                for i in starts_with.into_iter() {
                    if let Pattern::StartsWith(s) = i.pattern {
                        if i.ignore_case {
                            icontext.push(MatchType::StartsWith(s.clone()));
                            ineedles.push(s);
                        } else {
                            context.push(MatchType::StartsWith(s.clone()));
                            needles.push(s);
                        }
                    }
                }
                for i in contains.into_iter() {
                    if let Pattern::Contains(s) = i.pattern {
                        if i.ignore_case {
                            icontext.push(MatchType::Contains(s.clone()));
                            ineedles.push(s);
                        } else {
                            context.push(MatchType::Contains(s.clone()));
                            needles.push(s);
                        }
                    }
                }
                for i in ends_with.into_iter() {
                    if let Pattern::EndsWith(s) = i.pattern {
                        if i.ignore_case {
                            icontext.push(MatchType::EndsWith(s.clone()));
                            ineedles.push(s);
                        } else {
                            context.push(MatchType::EndsWith(s.clone()));
                            needles.push(s);
                        }
                    }
                }
                for i in exact.into_iter() {
                    if let Pattern::Exact(s) = i.pattern {
                        // NOTE: Do not allow empty string into the needles as it causes massive slow down,
                        // don't ask me why I have not looked into it!
                        if s.is_empty() {
                            group.push(Expression::Search(Search::Exact(s), f.to_owned(), cast));
                        } else if i.ignore_case {
                            icontext.push(MatchType::Exact(s.clone()));
                            ineedles.push(s);
                        } else {
                            context.push(MatchType::Exact(s.clone()));
                            needles.push(s);
                        }
                    }
                }
                for i in regex.into_iter() {
                    if let Pattern::Regex(r) = i.pattern {
                        if i.ignore_case {
                            iregex_set.push(r);
                        } else {
                            regex_set.push(r);
                        }
                    }
                }
                if !needles.is_empty() {
                    if needles.len() == 1 {
                        let s = match context.into_iter().next().expect("failed to get context") {
                            MatchType::Contains(c) => Search::Contains(c),
                            MatchType::EndsWith(c) => Search::EndsWith(c),
                            MatchType::Exact(c) => Search::Exact(c),
                            MatchType::StartsWith(c) => Search::StartsWith(c),
                        };
                        group.push(Expression::Search(s, f.to_owned(), cast));
                    } else {
                        multiple = true;
                        group.push(Expression::Search(
                            Search::AhoCorasick(
                                Box::new(
                                    AhoCorasickBuilder::new()
                                        .kind(Some(AhoCorasickKind::DFA))
                                        .build(needles)
                                        .expect("failed to build dfa"),
                                ),
                                context,
                                false,
                            ),
                            f.to_owned(),
                            cast,
                        ));
                    }
                }
                if !ineedles.is_empty() {
                    multiple = true;
                    group.push(Expression::Search(
                        Search::AhoCorasick(
                            Box::new(
                                AhoCorasickBuilder::new()
                                    .ascii_case_insensitive(true)
                                    .kind(Some(AhoCorasickKind::DFA))
                                    .build(ineedles)
                                    .expect("failed to build dfa"),
                            ),
                            icontext,
                            true,
                        ),
                        f.to_owned(),
                        cast,
                    ));
                }
                if !regex_set.is_empty() {
                    if regex_set.len() == 1 {
                        group.push(Expression::Search(
                            Search::Regex(
                                regex_set.into_iter().next().expect("failed to get regex"),
                                false,
                            ),
                            f.to_owned(),
                            cast,
                        ));
                    } else {
                        multiple = true;
                        group.push(Expression::Search(
                            Search::RegexSet(
                                RegexSetBuilder::new(
                                    regex_set
                                        .into_iter()
                                        .map(|r| r.as_str().to_string())
                                        .collect::<Vec<_>>(),
                                )
                                .build()
                                .expect("could not build regex set"),
                                false,
                            ),
                            f.to_owned(),
                            cast,
                        ));
                    }
                }
                if !iregex_set.is_empty() {
                    if iregex_set.len() == 1 {
                        group.push(Expression::Search(
                            Search::Regex(
                                iregex_set.into_iter().next().expect("failed to get regex"),
                                true,
                            ),
                            f.to_owned(),
                            cast,
                        ));
                    } else {
                        multiple = true;
                        group.push(Expression::Search(
                            Search::RegexSet(
                                RegexSetBuilder::new(
                                    iregex_set
                                        .into_iter()
                                        .map(|r| r.as_str().to_string())
                                        .collect::<Vec<_>>(),
                                )
                                .case_insensitive(true)
                                .build()
                                .expect("could not build regex set"),
                                true,
                            ),
                            f.to_owned(),
                            cast,
                        ));
                    }
                }
                group.extend(rest);
                if let Expression::Match(Match::All, _) | Expression::Match(Match::Of(_), _) = &e {
                    if boolean as i32 + mapping as i32 + number as i32 + string as i32 > 1 {
                        return Err(crate::error::parse_invalid_ident(
                            "when using sequence modifiers the all expressions must be of the same type",
                        ));
                    }
                }
                if let Some(misc) = &misc {
                    if let ModSym::Int = misc {
                        if boolean || mapping || string {
                            return Err(crate::error::parse_invalid_ident(
                                "when casting to int all expressions must be of type int",
                            ));
                        }
                    }
                    if let ModSym::Str = &misc {
                        if boolean || mapping || number {
                            return Err(crate::error::parse_invalid_ident(
                                "when casting to str all expressions must be of type str",
                            ));
                        }
                    }
                }
                if group.is_empty() {
                    return Err(crate::error::parse_invalid_ident("failed to parse mapping"));
                } else if !multiple && group.len() == 1 {
                    group.into_iter().next().expect("could not get expression")
                } else if let Expression::Match(m, _) = e {
                    if group.len() == 1 {
                        let group = group.into_iter().next().expect("could not get expression");
                        Expression::Match(m, Box::new(group))
                    } else {
                        Expression::Match(m, Box::new(Expression::BooleanGroup(BoolSym::Or, group)))
                    }
                } else {
                    Expression::BooleanGroup(BoolSym::Or, group)
                }
            }
            Yaml::Tagged(_) => {
                return Err(crate::error::parse_invalid_ident(
                    "!Tag syntax is not supported",
                ));
            }
        };
        if let Some(ModSym::Not) = misc {
            expressions.push(Expression::Negate(Box::new(expression)));
        } else {
            expressions.push(expression);
        }
    }
    if expressions.is_empty() {
        return Err(crate::error::parse_invalid_ident("failed to parse mapping"));
    } else if expressions.len() == 1 {
        return Ok(expressions.into_iter().next().expect("missing expression"));
    }
    Ok(Expression::BooleanGroup(BoolSym::And, expressions))
}

#[cfg(test)]
mod tests {
    use super::*;

    use serde_yaml::Value as Yaml;

    #[test]
    fn parse_bool_group_match_search() {
        let identifier = r"all(foo): [bar, '*']";
        let yaml: Yaml = serde_yaml::from_str(identifier).unwrap();
        let e = super::parse_identifier(&yaml).unwrap();
        assert_eq!(
            Expression::Match(
                Match::All,
                Box::new(Expression::BooleanGroup(
                    BoolSym::Or,
                    vec![
                        Expression::Search(
                            Search::Exact("bar".to_owned()),
                            "foo".to_owned(),
                            false
                        ),
                        Expression::Search(Search::Any, "foo".to_owned(), false)
                    ]
                ))
            ),
            e
        );
    }

    #[test]
    fn parse_bool_group_match_search_shake() {
        let identifier = r"all(foo): [bar]";
        let yaml: Yaml = serde_yaml::from_str(identifier).unwrap();
        let e = super::parse_identifier(&yaml).unwrap();
        assert_eq!(
            Expression::Search(Search::Exact("bar".to_owned()), "foo".to_string(), false),
            e
        );
    }

    #[test]
    fn parse_bool_expr() {
        let e = parse(&vec![
            Token::Identifier("foo".to_string()),
            Token::Operator(BoolSym::And),
            Token::Identifier("bar".to_string()),
        ])
        .unwrap();
        assert_eq!(
            Expression::BooleanExpression(
                Box::new(Expression::Identifier("foo".to_string())),
                BoolSym::And,
                Box::new(Expression::Identifier("bar".to_string()))
            ),
            e
        );
    }

    #[test]
    fn parse_cast() {
        let e = parse(&vec![
            Token::Modifier(ModSym::Int),
            Token::Delimiter(DelSym::LeftParenthesis),
            Token::Identifier("identifier".to_owned()),
            Token::Delimiter(DelSym::RightParenthesis),
        ])
        .unwrap();
        assert_eq!(Expression::Cast("identifier".to_string(), ModSym::Int), e);

        let e = parse(&vec![
            Token::Modifier(ModSym::Not),
            Token::Delimiter(DelSym::LeftParenthesis),
            Token::Identifier("identifier".to_owned()),
            Token::Delimiter(DelSym::RightParenthesis),
        ])
        .unwrap();
        assert_eq!(Expression::Cast("identifier".to_string(), ModSym::Not), e);

        let e = parse(&vec![
            Token::Modifier(ModSym::Str),
            Token::Delimiter(DelSym::LeftParenthesis),
            Token::Identifier("identifier".to_owned()),
            Token::Delimiter(DelSym::RightParenthesis),
        ])
        .unwrap();
        assert_eq!(Expression::Cast("identifier".to_string(), ModSym::Str), e);
    }

    #[test]
    fn parse_identifier() {
        let e = parse(&vec![Token::Identifier("condition".to_string())]).unwrap();
        assert_eq!(Expression::Identifier("condition".to_string()), e);
    }

    #[test]
    fn parse_integer() {
        let e = parse(&vec![Token::Integer(1)]).unwrap();
        assert_eq!(Expression::Integer(1), e);
    }

    #[test]
    fn parse_negate() {
        let e = parse(&vec![
            Token::Miscellaneous(MiscSym::Not),
            Token::Delimiter(DelSym::LeftParenthesis),
            Token::Identifier("foo".to_string()),
            Token::Operator(BoolSym::Or),
            Token::Identifier("bar".to_string()),
            Token::Delimiter(DelSym::RightParenthesis),
        ])
        .unwrap();
        assert_eq!(
            Expression::Negate(Box::new(Expression::BooleanExpression(
                Box::new(Expression::Identifier("foo".to_string())),
                BoolSym::Or,
                Box::new(Expression::Identifier("bar".to_string()))
            ))),
            e
        );
    }

    #[test]
    fn parse_nested() {
        let identifier = r"foo: {bar: baz}";
        let yaml: Yaml = serde_yaml::from_str(identifier).unwrap();
        let e = super::parse_identifier(&yaml).unwrap();
        assert_eq!(
            Expression::Nested(
                "foo".to_owned(),
                Box::new(Expression::Search(
                    Search::Exact("baz".to_owned()),
                    "bar".to_owned(),
                    false
                ))
            ),
            e
        );
    }

    #[test]
    fn parse_expression_0() {
        let t = parse(&vec![
            Token::Delimiter(DelSym::LeftParenthesis),
            Token::Identifier("foo".to_string()),
            Token::Operator(BoolSym::And),
            Token::Identifier("bar".to_string()),
            Token::Delimiter(DelSym::RightParenthesis),
            Token::Operator(BoolSym::Or),
            Token::Identifier("fooz".to_string()),
        ])
        .unwrap();
        assert_eq!(
            Expression::BooleanExpression(
                Box::new(Expression::BooleanExpression(
                    Box::new(Expression::Identifier("foo".to_string())),
                    BoolSym::And,
                    Box::new(Expression::Identifier("bar".to_string()))
                )),
                BoolSym::Or,
                Box::new(Expression::Identifier("fooz".to_string()))
            ),
            t
        );
    }

    #[test]
    fn parse_expression_1() {
        let t = parse(&vec![
            Token::Identifier("foo".to_string()),
            Token::Operator(BoolSym::And),
            Token::Delimiter(DelSym::LeftParenthesis),
            Token::Identifier("bar".to_string()),
            Token::Operator(BoolSym::Or),
            Token::Identifier("fooz".to_string()),
            Token::Delimiter(DelSym::RightParenthesis),
        ])
        .unwrap();
        assert_eq!(
            Expression::BooleanExpression(
                Box::new(Expression::Identifier("foo".to_string())),
                BoolSym::And,
                Box::new(Expression::BooleanExpression(
                    Box::new(Expression::Identifier("bar".to_string())),
                    BoolSym::Or,
                    Box::new(Expression::Identifier("fooz".to_string()))
                ))
            ),
            t
        );
    }

    #[test]
    fn parse_expression_2() {
        let t = parse(&vec![
            Token::Identifier("foo".to_string()),
            Token::Operator(BoolSym::And),
            Token::Delimiter(DelSym::LeftParenthesis),
            Token::Miscellaneous(MiscSym::Not),
            Token::Identifier("bar".to_string()),
            Token::Operator(BoolSym::And),
            Token::Miscellaneous(MiscSym::Not),
            Token::Identifier("fooz".to_string()),
            Token::Delimiter(DelSym::RightParenthesis),
        ])
        .unwrap();
        assert_eq!(
            Expression::BooleanExpression(
                Box::new(Expression::Identifier("foo".to_string())),
                BoolSym::And,
                Box::new(Expression::BooleanExpression(
                    Box::new(Expression::Negate(Box::new(Expression::Identifier(
                        "bar".to_string()
                    )))),
                    BoolSym::And,
                    Box::new(Expression::Negate(Box::new(Expression::Identifier(
                        "fooz".to_string()
                    ))))
                ))
            ),
            t
        );
    }

    #[test]
    fn parse_identifiers_0() {
        let identifier = "[foo: bar]";
        let yaml: Yaml = serde_yaml::from_str(&identifier).unwrap();
        let e = super::parse_identifier(&yaml).unwrap();
        assert_eq!(
            Expression::BooleanGroup(
                BoolSym::Or,
                vec![Expression::Search(
                    Search::Exact("bar".to_owned()),
                    "foo".to_owned(),
                    false
                )]
            ),
            e
        );
    }

    #[test]
    fn parse_invalid_0() {
        let e = parse(&vec![
            Token::Miscellaneous(MiscSym::Not),
            Token::Modifier(ModSym::Int),
            Token::Delimiter(DelSym::LeftParenthesis),
            Token::Identifier("condition".to_string()),
            Token::Delimiter(DelSym::RightParenthesis),
        ]);
        assert!(e.is_err());
    }
}
