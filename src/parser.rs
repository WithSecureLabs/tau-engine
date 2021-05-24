use std::iter::Iterator;
use std::iter::Peekable;

use aho_corasick::{AhoCorasick, AhoCorasickBuilder};
use regex::Regex;
use serde_yaml::{Mapping, Value as Yaml};
use tracing::debug;

use crate::identifier::{Identifier, IdentifierParser};
use crate::tokeniser::{BoolSym, DelSym, MiscSym, Token};

#[derive(Clone, Debug, PartialEq)]
pub enum MatchType {
    Contains(String),
    EndsWith(String),
    Exact(String),
    StartsWith(String),
}

#[derive(Clone, Debug)]
pub enum Search {
    AhoCorasick(Box<AhoCorasick>, Vec<MatchType>),
    Contains(String),
    EndsWith(String),
    Exact(String),
    Regex(Regex),
    StartsWith(String),
}
impl PartialEq for Search {
    fn eq(&self, other: &Search) -> bool {
        match (self, other) {
            (Search::AhoCorasick(_, m0), Search::AhoCorasick(_, m1)) => m0 == m1,
            (Search::Contains(s0), Search::Contains(s1)) => s0 == s1,
            (Search::EndsWith(s0), Search::EndsWith(s1)) => s0 == s1,
            (Search::Exact(s0), Search::Exact(s1)) => s0 == s1,
            (Search::Regex(r0), Search::Regex(r1)) => r0.as_str() == r1.as_str(),
            (Search::StartsWith(s0), Search::StartsWith(s1)) => s0 == s1,
            (_, _) => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    BooleanExpression(Box<Expression>, BoolSym, Box<Expression>),
    Cast(String, MiscSym),
    Field(String),
    Identifier(String),
    Integer(i32),
    Negate(Box<Expression>),
    Search(Search, String),
}

// Pratt Parser used to parse the token stream
//
// Left-Denotation (LED) - how an operator consumes to the right with a left-context
// Null-Denotation (NUD) - how an operator consumes to the right with no left-context

pub fn parse(tokens: &[Token]) -> crate::Result<Expression> {
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
                let symbol = match *s {
                    BoolSym::And => BoolSym::And,
                    BoolSym::Equal => BoolSym::Equal,
                    BoolSym::GreaterThan => BoolSym::GreaterThan,
                    BoolSym::GreaterThanOrEqual => BoolSym::GreaterThanOrEqual,
                    BoolSym::LessThan => BoolSym::LessThan,
                    BoolSym::LessThanOrEqual => BoolSym::LessThanOrEqual,
                    BoolSym::Or => BoolSym::Or,
                };
                let right = parse_expr(it, t.binding_power())?;
                // Handle special limited cases
                match symbol {
                    BoolSym::Equal => {
                        match left {
                            Expression::Cast(_, _) | Expression::Integer(_) => {}
                            _ => {
                                return Err(crate::error::parse_led_preceding(format!(
                                    "encountered - '{:?}'",
                                    t
                                )));
                            }
                        }
                        match right {
                            Expression::Cast(_, _) | Expression::Integer(_) => {}
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
                                Expression::Cast(_, MiscSym::Int),
                                Expression::Cast(_, MiscSym::Int),
                            ) => {}
                            (
                                Expression::Cast(_, MiscSym::Str),
                                Expression::Cast(_, MiscSym::Str),
                            ) => {}
                            (Expression::Cast(_, MiscSym::Int), Expression::Integer(_)) => {}
                            (Expression::Integer(_), Expression::Cast(_, MiscSym::Int)) => {}
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
                            Expression::Cast(_, _) | Expression::Integer(_) => {}
                            _ => {
                                return Err(crate::error::parse_led_preceding(format!(
                                    "encountered - '{:?}'",
                                    t
                                )));
                            }
                        }
                        match right {
                            Expression::Cast(_, _) | Expression::Integer(_) => {}
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
                                Expression::Cast(_, MiscSym::Int),
                                Expression::Cast(_, MiscSym::Int),
                            ) => {}
                            (Expression::Cast(_, MiscSym::Int), Expression::Integer(_)) => {}
                            (Expression::Integer(_), Expression::Cast(_, MiscSym::Int)) => {}
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
            | Token::Identifier(_)
            | Token::Integer(_)
            | Token::Miscellaneous(_)
            | Token::Search(_) => Err(crate::error::parse_invalid_token(format!(
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
                        while let Some(t) = it.next() {
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
                        parse(&&tokens)
                    }
                    DelSym::RightParenthesis => Err(crate::error::parse_invalid_token(format!(
                        "NUD encountered - '{:?}'",
                        t
                    ))),
                },
                Token::Identifier(ref n) => Ok(Expression::Identifier(n.to_string())),
                Token::Integer(ref n) => Ok(Expression::Integer(*n)),
                Token::Miscellaneous(ref m) => match *m {
                    MiscSym::Int => {
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
                                Ok(Expression::Cast(s.to_string(), MiscSym::Int))
                            }
                            _ => Err(crate::error::parse_invalid_token(
                                "NUD expected column identifier",
                            )),
                        }
                    }
                    MiscSym::Not => {
                        // Assume we can negate any expression, why not? Or should this only work for
                        // identifiers?
                        let right = parse_expr(it, t.binding_power())?;
                        Ok(Expression::Negate(Box::new(right)))
                    }
                    MiscSym::Str => {
                        // We expect String(column_identifier)
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
                                Ok(Expression::Cast(s.to_string(), MiscSym::Str))
                            }
                            _ => Err(crate::error::parse_invalid_token(
                                "NUD expected column identifier",
                            )),
                        }
                    }
                },
                Token::Operator(_) | Token::Search(_) => Err(crate::error::parse_invalid_token(
                    format!("NUD encountered - '{:?}'", t),
                )),
            }
        }
        None => Err(crate::error::parse_invalid_token("NUD expected token")),
    }
}

// FIXME: We should really be parsing the identifier blocks using the Pratt parser, but tokenising
// them is hard, so we just cheat for now... :P
pub fn parse_identifier(yaml: &Yaml) -> crate::Result<Expression> {
    match yaml {
        Yaml::Mapping(m) => parse_mapping(m),
        Yaml::Sequence(s) => {
            // We allow a sequence of maps only on the root
            let mut it = s.iter();
            match it.next() {
                Some(v) => match &v {
                    Yaml::Mapping(m) => {
                        let mut expression = parse_mapping(&m)?;
                        for value in it {
                            // NOTE: A sequence can only be one type
                            if let Yaml::Mapping(mapping) = value {
                                expression = Expression::BooleanExpression(
                                    Box::new(expression),
                                    BoolSym::Or,
                                    Box::new(parse_mapping(mapping)?),
                                );
                            } else {
                                return Err(crate::error::parse_invalid_ident(format!(
                                    "expected a sequence of mappings, encountered - {:?}",
                                    yaml
                                )));
                            }
                        }
                        Ok(expression)
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

fn parse_mapping(mapping: &Mapping) -> crate::Result<Expression> {
    let mut expression = None;
    for (k, v) in mapping {
        let k = match k {
            Yaml::String(s) => s,
            _ => {
                return Err(crate::error::parse_invalid_ident(format!(
                    "mapping key must be a string, encountered - {:?}",
                    k
                )))
            }
        };
        match v {
            Yaml::String(ref s) => {
                let search = s.to_owned().into_identifier()?;
                let expr = match search {
                    Identifier::Equal(i) => Expression::BooleanExpression(
                        Box::new(Expression::Field(k.to_owned())),
                        BoolSym::Equal,
                        Box::new(Expression::Integer(i)),
                    ),
                    Identifier::GreaterThan(i) => Expression::BooleanExpression(
                        Box::new(Expression::Field(k.to_owned())),
                        BoolSym::GreaterThan,
                        Box::new(Expression::Integer(i)),
                    ),
                    Identifier::GreaterThanOrEqual(i) => Expression::BooleanExpression(
                        Box::new(Expression::Field(k.to_owned())),
                        BoolSym::GreaterThanOrEqual,
                        Box::new(Expression::Integer(i)),
                    ),
                    Identifier::Contains(c) => {
                        Expression::Search(Search::Contains(c), k.to_owned())
                    }
                    Identifier::EndsWith(c) => {
                        Expression::Search(Search::EndsWith(c), k.to_owned())
                    }
                    Identifier::Exact(c) => Expression::Search(Search::Exact(c), k.to_owned()),
                    Identifier::LessThan(i) => Expression::BooleanExpression(
                        Box::new(Expression::Field(k.to_owned())),
                        BoolSym::LessThan,
                        Box::new(Expression::Integer(i)),
                    ),
                    Identifier::LessThanOrEqual(i) => Expression::BooleanExpression(
                        Box::new(Expression::Field(k.to_owned())),
                        BoolSym::LessThanOrEqual,
                        Box::new(Expression::Integer(i)),
                    ),
                    Identifier::Regex(c) => Expression::Search(Search::Regex(c), k.to_owned()),
                    Identifier::StartsWith(c) => {
                        Expression::Search(Search::StartsWith(c), k.to_owned())
                    }
                };
                match expression {
                    Some(e) => {
                        expression = Some(Expression::BooleanExpression(
                            Box::new(e),
                            BoolSym::And,
                            Box::new(expr),
                        ))
                    }
                    None => expression = Some(expr),
                }
            }
            Yaml::Sequence(ref s) => {
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
                let mut regex: Vec<Regex> = vec![];
                let mut rest: Vec<Expression> = vec![]; // NOTE: Don't care about speed of numbers atm
                for value in s {
                    let identifier = match value {
                        Yaml::String(s) => s.clone().into_identifier()?,
                        _ => {
                            return Err(crate::error::parse_invalid_ident(format!(
                                "value must be string, encountered - {:?}",
                                k
                            )))
                        }
                    };
                    match identifier {
                        Identifier::Exact(_) => exact.push(identifier),
                        Identifier::StartsWith(_) => starts_with.push(identifier),
                        Identifier::EndsWith(_) => ends_with.push(identifier),
                        Identifier::Contains(_) => contains.push(identifier),
                        Identifier::Regex(r) => regex.push(r),
                        Identifier::Equal(i) => rest.push(Expression::BooleanExpression(
                            Box::new(Expression::Field(k.to_owned())),
                            BoolSym::Equal,
                            Box::new(Expression::Integer(i)),
                        )),
                        Identifier::GreaterThan(i) => rest.push(Expression::BooleanExpression(
                            Box::new(Expression::Field(k.to_owned())),
                            BoolSym::GreaterThan,
                            Box::new(Expression::Integer(i)),
                        )),
                        Identifier::GreaterThanOrEqual(i) => {
                            rest.push(Expression::BooleanExpression(
                                Box::new(Expression::Field(k.to_owned())),
                                BoolSym::GreaterThanOrEqual,
                                Box::new(Expression::Integer(i)),
                            ))
                        }
                        Identifier::LessThan(i) => rest.push(Expression::BooleanExpression(
                            Box::new(Expression::Field(k.to_owned())),
                            BoolSym::LessThan,
                            Box::new(Expression::Integer(i)),
                        )),
                        Identifier::LessThanOrEqual(i) => rest.push(Expression::BooleanExpression(
                            Box::new(Expression::Field(k.to_owned())),
                            BoolSym::LessThanOrEqual,
                            Box::new(Expression::Integer(i)),
                        )),
                    }
                }
                let mut expressions: Vec<Expression> = vec![];
                let mut needles: Vec<String> = vec![];
                let mut context: Vec<MatchType> = vec![];
                for i in starts_with.into_iter() {
                    if let Identifier::StartsWith(i) = i {
                        context.push(MatchType::StartsWith(i.to_string()));
                        needles.push(i);
                    }
                }
                for i in contains.into_iter() {
                    if let Identifier::Contains(i) = i {
                        context.push(MatchType::Contains(i.to_string()));
                        needles.push(i);
                    }
                }
                for i in ends_with.into_iter() {
                    if let Identifier::EndsWith(i) = i {
                        context.push(MatchType::EndsWith(i.to_string()));
                        needles.push(i);
                    }
                }
                for i in exact.into_iter() {
                    if let Identifier::Exact(i) = i {
                        // NOTE: Do not allow empty string into the needles as it causes massive slow down,
                        // don't ask me why I have not looked into it!
                        if i == "" {
                            expressions.push(Expression::Search(Search::Exact(i), k.clone()));
                        } else {
                            context.push(MatchType::Exact(i.to_string()));
                            needles.push(i);
                        }
                    }
                }
                if !needles.is_empty() {
                    expressions.push(Expression::Search(
                        Search::AhoCorasick(
                            Box::new(
                                AhoCorasickBuilder::new()
                                    .ascii_case_insensitive(true) // XXX: This should be a setting
                                    .dfa(true)
                                    .build(needles),
                            ),
                            context,
                        ),
                        k.clone(),
                    ));
                }
                expressions.extend(
                    regex
                        .into_iter()
                        .map(|r| Expression::Search(Search::Regex(r), k.to_string())),
                );
                expressions.extend(rest);
                for expr in expressions {
                    match expression {
                        Some(e) => {
                            expression = Some(Expression::BooleanExpression(
                                Box::new(e),
                                BoolSym::Or,
                                Box::new(expr),
                            ))
                        }
                        None => expression = Some(expr),
                    }
                }
            }
            _ => {
                return Err(crate::error::parse_invalid_ident(format!(
                    "expected sequence or string, encountered - {:?}",
                    v
                )))
            }
        }
    }
    expression.ok_or(crate::error::parse_invalid_ident("failed to parse mapping"))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[cfg(feature = "benchmarks")]
    use test;

    #[test]
    fn parse_and() {
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
    fn parse_or() {
        let e = parse(&vec![
            Token::Identifier("foo".to_string()),
            Token::Operator(BoolSym::Or),
            Token::Identifier("bar".to_string()),
        ])
        .unwrap();
        assert_eq!(
            Expression::BooleanExpression(
                Box::new(Expression::Identifier("foo".to_string())),
                BoolSym::Or,
                Box::new(Expression::Identifier("bar".to_string()))
            ),
            e
        );
    }

    #[test]
    fn parse_identifier() {
        let e = parse(&vec![Token::Identifier("condition".to_string())]).unwrap();
        assert_eq!(Expression::Identifier("condition".to_string()), e);
    }

    #[test]
    fn parse_expression() {
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

    #[bench]
    #[cfg(feature = "benchmarks")]
    fn bench_simple_expression(b: &mut test::Bencher) {
        b.iter(|| {
            parse(&vec![
                Token::Delimiter(DelSym::LeftParenthesis),
                Token::Identifier("foo".to_string()),
                Token::Operator(BoolSym::And),
                Token::Identifier("bar".to_string()),
                Token::Delimiter(DelSym::RightParenthesis),
                Token::Operator(BoolSym::Or),
                Token::Identifier("fooz".to_string()),
            ])
        });
    }
}
