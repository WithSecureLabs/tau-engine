use std::iter::Iterator;
use std::iter::Peekable;

use tracing::debug;

use crate::tokeniser::{BoolSym, DelSym, MiscSym, SearchSym, Token};

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    BooleanExpression(Box<Expression>, BoolSym, Box<Expression>),
    Cast(String, MiscSym),
    Column(String),
    Identifier(String),
    Integer(i32),
    Negate(Box<Expression>),
    Search(SearchSym, String),
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
                Token::Search(ref s) => match *s {
                    SearchSym::All | SearchSym::Of(_) => {
                        // XXX: All and Of are not implemented...
                        // Munch following identifier
                        match it.next() {
                            Some(t) => match *t {
                                Token::Identifier(ref i) => {
                                    Ok(Expression::Search(s.clone(), i.to_string()))
                                }
                                _ => Err(crate::error::parse_invalid_token(format!(
                                    "NUD encountered - '{:?}'",
                                    t
                                ))),
                            },
                            None => Err(crate::error::parse_invalid_token("NUD expected token")),
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
