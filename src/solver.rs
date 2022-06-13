use std::collections::HashMap;
use std::fmt;

use aho_corasick::AhoCorasick;
use tracing::debug;

use crate::document::Document;
use crate::parser::{Expression, Match, MatchType, Search};
use crate::rule::Detection;
use crate::tokeniser::{BoolSym, ModSym};
use crate::value::Value;

#[derive(Debug, PartialEq)]
pub(crate) enum SolverResult {
    True,
    False,
    Missing,
}
impl fmt::Display for SolverResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Missing => write!(f, "missing"),
        }
    }
}

/// Evalutes a `Document` with a provided detection, returning true if the detection solves.
pub fn solve(detection: &Detection, document: &dyn Document) -> bool {
    match solve_expression(&detection.expression, &detection.identifiers, document) {
        SolverResult::True => true,
        SolverResult::False | SolverResult::Missing => false,
    }
}

pub(crate) fn solve_expression(
    expression: &Expression,
    identifiers: &HashMap<String, Expression>,
    document: &dyn Document,
) -> SolverResult {
    match *expression {
        Expression::BooleanGroup(BoolSym::And, ref group) => {
            for expression in group {
                match solve_expression(expression, identifiers, document) {
                    SolverResult::True => {}
                    SolverResult::False => return SolverResult::False,
                    SolverResult::Missing => return SolverResult::Missing,
                }
            }
            SolverResult::True
        }
        Expression::BooleanGroup(BoolSym::Or, ref group) => {
            let mut res = SolverResult::Missing;
            for expression in group {
                match solve_expression(expression, identifiers, document) {
                    SolverResult::True => return SolverResult::True,
                    SolverResult::False => res = SolverResult::False,
                    SolverResult::Missing => {}
                }
            }
            res
        }
        Expression::BooleanExpression(ref left, ref op, ref right) => {
            // Edge cases
            match (&**left, op, &**right) {
                (
                    Expression::Cast(ref left, ModSym::Str),
                    BoolSym::Equal,
                    Expression::Cast(ref right, ModSym::Str),
                ) => {
                    let x = match document.find(left) {
                        Some(x) => x,
                        None => {
                            debug!("evaluating missing, no left hand side for {}", expression);
                            return SolverResult::Missing;
                        }
                    };
                    let x = match x.to_string() {
                        Some(v) => v,
                        None => {
                            debug!(
                                "evaluating false, could not cast left field to string for {}",
                                expression
                            );
                            return SolverResult::False;
                        }
                    };
                    let y = match document.find(right) {
                        Some(x) => x,
                        None => {
                            debug!("evaluating missing, no right hand side for {}", expression);
                            return SolverResult::Missing;
                        }
                    };
                    let y = match y.to_string() {
                        Some(v) => v,
                        None => {
                            debug!(
                                "evaluating false, could not cast right field to string for {}",
                                expression
                            );
                            return SolverResult::False;
                        }
                    };
                    if x == y {
                        return SolverResult::True;
                    } else {
                        return SolverResult::False;
                    }
                }
                (Expression::Field(ref left), BoolSym::Equal, Expression::Boolean(b)) => {
                    let x = match document.find(left) {
                        Some(x) => x,
                        None => {
                            debug!("evaluating missing, no left hand side for {}", expression);
                            return SolverResult::Missing;
                        }
                    };
                    let x = match x.as_bool() {
                        Some(v) => v,
                        None => {
                            debug!(
                                "evaluating false, could not cast left field to boolean for {}",
                                expression
                            );
                            return SolverResult::False;
                        }
                    };
                    if x == *b {
                        return SolverResult::True;
                    } else {
                        return SolverResult::False;
                    }
                }
                (Expression::Field(ref left), BoolSym::Equal, Expression::Null) => {
                    let x = match document.find(left) {
                        Some(x) => x,
                        None => {
                            debug!("evaluating missing, no left hand side for {}", expression);
                            return SolverResult::Missing;
                        }
                    };
                    if x.is_null() {
                        return SolverResult::True;
                    } else {
                        return SolverResult::False;
                    }
                }
                _ => {}
            }
            // Boolean expressions
            match *op {
                BoolSym::Equal
                | BoolSym::GreaterThan
                | BoolSym::GreaterThanOrEqual
                | BoolSym::LessThan
                | BoolSym::LessThanOrEqual => {
                    // FIXME: Some very basic float support...
                    let float = match (left.as_ref(), right.as_ref()) {
                        (Expression::Float(_), _) | (_, Expression::Float(_)) => true,
                        (_, _) => false,
                    };
                    if float {
                        let x = match left.as_ref() {
                            Expression::Field(f) => {
                                let i = match document.find(f) {
                                    Some(i) => i,
                                    None => {
                                        debug!(
                                            "evaluating missing, no left hand side for {}",
                                            expression
                                        );
                                        return SolverResult::Missing;
                                    }
                                };
                                match i.as_f64() {
                                    Some(v) => v,
                                    None => {
                                        debug!(
                                            "evaluating false, no left hand side for {}",
                                            expression
                                        );
                                        return SolverResult::False;
                                    }
                                }
                            }
                            Expression::Float(f) => *f,
                            _ => {
                                debug!("encountered invalid left hand side for {}", expression);
                                return SolverResult::False;
                            }
                        };
                        let y = match right.as_ref() {
                            Expression::Field(f) => {
                                let i = match document.find(f) {
                                    Some(i) => i,
                                    None => {
                                        debug!(
                                            "evaluating missing, no right hand side for {}",
                                            expression
                                        );
                                        return SolverResult::Missing;
                                    }
                                };
                                match i.as_f64() {
                                    Some(v) => v,
                                    None => {
                                        debug!(
                                            "evaluating false, no right hand side for {}",
                                            expression
                                        );
                                        return SolverResult::False;
                                    }
                                }
                            }
                            Expression::Float(i) => *i,
                            _ => {
                                debug!("encountered invalid right hand side for {}", expression);
                                return SolverResult::False;
                            }
                        };
                        let res = match *op {
                            BoolSym::Equal => x == y,
                            BoolSym::GreaterThan => x > y,
                            BoolSym::GreaterThanOrEqual => x >= y,
                            BoolSym::LessThan => x < y,
                            BoolSym::LessThanOrEqual => x <= y,
                            _ => unreachable!(),
                        };
                        match res {
                            true => SolverResult::True,
                            _ => SolverResult::False,
                        }
                    } else {
                        let x = match left.as_ref() {
                            Expression::Field(f) => {
                                let i = match document.find(f) {
                                    Some(i) => i,
                                    None => {
                                        debug!(
                                            "evaluating missing, no left hand side for {}",
                                            expression
                                        );
                                        return SolverResult::Missing;
                                    }
                                };
                                match i.to_i64() {
                                    Some(v) => v,
                                    None => {
                                        debug!(
                                            "evaluating false, no left hand side for {}",
                                            expression
                                        );
                                        return SolverResult::False;
                                    }
                                }
                            }
                            Expression::Cast(field, ModSym::Int) => {
                                let i = match document.find(field) {
                                    Some(i) => i,
                                    None => {
                                        debug!(
                                            "evaluating missing, no left hand side for {}",
                                            expression
                                        );
                                        return SolverResult::Missing;
                                    }
                                };
                                match i {
                                    Value::Bool(x) => {
                                        if x {
                                            1
                                        } else {
                                            0
                                        }
                                    }
                                    Value::Int(x) => x,
                                    Value::String(x) => match x.parse::<i64>() {
                                        Ok(i) => i,
                                        Err(e) => {
                                            debug!(
                                                "evaluating false, could not cast left hand side for {} - {}",
                                                expression, e
                                            );
                                            return SolverResult::False;
                                        }
                                    },
                                    Value::UInt(x) => {
                                        if x <= i64::MAX as u64 {
                                            x as i64
                                        } else {
                                            debug!(
                                                "evaluating false, could not cast left hand side for {} - {}",
                                                expression, x
                                            );
                                            return SolverResult::False;
                                        }
                                    }
                                    _ => {
                                        debug!(
                                            "evaluating false, invalid type on left hand side for {}",
                                            expression
                                        );
                                        return SolverResult::False;
                                    }
                                }
                            }
                            Expression::Integer(i) => *i,
                            _ => {
                                debug!("encountered invalid left hand side for {}", expression);
                                return SolverResult::False;
                            }
                        };
                        let y = match right.as_ref() {
                            Expression::Field(f) => {
                                let i = match document.find(f) {
                                    Some(i) => i,
                                    None => {
                                        debug!(
                                            "evaluating missing, no right hand side for {}",
                                            expression
                                        );
                                        return SolverResult::Missing;
                                    }
                                };
                                match i.to_i64() {
                                    Some(v) => v,
                                    None => {
                                        debug!(
                                            "evaluating false, no right hand side for {}",
                                            expression
                                        );
                                        return SolverResult::False;
                                    }
                                }
                            }
                            Expression::Cast(field, ModSym::Int) => {
                                let i = match document.find(field) {
                                    Some(i) => i,
                                    None => {
                                        debug!(
                                            "evaluating missing, no right hand side for {}",
                                            expression
                                        );
                                        return SolverResult::Missing;
                                    }
                                };
                                match i.to_string() {
                                    Some(v) => match v.parse::<i64>() {
                                        Ok(i) => i,
                                        Err(e) => {
                                            debug!(
                                            "evaluating false, could not cast right hand side for {} - {}",
                                            expression, e
                                        );
                                            return SolverResult::False;
                                        }
                                    },
                                    None => {
                                        debug!(
                                        "evaluating false, invalid type on right hand side for {}",
                                        expression
                                    );
                                        return SolverResult::False;
                                    }
                                }
                            }
                            Expression::Integer(i) => *i,
                            _ => {
                                debug!("encountered invalid right hand side for {}", expression);
                                return SolverResult::False;
                            }
                        };
                        let res = match *op {
                            BoolSym::Equal => x == y,
                            BoolSym::GreaterThan => x > y,
                            BoolSym::GreaterThanOrEqual => x >= y,
                            BoolSym::LessThan => x < y,
                            BoolSym::LessThanOrEqual => x <= y,
                            _ => unreachable!(),
                        };
                        match res {
                            true => SolverResult::True,
                            _ => SolverResult::False,
                        }
                    }
                }
                BoolSym::And => {
                    let x = match solve_expression(&*left, identifiers, document) {
                        SolverResult::True => (true, false),
                        SolverResult::False => return SolverResult::False,
                        SolverResult::Missing => return SolverResult::Missing,
                    };
                    let y = match solve_expression(&*right, identifiers, document) {
                        SolverResult::True => (true, false),
                        SolverResult::False => (false, false),
                        SolverResult::Missing => (false, true),
                    };
                    debug!(
                        "evaluating {} ({}) for {}",
                        x.0 && y.0,
                        x.1 || y.1,
                        expression
                    );
                    if x.1 || y.1 {
                        SolverResult::Missing
                    } else if x.0 && y.0 {
                        SolverResult::True
                    } else {
                        SolverResult::False
                    }
                }
                BoolSym::Or => {
                    let x = match solve_expression(&*left, identifiers, document) {
                        SolverResult::True => return SolverResult::True,
                        SolverResult::False => (false, false),
                        SolverResult::Missing => (false, true),
                    };
                    let y = match solve_expression(&*right, identifiers, document) {
                        SolverResult::True => (true, false),
                        SolverResult::False => (false, false),
                        SolverResult::Missing => (false, true),
                    };
                    debug!(
                        "evaluating {} ({}) for {}",
                        x.0 || y.0,
                        x.1 || y.1,
                        expression
                    );
                    if x.0 || y.0 {
                        SolverResult::True
                    } else if x.1 && y.1 {
                        SolverResult::Missing
                    } else {
                        SolverResult::False
                    }
                }
            }
        }
        Expression::Identifier(ref i) => match identifiers.get(i) {
            Some(e) => solve_expression(e, identifiers, document),
            None => unreachable!(),
        },
        Expression::Match(Match::All, ref e) => {
            let (_, group) = match **e {
                Expression::Identifier(ref i) => match identifiers.get(i) {
                    Some(Expression::BooleanGroup(o, g)) => (o, g),
                    Some(e) => return solve_expression(e, identifiers, document),
                    _ => unreachable!(),
                },
                Expression::BooleanGroup(ref o, ref g) => (o, g),
                // FIXME: Needle optimisation in search means we can't just call solve_expression,
                // we need to basically copy the code below...
                _ => unreachable!(),
            };
            for expression in group {
                // NOTE: Because of needle optimisation we have to handle aho in a `slow` fashion here...
                if let Expression::Search(Search::AhoCorasick(a, m, _), i, c) = expression {
                    let value = match document.find(i) {
                        Some(v) => v,
                        None => {
                            debug!("evaluating missing, field not found for {}", expression);
                            return SolverResult::Missing;
                        }
                    };
                    match (value, c) {
                        (Value::String(ref x), _) => {
                            if slow_aho(a, m, x) != m.len() as u64 {
                                return SolverResult::False;
                            }
                        }
                        (Value::Array(x), _) => {
                            let mut found = false;
                            for v in x.iter() {
                                if let Some(x) = v.as_str() {
                                    if slow_aho(a, m, x) == m.len() as u64 {
                                        found = true;
                                        break;
                                    }
                                } else if *c {
                                    let x = match v {
                                        Value::Bool(x) => x.to_string(),
                                        Value::Float(x) => x.to_string(),
                                        Value::Int(x) => x.to_string(),
                                        Value::UInt(x) => x.to_string(),
                                        _ => continue,
                                    };
                                    if slow_aho(a, m, x.as_str()) == m.len() as u64 {
                                        found = true;
                                        break;
                                    }
                                }
                            }
                            if !found {
                                return SolverResult::False;
                            }
                        }
                        (Value::Bool(x), true) => {
                            let x = x.to_string();
                            if slow_aho(a, m, x.as_str()) != m.len() as u64 {
                                return SolverResult::False;
                            }
                        }
                        (Value::Float(x), true) => {
                            let x = x.to_string();
                            if slow_aho(a, m, x.as_str()) != m.len() as u64 {
                                return SolverResult::False;
                            }
                        }
                        (Value::Int(x), true) => {
                            let x = x.to_string();
                            if slow_aho(a, m, x.as_str()) != m.len() as u64 {
                                return SolverResult::False;
                            }
                        }
                        (Value::UInt(x), true) => {
                            let x = x.to_string();
                            if slow_aho(a, m, x.as_str()) != m.len() as u64 {
                                return SolverResult::False;
                            }
                        }
                        (_, _) => {
                            debug!(
                                "evaluating false, field is not an array of strings, or a string for {}",
                                expression
                            );
                            return SolverResult::Missing;
                        }
                    }
                } else if let Expression::Search(Search::RegexSet(s, _), i, c) = expression {
                    let value = match document.find(i) {
                        Some(v) => v,
                        None => {
                            debug!("evaluating missing, field not found for {}", expression);
                            return SolverResult::Missing;
                        }
                    };
                    match (value, c) {
                        (Value::String(ref x), _) => {
                            let mut hits = 0;
                            for _ in s.matches(x).iter() {
                                hits += 1;
                            }
                            if hits != s.patterns().len() {
                                return SolverResult::False;
                            }
                        }
                        (Value::Array(x), _) => {
                            let mut found = false;
                            for v in x.iter() {
                                if let Some(x) = v.as_str() {
                                    let mut hits = 0;
                                    for _ in s.matches(x).iter() {
                                        hits += 1;
                                    }
                                    if hits == s.patterns().len() {
                                        found = true;
                                        break;
                                    }
                                } else if *c {
                                    let x = match v {
                                        Value::Bool(x) => x.to_string(),
                                        Value::Float(x) => x.to_string(),
                                        Value::Int(x) => x.to_string(),
                                        Value::UInt(x) => x.to_string(),
                                        _ => continue,
                                    };
                                    let mut hits = 0;
                                    for _ in s.matches(x.as_str()).iter() {
                                        hits += 1;
                                    }
                                    if hits == s.patterns().len() {
                                        found = true;
                                        break;
                                    }
                                }
                            }
                            if !found {
                                return SolverResult::False;
                            }
                        }
                        (Value::Bool(x), true) => {
                            let x = x.to_string();
                            let mut hits = 0;
                            for _ in s.matches(x.as_str()).iter() {
                                hits += 1;
                            }
                            if hits != s.patterns().len() {
                                return SolverResult::False;
                            }
                        }
                        (Value::Float(x), true) => {
                            let x = x.to_string();
                            let mut hits = 0;
                            for _ in s.matches(x.as_str()).iter() {
                                hits += 1;
                            }
                            if hits != s.patterns().len() {
                                return SolverResult::False;
                            }
                        }
                        (Value::Int(x), true) => {
                            let x = x.to_string();
                            let mut hits = 0;
                            for _ in s.matches(x.as_str()).iter() {
                                hits += 1;
                            }
                            if hits != s.patterns().len() {
                                return SolverResult::False;
                            }
                        }
                        (Value::UInt(x), true) => {
                            let x = x.to_string();
                            let mut hits = 0;
                            for _ in s.matches(x.as_str()).iter() {
                                hits += 1;
                            }
                            if hits != s.patterns().len() {
                                return SolverResult::False;
                            }
                        }
                        _ => {
                            debug!(
                                "evaluating false, field is not an array of strings, or a string for {}",
                                expression
                            );
                            return SolverResult::Missing;
                        }
                    }
                } else {
                    match solve_expression(expression, identifiers, document) {
                        SolverResult::True => {}
                        SolverResult::False => return SolverResult::False,
                        SolverResult::Missing => return SolverResult::Missing,
                    }
                }
            }
            SolverResult::True
        }
        Expression::Match(Match::Of(c), ref e) => {
            let (_, group) = match **e {
                Expression::Identifier(ref identifier) => match identifiers.get(identifier) {
                    Some(Expression::BooleanGroup(o, g)) => (o, g),
                    Some(e) => {
                        return match solve_expression(e, identifiers, document) {
                            SolverResult::True => {
                                if c == 0 {
                                    SolverResult::False
                                } else {
                                    SolverResult::True
                                }
                            }
                            SolverResult::False => {
                                if c == 0 {
                                    SolverResult::True
                                } else {
                                    SolverResult::False
                                }
                            }
                            SolverResult::Missing => SolverResult::Missing,
                        }
                    }
                    _ => {
                        unreachable!();
                    }
                },
                Expression::BooleanGroup(ref o, ref g) => (o, g),
                _ => {
                    // NOTE: We should not really land here but people can write garbage which
                    // makes us land here...
                    return match solve_expression(e, identifiers, document) {
                        SolverResult::True => {
                            if c == 0 {
                                SolverResult::False
                            } else {
                                SolverResult::True
                            }
                        }
                        SolverResult::False => {
                            if c == 0 {
                                SolverResult::True
                            } else {
                                SolverResult::False
                            }
                        }
                        SolverResult::Missing => SolverResult::Missing,
                    };
                }
            };
            let mut count = 0;
            let mut res = SolverResult::Missing;
            for expression in group {
                if c == 0 {
                    match solve_expression(expression, identifiers, document) {
                        SolverResult::True => return SolverResult::False,
                        SolverResult::False => {
                            res = SolverResult::True;
                        }
                        SolverResult::Missing => {}
                    }
                // NOTE: Because of needle optimisation we have to handle aho in a `slow` fashion here...
                } else if let Expression::Search(Search::AhoCorasick(a, m, _), i, cast) = expression
                {
                    let value = match document.find(i) {
                        Some(v) => v,
                        None => {
                            debug!("evaluating missing, field not found for {}", expression);
                            return SolverResult::Missing;
                        }
                    };
                    match (value, cast) {
                        (Value::String(ref x), _) => {
                            count += slow_aho(a, m, x);
                            if count >= c {
                                return SolverResult::True;
                            }
                        }
                        (Value::Array(x), _) => {
                            let mut max = 0;
                            for v in x.iter() {
                                if let Some(x) = v.as_str() {
                                    let hits = slow_aho(a, m, x);
                                    if count + hits >= c {
                                        return SolverResult::True;
                                    } else if hits > max {
                                        max = hits;
                                    }
                                } else if *cast {
                                    let x = match v {
                                        Value::Bool(x) => x.to_string(),
                                        Value::Float(x) => x.to_string(),
                                        Value::Int(x) => x.to_string(),
                                        Value::UInt(x) => x.to_string(),
                                        _ => continue,
                                    };
                                    let hits = slow_aho(a, m, x.as_str());
                                    if count + hits >= c {
                                        return SolverResult::True;
                                    } else if hits > max {
                                        max = hits;
                                    }
                                }
                            }
                            count += max;
                        }
                        (Value::Bool(x), true) => {
                            let x = x.to_string();
                            count += slow_aho(a, m, x.as_str());
                            if count >= c {
                                return SolverResult::True;
                            }
                        }
                        (Value::Float(x), true) => {
                            let x = x.to_string();
                            count += slow_aho(a, m, x.as_str());
                            if count >= c {
                                return SolverResult::True;
                            }
                        }
                        (Value::Int(x), true) => {
                            let x = x.to_string();
                            count += slow_aho(a, m, x.as_str());
                            if count >= c {
                                return SolverResult::True;
                            }
                        }
                        (Value::UInt(x), true) => {
                            let x = x.to_string();
                            count += slow_aho(a, m, x.as_str());
                            if count >= c {
                                return SolverResult::True;
                            }
                        }
                        _ => {
                            debug!(
                                "evaluating false, field is not an array of strings, or a string for {}",
                                expression
                            );
                            return SolverResult::Missing;
                        }
                    }
                } else if let Expression::Search(Search::RegexSet(s, _), i, cast) = expression {
                    let value = match document.find(i) {
                        Some(v) => v,
                        None => {
                            debug!("evaluating missing, field not found for {}", expression);
                            return SolverResult::Missing;
                        }
                    };
                    match (value, cast) {
                        (Value::String(ref x), _) => {
                            for _ in s.matches(x).iter() {
                                count += 1;
                            }
                            if count >= c {
                                return SolverResult::True;
                            }
                        }
                        (Value::Array(x), _) => {
                            let mut max = 0;
                            for v in x.iter() {
                                if let Some(x) = v.as_str() {
                                    let mut hits = 0;
                                    for _ in s.matches(x).iter() {
                                        hits += 1;
                                    }
                                    if count + hits >= c {
                                        return SolverResult::True;
                                    } else if hits > max {
                                        max = hits;
                                    }
                                } else if *cast {
                                    let x = match v {
                                        Value::Bool(x) => x.to_string(),
                                        Value::Float(x) => x.to_string(),
                                        Value::Int(x) => x.to_string(),
                                        Value::UInt(x) => x.to_string(),
                                        _ => continue,
                                    };
                                    let mut hits = 0;
                                    for _ in s.matches(x.as_str()).iter() {
                                        hits += 1;
                                    }
                                    if count + hits >= c {
                                        return SolverResult::True;
                                    } else if hits > max {
                                        max = hits;
                                    }
                                }
                            }
                            count += max;
                        }
                        (Value::Bool(x), true) => {
                            let x = x.to_string();
                            for _ in s.matches(x.as_str()).iter() {
                                count += 1;
                            }
                            if count >= c {
                                return SolverResult::True;
                            }
                        }
                        (Value::Float(x), true) => {
                            let x = x.to_string();
                            for _ in s.matches(x.as_str()).iter() {
                                count += 1;
                            }
                            if count >= c {
                                return SolverResult::True;
                            }
                        }
                        (Value::Int(x), true) => {
                            let x = x.to_string();
                            for _ in s.matches(x.as_str()).iter() {
                                count += 1;
                            }
                            if count >= c {
                                return SolverResult::True;
                            }
                        }
                        (Value::UInt(x), true) => {
                            let x = x.to_string();
                            for _ in s.matches(x.as_str()).iter() {
                                count += 1;
                            }
                            if count >= c {
                                return SolverResult::True;
                            }
                        }
                        _ => {
                            debug!(
                                "evaluating false, field is not an array of strings, or a string for {}",
                                expression
                            );
                            return SolverResult::Missing;
                        }
                    }
                } else {
                    match solve_expression(expression, identifiers, document) {
                        SolverResult::True => {
                            count += 1;
                            if count >= c {
                                return SolverResult::True;
                            }
                        }
                        SolverResult::False => res = SolverResult::False,
                        SolverResult::Missing => {}
                    }
                }
            }
            res
        }
        Expression::Negate(ref e) => {
            let res = match solve_expression(e.as_ref(), identifiers, document) {
                SolverResult::True => SolverResult::False,
                SolverResult::False => SolverResult::True,
                SolverResult::Missing => SolverResult::False,
            };
            debug!("evaluating {} for {}", res, expression);
            res
        }
        Expression::Nested(ref s, ref e) => {
            let value = match document.find(s) {
                Some(v) => v,
                None => {
                    debug!("evaluating missing, field not found for {}", expression);
                    return SolverResult::Missing;
                }
            };
            match value {
                Value::Object(o) => solve_expression(e, identifiers, &o),
                Value::Array(a) => {
                    for v in a.iter() {
                        if let Some(x) = v.as_object() {
                            if solve_expression(e, identifiers, &x) == SolverResult::True {
                                return SolverResult::True;
                            }
                        }
                    }
                    SolverResult::False
                }
                _ => {
                    debug!(
                        "evaluating false, field is not an array of objects or object for {}",
                        expression
                    );
                    SolverResult::False
                }
            }
        }
        Expression::Search(ref s, ref f, ref c) => {
            let value = match document.find(f) {
                Some(v) => v,
                None => {
                    debug!("evaluating missing, field not found for {}", expression);
                    return SolverResult::Missing;
                }
            };
            let res = match (value, c) {
                (Value::String(ref x), _) => search(s, x),
                (Value::Array(a), _) => {
                    let mut res = SolverResult::False;
                    for v in a.iter() {
                        if let Some(x) = v.as_str() {
                            if search(s, x) == SolverResult::True {
                                res = SolverResult::True;
                                break;
                            }
                        } else if *c {
                            let x = match v {
                                Value::Bool(x) => x.to_string(),
                                Value::Float(x) => x.to_string(),
                                Value::Int(x) => x.to_string(),
                                Value::UInt(x) => x.to_string(),
                                _ => continue,
                            };
                            if search(s, x.as_str()) == SolverResult::True {
                                res = SolverResult::True;
                                break;
                            }
                        }
                    }
                    res
                }
                (Value::Bool(x), true) => {
                    let x = x.to_string();
                    search(s, x.as_str())
                }
                (Value::Float(x), true) => {
                    let x = x.to_string();
                    search(s, x.as_str())
                }
                (Value::Int(x), true) => {
                    let x = x.to_string();
                    search(s, x.as_str())
                }
                (Value::UInt(x), true) => {
                    let x = x.to_string();
                    search(s, x.as_str())
                }
                _ => {
                    debug!(
                        "evaluating false, field is not an array of strings, or a string for {}",
                        expression
                    );
                    return SolverResult::Missing;
                }
            };
            debug!("evaluating {} for {}", res, expression);
            res
        }
        Expression::BooleanGroup(_, _)
        | Expression::Boolean(_)
        | Expression::Cast(_, _)
        | Expression::Field(_)
        | Expression::Float(_)
        | Expression::Integer(_)
        | Expression::Null => unreachable!(),
    }
}

#[inline]
fn search(kind: &Search, value: &str) -> SolverResult {
    match kind {
        Search::Any => {
            return SolverResult::True;
        }
        Search::Exact(ref i) => {
            if i == value {
                return SolverResult::True;
            }
        }
        Search::Contains(ref i) => {
            if value.contains(i) {
                return SolverResult::True;
            }
        }
        Search::EndsWith(ref i) => {
            if value.ends_with(i) {
                return SolverResult::True;
            }
        }
        Search::StartsWith(ref i) => {
            if value.starts_with(i) {
                return SolverResult::True;
            }
        }
        Search::Regex(ref i, _) => {
            if i.is_match(value) {
                return SolverResult::True;
            }
        }
        Search::RegexSet(ref i, _) => {
            if i.is_match(value) {
                return SolverResult::True;
            }
        }
        Search::AhoCorasick(ref a, ref m, _) => {
            for i in a.find_overlapping_iter(value) {
                match m[i.pattern()] {
                    MatchType::Contains(_) => return SolverResult::True,
                    MatchType::EndsWith(_) => {
                        if i.end() == value.len() {
                            return SolverResult::True;
                        }
                    }
                    MatchType::Exact(_) => {
                        if i.start() == 0 && i.end() == value.len() {
                            return SolverResult::True;
                        }
                    }
                    MatchType::StartsWith(_) => {
                        if i.start() == 0 {
                            return SolverResult::True;
                        }
                    }
                }
            }
            return SolverResult::False;
        }
    }
    SolverResult::False
}

#[inline]
fn slow_aho(a: &AhoCorasick, m: &[MatchType], value: &str) -> u64 {
    // TODO: Benchmark properly to work out whether the bitmap really is better on average
    let len = m.len();
    if len < 64 {
        let mut map = 0;
        for i in a.find_overlapping_iter(value) {
            let p = i.pattern();
            match m[p] {
                MatchType::Contains(_) => {
                    map |= 1 << p;
                }
                MatchType::EndsWith(_) => {
                    if i.end() == value.len() {
                        map |= 1 << p;
                    }
                }
                MatchType::Exact(_) => {
                    if i.start() == 0 && i.end() == value.len() {
                        map |= 1 << p;
                    }
                }
                MatchType::StartsWith(_) => {
                    if i.start() == 0 {
                        map |= 1 << p;
                    }
                }
            }
        }
        let mut hits = 0;
        for i in 0..len {
            hits += (map >> i) & 0x1;
        }
        hits
    } else {
        let mut hits = std::collections::HashSet::with_capacity(len);
        for i in a.find_overlapping_iter(value) {
            let p = i.pattern();
            match m[p] {
                MatchType::Contains(_) => {
                    hits.insert(p);
                }
                MatchType::EndsWith(_) => {
                    if i.end() == value.len() {
                        hits.insert(p);
                    }
                }
                MatchType::Exact(_) => {
                    if i.start() == 0 && i.end() == value.len() {
                        hits.insert(p);
                    }
                }
                MatchType::StartsWith(_) => {
                    if i.start() == 0 {
                        hits.insert(p);
                    }
                }
            }
        }
        hits.len() as u64
    }
}
