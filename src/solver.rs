use std::collections::HashMap;
use std::fmt;

use aho_corasick::AhoCorasick;
use tracing::debug;

use crate::document::Document;
use crate::parser::{Expression, Match, MatchType, Search};
use crate::rule::Detection;
use crate::tokeniser::{BoolSym, ModSym};
use crate::value::Value;

struct Cache<'a>(&'a Vec<Option<Value<'a>>>);

impl<'a> Document for Cache<'a> {
    fn find(&self, key: &str) -> Option<Value> {
        let i = key.chars().nth(0).expect("could not get key") as u32;
        self.0[i as usize].clone()
    }
}

struct Passthrough<'a>(Option<Value<'a>>);

impl<'a> Document for Passthrough<'a> {
    fn find(&self, _: &str) -> Option<Value> {
        self.0.clone()
    }
}

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
                            match i {
                                Value::Float(_) | Value::Int(_) | Value::UInt(_) => i,
                                _ => {
                                    debug!(
                                        "evaluating false, no left hand side for {}",
                                        expression
                                    );
                                    return SolverResult::False;
                                }
                            }
                        }
                        Expression::Cast(field, ModSym::Flt) => {
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
                                        Value::Float(1.0)
                                    } else {
                                        Value::Float(1.0)
                                    }
                                }
                                Value::Float(x) => Value::Float(x),
                                Value::Int(x) => {
                                    if x <= f64::MAX as i64 {
                                        Value::Float(x as f64)
                                    } else {
                                        debug!(
                                                "evaluating false, could not cast left hand side for {} - {}",
                                                expression, x
                                            );
                                        return SolverResult::False;
                                    }
                                }
                                Value::String(x) => match x.parse::<f64>() {
                                    Ok(i) => Value::Float(i),
                                    Err(e) => {
                                        debug!(
                                                "evaluating false, could not cast left hand side for {} - {}",
                                                expression, e
                                            );
                                        return SolverResult::False;
                                    }
                                },
                                Value::UInt(x) => {
                                    if x <= f64::MAX as u64 {
                                        Value::Float(x as f64)
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
                                        Value::Int(1)
                                    } else {
                                        Value::Int(0)
                                    }
                                }
                                Value::Float(x) => Value::Int(x.round() as i64),
                                Value::Int(x) => Value::Int(x),
                                Value::String(x) => match x.parse::<i64>() {
                                    Ok(i) => Value::Int(i),
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
                                        Value::Int(x as i64)
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
                        Expression::Boolean(i) => Value::Bool(*i),
                        Expression::Float(i) => Value::Float(*i),
                        Expression::Integer(i) => Value::Int(*i),
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
                            match i {
                                Value::Float(_) | Value::Int(_) | Value::UInt(_) => i,
                                _ => {
                                    debug!(
                                        "evaluating false, no right hand side for {}",
                                        expression
                                    );
                                    return SolverResult::False;
                                }
                            }
                        }
                        Expression::Cast(field, ModSym::Flt) => {
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
                            match i {
                                Value::Bool(x) => {
                                    if x {
                                        Value::Float(1.0)
                                    } else {
                                        Value::Float(1.0)
                                    }
                                }
                                Value::Float(x) => Value::Float(x),
                                Value::Int(x) => {
                                    if x <= f64::MAX as i64 {
                                        Value::Float(x as f64)
                                    } else {
                                        debug!(
                                                "evaluating false, could not cast right hand side for {} - {}",
                                                expression, x
                                            );
                                        return SolverResult::False;
                                    }
                                }
                                Value::String(x) => match x.parse::<f64>() {
                                    Ok(i) => Value::Float(i),
                                    Err(e) => {
                                        debug!(
                                                "evaluating false, could not cast right hand side for {} - {}",
                                                expression, e
                                            );
                                        return SolverResult::False;
                                    }
                                },
                                Value::UInt(x) => {
                                    if x <= f64::MAX as u64 {
                                        Value::Float(x as f64)
                                    } else {
                                        debug!(
                                                "evaluating false, could not cast right hand side for {} - {}",
                                                expression, x
                                            );
                                        return SolverResult::False;
                                    }
                                }
                                _ => {
                                    debug!(
                                        "evaluating false, invalid type on right hand side for {}",
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
                            match i {
                                Value::Bool(x) => {
                                    if x {
                                        Value::Int(1)
                                    } else {
                                        Value::Int(0)
                                    }
                                }
                                Value::Float(x) => Value::Int(x.round() as i64),
                                Value::Int(x) => Value::Int(x),
                                Value::String(x) => match x.parse::<i64>() {
                                    Ok(i) => Value::Int(i),
                                    Err(e) => {
                                        debug!(
                                                "evaluating false, could not cast right hand side for {} - {}",
                                                expression, e
                                            );
                                        return SolverResult::False;
                                    }
                                },
                                Value::UInt(x) => {
                                    if x <= i64::MAX as u64 {
                                        Value::Int(x as i64)
                                    } else {
                                        debug!(
                                                "evaluating false, could not cast right hand side for {} - {}",
                                                expression, x
                                            );
                                        return SolverResult::False;
                                    }
                                }
                                _ => {
                                    debug!(
                                        "evaluating false, invalid type on right hand side for {}",
                                        expression
                                    );
                                    return SolverResult::False;
                                }
                            }
                        }
                        Expression::Boolean(i) => Value::Bool(*i),
                        Expression::Float(i) => Value::Float(*i),
                        Expression::Integer(i) => Value::Int(*i),
                        _ => {
                            debug!("encountered invalid right hand side for {}", expression);
                            return SolverResult::False;
                        }
                    };
                    let res = match (x, *op, y) {
                        (Value::Bool(x), BoolSym::Equal, Value::Bool(y)) => x == y,
                        (Value::Float(x), BoolSym::Equal, Value::Float(y)) => x == y,
                        (Value::Int(x), BoolSym::Equal, Value::Int(y)) => x == y,
                        (Value::UInt(x), BoolSym::Equal, Value::UInt(y)) => x == y,
                        (Value::UInt(x), BoolSym::Equal, Value::Int(y)) if x <= i64::MAX as u64 => {
                            x as i64 == y
                        }
                        (Value::Int(x), BoolSym::Equal, Value::UInt(y)) if y <= i64::MAX as u64 => {
                            x == y as i64
                        }
                        (_, BoolSym::Equal, _) => false,
                        (Value::Float(x), BoolSym::GreaterThan, Value::Float(y)) => x > y,
                        (Value::Int(x), BoolSym::GreaterThan, Value::Int(y)) => x > y,
                        (Value::UInt(x), BoolSym::GreaterThan, Value::UInt(y)) => x > y,
                        (Value::UInt(x), BoolSym::GreaterThan, Value::Int(y))
                            if x <= i64::MAX as u64 =>
                        {
                            x as i64 > y
                        }
                        (Value::Int(x), BoolSym::GreaterThan, Value::UInt(y))
                            if y <= i64::MAX as u64 =>
                        {
                            x > y as i64
                        }
                        (_, BoolSym::GreaterThan, _) => false,
                        (Value::Float(x), BoolSym::GreaterThanOrEqual, Value::Float(y)) => x >= y,
                        (Value::Int(x), BoolSym::GreaterThanOrEqual, Value::Int(y)) => x >= y,
                        (Value::UInt(x), BoolSym::GreaterThanOrEqual, Value::UInt(y)) => x >= y,
                        (Value::UInt(x), BoolSym::GreaterThanOrEqual, Value::Int(y))
                            if x <= i64::MAX as u64 =>
                        {
                            x as i64 >= y
                        }
                        (Value::Int(x), BoolSym::GreaterThanOrEqual, Value::UInt(y))
                            if y <= i64::MAX as u64 =>
                        {
                            x >= y as i64
                        }
                        (_, BoolSym::GreaterThanOrEqual, _) => false,
                        (Value::Float(x), BoolSym::LessThan, Value::Float(y)) => x < y,
                        (Value::Int(x), BoolSym::LessThan, Value::Int(y)) => x < y,
                        (Value::UInt(x), BoolSym::LessThan, Value::UInt(y)) => x < y,
                        (Value::UInt(x), BoolSym::LessThan, Value::Int(y))
                            if x <= i64::MAX as u64 =>
                        {
                            (x as i64) < y
                        }
                        (Value::Int(x), BoolSym::LessThan, Value::UInt(y))
                            if y <= i64::MAX as u64 =>
                        {
                            x < y as i64
                        }
                        (_, BoolSym::LessThan, _) => false,
                        (Value::Float(x), BoolSym::LessThanOrEqual, Value::Float(y)) => x <= y,
                        (Value::Int(x), BoolSym::LessThanOrEqual, Value::Int(y)) => x <= y,
                        (Value::UInt(x), BoolSym::LessThanOrEqual, Value::UInt(y)) => x <= y,
                        (Value::UInt(x), BoolSym::LessThanOrEqual, Value::Int(y))
                            if x <= i64::MAX as u64 =>
                        {
                            x as i64 <= y
                        }
                        (Value::Int(x), BoolSym::LessThanOrEqual, Value::UInt(y))
                            if y <= i64::MAX as u64 =>
                        {
                            x <= y as i64
                        }
                        (_, BoolSym::LessThanOrEqual, _) => false,
                        _ => unreachable!(),
                    };
                    match res {
                        true => SolverResult::True,
                        _ => SolverResult::False,
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
                    Some(e) => return match_all(e, identifiers, document),
                    _ => unreachable!(),
                },
                Expression::BooleanGroup(ref o, ref g) => (o, g),
                _ => return match_all(e, identifiers, document),
            };
            for expression in group {
                match solve_expression(expression, identifiers, document) {
                    SolverResult::True => {}
                    SolverResult::False => return SolverResult::False,
                    SolverResult::Missing => return SolverResult::Missing,
                }
            }
            SolverResult::True
        }
        Expression::Match(Match::Of(c), ref e) => {
            let (_, group) = match **e {
                Expression::Identifier(ref identifier) => match identifiers.get(identifier) {
                    Some(Expression::BooleanGroup(o, g)) => (o, g),
                    Some(e) => return match_of(e, identifiers, document, c),
                    _ => {
                        unreachable!();
                    }
                },
                Expression::BooleanGroup(ref o, ref g) => (o, g),
                _ => return match_of(e, identifiers, document, c),
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
        Expression::Matrix(ref columns, ref rows) => {
            // NOTE: Field and search widths must be the same or tau will panic, for now this is
            // fine as only the optimiser can write this expression, and for those using core it is
            // on them to ensure they don't break this. There are ways to lock this down and it
            // could be done in the future...
            let size = columns.len();
            let mut cache: Vec<Option<Value>> = Vec::with_capacity(size);
            for _ in 0..size {
                cache.push(None);
            }

            let mut res = SolverResult::Missing;
            for row in rows {
                let mut hit = SolverResult::True;
                for (i, expression) in row.iter().enumerate() {
                    if let Some(expression) = expression {
                        if cache[i].is_none() {
                            let value = match document.find(&columns[i]) {
                                Some(v) => v,
                                None => {
                                    debug!(
                                        "evaluating missing, field not found for {}",
                                        expression
                                    );
                                    hit = SolverResult::Missing;
                                    break;
                                }
                            };
                            let _ = std::mem::replace(&mut cache[i], Some(value));
                        }
                        match solve_expression(expression, identifiers, &Cache(&cache)) {
                            SolverResult::True => {}
                            SolverResult::False => {
                                hit = SolverResult::False;
                                break;
                            }
                            SolverResult::Missing => {
                                hit = SolverResult::Missing;
                                break;
                            }
                        }
                    }
                }
                match hit {
                    SolverResult::True => return SolverResult::True,
                    SolverResult::False => res = SolverResult::False,
                    SolverResult::Missing => {}
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
                    if let Expression::Match(Match::All, expression) = &**e {
                        if let Expression::BooleanGroup(BoolSym::Or, expressions) = &**expression {
                            for expression in expressions {
                                let mut res = SolverResult::Missing;
                                for v in a.iter() {
                                    if let Some(x) = v.as_object() {
                                        match solve_expression(expression, identifiers, &x) {
                                            SolverResult::True => {
                                                res = SolverResult::True;
                                                break;
                                            }
                                            SolverResult::False => res = SolverResult::False,
                                            SolverResult::Missing => {}
                                        }
                                    }
                                }
                                if res != SolverResult::True {
                                    return res;
                                }
                            }
                            return SolverResult::True;
                        } else if let Expression::Matrix(columns, rows) = &**expression {
                            // NOTE: We can't really make use of the optimisations provided by a
                            // matrix here as we have to loop through the array! For that reason we
                            // basically null this optimisation...
                            for row in rows {
                                let mut res = SolverResult::Missing;
                                for v in a.iter() {
                                    let mut hit = SolverResult::True;
                                    for (i, expression) in row.iter().enumerate() {
                                        if let Some(expression) = expression {
                                            if let Some(x) = v.as_object() {
                                                let value = x.find(&columns[i]);
                                                match solve_expression(
                                                    expression,
                                                    identifiers,
                                                    &Passthrough(value),
                                                ) {
                                                    SolverResult::True => {}
                                                    SolverResult::False => {
                                                        hit = SolverResult::False;
                                                        break;
                                                    }
                                                    SolverResult::Missing => {
                                                        hit = SolverResult::Missing;
                                                        break;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    if hit == SolverResult::True {
                                        res = SolverResult::True;
                                        break;
                                    }
                                }
                                if res != SolverResult::True {
                                    return res;
                                }
                            }
                            return SolverResult::True;
                        }
                    }
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
fn match_all(
    expression: &Expression,
    identifiers: &HashMap<String, Expression>,
    document: &dyn Document,
) -> SolverResult {
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
    } else if let Expression::Matrix(ref columns, ref rows) = expression {
        // NOTE: Field and search widths must be the same or tau will panic, for now this is
        // fine as only the optimiser can write this expression, and for those using core it is
        // on them to ensure they don't break this. There are ways to lock this down and it
        // could be done in the future...
        let size = columns.len();
        let mut cache: Vec<Option<Value>> = Vec::with_capacity(size);
        for _ in 0..size {
            cache.push(None);
        }
        for row in rows {
            let mut hit = SolverResult::True;
            for (i, expression) in row.iter().enumerate() {
                if let Some(expression) = expression {
                    if cache[i].is_none() {
                        let value = match document.find(&columns[i]) {
                            Some(v) => v,
                            None => {
                                debug!("evaluating missing, field not found for {}", expression);
                                hit = SolverResult::Missing;
                                break;
                            }
                        };
                        let _ = std::mem::replace(&mut cache[i], Some(value));
                    }
                    match solve_expression(expression, identifiers, &Cache(&cache)) {
                        SolverResult::True => {}
                        SolverResult::False => {
                            hit = SolverResult::False;
                            break;
                        }
                        SolverResult::Missing => {
                            hit = SolverResult::Missing;
                            break;
                        }
                    }
                }
            }
            match hit {
                SolverResult::True => {}
                SolverResult::False => return SolverResult::False,
                SolverResult::Missing => return SolverResult::Missing,
            }
        }
    } else {
        return solve_expression(expression, identifiers, document);
    }
    SolverResult::True
}

#[inline]
fn match_of(
    expression: &Expression,
    identifiers: &HashMap<String, Expression>,
    document: &dyn Document,
    count: u64,
) -> SolverResult {
    if count == 0 {
        return match solve_expression(expression, identifiers, document) {
            SolverResult::True => SolverResult::False,
            SolverResult::False => SolverResult::True,
            SolverResult::Missing => return SolverResult::Missing,
        };
    } else if let Expression::Search(Search::AhoCorasick(a, m, _), i, cast) = expression {
        let value = match document.find(i) {
            Some(v) => v,
            None => {
                debug!("evaluating missing, field not found for {}", expression);
                return SolverResult::Missing;
            }
        };
        match (value, cast) {
            (Value::String(ref x), _) => {
                let c = slow_aho(a, m, x);
                if c >= count {
                    return SolverResult::True;
                }
            }
            (Value::Array(x), _) => {
                for v in x.iter() {
                    if let Some(x) = v.as_str() {
                        let hits = slow_aho(a, m, x);
                        if hits >= count {
                            return SolverResult::True;
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
                        if hits >= count {
                            return SolverResult::True;
                        }
                    }
                }
            }
            (Value::Bool(x), true) => {
                let x = x.to_string();
                let c = slow_aho(a, m, x.as_str());
                if c >= count {
                    return SolverResult::True;
                }
            }
            (Value::Float(x), true) => {
                let x = x.to_string();
                let c = slow_aho(a, m, x.as_str());
                if c >= count {
                    return SolverResult::True;
                }
            }
            (Value::Int(x), true) => {
                let x = x.to_string();
                let c = slow_aho(a, m, x.as_str());
                if c >= count {
                    return SolverResult::True;
                }
            }
            (Value::UInt(x), true) => {
                let x = x.to_string();
                let c = slow_aho(a, m, x.as_str());
                if c >= count {
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
                let mut c = 0;
                for _ in s.matches(x).iter() {
                    c += 1;
                }
                if c >= count {
                    return SolverResult::True;
                }
            }
            (Value::Array(x), _) => {
                for v in x.iter() {
                    if let Some(x) = v.as_str() {
                        let mut hits = 0;
                        for _ in s.matches(x).iter() {
                            hits += 1;
                        }
                        if hits >= count {
                            return SolverResult::True;
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
                        if hits >= count {
                            return SolverResult::True;
                        }
                    }
                }
            }
            (Value::Bool(x), true) => {
                let x = x.to_string();
                let mut c = 0;
                for _ in s.matches(x.as_str()).iter() {
                    c += 1;
                }
                if c >= count {
                    return SolverResult::True;
                }
            }
            (Value::Float(x), true) => {
                let x = x.to_string();
                let mut c = 0;
                for _ in s.matches(x.as_str()).iter() {
                    c += 1;
                }
                if c >= count {
                    return SolverResult::True;
                }
            }
            (Value::Int(x), true) => {
                let x = x.to_string();
                let mut c = 0;
                for _ in s.matches(x.as_str()).iter() {
                    c += 1;
                }
                if c >= count {
                    return SolverResult::True;
                }
            }
            (Value::UInt(x), true) => {
                let x = x.to_string();
                let mut c = 0;
                for _ in s.matches(x.as_str()).iter() {
                    c += 1;
                }
                if c >= count {
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
    } else if let Expression::Matrix(ref columns, ref rows) = expression {
        // NOTE: Field and search widths must be the same or tau will panic, for now this is
        // fine as only the optimiser can write this expression, and for those using core it is
        // on them to ensure they don't break this. There are ways to lock this down and it
        // could be done in the future...
        let size = columns.len();
        let mut cache: Vec<Option<Value>> = Vec::with_capacity(size);
        for _ in 0..size {
            cache.push(None);
        }
        let mut hits = 0;
        let mut res = SolverResult::Missing;
        for row in rows {
            let mut hit = SolverResult::True;
            for (i, expression) in row.iter().enumerate() {
                if let Some(expression) = expression {
                    if cache[i].is_none() {
                        let value = match document.find(&columns[i]) {
                            Some(v) => v,
                            None => {
                                debug!("evaluating missing, field not found for {}", expression);
                                hit = SolverResult::Missing;
                                break;
                            }
                        };
                        let _ = std::mem::replace(&mut cache[i], Some(value));
                    }
                    match solve_expression(expression, identifiers, &Cache(&cache)) {
                        SolverResult::True => {}
                        SolverResult::False => {
                            hit = SolverResult::False;
                            break;
                        }
                        SolverResult::Missing => {
                            hit = SolverResult::Missing;
                            break;
                        }
                    }
                }
            }
            match hit {
                SolverResult::True => {
                    hits += 1;
                    if hits >= count {
                        return SolverResult::True;
                    }
                }
                SolverResult::False => res = SolverResult::False,
                SolverResult::Missing => {}
            }
        }
        return res;
    } else {
        return solve_expression(expression, identifiers, document);
    }
    SolverResult::False
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
