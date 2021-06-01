use std::collections::HashMap;
use std::convert::TryFrom;

use tracing::debug;

use crate::document::Document;
use crate::parser::{Expression, MatchType, Search};
use crate::rule::Detection;
use crate::tokeniser::{BoolSym, MiscSym};
use crate::value::Value;

#[derive(Debug, PartialEq)]
enum SolverResult {
    True,
    False,
    Missing,
}

pub fn solve(detection: &Detection, document: &dyn Document) -> bool {
    debug!("{:?}", detection.expression);
    debug!("{:?}", detection.identifiers);
    match solve_expression(&detection.expression, &detection.identifiers, document) {
        SolverResult::True => true,
        SolverResult::False | SolverResult::Missing => false,
    }
}

fn solve_expression(
    expression: &Expression,
    identifiers: &HashMap<String, Expression>,
    document: &dyn Document,
) -> SolverResult {
    match *expression {
        Expression::BooleanExpression(ref left, ref op, ref right) => {
            //
            match *op {
                BoolSym::Equal
                | BoolSym::GreaterThan
                | BoolSym::GreaterThanOrEqual
                | BoolSym::LessThan
                | BoolSym::LessThanOrEqual => {
                    let x = match left.as_ref() {
                        Expression::Field(f) => {
                            match document.get_value(f).and_then(|x| x.to_i64()) {
                                Some(v) => v,
                                None => {
                                    debug!("evaluating false (no i64 left) for {:?}", expression);
                                    return SolverResult::False;
                                }
                            }
                        }
                        Expression::Cast(c, MiscSym::Int) => {
                            match document.get_value(c).and_then(|x| x.to_string()) {
                                Some(v) => match v.parse::<i64>() {
                                    Ok(i) => i,
                                    Err(e) => {
                                        debug!(
                                            "could not cast left hand side for {:?} - {}",
                                            expression, e
                                        );
                                        return SolverResult::False;
                                    }
                                },
                                None => {
                                    debug!("evaluating false (no i64 left) for {:?}", expression);
                                    return SolverResult::False;
                                }
                            }
                        }
                        Expression::Integer(i) => *i,
                        _ => {
                            debug!("encountered invalid left hand side for {:?}", expression);
                            return SolverResult::False;
                        }
                    };
                    let y = match right.as_ref() {
                        Expression::Field(f) => {
                            match document.get_value(f).and_then(|x| x.to_i64()) {
                                Some(v) => v,
                                None => {
                                    debug!("evaluating false (no i64 left) for {:?}", expression);
                                    return SolverResult::False;
                                }
                            }
                        }
                        Expression::Cast(c, MiscSym::Int) => {
                            match document.get_value(c).and_then(|x| x.to_string()) {
                                Some(v) => match v.parse::<i64>() {
                                    Ok(i) => i,
                                    Err(e) => {
                                        debug!(
                                            "could not cast left hand side for {:?} - {}",
                                            expression, e
                                        );
                                        return SolverResult::False;
                                    }
                                },
                                None => {
                                    debug!("evaluating false (no i64 left) for {:?}", expression);
                                    return SolverResult::False;
                                }
                            }
                        }
                        Expression::Integer(i) => *i,
                        _ => {
                            debug!("encountered invalid left hand side for {:?}", expression);
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
                        "evaluating {} ({}) for {:?}",
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
                        "evaluating {} ({}) for {:?}",
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
        Expression::Negate(ref e) => {
            let res = match solve_expression(e.as_ref(), identifiers, document) {
                SolverResult::True => SolverResult::False,
                SolverResult::False => SolverResult::True,
                SolverResult::Missing => SolverResult::False,
            };
            debug!("evaluating {:?} for {:?}", res, expression);
            res
        }
        Expression::Nested(ref s, ref e) => {
            let value = match document.get_value(s) {
                Some(v) => v,
                None => {
                    debug!("evaluating missing, field not found for {:?}", expression);
                    return SolverResult::Missing;
                }
            };
            match value {
                Value::Object(o) => solve_expression(e, identifiers, &o),
                Value::Array(a) => {
                    for v in a.iter() {
                        if let Some(x) = v.as_object() {
                            match solve_expression(e, identifiers, &x) {
                                SolverResult::True => return SolverResult::True,
                                _ => {}
                            }
                        }
                    }
                    SolverResult::False
                }
                _ => {
                    debug!(
                        "evaluating false, field is not an array of objects or object for {:?}",
                        expression
                    );
                    SolverResult::False
                }
            }
        }
        Expression::Search(ref s, ref f) => {
            let value = match document.get_value(f) {
                Some(v) => v,
                None => {
                    debug!("evaluating missing, field not found for {:?}", expression);
                    return SolverResult::Missing;
                }
            };
            let res = match value {
                Value::String(ref x) => search(s, x),
                Value::Array(a) => {
                    let mut res = SolverResult::False;
                    for v in a.iter() {
                        if let Some(x) = v.as_str() {
                            match search(s, x) {
                                SolverResult::True => {
                                    res = SolverResult::True;
                                    break;
                                }
                                _ => {}
                            };
                        }
                    }
                    res
                }
                _ => {
                    debug!(
                        "evaluating false, field is not an array of strings, or a string for {:?}",
                        expression
                    );
                    return SolverResult::Missing;
                }
            };
            debug!("evaluating {:?} for {:?}", res, expression);
            res
        }
        Expression::Cast(_, _) | Expression::Field(_) | Expression::Integer(_) => unreachable!(),
    }
}

#[inline]
fn search(kind: &Search, value: &str) -> SolverResult {
    match kind {
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
        Search::Regex(ref i) => {
            if i.is_match(value) {
                return SolverResult::True;
            }
        }
        Search::AhoCorasick(ref a, ref m) => {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(feature = "benchmarks")]
    use test::Bencher;

    use crate::parser;
    use crate::tokeniser::Tokeniser;

    #[bench]
    #[cfg(feature = "benchmarks")]
    fn bench_regex_rule(b: &mut Bencher) {
        // Expression
        let expression = parser::parse(&"A".to_string().tokenise().unwrap()).unwrap();

        // Identifiers
        let mut identifiers: HashMap<String, Expression> = HashMap::new();
        let y = "Ex.Args: '?(([^\\$\n])+\\$){22,}'\nEx.Name: '?powershell.exe'";
        let v: serde_yaml::Value = serde_yaml::from_str(&y).unwrap();
        identifiers.insert("A".to_string(), parser::parse_identifier(&v).unwrap());

        // Fake data
        let j = "{
            \"Ex\": {
                \"Name\": \"POWERSHELL.exe\",
                \"Args\": \"one$two$three$four$five$six$seven$eight$9$10$11$12$13$14$15$16$17$18$19$20$21$22$\"
            }
        }";
        let data: serde_json::Value = serde_json::from_str(&j).unwrap();

        // Assert it cause it could be broken!
        assert_eq!(
            solve_expression(&expression, &identifiers, data.as_object().unwrap()),
            SolverResult::True
        );

        // Benchmark
        b.iter(|| solve_expression(&expression, &identifiers, data.as_object().unwrap()));
    }

    #[bench]
    #[cfg(feature = "benchmarks")]
    fn bench_simple_rule(b: &mut test::Bencher) {
        // Expression
        let expression = parser::parse(&"A".to_string().tokenise().unwrap()).unwrap();

        // Identifiers
        let mut identifiers: HashMap<String, Expression> = HashMap::new();
        let y = "Mem.HiddenDllType: ['adcsa', 'fasdfas', 'reflective_load*']\nEx.Name: 'powershell.exe'";
        let v: serde_yaml::Value = serde_yaml::from_str(&y).unwrap();
        identifiers.insert("A".to_string(), parser::parse_identifier(&v).unwrap());

        // Fake data
        let j = "{
            \"Ex\": {
                \"Name\": \"POWERSHELL.exe\"
            },
            \"Mem\": {
                \"HiddenDllType\": \"reflective_loadsdfajklsdfajklsdfjklasdfjklreflective_loadsdfhjksdfahjklsdfahjklsdfahjklasdfreflective_load\"
            }
        }";
        let data: serde_json::Value = serde_json::from_str(&j).unwrap();

        // Assert it cause it could be broken!
        assert_eq!(
            solve_expression(&expression, &identifiers, data.as_object().unwrap()),
            SolverResult::True
        );

        // Benchmark
        b.iter(|| solve_expression(&expression, &identifiers, data.as_object().unwrap()));
    }
}
