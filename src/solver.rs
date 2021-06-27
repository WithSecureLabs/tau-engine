use std::collections::HashMap;
use std::fmt;

use aho_corasick::AhoCorasick;
use tracing::debug;

use crate::document::Document;
use crate::parser::{Expression, Match, MatchType, Search};
use crate::rule::Detection;
use crate::tokeniser::{BoolSym, MiscSym};
use crate::value::Value;

#[derive(Debug, PartialEq)]
enum SolverResult {
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

pub fn solve(detection: &Detection, document: &dyn Document) -> bool {
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
                    Expression::Cast(ref left, MiscSym::Str),
                    BoolSym::Equal,
                    Expression::Cast(ref right, MiscSym::Str),
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
                        Expression::Cast(c, MiscSym::Int) => {
                            let i = match document.find(c) {
                                Some(i) => i,
                                None => {
                                    debug!(
                                        "evaluating missing, no left hand side for {}",
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
                                            "evaluating false, could not cast left hand side for {} - {}",
                                            expression, e
                                        );
                                        return SolverResult::False;
                                    }
                                },
                                None => {
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
                        Expression::Cast(c, MiscSym::Int) => {
                            let i = match document.find(c) {
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
                    _ => unreachable!(),
                },
                Expression::BooleanGroup(ref o, ref g) => (o, g),
                _ => unreachable!(),
            };
            for expression in group {
                // NOTE: Because of needle optimisation we have to handle aho in a `slow` fashion here...
                if let Expression::Search(Search::AhoCorasick(a, m), i) = expression {
                    let value = match document.find(i) {
                        Some(v) => v,
                        None => {
                            debug!("evaluating missing, field not found for {}", expression);
                            return SolverResult::Missing;
                        }
                    };
                    match value {
                        Value::String(ref x) => {
                            if slow_aho(a, m, x) != m.len() as u64 {
                                return SolverResult::False;
                            }
                        }
                        Value::Array(x) => {
                            let mut found = false;
                            for v in x.iter() {
                                if let Some(x) = v.as_str() {
                                    if slow_aho(a, m, x) == m.len() as u64 {
                                        found = true;
                                        break;
                                    }
                                }
                            }
                            if !found {
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
                } else if let Expression::Search(Search::RegexSet(s), i) = expression {
                    let value = match document.find(i) {
                        Some(v) => v,
                        None => {
                            debug!("evaluating missing, field not found for {}", expression);
                            return SolverResult::Missing;
                        }
                    };
                    match value {
                        Value::String(ref x) => {
                            let mut hits = 0;
                            for _ in s.matches(x).iter() {
                                hits += 1;
                            }
                            if hits != s.patterns().len() {
                                return SolverResult::False;
                            }
                        }
                        Value::Array(x) => {
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
                                }
                            }
                            if !found {
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
                Expression::Identifier(ref i) => match identifiers.get(i) {
                    Some(Expression::BooleanGroup(o, g)) => (o, g),
                    _ => unreachable!(),
                },
                Expression::BooleanGroup(ref o, ref g) => (o, g),
                _ => unreachable!(),
            };
            let mut count = 0;
            let mut res = SolverResult::Missing;
            for expression in group {
                // NOTE: Because of needle optimisation we have to handle aho in a `slow` fashion here...
                if let Expression::Search(Search::AhoCorasick(a, m), i) = expression {
                    let value = match document.find(i) {
                        Some(v) => v,
                        None => {
                            debug!("evaluating missing, field not found for {}", expression);
                            return SolverResult::Missing;
                        }
                    };
                    match value {
                        Value::String(ref x) => {
                            count += slow_aho(a, m, x);
                            if count >= c {
                                return SolverResult::True;
                            }
                        }
                        Value::Array(x) => {
                            let mut max = 0;
                            for v in x.iter() {
                                if let Some(x) = v.as_str() {
                                    let hits = slow_aho(a, m, x);
                                    if count + hits >= c {
                                        return SolverResult::True;
                                    } else if hits > max {
                                        max = hits;
                                    }
                                }
                            }
                            count += max;
                        }
                        _ => {
                            debug!(
                                "evaluating false, field is not an array of strings, or a string for {}",
                                expression
                            );
                            return SolverResult::Missing;
                        }
                    }
                } else if let Expression::Search(Search::RegexSet(s), i) = expression {
                    let value = match document.find(i) {
                        Some(v) => v,
                        None => {
                            debug!("evaluating missing, field not found for {}", expression);
                            return SolverResult::Missing;
                        }
                    };
                    match value {
                        Value::String(ref x) => {
                            for _ in s.matches(x).iter() {
                                count += 1;
                            }
                            if count >= c {
                                return SolverResult::True;
                            }
                        }
                        Value::Array(x) => {
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
                                }
                            }
                            count += max;
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
                        "evaluating false, field is not an array of objects or object for {}",
                        expression
                    );
                    SolverResult::False
                }
            }
        }
        Expression::Search(ref s, ref f) => {
            let value = match document.find(f) {
                Some(v) => v,
                None => {
                    debug!("evaluating missing, field not found for {}", expression);
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
        | Expression::Integer(_) => unreachable!(),
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
        Search::RegexSet(ref i) => {
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

#[inline]
fn slow_aho(a: &AhoCorasick, m: &Vec<MatchType>, value: &str) -> u64 {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(feature = "benchmarks")]
    use test::Bencher;

    use crate::parser;
    use crate::tokeniser::Tokeniser;

    #[cfg(feature = "json")]
    #[test]
    fn solve_of() {
        // Expression
        let expression = parser::parse(&"A".to_string().tokenise().unwrap()).unwrap();

        // Identifiers
        let mut identifiers: HashMap<String, Expression> = HashMap::new();
        let y = "of(Ex.Args, 3):\n- '?(([^\\$\n])+\\$){22,}'\n- '*five*'\n- '*six*'\n- '*foo*'\nEx.Name: 'i?powershell.exe'";
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
    }

    #[test]
    #[cfg(feature = "json")]
    fn solve_all() {
        // Expression
        let expression = parser::parse(&"A".to_string().tokenise().unwrap()).unwrap();

        // Identifiers
        let mut identifiers: HashMap<String, Expression> = HashMap::new();
        let y = "all(Ex.Args):\n  - '?(([^\\$\n])+\\$){22,}'\n  - '*five*'\n  - '*six*'\nEx.Name: 'i?powershell.exe'";
        let v: serde_yaml::Value = serde_yaml::from_str(&y).unwrap();
        identifiers.insert("A".to_string(), parser::parse_identifier(&v).unwrap());

        // Fake data
        let j = "{
            \"Ex\": {
                \"Name\": \"POWERSHELL.exe\",
                \"Args\": \"one$two$three$four$five$six$six$seven$eight$9$10$11$12$13$14$15$16$17$18$19$20$21$22$\"
            }
        }";
        let data: serde_json::Value = serde_json::from_str(&j).unwrap();

        // Assert it cause it could be broken!
        assert_eq!(
            solve_expression(&expression, &identifiers, data.as_object().unwrap()),
            SolverResult::True
        );
    }

    #[bench]
    #[cfg(feature = "benchmarks")]
    fn bench_solve_all(b: &mut Bencher) {
        // Expression
        let expression = parser::parse(&"A".to_string().tokenise().unwrap()).unwrap();

        // Identifiers
        let mut identifiers: HashMap<String, Expression> = HashMap::new();
        let y = "all(Ex.Args):\n  - '?(([^\\$\n])+\\$){22,}'\n  - '*five*'\n  - '*six*'\nEx.Name: 'i?powershell.exe'";
        let v: serde_yaml::Value = serde_yaml::from_str(&y).unwrap();
        identifiers.insert("A".to_string(), parser::parse_identifier(&v).unwrap());

        // Fake data
        let j = "{
            \"Ex\": {
                \"Name\": \"POWERSHELL.exe\",
                \"Args\": \"one$two$three$four$five$six$six$seven$eight$9$10$11$12$13$14$15$16$17$18$19$20$21$22$\"
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
    fn bench_regex_rule(b: &mut Bencher) {
        // Expression
        let expression = parser::parse(&"A".to_string().tokenise().unwrap()).unwrap();

        // Identifiers
        let mut identifiers: HashMap<String, Expression> = HashMap::new();
        let y = "Ex.Args: '?(([^\\$\n])+\\$){22,}'\nEx.Name: 'i?powershell.exe'";
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
        let y = "Mem.HiddenDllType: ['adcsa', 'fasdfas', 'reflective_load*']\nEx.Name: 'ipowershell.exe'";
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
