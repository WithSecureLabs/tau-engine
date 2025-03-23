use std::collections::HashMap;

use aho_corasick::{AhoCorasickBuilder, AhoCorasickKind};
use regex::{RegexBuilder, RegexSetBuilder};

use crate::parser::{Expression, Match, MatchType, Search};
use crate::tokeniser::BoolSym;

/// The types of optimisations to apply to a rule.
pub struct Optimisations {
    /// caalesce the identifier's expressions into the condition.
    pub coalesce: bool,
    /// make use of matrix expressions.
    pub matrix: bool,
    /// rewrite inefficient string searches.
    pub rewrite: bool,
    /// tree shake the rule logic to ensure efficiency.
    pub shake: bool,
}

impl Default for Optimisations {
    fn default() -> Self {
        Self {
            coalesce: true,
            matrix: true,
            rewrite: true,
            shake: true,
        }
    }
}

pub fn coalesce(expression: Expression, identifiers: &HashMap<String, Expression>) -> Expression {
    match expression {
        Expression::BooleanGroup(symbol, expressions) => {
            let mut scratch = vec![];
            for expression in expressions {
                scratch.push(coalesce(expression, identifiers));
            }
            Expression::BooleanGroup(symbol, scratch)
        }
        Expression::BooleanExpression(left, symbol, right) => {
            let left = coalesce(*left, identifiers);
            let right = coalesce(*right, identifiers);
            Expression::BooleanExpression(Box::new(left), symbol, Box::new(right))
        }
        Expression::Identifier(i) => identifiers
            .get(&i)
            .expect("could not get identifier")
            .clone(),
        Expression::Match(symbol, expression) => {
            Expression::Match(symbol, Box::new(coalesce(*expression, identifiers)))
        }
        Expression::Negate(expression) => {
            Expression::Negate(Box::new(coalesce(*expression, identifiers)))
        }
        Expression::Nested(field, expression) => {
            Expression::Nested(field, Box::new(coalesce(*expression, identifiers)))
        }
        Expression::Boolean(_)
        | Expression::Cast(_, _)
        | Expression::Field(_)
        | Expression::Float(_)
        | Expression::Integer(_)
        | Expression::Matrix(_, _)
        | Expression::Null
        | Expression::Search(_, _, _) => expression,
    }
}

pub fn matrix(expression: Expression) -> Expression {
    match expression {
        Expression::BooleanGroup(BoolSym::And, expressions) => {
            let mut scratch = vec![];
            for expression in expressions {
                scratch.push(matrix(expression));
            }
            Expression::BooleanGroup(BoolSym::And, scratch)
        }
        Expression::BooleanGroup(BoolSym::Or, expressions) => {
            // TODO: Clean this logic
            let mut scratch = vec![];
            for expression in expressions {
                scratch.push(matrix(expression));
            }

            let mut fields: HashMap<String, u32> = HashMap::new();
            for expression in &scratch {
                match expression {
                    Expression::BooleanGroup(BoolSym::And, expressions) => {
                        let mut valid = true;
                        for expression in expressions {
                            match expression {
                                Expression::BooleanExpression(left, _, right) => {
                                    match (&**left, &**right) {
                                        (Expression::Cast(_, _), Expression::Boolean(_))
                                        | (Expression::Cast(_, _), Expression::Float(_))
                                        | (Expression::Cast(_, _), Expression::Integer(_))
                                        | (Expression::Cast(_, _), Expression::Null)
                                        | (Expression::Field(_), Expression::Boolean(_))
                                        | (Expression::Field(_), Expression::Float(_))
                                        | (Expression::Field(_), Expression::Integer(_))
                                        | (Expression::Field(_), Expression::Null) => {}
                                        (_, _) => {
                                            valid = false;
                                            break;
                                        }
                                    }
                                }
                                Expression::Nested(_, _) | Expression::Search(_, _, _) => {}
                                _ => {
                                    valid = false;
                                    break;
                                }
                            }
                        }
                        if valid {
                            for expression in expressions {
                                match expression {
                                    Expression::BooleanExpression(left, _, right) => {
                                        match (&**left, &**right) {
                                            (
                                                Expression::Cast(field, _),
                                                Expression::Boolean(_),
                                            )
                                            | (Expression::Cast(field, _), Expression::Float(_))
                                            | (
                                                Expression::Cast(field, _),
                                                Expression::Integer(_),
                                            )
                                            | (Expression::Cast(field, _), Expression::Null)
                                            | (Expression::Field(field), Expression::Boolean(_))
                                            | (Expression::Field(field), Expression::Float(_))
                                            | (Expression::Field(field), Expression::Integer(_))
                                            | (Expression::Field(field), Expression::Null) => {
                                                let count =
                                                    fields.entry(field.clone()).or_insert(0);
                                                *count += 1;
                                            }
                                            (_, _) => {}
                                        }
                                    }
                                    Expression::Nested(field, _)
                                    | Expression::Search(_, field, _) => {
                                        let count = fields.entry(field.clone()).or_insert(0);
                                        *count += 1;
                                    }
                                    _ => {}
                                }
                            }
                        }
                    }
                    Expression::BooleanExpression(left, _, _) => match **left {
                        Expression::Cast(ref field, _) | Expression::Field(ref field) => {
                            let count = fields.entry(field.clone()).or_insert(0);
                            *count += 1;
                        }
                        _ => {}
                    },
                    Expression::Nested(field, _) | Expression::Search(_, field, _) => {
                        let count = fields.entry(field.clone()).or_insert(0);
                        *count += 1;
                    }
                    _ => {}
                }
            }

            let mut matrix = false;
            for count in fields.values() {
                if *count > 1 && *count < 256 {
                    matrix = true;
                }
            }

            if matrix {
                let mut columns: Vec<(String, u32)> = fields.into_iter().collect();
                columns.sort_by(|x, y| x.1.cmp(&y.1));
                let columns: Vec<String> = columns.into_iter().map(|(c, _)| c).collect();
                let mut rows = vec![];
                let mut rest = vec![];
                for expression in scratch {
                    match expression {
                        Expression::BooleanGroup(BoolSym::And, expressions) => {
                            let mut lookup = HashMap::new();
                            let mut valid = true;
                            for expression in &expressions {
                                match expression {
                                    Expression::BooleanExpression(left, _, _) => match **left {
                                        Expression::Cast(ref field, _)
                                        | Expression::Field(ref field) => {
                                            if lookup.contains_key(field) {
                                                valid = false;
                                                break;
                                            }
                                            lookup.insert(field.clone(), expression.clone());
                                        }
                                        _ => {}
                                    },
                                    Expression::Nested(field, _)
                                    | Expression::Search(_, field, _) => {
                                        if lookup.contains_key(field) {
                                            valid = false;
                                            break;
                                        }
                                        lookup.insert(field.clone(), expression.clone());
                                    }
                                    _ => {
                                        valid = false;
                                        break;
                                    }
                                }
                            }
                            if valid {
                                let mut row = vec![];
                                for (i, column) in columns.iter().enumerate() {
                                    if let Some(expression) = lookup.remove(column) {
                                        let key = std::char::from_u32(i as u32)
                                            .expect("could not make key");
                                        match expression {
                                            Expression::BooleanExpression(left, symbol, right) => {
                                                match *left {
                                                    Expression::Cast(_, kind) => {
                                                        row.push(Some(
                                                            Expression::BooleanExpression(
                                                                Box::new(Expression::Cast(
                                                                    key.to_string(),
                                                                    kind.clone(),
                                                                )),
                                                                symbol,
                                                                right.clone(),
                                                            ),
                                                        ));
                                                    }
                                                    Expression::Field(_) => {
                                                        row.push(Some(
                                                            Expression::BooleanExpression(
                                                                Box::new(Expression::Field(
                                                                    key.to_string(),
                                                                )),
                                                                symbol,
                                                                right.clone(),
                                                            ),
                                                        ));
                                                    }
                                                    _ => {}
                                                }
                                            }
                                            Expression::Nested(_, expression) => {
                                                row.push(Some(Expression::Nested(
                                                    key.to_string(),
                                                    expression,
                                                )));
                                            }
                                            Expression::Search(search, _, cast) => {
                                                row.push(Some(Expression::Search(
                                                    search,
                                                    key.to_string(),
                                                    cast,
                                                )));
                                            }
                                            _ => {}
                                        }
                                    } else {
                                        row.push(None);
                                    }
                                }
                                rows.push(row);
                            } else {
                                rest.push(Expression::BooleanGroup(BoolSym::And, expressions));
                            }
                        }
                        Expression::BooleanExpression(left, symbol, right) => {
                            match (*left, &*right) {
                                (Expression::Cast(field, kind), Expression::Boolean(_))
                                | (Expression::Cast(field, kind), Expression::Float(_))
                                | (Expression::Cast(field, kind), Expression::Integer(_))
                                | (Expression::Cast(field, kind), Expression::Null) => {
                                    let mut row = vec![];
                                    for (i, column) in columns.iter().enumerate() {
                                        if column == &field {
                                            let key = std::char::from_u32(i as u32)
                                                .expect("could not make key");
                                            row.push(Some(Expression::BooleanExpression(
                                                Box::new(Expression::Cast(
                                                    key.to_string(),
                                                    kind.clone(),
                                                )),
                                                symbol,
                                                right.clone(),
                                            )));
                                        } else {
                                            row.push(None);
                                        }
                                    }
                                    rows.push(row);
                                }
                                (Expression::Field(field), Expression::Boolean(_))
                                | (Expression::Field(field), Expression::Float(_))
                                | (Expression::Field(field), Expression::Integer(_))
                                | (Expression::Field(field), Expression::Null) => {
                                    let mut row = vec![];
                                    for (i, column) in columns.iter().enumerate() {
                                        if column == &field {
                                            let key = std::char::from_u32(i as u32)
                                                .expect("could not make key");
                                            row.push(Some(Expression::BooleanExpression(
                                                Box::new(Expression::Field(key.to_string())),
                                                symbol,
                                                right.clone(),
                                            )));
                                        } else {
                                            row.push(None);
                                        }
                                    }
                                    rows.push(row);
                                }
                                (left, _) => {
                                    rest.push(Expression::BooleanExpression(
                                        Box::new(left),
                                        symbol,
                                        right,
                                    ));
                                }
                            }
                        }
                        Expression::Nested(field, expression) => {
                            let mut row = vec![];
                            for (i, column) in columns.iter().enumerate() {
                                if column == &field {
                                    let key =
                                        std::char::from_u32(i as u32).expect("could not make key");
                                    row.push(Some(Expression::Nested(
                                        key.to_string(),
                                        expression.clone(),
                                    )));
                                } else {
                                    row.push(None);
                                }
                            }
                            rows.push(row);
                        }
                        Expression::Search(search, field, cast) => {
                            let mut row = vec![];
                            for (i, column) in columns.iter().enumerate() {
                                if column == &field {
                                    let key =
                                        std::char::from_u32(i as u32).expect("could not make key");
                                    row.push(Some(Expression::Search(
                                        search.clone(),
                                        key.to_string(),
                                        cast,
                                    )));
                                } else {
                                    row.push(None);
                                }
                            }
                            rows.push(row);
                        }
                        _ => rest.push(expression),
                    }
                }

                let mut expressions = vec![];
                if !rows.is_empty() {
                    expressions.push(Expression::Matrix(columns, rows));
                }
                expressions.extend(rest);
                if expressions.len() == 1 {
                    expressions
                        .into_iter()
                        .next()
                        .expect("could not get expression")
                } else {
                    Expression::BooleanGroup(BoolSym::Or, expressions)
                }
            } else {
                Expression::BooleanGroup(BoolSym::Or, scratch)
            }
        }
        Expression::BooleanExpression(left, symbol, right) => {
            let left = matrix(*left);
            let right = matrix(*right);
            Expression::BooleanExpression(Box::new(left), symbol, Box::new(right))
        }
        Expression::Match(kind, expression) => match *expression {
            Expression::BooleanGroup(symbol, expressions) => {
                let mut scratch = vec![];
                for expression in expressions {
                    scratch.push(shake_1(expression));
                }
                Expression::Match(kind, Box::new(Expression::BooleanGroup(symbol, scratch)))
            }
            expression => Expression::Match(kind, Box::new(shake_1(expression))),
        },
        Expression::Negate(expression) => Expression::Negate(Box::new(matrix(*expression))),
        Expression::Nested(field, expression) => {
            Expression::Nested(field, Box::new(matrix(*expression)))
        }
        Expression::Boolean(_)
        | Expression::BooleanGroup(_, _)
        | Expression::Cast(_, _)
        | Expression::Field(_)
        | Expression::Float(_)
        | Expression::Identifier(_)
        | Expression::Integer(_)
        | Expression::Matrix(_, _)
        | Expression::Null
        | Expression::Search(_, _, _) => expression,
    }
}

fn rewrite_search(search: Search) -> Search {
    match search {
        Search::Regex(regex, insensitive) => {
            let mut pattern = regex.as_str().to_owned();
            if let Some(tail) = pattern.strip_prefix(".*") {
                pattern = tail.to_owned();
            }
            if let Some(head) = pattern.strip_suffix(".*") {
                pattern = head.to_owned();
            }
            Search::Regex(
                RegexBuilder::new(&pattern)
                    .case_insensitive(insensitive)
                    .build()
                    .expect("could not build regex"),
                insensitive,
            )
        }
        Search::RegexSet(regex, insensitive) => {
            let mut patterns = vec![];
            for pattern in regex.patterns() {
                let mut pattern = pattern.to_owned();
                if let Some(tail) = pattern.strip_prefix(".*") {
                    pattern = tail.to_owned();
                }
                if let Some(head) = pattern.strip_suffix(".*") {
                    pattern = head.to_owned();
                }
                patterns.push(pattern);
            }
            Search::RegexSet(
                RegexSetBuilder::new(patterns)
                    .case_insensitive(insensitive)
                    .build()
                    .expect("could not build regex"),
                insensitive,
            )
        }
        _ => search,
    }
}

pub fn rewrite(expression: Expression) -> Expression {
    match expression {
        Expression::BooleanGroup(symbol, expressions) => {
            let mut scratch = vec![];
            for expression in expressions {
                let rewriten = rewrite(expression);
                scratch.push(rewriten);
            }
            Expression::BooleanGroup(symbol, scratch)
        }
        Expression::BooleanExpression(left, symbol, right) => {
            let left = rewrite(*left);
            let right = rewrite(*right);
            Expression::BooleanExpression(Box::new(left), symbol, Box::new(right))
        }
        Expression::Match(symbol, expression) => {
            Expression::Match(symbol, Box::new(rewrite(*expression)))
        }
        Expression::Negate(expression) => Expression::Negate(Box::new(rewrite(*expression))),
        Expression::Nested(field, expression) => {
            Expression::Nested(field, Box::new(rewrite(*expression)))
        }
        Expression::Search(search, f, c) => Expression::Search(rewrite_search(search), f, c),
        Expression::Boolean(_)
        | Expression::Cast(_, _)
        | Expression::Field(_)
        | Expression::Float(_)
        | Expression::Identifier(_)
        | Expression::Integer(_)
        | Expression::Matrix(_, _)
        | Expression::Null => expression,
    }
}

pub fn shake(expression: Expression) -> Expression {
    // This is a tad lazy but due to the inability to set shake priority its actually easier to run
    // the main shaking, then once we know they are in a set state, perform additional shaking on
    // top of that...
    let expression = shake_0(expression);
    shake_1(expression)
}

fn shake_0(expression: Expression) -> Expression {
    match expression {
        Expression::BooleanGroup(symbol, expressions) => {
            let length = expressions.len();
            let expressions = match symbol {
                BoolSym::And => {
                    // NOTE: We need BooleanExpression to be fully shaken before we run this, so
                    // this optimisation is done in shake_1.
                    let mut scratch = vec![];
                    for expression in expressions {
                        let shaken = shake_0(expression);
                        scratch.push(shaken);
                    }
                    scratch
                }
                BoolSym::Or => {
                    let mut scratch = vec![];
                    for expression in expressions {
                        let shaken = shake_0(expression);
                        scratch.push(shaken);
                    }
                    scratch
                }
                _ => unreachable!(),
            };
            if expressions.len() != length {
                shake_0(Expression::BooleanGroup(symbol, expressions))
            } else if expressions.len() == 1 {
                expressions
                    .into_iter()
                    .next()
                    .expect("could not get expression")
            } else {
                Expression::BooleanGroup(symbol, expressions)
            }
        }
        Expression::BooleanExpression(left, symbol, right) => {
            let left = shake_0(*left);
            let right = shake_0(*right);
            match (left, symbol, right) {
                (
                    Expression::BooleanGroup(BoolSym::And, mut left),
                    BoolSym::And,
                    Expression::BooleanGroup(BoolSym::And, right),
                ) => {
                    left.extend(right);
                    shake_0(Expression::BooleanGroup(BoolSym::And, left))
                }
                (Expression::BooleanGroup(BoolSym::And, mut left), BoolSym::And, right) => {
                    left.push(right);
                    shake_0(Expression::BooleanGroup(BoolSym::And, left))
                }
                (left, BoolSym::And, Expression::BooleanGroup(BoolSym::And, right)) => {
                    let mut left = vec![left];
                    left.extend(right);
                    shake_0(Expression::BooleanGroup(BoolSym::And, left))
                }
                (
                    Expression::BooleanGroup(BoolSym::Or, mut left),
                    BoolSym::Or,
                    Expression::BooleanGroup(BoolSym::Or, right),
                ) => {
                    left.extend(right);
                    shake_0(Expression::BooleanGroup(BoolSym::Or, left))
                }
                (Expression::BooleanGroup(BoolSym::Or, mut left), BoolSym::Or, right) => {
                    left.push(right);
                    shake_0(Expression::BooleanGroup(BoolSym::Or, left))
                }
                (left, BoolSym::Or, Expression::BooleanGroup(BoolSym::Or, right)) => {
                    let mut left = vec![left];
                    left.extend(right);
                    shake_0(Expression::BooleanGroup(BoolSym::Or, left))
                }
                (Expression::BooleanExpression(x, BoolSym::And, y), BoolSym::And, z) => {
                    shake_0(Expression::BooleanGroup(BoolSym::And, vec![*x, *y, z]))
                }
                (x, BoolSym::And, Expression::BooleanExpression(y, BoolSym::And, z)) => {
                    shake_0(Expression::BooleanGroup(BoolSym::And, vec![x, *y, *z]))
                }
                (Expression::BooleanExpression(x, BoolSym::Or, y), BoolSym::Or, z) => {
                    shake_0(Expression::BooleanGroup(BoolSym::Or, vec![*x, *y, z]))
                }
                (x, BoolSym::Or, Expression::BooleanExpression(y, BoolSym::Or, z)) => {
                    shake_0(Expression::BooleanGroup(BoolSym::Or, vec![x, *y, *z]))
                }
                // FIXME: This will cause false positives due to how true/false/missing is
                // calculated, there may be a way around this but the speed up is not worth it. We
                // could probably put faith in brackets?
                //(Expression::Negate(left), BoolSym::And, Expression::Negate(right)) => {
                //    shake_0(Expression::Negate(Box::new(shake_0(
                //        Expression::BooleanExpression(left, BoolSym::Or, right),
                //    ))))
                //}
                (left, _, right) => {
                    Expression::BooleanExpression(Box::new(left), symbol, Box::new(right))
                }
            }
        }
        Expression::Match(m, expression) => {
            let expression = shake_0(*expression);
            Expression::Match(m, Box::new(expression))
        }
        Expression::Negate(expression) => {
            let expression = shake_0(*expression);
            match expression {
                Expression::Negate(inner) => shake_0(*inner),
                _ => Expression::Negate(Box::new(expression)),
            }
        }
        Expression::Nested(field, expression) => {
            Expression::Nested(field, Box::new(shake_0(*expression)))
        }
        Expression::Boolean(_)
        | Expression::Cast(_, _)
        | Expression::Field(_)
        | Expression::Float(_)
        | Expression::Identifier(_)
        | Expression::Integer(_)
        | Expression::Matrix(_, _)
        | Expression::Null
        | Expression::Search(_, _, _) => expression,
    }
}

fn shake_1(expression: Expression) -> Expression {
    match expression {
        // TODO: Due to limitations with how we handle accessing array data it is not possible
        // to enable this optimisation yet... There is no way to say match all within an array of
        // fields...
        Expression::BooleanGroup(BoolSym::And, expressions) => {
            let length = expressions.len();

            let mut nested = HashMap::new();

            let mut scratch = vec![];

            for expression in expressions {
                let shaken = shake_1(expression);
                match shaken {
                    Expression::Nested(field, expression) => {
                        let expressions = nested.entry(field).or_insert(vec![]);
                        (*expressions).push(*expression);
                    }
                    shaken => scratch.push(shaken),
                };
            }
            for (field, expressions) in nested {
                let shaken = if expressions.len() == 1 {
                    shake_1(
                        expressions
                            .into_iter()
                            .next()
                            .expect("could not get expression"),
                    )
                } else {
                    shake_1(Expression::Match(
                        Match::All,
                        Box::new(Expression::BooleanGroup(BoolSym::Or, expressions)),
                    ))
                };
                scratch.push(Expression::Nested(field, Box::new(shaken)));
            }
            if scratch.len() != length {
                shake_1(Expression::BooleanGroup(BoolSym::And, scratch))
            } else if scratch.len() == 1 {
                scratch
                    .into_iter()
                    .next()
                    .expect("could not get expression")
            } else {
                Expression::BooleanGroup(BoolSym::And, scratch)
            }
        }
        Expression::BooleanGroup(BoolSym::Or, expressions) => {
            let length = expressions.len();
            let expressions = {
                let mut needles = HashMap::new();
                let mut nested = HashMap::new();
                let mut patterns = HashMap::new();

                // NOTE: Order is crucial here just like in the parser, thus we copy its ideal
                // ordering.
                let mut any = vec![];
                let mut exact = vec![];
                let mut starts_with = vec![];
                let mut ends_with = vec![];
                let mut contains = vec![];
                let mut aho = vec![];
                let mut regex = vec![];
                let mut regex_set = vec![];
                let mut rest = vec![];

                for expression in expressions {
                    let shaken = shake_1(expression);

                    match shaken {
                        Expression::Nested(field, expression) => {
                            let expressions = nested.entry(field).or_insert(vec![]);
                            (*expressions).push(*expression);
                        }
                        Expression::Search(
                            Search::AhoCorasick(_, contexts, insensitive),
                            field,
                            cast,
                        ) => {
                            let expressions =
                                needles.entry((field, cast, insensitive)).or_insert(vec![]);
                            for context in contexts {
                                let value = context.value().to_owned();
                                (*expressions).push((context.clone(), value));
                            }
                        }
                        Expression::Search(Search::Contains(value), field, cast) => {
                            let expressions = needles.entry((field, cast, false)).or_insert(vec![]);
                            (*expressions).push((MatchType::Contains(value.clone()), value));
                        }
                        Expression::Search(Search::EndsWith(value), field, cast) => {
                            let expressions = needles.entry((field, cast, false)).or_insert(vec![]);
                            (*expressions).push((MatchType::EndsWith(value.clone()), value));
                        }
                        Expression::Search(Search::Exact(value), field, cast) => {
                            let expressions = needles.entry((field, cast, false)).or_insert(vec![]);
                            (*expressions).push((MatchType::Exact(value.clone()), value));
                        }
                        Expression::Search(Search::StartsWith(value), field, cast) => {
                            let expressions = needles.entry((field, cast, false)).or_insert(vec![]);
                            (*expressions).push((MatchType::StartsWith(value.clone()), value));
                        }
                        Expression::Search(Search::Any, _, _) => {
                            any.push(shaken);
                        }
                        Expression::Search(Search::Regex(r, insensitive), field, cast) => {
                            let patterns =
                                patterns.entry((field, cast, insensitive)).or_insert(vec![]);
                            (*patterns).push(r.as_str().to_owned());
                        }
                        Expression::Search(Search::RegexSet(r, insensitive), field, cast) => {
                            let patterns =
                                patterns.entry((field, cast, insensitive)).or_insert(vec![]);
                            for pattern in r.patterns() {
                                (*patterns).push(pattern.to_owned());
                            }
                        }
                        _ => rest.push(shaken),
                    }
                }

                for ((field, cast, insensitive), searches) in needles {
                    if !insensitive && searches.len() == 1 {
                        let search = searches.into_iter().next().expect("could not get search");
                        match search.0 {
                            MatchType::Contains(v) => {
                                contains.push(Expression::Search(Search::Contains(v), field, cast));
                            }
                            MatchType::EndsWith(v) => {
                                ends_with.push(Expression::Search(
                                    Search::EndsWith(v),
                                    field,
                                    cast,
                                ));
                            }
                            MatchType::Exact(v) => {
                                exact.push(Expression::Search(Search::Exact(v), field, cast));
                            }
                            MatchType::StartsWith(v) => {
                                starts_with.push(Expression::Search(
                                    Search::StartsWith(v),
                                    field,
                                    cast,
                                ));
                            }
                        };
                    } else {
                        let (context, needles): (Vec<_>, Vec<_>) = searches.into_iter().unzip();
                        let expression = Expression::Search(
                            Search::AhoCorasick(
                                Box::new(
                                    AhoCorasickBuilder::new()
                                        .ascii_case_insensitive(insensitive)
                                        .kind(Some(AhoCorasickKind::DFA))
                                        .build(needles)
                                        .expect("failed to build dfa"),
                                ),
                                context,
                                insensitive,
                            ),
                            field,
                            cast,
                        );
                        aho.push(expression);
                    };
                }

                for (field, expressions) in nested {
                    let shaken = if expressions.len() == 1 {
                        shake_1(
                            expressions
                                .into_iter()
                                .next()
                                .expect("could not get expression"),
                        )
                    } else {
                        shake_1(Expression::BooleanGroup(BoolSym::Or, expressions))
                    };
                    rest.push(Expression::Nested(field, Box::new(shaken)));
                }

                for ((field, cast, insensitive), patterns) in patterns {
                    if patterns.len() == 1 {
                        let pattern = patterns.into_iter().next().expect("could not get pattern");
                        let expression = Expression::Search(
                            Search::Regex(
                                RegexBuilder::new(&pattern)
                                    .case_insensitive(insensitive)
                                    .build()
                                    .expect("could not build regex"),
                                insensitive,
                            ),
                            field,
                            cast,
                        );
                        regex.push(expression);
                    } else {
                        let expression = Expression::Search(
                            Search::RegexSet(
                                RegexSetBuilder::new(patterns)
                                    .case_insensitive(insensitive)
                                    .build()
                                    .expect("could not build regex set"),
                                insensitive,
                            ),
                            field,
                            cast,
                        );
                        regex_set.push(expression);
                    }
                }

                let mut scratch = vec![];
                scratch.extend(any);
                exact.sort_by(|x, y| match (x, y) {
                    (
                        Expression::Search(Search::Exact(a), _, _),
                        Expression::Search(Search::Exact(b), _, _),
                    ) => a.len().cmp(&b.len()),
                    _ => std::cmp::Ordering::Equal,
                });
                scratch.extend(exact);
                starts_with.sort_by(|x, y| match (x, y) {
                    (
                        Expression::Search(Search::StartsWith(a), _, _),
                        Expression::Search(Search::StartsWith(b), _, _),
                    ) => a.len().cmp(&b.len()),
                    _ => std::cmp::Ordering::Equal,
                });
                scratch.extend(starts_with);
                ends_with.sort_by(|x, y| match (x, y) {
                    (
                        Expression::Search(Search::EndsWith(a), _, _),
                        Expression::Search(Search::EndsWith(b), _, _),
                    ) => a.len().cmp(&b.len()),
                    _ => std::cmp::Ordering::Equal,
                });
                scratch.extend(ends_with);
                contains.sort_by(|x, y| match (x, y) {
                    (
                        Expression::Search(Search::Contains(a), _, _),
                        Expression::Search(Search::Contains(b), _, _),
                    ) => a.len().cmp(&b.len()),
                    _ => std::cmp::Ordering::Equal,
                });
                scratch.extend(contains);
                aho.sort_by(|x, y| match (x, y) {
                    (
                        Expression::Search(Search::AhoCorasick(_, a, case0), _, _),
                        Expression::Search(Search::AhoCorasick(_, b, case1), _, _),
                    ) => (b.len(), case1).cmp(&(a.len(), case0)),
                    _ => std::cmp::Ordering::Equal,
                });
                scratch.extend(aho);
                regex.sort_by(|x, y| match (x, y) {
                    (
                        Expression::Search(Search::Regex(reg0, case0), _, _),
                        Expression::Search(Search::Regex(reg1, case1), _, _),
                    ) => (reg0.as_str(), case0).cmp(&(reg1.as_str(), case1)),
                    _ => std::cmp::Ordering::Equal,
                });
                scratch.extend(regex);
                regex_set.sort_by(|x, y| match (x, y) {
                    (
                        Expression::Search(Search::RegexSet(set0, case0), _, _),
                        Expression::Search(Search::RegexSet(set1, case1), _, _),
                    ) => (set0.patterns(), case0).cmp(&(set1.patterns(), case1)),
                    _ => std::cmp::Ordering::Equal,
                });
                scratch.extend(regex_set);
                scratch.extend(rest);
                scratch
            };
            if expressions.len() != length {
                shake_1(Expression::BooleanGroup(BoolSym::Or, expressions))
            } else if expressions.len() == 1 {
                expressions
                    .into_iter()
                    .next()
                    .expect("could not get expression")
            } else {
                Expression::BooleanGroup(BoolSym::Or, expressions)
            }
        }
        Expression::BooleanGroup(symbol, expressions) => {
            let mut scratch = vec![];
            for expression in expressions {
                scratch.push(shake_1(expression));
            }
            Expression::BooleanGroup(symbol, scratch)
        }
        Expression::BooleanExpression(left, symbol, right) => {
            let left = shake_1(*left);
            let right = shake_1(*right);
            Expression::BooleanExpression(Box::new(left), symbol, Box::new(right))
        }
        Expression::Match(kind, expression) => match *expression {
            Expression::BooleanGroup(symbol, expressions) => {
                let mut scratch = vec![];
                for expression in expressions {
                    scratch.push(shake_1(expression));
                }
                Expression::Match(kind, Box::new(Expression::BooleanGroup(symbol, scratch)))
            }
            expression => Expression::Match(kind, Box::new(shake_1(expression))),
        },
        Expression::Negate(expression) => Expression::Negate(Box::new(shake_1(*expression))),
        Expression::Nested(field, expression) => {
            Expression::Nested(field, Box::new(shake_1(*expression)))
        }
        Expression::Boolean(_)
        | Expression::Cast(_, _)
        | Expression::Field(_)
        | Expression::Float(_)
        | Expression::Identifier(_)
        | Expression::Integer(_)
        | Expression::Matrix(_, _)
        | Expression::Null
        | Expression::Search(_, _, _) => expression,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn coalesce_basic() {
        let mut identifiers = HashMap::new();
        identifiers.insert(
            "A".to_owned(),
            Expression::BooleanExpression(
                Box::new(Expression::Field("count".to_owned())),
                BoolSym::Equal,
                Box::new(Expression::Integer(1)),
            ),
        );
        let expression = Expression::Identifier("A".to_owned());

        let coalesced = coalesce(expression, &identifiers);

        let expected = Expression::BooleanExpression(
            Box::new(Expression::Field("count".to_owned())),
            BoolSym::Equal,
            Box::new(Expression::Integer(1)),
        );

        assert_eq!(coalesced, expected);
    }

    // FIXME: Disabled for now...
    //#[test]
    //fn shake_and_nots() {
    //    let expression = Expression::BooleanExpression(
    //        Box::new(Expression::Negate(Box::new(Expression::Null))),
    //        BoolSym::And,
    //        Box::new(Expression::Negate(Box::new(Expression::Null))),
    //    );
    //    let shaken = shake(expression);

    //    let expected = Expression::Negate(Box::new(Expression::BooleanExpression(
    //        Box::new(Expression::Null),
    //        BoolSym::Or,
    //        Box::new(Expression::Null),
    //    )));

    //    assert_eq!(shaken, expected);
    //}

    #[test]
    fn shake_ands() {
        let expression = Expression::BooleanExpression(
            Box::new(Expression::Null),
            BoolSym::And,
            Box::new(Expression::Null),
        );
        let shaken = shake(expression);

        let expected = Expression::BooleanExpression(
            Box::new(Expression::Null),
            BoolSym::And,
            Box::new(Expression::Null),
        );

        assert_eq!(shaken, expected);

        let expression = Expression::BooleanExpression(
            Box::new(Expression::Null),
            BoolSym::And,
            Box::new(Expression::BooleanExpression(
                Box::new(Expression::Null),
                BoolSym::And,
                Box::new(Expression::Null),
            )),
        );
        let shaken = shake(expression);

        let expected = Expression::BooleanGroup(
            BoolSym::And,
            vec![Expression::Null, Expression::Null, Expression::Null],
        );

        assert_eq!(shaken, expected);
    }

    #[test]
    fn shake_ors() {
        let expression = Expression::BooleanExpression(
            Box::new(Expression::Null),
            BoolSym::Or,
            Box::new(Expression::Null),
        );
        let shaken = shake(expression);

        let expected = Expression::BooleanExpression(
            Box::new(Expression::Null),
            BoolSym::Or,
            Box::new(Expression::Null),
        );

        assert_eq!(shaken, expected);

        let expression = Expression::BooleanExpression(
            Box::new(Expression::Null),
            BoolSym::Or,
            Box::new(Expression::BooleanExpression(
                Box::new(Expression::Null),
                BoolSym::Or,
                Box::new(Expression::Null),
            )),
        );
        let shaken = shake(expression);

        let expected = Expression::BooleanGroup(
            BoolSym::Or,
            vec![Expression::Null, Expression::Null, Expression::Null],
        );

        assert_eq!(shaken, expected);
    }

    #[test]
    fn shake_group_of_nested() {
        let expression = Expression::BooleanGroup(
            BoolSym::Or,
            vec![
                Expression::Nested(
                    "ids".to_owned(),
                    Box::new(Expression::Search(
                        Search::Exact("e2ec14cb-299e-4adf-bb09-04a6a8417bca".to_owned()),
                        "id".to_owned(),
                        false,
                    )),
                ),
                Expression::Nested(
                    "ids".to_owned(),
                    Box::new(Expression::Search(
                        Search::Exact("e2ec14cb-299e-4adf-bb09-04a6a8417bcb".to_owned()),
                        "id".to_owned(),
                        false,
                    )),
                ),
                Expression::Nested(
                    "ids".to_owned(),
                    Box::new(Expression::Search(
                        Search::Exact("e2ec14cb-299e-4adf-bb09-04a6a8417bcc".to_owned()),
                        "id".to_owned(),
                        false,
                    )),
                ),
            ],
        );
        let shaken = shake(expression);

        let expected = Expression::Nested(
            "ids".to_owned(),
            Box::new(Expression::Search(
                Search::AhoCorasick(
                    Box::new(
                        AhoCorasickBuilder::new()
                            .kind(Some(AhoCorasickKind::DFA))
                            .build(vec![
                                "e2ec14cb-299e-4adf-bb09-04a6a8417bca",
                                "e2ec14cb-299e-4adf-bb09-04a6a8417bcb",
                                "e2ec14cb-299e-4adf-bb09-04a6a8417bcc",
                            ])
                            .expect("failed to build dfa"),
                    ),
                    vec![
                        MatchType::Exact("e2ec14cb-299e-4adf-bb09-04a6a8417bca".to_owned()),
                        MatchType::Exact("e2ec14cb-299e-4adf-bb09-04a6a8417bcb".to_owned()),
                        MatchType::Exact("e2ec14cb-299e-4adf-bb09-04a6a8417bcc".to_owned()),
                    ],
                    false,
                ),
                "id".to_owned(),
                false,
            )),
        );

        assert_eq!(shaken, expected);
    }

    #[test]
    fn shake_group_or() {
        // NOTE: This is not a solvable expression but tests what we need testing
        let expression = Expression::BooleanGroup(
            BoolSym::Or,
            vec![
                Expression::Search(
                    Search::AhoCorasick(
                        Box::new(
                            AhoCorasickBuilder::new()
                                .kind(Some(AhoCorasickKind::DFA))
                                .ascii_case_insensitive(false)
                                .build(vec![
                                    "Quick".to_owned(),
                                    "Brown".to_owned(),
                                    "Fox".to_owned(),
                                ])
                                .expect("failed to build dfa"),
                        ),
                        vec![
                            MatchType::Contains("Quick".to_owned()),
                            MatchType::Exact("Brown".to_owned()),
                            MatchType::EndsWith("Fox".to_owned()),
                        ],
                        false,
                    ),
                    "name".to_owned(),
                    false,
                ),
                Expression::Search(
                    Search::AhoCorasick(
                        Box::new(
                            AhoCorasickBuilder::new()
                                .kind(Some(AhoCorasickKind::DFA))
                                .ascii_case_insensitive(true)
                                .build(vec![
                                    "quick".to_owned(),
                                    "brown".to_owned(),
                                    "fox".to_owned(),
                                ])
                                .expect("failed to build dfa"),
                        ),
                        vec![
                            MatchType::Contains("quick".to_owned()),
                            MatchType::Exact("brown".to_owned()),
                            MatchType::EndsWith("fox".to_owned()),
                        ],
                        true,
                    ),
                    "name".to_owned(),
                    false,
                ),
                Expression::Search(Search::Any, "name".to_owned(), false),
                Expression::Search(Search::Contains("afoo".to_owned()), "a".to_owned(), false),
                Expression::Search(Search::Contains("foo".to_owned()), "name".to_owned(), false),
                Expression::Search(Search::EndsWith("bbar".to_owned()), "b".to_owned(), false),
                Expression::Search(Search::EndsWith("bar".to_owned()), "name".to_owned(), false),
                Expression::Search(Search::Exact("cbaz".to_owned()), "c".to_owned(), false),
                Expression::Search(Search::Exact("baz".to_owned()), "name".to_owned(), false),
                Expression::Search(
                    Search::Regex(
                        RegexBuilder::new("foo")
                            .case_insensitive(false)
                            .build()
                            .unwrap(),
                        false,
                    ),
                    "name".to_owned(),
                    false,
                ),
                Expression::Search(
                    Search::Regex(
                        RegexBuilder::new("bar")
                            .case_insensitive(true)
                            .build()
                            .unwrap(),
                        true,
                    ),
                    "name".to_owned(),
                    false,
                ),
                Expression::Search(
                    Search::RegexSet(
                        RegexSetBuilder::new(vec!["lorem"])
                            .case_insensitive(false)
                            .build()
                            .unwrap(),
                        false,
                    ),
                    "name".to_owned(),
                    false,
                ),
                Expression::Search(
                    Search::RegexSet(
                        RegexSetBuilder::new(vec!["ipsum"])
                            .case_insensitive(true)
                            .build()
                            .unwrap(),
                        true,
                    ),
                    "name".to_owned(),
                    false,
                ),
                Expression::Search(
                    Search::StartsWith("dfoobar".to_owned()),
                    "d".to_owned(),
                    false,
                ),
                Expression::Search(
                    Search::StartsWith("foobar".to_owned()),
                    "name".to_owned(),
                    false,
                ),
            ],
        );
        let shaken = shake(expression);

        let expected = Expression::BooleanGroup(
            BoolSym::Or,
            vec![
                Expression::Search(Search::Any, "name".to_owned(), false),
                Expression::Search(Search::Exact("cbaz".to_owned()), "c".to_owned(), false),
                Expression::Search(
                    Search::StartsWith("dfoobar".to_owned()),
                    "d".to_owned(),
                    false,
                ),
                Expression::Search(Search::EndsWith("bbar".to_owned()), "b".to_owned(), false),
                Expression::Search(Search::Contains("afoo".to_owned()), "a".to_owned(), false),
                Expression::Search(
                    Search::AhoCorasick(
                        Box::new(
                            AhoCorasickBuilder::new()
                                .kind(Some(AhoCorasickKind::DFA))
                                .ascii_case_insensitive(false)
                                .build(vec![
                                    "Quick".to_owned(),
                                    "Brown".to_owned(),
                                    "Fox".to_owned(),
                                    "foo".to_owned(),
                                    "bar".to_owned(),
                                    "baz".to_owned(),
                                    "foobar".to_owned(),
                                ])
                                .expect("failed to build dfa"),
                        ),
                        vec![
                            MatchType::Contains("Quick".to_owned()),
                            MatchType::Exact("Brown".to_owned()),
                            MatchType::EndsWith("Fox".to_owned()),
                            MatchType::Contains("foo".to_owned()),
                            MatchType::EndsWith("bar".to_owned()),
                            MatchType::Exact("baz".to_owned()),
                            MatchType::StartsWith("foobar".to_owned()),
                        ],
                        false,
                    ),
                    "name".to_owned(),
                    false,
                ),
                Expression::Search(
                    Search::AhoCorasick(
                        Box::new(
                            AhoCorasickBuilder::new()
                                .kind(Some(AhoCorasickKind::DFA))
                                .ascii_case_insensitive(true)
                                .build(vec![
                                    "quick".to_owned(),
                                    "brown".to_owned(),
                                    "fox".to_owned(),
                                ])
                                .expect("failed to build dfa"),
                        ),
                        vec![
                            MatchType::Contains("quick".to_owned()),
                            MatchType::Exact("brown".to_owned()),
                            MatchType::EndsWith("fox".to_owned()),
                        ],
                        true,
                    ),
                    "name".to_owned(),
                    false,
                ),
                Expression::Search(
                    Search::RegexSet(
                        RegexSetBuilder::new(vec!["bar", "ipsum"])
                            .case_insensitive(true)
                            .build()
                            .unwrap(),
                        true,
                    ),
                    "name".to_owned(),
                    false,
                ),
                Expression::Search(
                    Search::RegexSet(
                        RegexSetBuilder::new(vec!["foo", "lorem"])
                            .case_insensitive(false)
                            .build()
                            .unwrap(),
                        false,
                    ),
                    "name".to_owned(),
                    false,
                ),
            ],
        );

        assert_eq!(shaken, expected);
    }

    #[test]
    fn shake_group_or_1() {
        // NOTE: This is not a solvable expression but tests what we need testing
        let expression = Expression::BooleanGroup(BoolSym::Or, vec![Expression::Null]);
        let shaken = shake(expression);

        let expected = Expression::Null;

        assert_eq!(shaken, expected);
    }

    #[test]
    fn shake_nested() {
        let expression = Expression::Nested(
            "ids".to_owned(),
            Box::new(Expression::BooleanGroup(
                BoolSym::Or,
                vec![
                    Expression::Search(
                        Search::Exact("e2ec14cb-299e-4adf-bb09-04a6a8417bca".to_owned()),
                        "id".to_owned(),
                        false,
                    ),
                    Expression::Search(
                        Search::Exact("e2ec14cb-299e-4adf-bb09-04a6a8417bcb".to_owned()),
                        "id".to_owned(),
                        false,
                    ),
                    Expression::Search(
                        Search::Exact("e2ec14cb-299e-4adf-bb09-04a6a8417bcc".to_owned()),
                        "id".to_owned(),
                        false,
                    ),
                ],
            )),
        );
        let shaken = shake(expression);

        let expected = Expression::Nested(
            "ids".to_owned(),
            Box::new(Expression::Search(
                Search::AhoCorasick(
                    Box::new(
                        AhoCorasickBuilder::new()
                            .kind(Some(AhoCorasickKind::DFA))
                            .build(vec![
                                "e2ec14cb-299e-4adf-bb09-04a6a8417bca",
                                "e2ec14cb-299e-4adf-bb09-04a6a8417bcb",
                                "e2ec14cb-299e-4adf-bb09-04a6a8417bcc",
                            ])
                            .expect("failed to build dfa"),
                    ),
                    vec![
                        MatchType::Exact("e2ec14cb-299e-4adf-bb09-04a6a8417bca".to_owned()),
                        MatchType::Exact("e2ec14cb-299e-4adf-bb09-04a6a8417bcb".to_owned()),
                        MatchType::Exact("e2ec14cb-299e-4adf-bb09-04a6a8417bcc".to_owned()),
                    ],
                    false,
                ),
                "id".to_owned(),
                false,
            )),
        );

        assert_eq!(shaken, expected);
    }

    #[test]
    fn shake_match() {
        let expression = Expression::Match(
            Match::All,
            Box::new(Expression::BooleanGroup(
                BoolSym::Or,
                vec![Expression::Null, Expression::Null],
            )),
        );
        let shaken = shake(expression);

        let expected = Expression::Match(
            Match::All,
            Box::new(Expression::BooleanGroup(
                BoolSym::Or,
                vec![Expression::Null, Expression::Null],
            )),
        );

        assert_eq!(shaken, expected);
    }

    #[test]
    fn shake_negate() {
        let expression =
            Expression::Negate(Box::new(Expression::Negate(Box::new(Expression::Null))));
        let shaken = shake(expression);

        let expected = Expression::Null;

        assert_eq!(shaken, expected);
    }

    #[test]
    fn rewrite_regex() {
        let expression = Expression::Search(
            Search::Regex(RegexBuilder::new(".*foo.*").build().unwrap(), false),
            "name".to_owned(),
            false,
        );
        let rewriten = rewrite(expression);

        let expected = Expression::Search(
            Search::Regex(RegexBuilder::new("foo").build().unwrap(), false),
            "name".to_owned(),
            false,
        );

        assert_eq!(rewriten, expected);

        let expression = Expression::Search(
            Search::RegexSet(
                RegexSetBuilder::new(vec![".*foo.*"]).build().unwrap(),
                false,
            ),
            "name".to_owned(),
            false,
        );
        let rewriten = rewrite(expression);

        let expected = Expression::Search(
            Search::RegexSet(RegexSetBuilder::new(vec!["foo"]).build().unwrap(), false),
            "name".to_owned(),
            false,
        );

        assert_eq!(rewriten, expected);
    }

    #[test]
    fn rewrite_rule_0() {
        let expression = Expression::BooleanExpression(
            Box::new(Expression::BooleanExpression(
                Box::new(Expression::Match(
                    Match::All,
                    Box::new(Expression::Search(
                        Search::Exact("a".to_owned()),
                        "a".to_owned(),
                        false,
                    )),
                )),
                BoolSym::Or,
                Box::new(Expression::Null),
            )),
            BoolSym::And,
            Box::new(Expression::Negate(Box::new(Expression::Null))),
        );
        let shaken = shake(expression);

        let expected = Expression::BooleanExpression(
            Box::new(Expression::BooleanExpression(
                Box::new(Expression::Match(
                    Match::All,
                    Box::new(Expression::Search(
                        Search::Exact("a".to_owned()),
                        "a".to_owned(),
                        false,
                    )),
                )),
                BoolSym::Or,
                Box::new(Expression::Null),
            )),
            BoolSym::And,
            Box::new(Expression::Negate(Box::new(Expression::Null))),
        );

        assert_eq!(shaken, expected);
    }

    #[test]
    fn rewrite_rule_edge_0() {
        // FIXME: For now due to complex searches, we force into a group...
        let expression = Expression::Negate(Box::new(Expression::BooleanGroup(
            BoolSym::Or,
            vec![Expression::BooleanGroup(
                BoolSym::And,
                vec![
                    Expression::Search(Search::Exact("a".to_owned()), "a".to_owned(), false),
                    Expression::Search(Search::Exact("b".to_owned()), "b".to_owned(), false),
                ],
            )],
        )));
        let shaken = shake(expression);

        let expected = Expression::Negate(Box::new(Expression::BooleanGroup(
            BoolSym::And,
            vec![
                Expression::Search(Search::Exact("a".to_owned()), "a".to_owned(), false),
                Expression::Search(Search::Exact("b".to_owned()), "b".to_owned(), false),
            ],
        )));

        assert_eq!(shaken, expected);
    }
}
