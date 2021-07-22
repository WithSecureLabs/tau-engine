# Tau Engine

[![crates.io](https://img.shields.io/crates/v/tau-engine.svg)](https://crates.io/crates/tau-engine)
[![Documentation](https://docs.rs/tau-engine/badge.svg)](https://docs.rs/tau-engine)

This crate provides a library that tags documents by running and matching rules over them.

## Overview

The engine makes use of a Pratt parser and a tree solver in order to evaluate the detection logic of a rule against a document, if the outcome is true the document is considered tagged by that rule.

## Rules

A rule is used to tag a document and is made up of three parts:
- `detection`: the logic used to evaluate a document.
- `true positive`: an example document that must evaluate to true for the given detection.
- `true negative`: an example document that must evaluate to false for the given detection.

The detection block is made up of a condition, and identifiers. This allows for simple but
expressive rules, below is a brief summary:

### Identifiers

Identifiers are used to help keep the condition concise and generally contain the core of the
matching logic. They consist of Key/Value pairs which allow for the extraction of data from the
document and the evaluate of its value. It should be noted that mappings are treated as
conjunctions, while sequences are treated as disjunctions.

Identifiers make use of the following matching logic:
- `foobar`: an exact match of foobar
- `foobar*`: starts with foobar
- `*foobar`: ends with foobar
- `*foobar*`: contains foobar
- `?foobar`: regex foobar

Any of the above can be made case insensitive with the `i` prefix, for example:
- `ifoobar`
- `ifoobar*`

Escaping can be achieved with a combination of `'` and `"`.

### Condition

The condition is just a boolean expression and supports the following:
- `and`: logical conjunction
- `or`: logical disjunction
- `==`: equality comparison
- `>`, `>=`, `<`, `<=`: numeric comparisons
- `not`: negate
- `all(i)`: make sequences behave as conjunctions
- `of(i, x)`: ensure a sequence has a minimum number of matches

## Examples

This is an example of how the engine can tag a document against a provided rule:

```toml
tau-engine = "1.0"
```

```rust
use std::borrow::Cow;

use tau_engine::{Document, Rule, Value};

// Define a document.
struct Foo {
    foo: String,
}
impl Document for Foo {
    fn find(&self, key: &str) -> Option<Value<'_>> {
        match key {
            "foo" => Some(Value::String(Cow::Borrowed(&self.foo))),
            _ => None,
        }
    }
}

// Write a rule.
let rule = r#"
detection:
  A:
    foo: foobar
  condition: A
true_positives:
- foo: foobar
true_negatives:
- foo: foo
"#;

// Load and validate a rule.
let rule = Rule::load(rule).unwrap();
assert_eq!(rule.validate().unwrap(), true);

// Create a document.
let foo = Foo {
    foo: "foobar".to_owned(),
};

// Evalute the document with the rule.
assert_eq!(rule.matches(&foo), true);
```

This is an example of how the engine can be used to tag on JSON.

```toml
tau-engine = { version = "1.0", features = ["json"] }
```

```rust
use serde_json::json;
use tau_engine::{Document, Rule};

// Write a rule.
let rule = r#"
detection:
  A:
    foo: foobar
  condition: A
true_positives:
- foo: foobar
true_negatives:
- foo: foo
"#;

// Load and validate a rule.
let rule = Rule::load(rule).unwrap();
assert_eq!(rule.validate().unwrap(), true);

// Create a document.
let foo = json!({
    "foo": "foobar",
});

// Evalute the document with the rule.
assert_eq!(rule.matches(&foo), true);
```
