use std::path::Path;

use tau_engine::{Error, Rule};

pub fn load_rule(name: &str) -> Result<Rule, Error> {
    let rule = if name.ends_with(".yml") {
        name.to_owned()
    } else {
        format!("{}.yml", name)
    };
    let root = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(root).join("tests/rules").join(rule);
    Rule::loader().optimise(None).load(&path)
}

// NOTE: Warns about being dead event though it is not...
#[allow(dead_code)]
pub fn load_optimised_rule(name: &str) -> Result<Rule, Error> {
    let rule = if name.ends_with(".yml") {
        name.to_owned()
    } else {
        format!("{}.yml", name)
    };
    let root = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(root).join("tests/rules").join(rule);
    Rule::loader()
        .optimise(Some(Default::default()))
        .load(&path)
}
