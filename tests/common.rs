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
    Rule::loader().shake(false).load(&path)
}

// NOTE: Warns about being dead event though it is not...
#[allow(dead_code)]
pub fn load_shaken_rule(name: &str) -> Result<Rule, Error> {
    let rule = if name.ends_with(".yml") {
        name.to_owned()
    } else {
        format!("{}.yml", name)
    };
    let root = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(root).join("tests/rules").join(rule);
    Rule::loader()
        //.coalesce(true)
        //.rewrite(true)
        .shake(true)
        .load(&path)
}
