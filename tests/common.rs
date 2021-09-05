use std::fs;
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
    let contents = fs::read_to_string(path).expect("failed to read rule");
    Rule::load(&contents)
}
