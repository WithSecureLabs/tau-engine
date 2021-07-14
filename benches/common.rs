use std::fs;
use std::path::Path;

use tau_engine::Rule;

pub fn load_rule(prefix: &str, name: &str) -> Rule {
    let rule = if name.ends_with(".yml") {
        name.to_owned()
    } else {
        format!("{}.yml", name)
    };
    let root = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(root).join(prefix).join("rules").join(rule);
    let contents = fs::read_to_string(path).expect("failed to read rule");
    Rule::load(&contents).expect("invalid rule")
}
