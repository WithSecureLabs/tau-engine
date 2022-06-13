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
    Rule::loader()
        .shake(false)
        .load(&path)
        .expect("invalid rule")
}

pub fn load_shaken_rule(prefix: &str, name: &str) -> Rule {
    let rule = if name.ends_with(".yml") {
        name.to_owned()
    } else {
        format!("{}.yml", name)
    };
    let root = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(root).join(prefix).join("rules").join(rule);
    Rule::loader()
        .rewrite(true)
        .shake(true)
        .load(&path)
        .expect("invalid rule")
}
