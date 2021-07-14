mod common;

macro_rules! solve_rule {
    ($rule:expr) => {
        paste::item! {
            #[test]
            fn [< solve_ $rule >] () {
                let rule = common::load_rule($rule);
                assert_eq!(rule.validate().unwrap(), true);
            }
        }
    };
}

solve_rule!("boolean_group");
solve_rule!("identifier");
