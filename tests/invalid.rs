mod common;

macro_rules! invalid_rule {
    ($rule:expr) => {
        paste::item! {
            #[test]
            fn [< invalid_ $rule >] () {
                let rule = common::load_rule($rule);
                assert_eq!(rule.is_err(), true);
            }
        }
    };
}

invalid_rule!("cast_int_nested");
invalid_rule!("cast_str_nested");
invalid_rule!("identifier_missing");
invalid_rule!("match_all_invalid");
invalid_rule!("match_of_invalid");
