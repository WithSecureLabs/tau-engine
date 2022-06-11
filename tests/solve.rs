mod common;

macro_rules! solve_rule {
    ($rule:expr) => {
        paste::item! {
            #[test]
            fn [< solve_ $rule >] () {
                let rule = common::load_rule($rule).expect("invalid rule");
                assert_eq!(rule.validate().unwrap(), true);
            }

            #[test]
            fn [< solve_ $rule _shaken >] () {
                let rule = common::load_shaken_rule($rule).expect("invalid rule");
                assert_eq!(rule.validate().unwrap(), true);
            }
        }
    };
}

// Expressions
solve_rule!("boolean");
solve_rule!("boolean_group_and");
solve_rule!("boolean_group_or");
solve_rule!("boolean_expression_and");
solve_rule!("boolean_expression_equal");
solve_rule!("boolean_expression_greater_than");
solve_rule!("boolean_expression_greater_than_or_equal");
solve_rule!("boolean_expression_less_than");
solve_rule!("boolean_expression_less_than_or_equal");
solve_rule!("boolean_expression_or");
solve_rule!("cast_int");
solve_rule!("cast_int_field");
solve_rule!("cast_str");
solve_rule!("cast_str_field");
solve_rule!("hash");
solve_rule!("float");
solve_rule!("identifier");
solve_rule!("integer");
solve_rule!("match_all");
solve_rule!("match_all_identifier");
solve_rule!("match_of_0");
solve_rule!("match_of_1");
solve_rule!("match_of_2");
solve_rule!("negate");
solve_rule!("negate_sequence");
solve_rule!("nested");
solve_rule!("nested_dot_notation");
solve_rule!("search_contains");
solve_rule!("search_ends_with");
solve_rule!("search_exact");
solve_rule!("search_regex");
solve_rule!("search_starts_with");
