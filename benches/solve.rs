#![cfg_attr(feature = "benchmarks", feature(test))]

#[cfg(feature = "benchmarks")]
extern crate test;

mod common;

#[cfg(feature = "benchmarks")]
use test::Bencher;

fn print_rule(rule: &tau_engine::Rule) {
    println!("condition: {}", rule.detection.expression);
    let mut keys = rule
        .detection
        .identifiers
        .keys()
        .cloned()
        .collect::<Vec<_>>();
    keys.sort();
    for key in &keys {
        println!(
            "\t{}: {}",
            key,
            rule.detection.identifiers.get(key).unwrap()
        );
    }
}

macro_rules! bench_rule {
    ($rule:expr) => {
        paste::item! {
            #[cfg(feature = "benchmarks")]
            #[bench]
            fn [< bench_ $rule _true_positive >] (b: &mut Bencher) {
                let rule = common::load_rule("benches", $rule);
                assert_eq!(rule.validate().unwrap(), true);

                print_rule(&rule);

                let document = rule.true_positives[0].as_mapping().unwrap();
                b.iter(|| tau_engine::solve(&rule.detection, document))
            }

            #[cfg(feature = "benchmarks")]
            #[bench]
            fn [< bench_ $rule _true_positive_shaken >] (b: &mut Bencher) {
                let rule = common::load_shaken_rule("benches", $rule);
                assert_eq!(rule.validate().unwrap(), true);

                print_rule(&rule);

                let document = rule.true_positives[0].as_mapping().unwrap();
                b.iter(|| tau_engine::solve(&rule.detection, document))
            }

            #[cfg(feature = "benchmarks")]
            #[bench]
            fn [< bench_ $rule _true_negative>] (b: &mut Bencher) {
                let rule = common::load_rule("benches", $rule);
                assert_eq!(rule.validate().unwrap(), true);

                print_rule(&rule);

                let document = rule.true_negatives[0].as_mapping().unwrap();
                b.iter(|| tau_engine::solve(&rule.detection, document))
            }

            #[cfg(feature = "benchmarks")]
            #[bench]
            fn [< bench_ $rule _true_negative_shaken>] (b: &mut Bencher) {
                let rule = common::load_shaken_rule("benches", $rule);
                assert_eq!(rule.validate().unwrap(), true);

                print_rule(&rule);

                let document = rule.true_negatives[0].as_mapping().unwrap();
                b.iter(|| tau_engine::solve(&rule.detection, document))
            }
        }
    };
}

macro_rules! bench_test {
    ($rule:expr) => {
        paste::item! {
            #[cfg(feature = "benchmarks")]
            #[bench]
            fn [< bench_ $rule _true_positive >] (b: &mut Bencher) {
                let rule = common::load_rule("tests", $rule);
                assert_eq!(rule.validate().unwrap(), true);

                print_rule(&rule);

                let document = rule.true_positives[0].as_mapping().unwrap();
                b.iter(|| tau_engine::solve(&rule.detection, document))
            }

            #[cfg(feature = "benchmarks")]
            #[bench]
            fn [< bench_ $rule _true_positive_shaken >] (b: &mut Bencher) {
                let rule = common::load_shaken_rule("tests", $rule);
                assert_eq!(rule.validate().unwrap(), true);

                print_rule(&rule);

                let document = rule.true_positives[0].as_mapping().unwrap();
                b.iter(|| tau_engine::solve(&rule.detection, document))
            }

            #[cfg(feature = "benchmarks")]
            #[bench]
            fn [< bench_ $rule _true_negative>] (b: &mut Bencher) {
                let rule = common::load_rule("tests", $rule);
                assert_eq!(rule.validate().unwrap(), true);

                print_rule(&rule);

                let document = rule.true_negatives[0].as_mapping().unwrap();
                b.iter(|| tau_engine::solve(&rule.detection, document))
            }

            #[cfg(feature = "benchmarks")]
            #[bench]
            fn [< bench_ $rule _true_negative_shaken>] (b: &mut Bencher) {
                let rule = common::load_shaken_rule("tests", $rule);
                assert_eq!(rule.validate().unwrap(), true);

                print_rule(&rule);

                let document = rule.true_negatives[0].as_mapping().unwrap();
                b.iter(|| tau_engine::solve(&rule.detection, document))
            }
        }
    };
}

// Expressions
bench_test!("boolean");
bench_test!("boolean_group_and");
bench_test!("boolean_group_or");
bench_test!("boolean_expression_and");
bench_test!("boolean_expression_equal");
bench_test!("boolean_expression_greater_than");
bench_test!("boolean_expression_greater_than_or_equal");
bench_test!("boolean_expression_less_than");
bench_test!("boolean_expression_less_than_or_equal");
bench_test!("boolean_expression_or");
bench_test!("cast_int");
bench_test!("cast_int_field");
bench_test!("cast_str");
bench_test!("cast_str_field");
bench_test!("float");
bench_test!("identifier");
bench_test!("integer");
bench_test!("many_ands");
bench_test!("many_and_nots");
bench_test!("match_all");
bench_test!("match_all_identifier");
bench_test!("match_of_0");
bench_test!("match_of_1");
bench_test!("match_of_2");
bench_test!("negate");
bench_test!("negate_sequence");
bench_test!("nested");
bench_test!("nested_dot_notation");
bench_test!("search_contains");
bench_test!("search_ends_with");
bench_test!("search_exact");
bench_test!("search_regex");
bench_test!("search_starts_with");

// Rules
bench_rule!("complex");
bench_rule!("contains_multiple");
bench_rule!("regex_multiple");
bench_rule!("searches");
bench_rule!("simple");
