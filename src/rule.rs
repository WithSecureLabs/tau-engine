use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::path::Path;

use serde::de::{self, Deserializer, MapAccess, Visitor};
use serde::{Deserialize, Serialize};
use serde_yaml::Value as Yaml;

use crate::document::Document;
use crate::optimiser;
use crate::parser::{self, Expression};
use crate::solver;
use crate::tokeniser::{ModSym, Token, Tokeniser};

/// The detection block, this contains the logic that is to be run through the solver to evaluate a
/// `Document`.
#[derive(Clone, Serialize)]
pub struct Detection {
    /// The core expression.
    #[serde(skip_serializing)]
    pub expression: Expression,
    /// Additional expressions, defined using key/value pairs.
    #[serde(skip_serializing)]
    pub identifiers: HashMap<String, Expression>,

    #[serde(rename = "condition")]
    expression_raw: String,
    #[serde(flatten)]
    identifiers_raw: HashMap<String, Yaml>,
}

impl fmt::Debug for Detection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Detection")
            .field("expression", &self.expression_raw)
            .field("identifiers", &self.identifiers_raw)
            .finish()
    }
}

impl<'de> Deserialize<'de> for Detection {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct DetectionVisitor;
        impl<'de> Visitor<'de> for DetectionVisitor {
            type Value = Detection;
            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("struct Detection")
            }
            fn visit_map<V>(self, mut map: V) -> Result<Detection, V::Error>
            where
                V: MapAccess<'de>,
            {
                let mut identifiers: HashMap<String, Expression> = HashMap::new();
                let mut identifiers_raw: HashMap<String, Yaml> = HashMap::new();
                let mut expression = None;
                while let Some(key) = map.next_key::<String>()? {
                    match key.as_ref() {
                        "condition" => {
                            if expression.is_some() {
                                return Err(de::Error::duplicate_field("condition"));
                            }
                            expression = Some(map.next_value::<String>()?);
                        }
                        _ => {
                            if identifiers.get(&key).is_some() {
                                return Err(de::Error::custom(format_args!(
                                    "duplicate field `{}`",
                                    key
                                )));
                            }
                            let v: Yaml = map.next_value()?;
                            identifiers.insert(
                                key.to_string(),
                                parser::parse_identifier(&v).map_err(|e| {
                                    de::Error::custom(format!(
                                        "failed to parse identifier - {:?}",
                                        e
                                    ))
                                })?,
                            );
                            identifiers_raw.insert(key.to_string(), v.clone());
                        }
                    }
                }
                let expression_raw =
                    expression.ok_or_else(|| de::Error::missing_field("condition"))?;
                let tokens = match expression_raw.tokenise() {
                    Ok(tokens) => tokens,
                    Err(err) => {
                        return Err(de::Error::custom(format_args!(
                            "invalid value: condition, failed to tokenise - {}",
                            err
                        )));
                    }
                };

                // Loop through the tokens making sure that all identifiers are present, this is a
                // pain because we need to ignore fields... For now we can just check for misc
                // symbol prefix and skip those if present
                let mut i = 0;
                for token in &tokens {
                    if i > 1 {
                        if let Token::Modifier(m) = &tokens[i - 2] {
                            match m {
                                ModSym::Int | ModSym::Not | ModSym::Str => {
                                    i += 1;
                                    continue;
                                }
                            }
                        }
                    }
                    if let Token::Identifier(id) = token {
                        if !identifiers.contains_key(id) {
                            return Err(de::Error::custom(format_args!(
                                "invalid condition: identifier not found - {}",
                                id
                            )));
                        }
                    }
                    i += 1;
                }

                let expression = match parser::parse(&tokens) {
                    Ok(expression) => expression,
                    Err(err) => {
                        return Err(de::Error::custom(format_args!(
                            "invalid value: condition, failed to parse - {}",
                            err
                        )));
                    }
                };
                if !expression.is_solvable() {
                    return Err(de::Error::custom(format_args!(
                        "invalid value: condition, not solveable - {}",
                        expression
                    )));
                }
                Ok(Detection {
                    expression,
                    identifiers,
                    expression_raw,
                    identifiers_raw,
                })
            }
        }
        const FIELDS: &[&str] = &["identifiers", "condition"];
        deserializer.deserialize_struct("Detection", FIELDS, DetectionVisitor)
    }
}

// TODO: Should probably just remove this and have an optimise on the Rule where we parse optimise
// options...
/// A `RuleLoader` can be used to create a `Rule` with custom configuration.
pub struct RuleLoader {
    coalesce: bool,
    rewrite: bool,
    shake: bool,
}

impl Default for RuleLoader {
    fn default() -> Self {
        Self {
            coalesce: false,
            rewrite: false,
            shake: true,
        }
    }
}

impl RuleLoader {
    /// Create a new loader for configuring how a Rule is loaded.
    pub fn new() -> Self {
        RuleLoader::default()
    }

    /// Loads the rule using the configuration set on the loader.
    pub fn load(self, path: &Path) -> crate::Result<Rule> {
        let contents = fs::read_to_string(path).map_err(crate::error::rule_invalid)?;
        self.from_str(&contents)
    }

    /// Loads the rule from a YAML string using the configuration set on the loader.
    pub fn from_str(self, s: &str) -> crate::Result<Rule> {
        let rule: Rule = serde_yaml::from_str(s).map_err(crate::error::rule_invalid)?;
        // FIXME: If we debug with these there will be confusion, as the raw values will be
        // incorrect.
        let mut detection = rule.detection;
        if self.coalesce {
            detection.expression =
                optimiser::coalesce(detection.expression, &detection.identifiers);
            detection.identifiers.clear();
        }
        if self.shake {
            detection.expression = optimiser::shake(detection.expression, self.rewrite);
            detection.identifiers = detection
                .identifiers
                .into_iter()
                .map(|(k, v)| (k, optimiser::shake(v, self.rewrite)))
                .collect();
        }
        Ok(Rule {
            detection,
            true_negatives: rule.true_negatives,
            true_positives: rule.true_positives,
        })
    }

    /// Loads the rule from a YAML string using the configuration set on the loader.
    pub fn from_value(self, value: serde_yaml::Value) -> crate::Result<Rule> {
        let rule: Rule = serde_yaml::from_value(value).map_err(crate::error::rule_invalid)?;
        // FIXME: If we debug with these there will be confusion, as the raw values will be
        // incorrect.
        let mut detection = rule.detection;
        if self.coalesce {
            detection.expression =
                optimiser::coalesce(detection.expression, &detection.identifiers);
            detection.identifiers.clear();
        }
        if self.shake {
            detection.expression = optimiser::shake(detection.expression, self.rewrite);
            detection.identifiers = detection
                .identifiers
                .into_iter()
                .map(|(k, v)| (k, optimiser::shake(v, self.rewrite)))
                .collect();
        }
        Ok(Rule {
            detection,
            true_negatives: rule.true_negatives,
            true_positives: rule.true_positives,
        })
    }

    /// Allow Tau to coalesce the identifier's expressions into the condition.
    ///
    /// This allows for the identifier's expressions to be embedded for increased speed at the cost
    /// of space.
    ///
    /// This option is disabled by default.
    pub fn coalesce(mut self, yes: bool) -> Self {
        self.coalesce = yes;
        self
    }

    /// Allow Tau to rewrite inefficient string searches.
    ///
    /// This option is disabled by default. This option is only applied if shaking is enabled.
    pub fn rewrite(mut self, yes: bool) -> Self {
        self.rewrite = yes;
        self
    }

    /// Allow Tau to optimise the rule when loaded.
    ///
    /// This option is enabled by default.
    pub fn shake(mut self, yes: bool) -> Self {
        self.shake = yes;
        self
    }
}

/// A rule used by the solver to evaluate a `Document`.
///
/// A rule contains the detection logic, along with the true positive and negative tests. The
/// inclusion of these basic test allows for a basic level of verification to be ensured.
///
/// Rules are written in YAML and have a simple but powerful syntax.
///
/// # Syntax
///
/// There are two parts to a rule's logic: the condition & the identifiers.
///
/// ## Condition
///
/// The condition is the main expression and describes the top level logic for the rule. It can be
/// comprised of the following:
///
/// <table>
///     <thead>
///         <tr>
///             <th>Expression</th>
///             <th>Description</th>
///         </tr>
///     </thead>
///     <tbody>
///         <tr>
///             <td>_ <code>and</code> _</td>
///             <td>
///                 <span>The logical conjunction of two operands, where the operands are any of the following:</span>
///                 <ul>
///                     <li>
///                         <code>expression</code><span>: a nested expression.</span>
///                     </li>
///                     <li>
///                         <code>identifier</code><span>: a key that matches an identifier in the detection block.</span>
///                     </li>
///                 </ul>
///             </td>
///         </tr>
///         <tr>
///             <td>_ <code>or</code> _</td>
///             <td>
///                 <span>The logical disjunction of two operands, where the operands are any of the following:</span>
///                 <ul>
///                     <li>
///                         <code>expression</code><span>: a nested expression.</span>
///                     </li>
///                     <li>
///                         <code>identifier</code><span>: a key that matches an identifier in the detection block.</span>
///                     </li>
///                 </ul>
///             </td>
///         </tr>
///         <tr>
///             <td>_ <code>==</code> _</td>
///             <td>
///                 <span>The equality comparison of two operands, where the operands are any of the following:</span>
///                 <ul>
///                     <li>
///                         <code>integer</code><span>: an integer.</span>
///                     </li>
///                     <li>
///                         <code>string</code><span>: a string.</span>
///                     </li>
///                     <li>
///                         <code>int(field)</code><span>: a field that should be cast as an
///                         integer.</span>
///                     </li>
///                     <li>
///                         <code>str(field)</code><span>: a field that should be cast as a
///                         string.</span>
///                     </li>
///                 </ul>
///             </td>
///         </tr>
///         <tr>
///             <td>_ <code>&gt</code> _</td>
///             <td>
///                 <span>The greater than comparison of two operands, where the operands are any of the following:</span>
///                 <ul>
///                     <li>
///                         <code>integer</code><span>: an integer.</span>
///                     </li>
///                     <li>
///                         <code>int(field)</code><span>: a field that should be cast as an
///                         integer.</span>
///                     </li>
///                 </ul>
///             </td>
///         </tr>
///         <tr>
///             <td>_ <code>&gt=</code> _</td>
///             <td>
///                 <span>The greater than or equal comparison of two operands, where the operands are any of the following:</span>
///                 <ul>
///                     <li>
///                         <code>integer</code><span>: an integer.</span>
///                     </li>
///                     <li>
///                         <code>int(field)</code><span>: a field that should be cast as an
///                         integer.</span>
///                     </li>
///                 </ul>
///             </td>
///         </tr>
///         <tr>
///             <td>_ <code>&lt</code> _</td>
///             <td>
///                 <span>The less than comparison of two operands, where the operands are any of the following:</span>
///                 <ul>
///                     <li>
///                         <code>integer</code><span>: an integer.</span>
///                     </li>
///                     <li>
///                         <code>int(field)</code><span>: a field that should be cast as an
///                         integer.</span>
///                     </li>
///                 </ul>
///             </td>
///         </tr>
///         <tr>
///             <td>_ <code>&lt=</code> _</td>
///             <td>
///                 <span>The less than or equal comparison of two operands, where the operands are any of the following:</span>
///                 <ul>
///                     <li>
///                         <code>integer</code><span>: an integer.</span>
///                     </li>
///                     <li>
///                         <code>int(field)</code><span>: a field that should be cast as an
///                         integer.</span>
///                     </li>
///                 </ul>
///             </td>
///         </tr>
///         <tr>
///             <td><code>all(i)</code></td>
///             <td>
///                 <span>An identifier mutator that evaluates to true only if all conditions for identifier <code>i</code> match.</span>
///             </td>
///         </tr>
///         <tr>
///             <td><code>not</code> _</td>
///             <td>
///                 <span>Negate the result of an expression.</span>
///                 <span>NOTE: This will only negate a result that is true or false, it will
///                 noop if the result is missing.</span>
///             </td>
///         </tr>
///         <tr>
///             <td><code style="white-space:nowrap">of(i, x)</code></td>
///             <td>
///                 <span>An identifier mutator that evaluates to true only if a minimum of <code>x</code> conditions for identifier <code>i</code> match.</span>
///             </td>
///         </tr>
///     </tbody>
/// </table>
///
/// # Identifiers
///
/// Identifiers are used to describe the matching logic for the values contained within documents.
/// These are then collected by the condition in order to create a rule that can be used to tag a
/// document.
///
/// Due to the nature of an identifier, they are essentially just variations on key/value
/// pairs. The following variations are supported, where mappings are treated as conjunctions and
/// sequences are treated as disjunctions:
///
/// ```text
/// # K/V Pairs
/// IDENTIFIER:
///     KEY: MATCH
///
/// # K/V Pairs with multiple matches
/// IDENTIFIER:
///     KEY:
///     - MATCH_0
///     - MATCH_1
///
/// # K/V Pairs (Grouped)
/// IDENTIFIER:
///     - KEY: MATCH
///
/// # K/V Pairs (Nested)
/// IDENTIFIER:
///     KEY:
///         KEY: MATCH
/// ```
///
/// Identifiers are unique keys that can be referenced in the `condition`.
///
/// Keys are used to get the values from documents. Keys can be wrapped in the following modifiers:
///
/// <table>
///     <thead>
///         <tr>
///             <th>Expression</th>
///             <th>Description</th>
///         </tr>
///     </thead>
///     <tbody>
///         <tr>
///             <td><code>all(k)</code></td>
///             <td>
///                 <span>A key mutator that evaluates to true only if all matches for keys <code>k</code> match.</span>
///             </td>
///         </tr>
///         <tr>
///             <td><code style="white-space:nowrap">of(k, x)</code></td>
///             <td>
///                 <span>A key mutator that evaluates to true only if a minimum of <code>x</code> matches for key <code>k</code> match.</span>
///             </td>
///         </tr>
///     </tbody>
/// </table>
///
/// Matches are the expressions which are evaluated against values returned by keys. They support the
/// following syntax:
///
/// <table>
///     <thead>
///         <tr>
///             <th>Expression</th>
///             <th>Description</th>
///         </tr>
///     </thead>
///     <tbody>
///         <tr>
///             <td><code>foo</code></td>
///             <td><span>An exact match</span></td>
///         </tr>
///         <tr>
///             <td><code>foo*</code></td>
///             <td><span>Starts with</span></td>
///         </tr>
///         <tr>
///             <td><code>*foo</code></td>
///             <td><span>Ends with</span></td>
///         </tr>
///         <tr>
///             <td><code>*foo*</code></td>
///             <td><span>Contains</span></td>
///         </tr>
///         <tr>
///             <td><code>?foo</code></td>
///             <td><span>Regex</span></td>
///         </tr>
///         <tr>
///             <td><code>i</code>_</td>
///             <td><span>A prefix to convert the match into a case insensitive match.</span></td>
///         </tr>
///     </tbody>
/// </table>
///
/// To escape any of the above in order to achieve literal string matching, combinations of `'` and `"` can be used.
///
/// # Examples
///
/// Here is a very simple rule example:
///
/// ```text
/// detection:
///   A:
///     foo: "foo*"
///     bar: "*bar"
///   B:
///     foobar:
///     - foobar
///     - foobaz
///
///   condition: A and B
///
/// true_positives:
/// - foo: foobar
///   bar: foobar
///   foobar: foobar
///
/// true_negatives:
/// - foo: bar
///   bar: foo
///   foobar: barfoo
/// ```
///
/// Here is a slightly more complex rule example:
///
/// ```text
/// detection:
///   A:
///     all(phrase):
///     - "*quick*"
///     - "*brown*"
///   B:
///     phrase: ibear
///
///   condition: A and not B
///
/// true_positives:
/// - phrase: the quick brown fox
///
/// true_negatives:
/// - foo: the quick brown BEAR
/// ```
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Rule {
    pub detection: Detection,
    pub true_positives: Vec<Yaml>,
    pub true_negatives: Vec<Yaml>,
}

impl Rule {
    /// Creates a `RuleLoader` to configure a `Rule`.
    ///
    /// This is the same as `RuleLoader::new()`.
    pub fn loader() -> RuleLoader {
        RuleLoader::default()
    }

    /// Load a rule from a YAML file.
    pub fn load(path: &Path) -> crate::Result<Self> {
        RuleLoader::new().load(path)
    }

    /// Load a rule from a YAML string.
    pub fn from_str(s: &str) -> crate::Result<Self> {
        RuleLoader::new().from_str(s)
    }

    /// Load a rule from a YAML Value.
    pub fn from_value(value: serde_yaml::Value) -> crate::Result<Self> {
        RuleLoader::new().from_value(value)
    }

    /// Allow Tau to optimise the rule when loaded.
    ///
    /// # Options
    /// - coalesce: tau will caalesce the identifier's expressions into the condition.
    /// - rewrite: tau will try to rewrite inefficient string searches.
    pub fn optimise(mut self, coalesce: bool, rewrite: bool) -> Self {
        if coalesce {
            self.detection.expression =
                optimiser::coalesce(self.detection.expression, &self.detection.identifiers);
            self.detection.identifiers.clear();
        }
        self.detection.expression = optimiser::shake(self.detection.expression, rewrite);
        self.detection.identifiers = self
            .detection
            .identifiers
            .into_iter()
            .map(|(k, v)| (k, optimiser::shake(v, rewrite)))
            .collect();
        self
    }

    /// Evaluates the rule against the provided `Document`, returning true if it has matched.
    #[inline]
    pub fn matches(&self, document: &dyn Document) -> bool {
        solver::solve(&self.detection, document)
    }

    /// Validates the rule's detection logic against the provided true positives and negatives.
    pub fn validate(&self) -> crate::Result<bool> {
        let mut errors = vec![];
        for test in &self.true_positives {
            if !(solver::solve(&self.detection, test.as_mapping().unwrap())) {
                errors.push(format!(
                    "failed to validate true positive check '{:?}'",
                    test
                ));
            }
        }
        for test in &self.true_negatives {
            if solver::solve(&self.detection, test.as_mapping().unwrap()) {
                errors.push(format!(
                    "failed to validate true negative check '{:?}'",
                    test
                ));
            }
        }
        if !errors.is_empty() {
            return Err(crate::Error::new(crate::error::Kind::Validation).with(errors.join(";")));
        }
        Ok(true)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rule() {
        let rule = r#"
        detection:
          A:
            foo: 'foo*'
            bar: '*bar'
          B:
            foobar:
            - foobar
            - foobaz

          condition: A and B

        true_positives:
        - foo: foobar
          bar: foobar
          foobar: foobar

        true_negatives:
        - foo: bar
          bar: foo
          foobar: barfoo
        "#;
        let rule = Rule::from_str(rule).unwrap();
        assert_eq!(rule.validate().unwrap(), true);
    }
}
