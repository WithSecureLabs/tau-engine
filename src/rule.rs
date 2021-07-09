use std::collections::HashMap;
use std::fmt;

use serde::de::{self, Deserializer, MapAccess, Visitor};
use serde::{Deserialize, Serialize};
use serde_yaml::Value as Yaml;

use crate::document::Document;
use crate::parser::{self, Expression};
use crate::solver;
use crate::tokeniser::Tokeniser;

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
                let expression = match parser::parse(&tokens) {
                    Ok(expression) => expression,
                    Err(err) => {
                        return Err(de::Error::custom(format_args!(
                            "invalid value: condition, failed to parse - {}",
                            err
                        )));
                    }
                };
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

/// A Rule used by the solver to evaluate a `Document`.
///
/// A rule contains the detection logic, along with the true positive and negative tests. The
/// inclusion of these basic test allows for a basic level of verification to be ensured.
///
/// Rules are written in YAML and have a simple but powerful syntax.
///
/// # Syntax
///
/// There are two parts to a rule's logic: the condition block & the identifier blocks.
///
/// ## Condition
///
/// The condition is the main expression and describes the top level logic for the rule. It can be
/// comprised of the following:
///
/// <table>
///     <thead>
///         <tr>
///             <th>Syntax</th>
///             <th>Description</th>
///         </tr>
///     </thead>
///     <tbody>
///         <tr>
///             <td>_ <code>and</code> _</td>
///             <td>
///                 <span>This allows for the logical conjunction of two operands, where the
///                 operands are any of the following:</span>
///                 <ul>
///                     <li>
///                         <code>identifier</code><span>: a key that matches an identifier in the detection block.</span>
///                     </li>
///                 </ul>
///             </tr>
///     </tbody>
/// </table>
///
/// # Examples
///
/// Here is a very simple rule.
///
/// ```text
/// detection:
///   A:
///     foo: foo*
///     bar: *bar
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
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Rule {
    pub detection: Detection,
    pub true_positives: Vec<Yaml>,
    pub true_negatives: Vec<Yaml>,
}

impl Rule {
    #[inline]
    pub fn matches(&self, document: &dyn Document) -> bool {
        solver::solve(&self.detection, document)
    }

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
