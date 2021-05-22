use std::collections::HashMap;
use std::fmt;

use serde::de::{self, Deserializer, MapAccess, Visitor};
use serde::{Deserialize, Serialize};
use serde_yaml::Value;

use crate::parser::{self, Expression};
use crate::solver::{self, SearchIdentifier};
use crate::tokeniser::Tokeniser;

#[derive(Clone, Serialize)]
pub struct Detection {
    #[serde(skip_serializing)]
    pub expression: Expression,
    #[serde(skip_serializing)]
    pub identifiers: HashMap<String, SearchIdentifier>,

    #[serde(rename = "condition")]
    expression_raw: String,
    #[serde(flatten)]
    identifiers_raw: HashMap<String, Value>,
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
                let mut identifiers: HashMap<String, SearchIdentifier> = HashMap::new();
                let mut identifiers_raw: HashMap<String, Value> = HashMap::new();
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
                            let v: Value = map.next_value()?;
                            identifiers_raw.insert(key.to_string(), v.clone());
                            let value = match v {
                                Value::Sequence(v) => match SearchIdentifier::from_sequence(&v) {
                                    Ok(value) => value,
                                    Err(err) => {
                                        return Err(de::Error::custom(format_args!(
                                            "invalid value: {}",
                                            err
                                        )));
                                    }
                                },
                                Value::Mapping(m) => match SearchIdentifier::from_mapping(&m) {
                                    Ok(value) => value,
                                    Err(err) => {
                                        return Err(de::Error::custom(format_args!(
                                            "invalid value: {}",
                                            err
                                        )));
                                    }
                                },
                                Value::Bool(_) => {
                                    return Err(de::Error::custom(format_args!(
                                        "invalid type: bool, expected map or sequence"
                                    )));
                                }
                                Value::Number(_) => {
                                    return Err(de::Error::custom(format_args!(
                                        "invalid type: number, expected map or sequence"
                                    )));
                                }
                                Value::Null => {
                                    return Err(de::Error::custom(format_args!(
                                        "invalid type: null, expected map or sequence"
                                    )));
                                }
                                Value::String(_) => {
                                    return Err(de::Error::custom(format_args!(
                                        "invalid type: string, expected map or sequence"
                                    )));
                                }
                            };
                            identifiers.insert(key.to_string(), value);
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
                    identifiers,
                    identifiers_raw,
                    expression,
                    expression_raw,
                })
            }
        }
        const FIELDS: &[&str] = &["identifiers", "condition"];
        deserializer.deserialize_struct("Detection", FIELDS, DetectionVisitor)
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Rule {
    pub detection: Detection,
    pub true_positives: Vec<Value>,
    pub true_negatives: Vec<Value>,
}

impl Rule {
    pub fn validate(&self) -> crate::Result<bool> {
        //let mut errors = vec![];
        for test in &self.true_positives {
            //// FIXME: Remove the need for JSON Value...
            //let s = serde_json::to_string(&test)
            //    .map_err(|e| crate::Error::new(crate::error::Kind::InvalidJson).with(e))?;
            //let t: serde_json::Value = serde_json::from_str(&s)
            //    .map_err(|e| crate::Error::new(crate::error::Kind::InvalidJson).with(e))?;
            //if !(solver::solve_expression(
            //    &self.detection.expression,
            //    &self.detection.identifiers,
            //    (&t, None),
            //)?) {
            //    errors.push(format!(
            //        "failed to validate true positive check '{:?}'",
            //        test
            //    ));
            //}
        }
        for test in &self.true_negatives {
            //// FIXME: Remove the need for JSON Value...
            //let s = serde_json::to_string(&test)
            //    .map_err(|e| crate::Error::new(crate::error::Kind::InvalidJson).with(e))?;
            //let t: serde_json::Value = serde_json::from_str(&s)
            //    .map_err(|e| crate::Error::new(crate::error::Kind::InvalidJson).with(e))?;
            //if solver::solve_expression(
            //    &self.detection.expression,
            //    &self.detection.identifiers,
            //    (&t, None),
            //)? {
            //    errors.push(format!(
            //        "failed to validate true negative check '{:?}'",
            //        test
            //    ));
            //}
        }
        //if !errors.is_empty() {
        //    return Err(crate::Error::new(crate::error::Kind::Validation).with(errors.join(";")));
        //}
        Ok(true)
    }
}
