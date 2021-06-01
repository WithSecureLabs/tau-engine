use std::borrow::Cow;

pub use serde_yaml::{Mapping, Number, Value as Yaml};

use crate::value::{AsValue, Object, Value};

impl AsValue for Yaml {
    #[inline]
    fn as_value(&self) -> Value<'_> {
        match self {
            Self::Null => Value::Null,
            Self::String(s) => Value::String(Cow::Borrowed(s)),
            Self::Number(n) => {
                if n.is_u64() {
                    Value::UInt(n.as_u64().unwrap())
                } else if n.is_i64() {
                    Value::Int(n.as_i64().unwrap())
                } else if n.is_f64() {
                    Value::Float(n.as_f64().unwrap())
                } else {
                    unreachable!()
                }
            }
            Self::Bool(b) => Value::Bool(*b),
            Self::Mapping(o) => Value::Object(o),
            Self::Sequence(s) => Value::Array(s),
        }
    }
}

impl Object for Mapping {
    #[inline]
    fn get(&self, key: &str) -> Option<Value<'_>> {
        self.get(&Yaml::String(key.to_string()))
            .map(|v| v.as_value())
    }

    #[inline]
    fn keys(&self) -> Vec<Cow<'_, str>> {
        self.iter()
            .filter_map(|(k, _)| k.as_value().to_string().map(|s| Cow::Owned(s)))
            .collect()
    }

    #[inline]
    fn len(&self) -> usize {
        self.len()
    }
}
