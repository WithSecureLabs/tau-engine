use std::borrow::Cow;

use serde_json::map::Map;
pub use serde_json::{Number, Value as Json};

use crate::document::Document;
use crate::value::{AsValue, Object, Value};

impl AsValue for Json {
    #[inline]
    fn as_value_ref(&self) -> Value<'_> {
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
            Self::Object(o) => Value::Object(o),
            Self::Array(a) => Value::Array(a),
        }
    }
}

impl Document for Map<String, Json> {
    fn get_value(&self, key: &str) -> Option<Value<'_>> {
        let mut v: Option<Value<'_>> = None;
        for k in key.split('.') {
            match v {
                Some(value) => match value {
                    Value::Object(value) => v = value.get(k),
                    _ => return None,
                },
                None => {
                    v = match <Self as Object>::get(self, k) {
                        Some(v) => Some(v),
                        None => return None,
                    }
                }
            }
        }
        return v;
    }
}

impl Object for Map<String, Json> {
    #[inline]
    fn get(&self, key: &str) -> Option<Value<'_>> {
        self.get(key).map(|v| v.as_value_ref())
    }

    #[inline]
    fn keys(&self) -> Vec<&str> {
        self.keys().map(|k| k.as_str()).collect()
    }

    #[inline]
    fn len(&self) -> usize {
        self.len()
    }
}
