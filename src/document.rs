use crate::value::Value;

pub trait Document {
    fn get_value(&self, key: &str) -> Option<Value<'_>>;
}
