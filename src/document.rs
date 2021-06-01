use crate::value::{Object, Value};

pub trait Document {
    fn get_value(&self, key: &str) -> Option<Value<'_>>;
}

impl Document for &dyn Object {
    #[inline]
    fn get_value(&self, key: &str) -> Option<Value<'_>> {
        self.find(key)
    }
}

impl<O: Object> Document for O {
    #[inline]
    fn get_value(&self, key: &str) -> Option<Value<'_>> {
        self.find(key)
    }
}
