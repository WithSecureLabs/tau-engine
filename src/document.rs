use crate::value::{Object, Value};

pub trait Document {
    fn find(&self, key: &str) -> Option<Value<'_>>;
}

impl Document for &dyn Object {
    #[inline]
    fn find(&self, key: &str) -> Option<Value<'_>> {
        Object::find(*self, key)
    }
}

impl<O: Object> Document for O {
    #[inline]
    fn find(&self, key: &str) -> Option<Value<'_>> {
        Object::find(self, key)
    }
}
