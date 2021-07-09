use crate::value::{Object, Value};

/// A `Document` that can be evaluated by the solver.
///
/// Rules are solved against Documents, and thus to solve against a data type it must implement
/// `Document`. Implementing `Object` gives this trait for free. Most of the time this trait will
/// not need to be implmented, it is there to enable complex use cases.
///
/// # Implementations
///
/// The implementation for `Object` will just pass the `find` call along to the `Object`
/// implementation of find. If for some reason this was undesired or just not needed below is an
/// example of how to implement `Document`.
///
/// ```
/// use std::borrow::Cow;
///
/// use tau_engine::{Document, Value};
///
/// struct Foo {
///     bar: String,
///     baz: String,
/// }
///
/// impl Document for Foo {
///     fn find(&self, key: &str) -> Option<Value<'_>> {
///         match key {
///             "bar" => Some(Value::String(Cow::Borrowed(&self.bar))),
///             "baz" => Some(Value::String(Cow::Borrowed(&self.baz))),
///             _ => None,
///         }
///     }
/// }
/// ```
pub trait Document {
    /// Looks for a `Value` by key and returns it if found.
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
